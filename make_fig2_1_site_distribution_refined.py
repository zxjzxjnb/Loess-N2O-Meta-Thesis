#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Rebuild Fig. 2-1 as a publication-style site distribution map.

The command-line R runtime is not always available on this machine, so this
script uses the bundled Python runtime plus Pillow. It reads the same source
spreadsheet and the local R `maps/mapdata` binary map files already present in
the project.
"""

from __future__ import annotations

import csv
import math
import re
import struct
import zipfile
from pathlib import Path
from xml.etree import ElementTree

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_DIR = Path(__file__).resolve().parent
INPUT_XLSX = PROJECT_DIR / "TableS1.xlsx"
OUTPUT_DIR = PROJECT_DIR / "output" / "figures"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

PNG_FILE = OUTPUT_DIR / "Fig2-1_site_distribution_CN.png"
TIFF_FILE = OUTPUT_DIR / "Fig2-1_site_distribution_CN.tiff"
CSV_FILE = OUTPUT_DIR / "Fig2-1_site_points_used.csv"

CHINA_MAP = PROJECT_DIR / "Rlib" / "mapdata" / "mapdata" / "china.L"
WORLD_MAP = PROJECT_DIR / "Rlib" / "maps" / "mapdata" / "world.L"
LOESS_KMZ = PROJECT_DIR / "data" / "geo" / "LoessPlateauRegion.kmz"

WIDTH, HEIGHT = 4488, 3307  # 190 mm x 140 mm at 600 dpi
DPI = 600
PANEL = (360, 150, 4320, 2830)

XLIM = (99.6, 115.8)
YLIM = (33.2, 41.9)

FONT_REGULAR = Path("/System/Library/Fonts/STHeiti Light.ttc")
FONT_BOLD = Path("/System/Library/Fonts/STHeiti Medium.ttc")
FONT_LATIN = Path("/System/Library/Fonts/Supplemental/Arial.ttf")

CROP_LOOKUP = {
    "spring maize": "春玉米",
    "winter wheat": "冬小麦",
    "spring wheat": "春小麦",
    "apple": "苹果园",
    "potato": "马铃薯",
}
TARGET_KEYS = ["spring maize", "winter wheat", "spring wheat", "apple"]
TARGET_LABELS = [CROP_LOOKUP[key] for key in TARGET_KEYS]

COLORS = {
    "春玉米": "#256FB3",
    "冬小麦": "#D95F4F",
    "春小麦": "#F2A12E",
    "苹果园": "#3A9C99",
}
SHAPES = {
    "春玉米": "circle",
    "冬小麦": "triangle",
    "春小麦": "square",
    "苹果园": "diamond",
}


def font(path: Path, size: int) -> ImageFont.FreeTypeFont:
    return ImageFont.truetype(str(path), size=size)


F_AXIS = font(FONT_LATIN, 58)
F_SMALL = font(FONT_LATIN, 35)
F_SMALL_BOLD = font(FONT_LATIN, 38)
F_LABEL = font(FONT_REGULAR, 40)
F_LABEL_BOLD = font(FONT_BOLD, 50)
F_LEGEND = font(FONT_REGULAR, 38)
F_LEGEND_TITLE = font(FONT_BOLD, 42)
F_NORTH = font(FONT_LATIN, 72)


def text_size(draw: ImageDraw.ImageDraw, text: str, fnt: ImageFont.FreeTypeFont) -> tuple[int, int]:
    box = draw.textbbox((0, 0), text, font=fnt)
    return box[2] - box[0], box[3] - box[1]


def parse_coordinate(value: object) -> float:
    if pd.isna(value):
        return np.nan
    text = str(value).strip()
    if not text:
        return np.nan
    try:
        return float(text)
    except ValueError:
        pass

    cleaned = re.sub(r"[°º′'″\"NSEWnsew\s]", "", text).replace("，", ".")
    parts = [part for part in cleaned.split(".") if part]
    if len(parts) >= 3 and all(re.fullmatch(r"[0-9]+", part) for part in parts):
        decimal_tail = float("0." + "".join(parts[1:]))
        return float(parts[0]) + decimal_tail / 60.0
    try:
        return float(cleaned)
    except ValueError:
        return np.nan


def build_site_table() -> pd.DataFrame:
    raw = pd.read_excel(INPUT_XLSX, sheet_name=0, dtype=str)
    fill_cols = ["Provice", "Latitude", "Longitude", "Crop", "References"]
    raw[fill_cols] = raw[fill_cols].ffill()

    data = raw.assign(
        crop_key=raw["Crop"].astype(str).str.strip().str.lower(),
        latitude=raw["Latitude"].map(parse_coordinate),
        longitude=raw["Longitude"].map(parse_coordinate),
    )
    data["crop_label"] = data["crop_key"].map(CROP_LOOKUP)

    sites = (
        data.dropna(subset=["References", "crop_key", "crop_label", "latitude", "longitude"])
        .drop_duplicates(["References", "crop_key", "crop_label", "Provice", "longitude", "latitude"])
        .query("crop_key in @TARGET_KEYS")
        .copy()
    )
    sites["crop_label"] = pd.Categorical(sites["crop_label"], categories=TARGET_LABELS, ordered=True)
    sites = sites.sort_values(["longitude", "latitude", "crop_label", "References"]).reset_index(drop=True)

    grouped = sites.groupby(["longitude", "latitude"], sort=False, observed=True)
    overlap_n = grouped["References"].transform("size")
    overlap_i = grouped.cumcount() + 1
    angle = 2 * math.pi * (overlap_i - 1) / np.maximum(overlap_n, 1)
    offset = np.where(overlap_n > 1, 0.115, 0.0)
    sites["overlap_n"] = overlap_n.astype(int)
    sites["overlap_i"] = overlap_i.astype(int)
    sites["plot_x"] = sites["longitude"] + offset * np.cos(angle)
    sites["plot_y"] = sites["latitude"] + offset * np.sin(angle)
    return sites


def read_maps_l(path: Path) -> list[dict[str, object]]:
    data = path.read_bytes()
    map_type, n_records = struct.unpack("<2i", data[:8])
    if map_type != 2:
        raise ValueError(f"Unsupported map type in {path}: {map_type}")

    records = []
    offsets = []
    for index in range(n_records):
        start = 8 + index * 28
        offset, meta, _unused = struct.unpack("<3i", data[start : start + 12])
        bbox_rad = struct.unpack("<4f", data[start + 12 : start + 28])
        bbox = tuple(value * 180.0 / math.pi for value in bbox_rad)
        records.append({"offset": offset, "meta": meta, "bbox": bbox})
        offsets.append(offset)

    offsets.append(len(data))
    segments = []
    for index, record in enumerate(records):
        start, end = offsets[index], offsets[index + 1]
        raw = data[start:end]
        if len(raw) < 8:
            continue
        values = struct.unpack("<" + "f" * (len(raw) // 4), raw[: len(raw) - len(raw) % 4])
        coords = [
            (values[i] * 180.0 / math.pi, values[i + 1] * 180.0 / math.pi)
            for i in range(0, len(values) - 1, 2)
            if math.isfinite(values[i]) and math.isfinite(values[i + 1])
        ]
        if len(coords) >= 2:
            segments.append({"coords": coords, "bbox": record["bbox"], "meta": record["meta"]})
    return segments


def read_loess_boundary(path: Path) -> list[tuple[float, float]]:
    """Read the public Loess Plateau Region KMZ boundary as lon/lat pairs."""
    with zipfile.ZipFile(path) as archive:
        kml_text = archive.read("doc.kml").decode("utf-8")
    # The source KML uses an xsi-prefixed attribute without declaring xsi.
    kml_text = re.sub(r'\s+xsi:schemaLocation="[^"]*"', "", kml_text)
    root = ElementTree.fromstring(kml_text)
    namespace = {"k": "http://www.opengis.net/kml/2.2"}
    rings: list[list[tuple[float, float]]] = []
    for node in root.findall(".//k:coordinates", namespace):
        text = (node.text or "").strip()
        points: list[tuple[float, float]] = []
        for token in re.split(r"\s+", text):
            if not token:
                continue
            parts = token.split(",")
            if len(parts) >= 2:
                points.append((float(parts[0]), float(parts[1])))
        if points:
            rings.append(points)
    if not rings:
        raise ValueError(f"No coordinates found in {path}")
    return max(rings, key=len)


def intersects(bbox: tuple[float, float, float, float], xlim: tuple[float, float], ylim: tuple[float, float]) -> bool:
    xmin, ymin, xmax, ymax = bbox
    return xmax >= xlim[0] and xmin <= xlim[1] and ymax >= ylim[0] and ymin <= ylim[1]


def make_projector(width: int, height: int, xlim: tuple[float, float], ylim: tuple[float, float]):
    def project(lon: float, lat: float) -> tuple[int, int]:
        x = (lon - xlim[0]) / (xlim[1] - xlim[0]) * width
        y = (ylim[1] - lat) / (ylim[1] - ylim[0]) * height
        return int(round(x)), int(round(y))

    return project


def catmull_rom(points: list[tuple[float, float]], samples: int = 8, closed: bool = False) -> list[tuple[float, float]]:
    if len(points) < 4:
        return points
    pts = points[:]
    if closed:
        pts = [points[-1]] + points + [points[0], points[1]]
        limit = len(points) + 1
    else:
        pts = [points[0]] + points + [points[-1]]
        limit = len(points)

    out = []
    for i in range(1, limit):
        p0, p1, p2, p3 = pts[i - 1], pts[i], pts[i + 1], pts[i + 2]
        for step in range(samples):
            t = step / samples
            t2 = t * t
            t3 = t2 * t
            x = 0.5 * (
                2 * p1[0]
                + (-p0[0] + p2[0]) * t
                + (2 * p0[0] - 5 * p1[0] + 4 * p2[0] - p3[0]) * t2
                + (-p0[0] + 3 * p1[0] - 3 * p2[0] + p3[0]) * t3
            )
            y = 0.5 * (
                2 * p1[1]
                + (-p0[1] + p2[1]) * t
                + (2 * p0[1] - 5 * p1[1] + 4 * p2[1] - p3[1]) * t2
                + (-p0[1] + 3 * p1[1] - 3 * p2[1] + p3[1]) * t3
            )
            out.append((x, y))
    if closed:
        out.append(out[0])
    else:
        out.append(points[-1])
    return out


def draw_text_with_halo(
    draw: ImageDraw.ImageDraw,
    xy: tuple[int, int],
    text: str,
    fnt: ImageFont.FreeTypeFont,
    fill: str,
    anchor: str = "mm",
    halo: str = "white",
    stroke_width: int = 3,
    **kwargs: object,
) -> None:
    draw.text(xy, text, font=fnt, fill=fill, anchor=anchor, stroke_width=stroke_width, stroke_fill=halo, **kwargs)


def draw_marker(
    draw: ImageDraw.ImageDraw,
    x: int,
    y: int,
    shape: str,
    fill: str,
    size: int = 66,
    outline: str = "white",
    stroke: int = 5,
) -> None:
    half = size // 2
    if shape == "circle":
        draw.ellipse((x - half, y - half, x + half, y + half), fill=fill, outline=outline, width=stroke)
    elif shape == "square":
        draw.rectangle((x - half, y - half, x + half, y + half), fill=fill, outline=outline, width=stroke)
    elif shape == "triangle":
        points = [(x, y - half - 4), (x - half - 5, y + half), (x + half + 5, y + half)]
        draw.polygon(points, fill=fill)
        draw.line(points + [points[0]], fill=outline, width=stroke, joint="curve")
    elif shape == "diamond":
        points = [(x, y - half - 3), (x + half + 3, y), (x, y + half + 3), (x - half - 3, y)]
        draw.polygon(points, fill=fill)
        draw.line(points + [points[0]], fill=outline, width=stroke, joint="curve")


def draw_graticule(draw: ImageDraw.ImageDraw, project, width: int, height: int) -> None:
    for lon in range(100, 116, 3):
        x, _ = project(lon, YLIM[0])
        draw.line((x, 0, x, height), fill=(255, 255, 255, 105), width=2)
    for lat in range(34, 42, 2):
        _, y = project(XLIM[0], lat)
        draw.line((0, y, width, y), fill=(255, 255, 255, 105), width=2)


def draw_scale_bar(draw: ImageDraw.ImageDraw, project) -> None:
    lat = 34.18
    lon0 = 100.25
    lon100 = lon0 + 100.0 / (111.32 * math.cos(math.radians(lat)))
    lon200 = lon0 + 200.0 / (111.32 * math.cos(math.radians(lat)))
    x0, y = project(lon0, lat)
    x1, _ = project(lon100, lat)
    x2, _ = project(lon200, lat)
    bar_h = 34
    draw.rectangle((x0, y, x1, y + bar_h), fill="#1F1F1F", outline="#1F1F1F", width=2)
    draw.rectangle((x1, y, x2, y + bar_h), fill="#FFFFFF", outline="#1F1F1F", width=2)
    for x in (x0, x1, x2):
        draw.line((x, y, x, y + bar_h + 12), fill="#1F1F1F", width=2)
    label_y = y + bar_h + 22
    draw.text((x0, label_y), "0", font=F_SMALL, fill="#222222", anchor="mt")
    draw.text((x1, label_y), "100", font=F_SMALL, fill="#222222", anchor="mt")
    draw.text((x2, label_y), "200 km", font=F_SMALL, fill="#222222", anchor="mt")


def draw_north_arrow(draw: ImageDraw.ImageDraw, project) -> None:
    x, y_base = project(115.05, 39.45)
    _, y_tip = project(115.05, 40.50)
    draw.line((x, y_base, x, y_tip + 46), fill="#222222", width=8)
    head = [(x, y_tip), (x - 34, y_tip + 68), (x + 34, y_tip + 68)]
    draw.polygon(head, fill="#222222")
    draw.text((x, y_tip - 20), "N", font=F_NORTH, fill="#222222", anchor="mb")


def draw_legend(panel: Image.Image, sites: pd.DataFrame) -> None:
    draw = ImageDraw.Draw(panel, "RGBA")
    counts = sites["crop_label"].value_counts().to_dict()
    x, y, w, h = 70, 82, 450, 345
    draw.rounded_rectangle((x, y, x + w, y + h), radius=14, fill=(255, 255, 255, 232), outline=(80, 80, 80, 95), width=2)
    draw.text((x + 34, y + 30), "作物体系", font=F_LEGEND_TITLE, fill="#222222", anchor="la")
    row_y = y + 105
    for label in TARGET_LABELS:
        marker_x = x + 55
        marker_y = row_y + 8
        draw_marker(draw, marker_x, marker_y, SHAPES[label], COLORS[label], size=42, stroke=3)
        text = f"{label} (n={int(counts.get(label, 0))})"
        draw.text((x + 102, row_y - 14), text, font=F_LEGEND, fill="#222222", anchor="la")
        row_y += 58


def draw_inset(world_segments: list[dict[str, object]]) -> Image.Image:
    inset_w, inset_h = 900, 650
    xlim, ylim = (73, 136), (15, 54)
    inset = Image.new("RGBA", (inset_w, inset_h), "#E7F1F8")
    draw = ImageDraw.Draw(inset, "RGBA")
    project = make_projector(inset_w, inset_h, xlim, ylim)
    for segment in world_segments:
        bbox = segment["bbox"]  # type: ignore[assignment]
        if not intersects(bbox, xlim, ylim):
            continue
        pts = [project(lon, lat) for lon, lat in segment["coords"]]  # type: ignore[index]
        if len(pts) >= 2:
            draw.line(pts, fill="#AFAFA8", width=2)

    x0, y0 = project(XLIM[0], YLIM[1])
    x1, y1 = project(XLIM[1], YLIM[0])
    draw.rectangle((x0, y0, x1, y1), outline="#C92512", width=10)
    draw.rectangle((0, 0, inset_w - 1, inset_h - 1), outline="#333333", width=4)
    return inset


def write_site_csv(sites: pd.DataFrame) -> None:
    out = sites.assign(crop_label=sites["crop_label"].astype(str))[
        ["References", "crop_label", "Provice", "longitude", "latitude", "plot_x", "plot_y", "overlap_n"]
    ].rename(
        columns={
            "References": "参考文献",
            "crop_label": "作物体系",
            "Provice": "省份",
            "longitude": "经度",
            "latitude": "纬度",
            "plot_x": "绘图经度",
            "plot_y": "绘图纬度",
            "overlap_n": "同坐标记录数",
        }
    )
    out.to_csv(CSV_FILE, index=False, encoding="utf-8-sig", quoting=csv.QUOTE_MINIMAL)


def main() -> None:
    sites = build_site_table()
    china_segments = read_maps_l(CHINA_MAP)
    world_segments = read_maps_l(WORLD_MAP)

    panel_w = PANEL[2] - PANEL[0]
    panel_h = PANEL[3] - PANEL[1]
    project = make_projector(panel_w, panel_h, XLIM, YLIM)

    panel = Image.new("RGBA", (panel_w, panel_h), "#EDF5FA")
    draw = ImageDraw.Draw(panel, "RGBA")
    draw_graticule(draw, project, panel_w, panel_h)

    for segment in china_segments:
        bbox = segment["bbox"]  # type: ignore[assignment]
        if not intersects(bbox, XLIM, YLIM):
            continue
        pts = [project(lon, lat) for lon, lat in segment["coords"]]  # type: ignore[index]
        # The maps/mapdata records are boundary strokes, not reliable filled
        # rings. Drawing them as polygons creates artificial closing chords.
        if len(pts) >= 2:
            draw.line(pts, fill="#B8B8AE", width=2)

    if LOESS_KMZ.exists():
        loess_boundary = read_loess_boundary(LOESS_KMZ)
    else:
        loess_boundary = [
            (100.70, 35.75), (100.95, 36.55), (101.40, 37.35), (102.10, 38.20),
            (103.05, 39.00), (104.10, 39.70), (105.25, 40.25), (106.50, 40.50),
            (107.90, 40.72), (109.15, 40.70), (110.35, 40.40), (111.35, 39.95),
            (112.25, 39.25), (113.05, 38.45), (113.70, 37.55), (114.08, 36.60),
            (114.10, 35.72), (113.65, 34.95), (112.55, 34.42), (111.15, 34.10),
            (109.80, 33.95), (108.45, 33.93), (107.15, 34.05), (105.85, 34.28),
            (104.55, 34.52), (103.35, 34.82), (102.20, 35.02), (101.25, 35.20),
            (100.70, 35.75),
        ]
    loess_px = [project(lon, lat) for lon, lat in loess_boundary]
    overlay = Image.new("RGBA", (panel_w, panel_h), (0, 0, 0, 0))
    over_draw = ImageDraw.Draw(overlay, "RGBA")
    over_draw.polygon(loess_px, fill=(238, 217, 174, 168))
    over_draw.line(loess_px, fill="#7B5D2E", width=5, joint="curve")
    panel = Image.alpha_composite(panel, overlay)
    draw = ImageDraw.Draw(panel, "RGBA")

    province_labels = [
        (104.9, 36.5, "甘肃"), (106.2, 38.2, "宁夏"), (107.8, 35.2, "陕西"),
        (111.7, 37.2, "山西"), (110.5, 40.15, "内蒙古"), (113.1, 34.85, "河南"),
    ]
    for lon, lat, label in province_labels:
        draw_text_with_halo(draw, project(lon, lat), label, F_LABEL, "#858177", stroke_width=2)
    draw_text_with_halo(draw, project(108.2, 37.15), "黄土高原", F_LABEL_BOLD, "#6E5225", stroke_width=2)

    draw_scale_bar(draw, project)
    draw_north_arrow(draw, project)
    draw_legend(panel, sites)

    for _, row in sites.iterrows():
        x, y = project(float(row["plot_x"]), float(row["plot_y"]))
        label = str(row["crop_label"])
        draw_marker(draw, x, y, SHAPES[label], COLORS[label], size=64, stroke=5)

    inset = draw_inset(world_segments)
    panel.alpha_composite(inset, (panel_w - 990, panel_h - 760))

    canvas = Image.new("RGB", (WIDTH, HEIGHT), "white")
    canvas.paste(panel.convert("RGB"), (PANEL[0], PANEL[1]))
    draw_canvas = ImageDraw.Draw(canvas)
    draw_canvas.rectangle(PANEL, outline="#2A2A2A", width=3)

    for lon in range(100, 116, 3):
        x, _ = project(lon, YLIM[0])
        gx = PANEL[0] + x
        draw_canvas.line((gx, PANEL[3], gx, PANEL[3] + 16), fill="#3D3D3D", width=3)
        draw_canvas.text((gx, PANEL[3] + 36), f"{lon}°E", font=F_AXIS, fill="#333333", anchor="mt")
    for lat in range(34, 42, 2):
        _, y = project(XLIM[0], lat)
        gy = PANEL[1] + y
        draw_canvas.line((PANEL[0] - 16, gy, PANEL[0], gy), fill="#3D3D3D", width=3)
        draw_canvas.text((PANEL[0] - 34, gy), f"{lat}°N", font=F_AXIS, fill="#333333", anchor="rm")

    canvas.save(PNG_FILE, dpi=(DPI, DPI))
    canvas.save(TIFF_FILE, dpi=(DPI, DPI), compression="tiff_lzw")
    write_site_csv(sites)

    counts = sites["crop_label"].value_counts().reindex(TARGET_LABELS, fill_value=0)
    print(f"Output PNG:  {PNG_FILE}")
    print(f"Output TIFF: {TIFF_FILE}")
    print(f"Output CSV:  {CSV_FILE}")
    print("Drawn records by crop:")
    for label, count in counts.items():
        print(f"  {label}: {int(count)}")
    print(f"Total: {len(sites)}")


if __name__ == "__main__":
    main()

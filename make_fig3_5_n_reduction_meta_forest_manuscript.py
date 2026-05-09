#!/usr/bin/env python3
"""Recalculate Fig. 3-5 EF effects with the manuscript standard.

The revised thesis analysis used a strict EF baseline match, which made the
overall N-reduction effect on EF non-significant. The manuscript figure used a
broader EF standard: a reference-by-crop zero-N baseline and the additional
<25% N-reduction contrasts available within the same source/farming context.

This script keeps the existing N2O, yield and YSE summary rows, recalculates
only the EF rows with that manuscript standard, and redraws the Chinese Fig. 3-5.
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont


PROJECT_DIR = Path(__file__).resolve().parent
TABLE_FILE = PROJECT_DIR / "TableS1.xlsx"
OUT_DIR = PROJECT_DIR / "output" / "figures"
STRICT_PAIRS_FILE = OUT_DIR / "Fig3-5_N_reduction_meta_pairs.csv"
SUMMARY_FILE = OUT_DIR / "Fig3-5_N_reduction_meta_summary.csv"
EF_PAIRS_FILE = OUT_DIR / "Fig3-5_N_reduction_meta_pairs_EF_manuscript.csv"
PNG_FILE = OUT_DIR / "Fig3-5_N_reduction_meta_forest_CN.png"
TIFF_FILE = OUT_DIR / "Fig3-5_N_reduction_meta_forest_CN.tiff"


COLS = [
    "Province",
    "Latitude",
    "Longitude",
    "AnnualTemp",
    "AnnualPrecip",
    "SOC",
    "TN",
    "BD",
    "pH",
    "Crop",
    "Year",
    "Treatment",
    "N_source",
    "Farming",
    "Replicates",
    "N_rate",
    "N2O",
    "N2O_sd",
    "Yield",
    "Yield_sd",
    "References",
]

GROUP_SPECS = pd.DataFrame(
    [
        ("overall", "总体减氮", "总体"),
        ("spring_maize", "春玉米", "作物类型"),
        ("spring_wheat", "春小麦", "作物类型"),
        ("red_lt25", "减氮<25%", "减氮幅度"),
        ("red_25_50", "减氮25%-50%", "减氮幅度"),
        ("red_gt50", "减氮>50%", "减氮幅度"),
    ],
    columns=["group", "group_label", "group_type"],
)


def key_text(value: object) -> str:
    if pd.isna(value):
        return "none"
    text = str(value).strip().lower()
    return text if text and text not in {"na", "nan"} else "none"


def read_table() -> pd.DataFrame:
    dat = pd.read_excel(
        TABLE_FILE,
        sheet_name=0,
        skiprows=2,
        header=None,
        usecols=range(len(COLS)),
        names=COLS,
    )
    fill_cols = [
        "Province",
        "Latitude",
        "Longitude",
        "AnnualTemp",
        "AnnualPrecip",
        "SOC",
        "TN",
        "BD",
        "pH",
        "Crop",
        "Year",
        "References",
    ]
    dat[fill_cols] = dat[fill_cols].ffill()
    num_cols = [
        "Latitude",
        "Longitude",
        "Replicates",
        "N_rate",
        "N2O",
        "N2O_sd",
        "Yield",
        "Yield_sd",
    ]
    for col in num_cols:
        dat[col] = pd.to_numeric(dat[col], errors="coerce")
    dat["row_id"] = np.arange(1, len(dat) + 1)
    dat["source_key"] = dat["N_source"].map(key_text)
    dat["farming_key"] = dat["Farming"].map(key_text)
    return dat


def build_ef_pair_set(dat: pd.DataFrame, strict_pairs: pd.DataFrame) -> pd.DataFrame:
    pairs = strict_pairs.rename(
        columns={"target_row_id": "target_row_id", "control_row_id": "control_row_id"}
    ).copy()

    eligible = dat.loc[
        dat["N_rate"].notna() & (dat["N_rate"] > 0) & dat["N2O"].notna()
    ].copy()
    context_cols = [
        "References",
        "Province",
        "Latitude",
        "Longitude",
        "Crop",
        "Year",
        "source_key",
        "farming_key",
    ]

    extra_rows: list[dict[str, object]] = []
    for _, block in eligible.groupby(context_cols, dropna=False):
        for _, target in block.iterrows():
            higher = block.loc[block["N_rate"] > target["N_rate"]]
            for _, control in higher.iterrows():
                reduction_ratio = (control["N_rate"] - target["N_rate"]) / control["N_rate"]
                if reduction_ratio < 0.25:
                    extra_rows.append(
                        {
                            "target_row_id": int(target["row_id"]),
                            "control_row_id": int(control["row_id"]),
                        }
                    )

    extra = pd.DataFrame(extra_rows)
    pair_ids = pd.concat(
        [pairs[["target_row_id", "control_row_id"]], extra],
        ignore_index=True,
    ).drop_duplicates()

    lookup = dat.set_index("row_id")
    records: list[dict[str, object]] = []
    crop_label = {
        "spring maize": "春玉米",
        "winter wheat": "冬小麦",
        "spring wheat": "春小麦",
        "apple": "苹果园",
    }
    for _, row in pair_ids.iterrows():
        target_id = int(row["target_row_id"])
        control_id = int(row["control_row_id"])
        target = lookup.loc[target_id]
        control = lookup.loc[control_id]
        reduction_ratio = (control["N_rate"] - target["N_rate"]) / control["N_rate"]
        if reduction_ratio < 0.25:
            reduction_group = "red_lt25"
            reduction_label = "减氮<25%"
        elif reduction_ratio <= 0.50:
            reduction_group = "red_25_50"
            reduction_label = "减氮25%-50%"
        else:
            reduction_group = "red_gt50"
            reduction_label = "减氮>50%"

        records.append(
            {
                "pair_key": f"n_reduction_{target_id}_{control_id}",
                "target_row_id": target_id,
                "control_row_id": control_id,
                "References": target["References"],
                "Province": target["Province"],
                "Latitude": target["Latitude"],
                "Longitude": target["Longitude"],
                "Crop": target["Crop"],
                "crop_label": crop_label.get(target["Crop"], target["Crop"]),
                "Year": target["Year"],
                "reduction_group": reduction_group,
                "reduction_label": reduction_label,
                "reduction_ratio": reduction_ratio,
                "reduction_percent": reduction_ratio * 100,
                "N_rate_t": target["N_rate"],
                "N_rate_c": control["N_rate"],
                "treatment_t": target["Treatment"],
                "treatment_c": control["Treatment"],
                "n_source_t": target["N_source"],
                "n_source_c": control["N_source"],
                "farming_t": target["Farming"],
                "farming_c": control["Farming"],
                "n_t": target["Replicates"],
                "n_c": control["Replicates"],
                "N2O_t": target["N2O"],
                "N2O_c": control["N2O"],
                "N2O_sd_t": target["N2O_sd"],
                "N2O_sd_c": control["N2O_sd"],
                "Yield_t": target["Yield"],
                "Yield_c": control["Yield"],
                "Yield_sd_t": target["Yield_sd"],
                "Yield_sd_c": control["Yield_sd"],
            }
        )
    return pd.DataFrame(records)


def add_manuscript_ef(dat: pd.DataFrame, pairs: pd.DataFrame) -> pd.DataFrame:
    zero = dat.loc[dat["N_rate"].notna() & np.isclose(dat["N_rate"], 0) & dat["N2O"].notna()]
    baseline = (
        zero.groupby(["References", "Crop"], dropna=False)
        .agg(
            baseline_N2O=("N2O", "mean"),
            baseline_sd=("N2O_sd", "mean"),
            baseline_n=("Replicates", "mean"),
            baseline_rows=("row_id", lambda x: ";".join(map(str, x))),
        )
        .reset_index()
    )
    dat_ef = dat.merge(baseline, on=["References", "Crop"], how="left")
    ok = (
        dat_ef["N_rate"].notna()
        & (dat_ef["N_rate"] > 0)
        & dat_ef["N2O"].notna()
        & dat_ef["N2O_sd"].notna()
        & dat_ef["Replicates"].notna()
        & dat_ef["baseline_N2O"].notna()
        & dat_ef["baseline_sd"].notna()
        & dat_ef["baseline_n"].notna()
    )
    dat_ef["EF"] = np.nan
    dat_ef["EF_sd"] = np.nan
    dat_ef.loc[ok, "EF"] = (
        (dat_ef.loc[ok, "N2O"] - dat_ef.loc[ok, "baseline_N2O"])
        / dat_ef.loc[ok, "N_rate"]
        * 100
    )
    dat_ef.loc[ok, "EF_sd"] = (
        np.sqrt(
            (dat_ef.loc[ok, "N2O_sd"] / np.sqrt(dat_ef.loc[ok, "Replicates"])) ** 2
            + (dat_ef.loc[ok, "baseline_sd"] / np.sqrt(dat_ef.loc[ok, "baseline_n"])) ** 2
        )
        / dat_ef.loc[ok, "N_rate"]
        * 100
        * np.sqrt(dat_ef.loc[ok, "Replicates"])
    )
    ef_lookup = dat_ef.set_index("row_id")[["EF", "EF_sd", "baseline_rows"]]

    out = pairs.copy()
    for suffix, id_col in [("t", "target_row_id"), ("c", "control_row_id")]:
        out[f"EF_{suffix}"] = out[id_col].map(ef_lookup["EF"])
        out[f"EF_sd_{suffix}"] = out[id_col].map(ef_lookup["EF_sd"])
        out[f"EF_baseline_{suffix}"] = out[id_col].map(ef_lookup["baseline_rows"])
    return out


def hedges_d(row: pd.Series) -> tuple[float, float]:
    mt, mc = row["EF_t"], row["EF_c"]
    sdt, sdc = row["EF_sd_t"], row["EF_sd_c"]
    nt, nc = row["n_t"], row["n_c"]
    vals = [mt, mc, sdt, sdc, nt, nc]
    if not all(np.isfinite(vals)) or nt <= 1 or nc <= 1 or sdt < 0 or sdc < 0:
        return np.nan, np.nan
    sp2 = ((nt - 1) * sdt**2 + (nc - 1) * sdc**2) / (nt + nc - 2)
    if not np.isfinite(sp2) or sp2 <= 0:
        return np.nan, np.nan
    g = (1 - 3 / (4 * (nt + nc) - 9)) * ((mt - mc) / math.sqrt(sp2))
    vi = (nt + nc) / (nt * nc) + (g**2) / (2 * (nt + nc - 2))
    return float(g), float(vi)


def dl_summary(yi: np.ndarray, vi: np.ndarray) -> tuple[int, float, float, float]:
    ok = np.isfinite(yi) & np.isfinite(vi) & (vi > 0)
    yi = yi[ok]
    vi = vi[ok]
    k = len(yi)
    if k == 0:
        return 0, np.nan, np.nan, np.nan
    if k == 1:
        return 1, yi[0], math.sqrt(vi[0]), 0.0
    wi = 1 / vi
    fixed = np.sum(wi * yi) / np.sum(wi)
    q = np.sum(wi * (yi - fixed) ** 2)
    c_val = np.sum(wi) - np.sum(wi**2) / np.sum(wi)
    tau2 = max(0.0, (q - (k - 1)) / c_val) if c_val > 0 else 0.0
    w_re = 1 / (vi + tau2)
    effect = np.sum(w_re * yi) / np.sum(w_re)
    se = math.sqrt(1 / np.sum(w_re))
    return k, float(effect), float(se), float(tau2)


def bootstrap_ci(yi: np.ndarray, vi: np.ndarray, n_boot: int = 999) -> tuple[float, float]:
    ok = np.isfinite(yi) & np.isfinite(vi) & (vi > 0)
    yi = yi[ok]
    vi = vi[ok]
    if len(yi) < 3:
        return np.nan, np.nan
    rng = np.random.default_rng(20260428)
    boots = []
    for _ in range(n_boot):
        idx = rng.integers(0, len(yi), len(yi))
        _, effect, _, _ = dl_summary(yi[idx], vi[idx])
        boots.append(effect)
    low, high = np.quantile(boots, [0.025, 0.975])
    return float(low), float(high)


def summarize_ef(pairs: pd.DataFrame) -> pd.DataFrame:
    effect_rows = []
    for _, row in pairs.iterrows():
        yi, vi = hedges_d(row)
        effect_rows.append(
            {
                "pair_key": row["pair_key"],
                "References": row["References"],
                "Crop": row["Crop"],
                "reduction_group": row["reduction_group"],
                "yi": yi,
                "vi": vi,
            }
        )
    effects = pd.DataFrame(effect_rows)

    rows = []
    for _, spec in GROUP_SPECS.iterrows():
        group = spec["group"]
        if group == "overall":
            sub = effects.copy()
        elif group == "spring_maize":
            sub = effects.loc[effects["Crop"] == "spring maize"].copy()
        elif group == "spring_wheat":
            sub = effects.loc[effects["Crop"] == "spring wheat"].copy()
        else:
            sub = effects.loc[effects["reduction_group"] == group].copy()

        valid = sub.loc[np.isfinite(sub["yi"]) & np.isfinite(sub["vi"]) & (sub["vi"] > 0)]
        k = len(valid)
        n_refs = valid["References"].nunique()
        if k < 3 or n_refs < 3:
            rows.append(
                {
                    "group": group,
                    "group_label": spec["group_label"],
                    "group_type": spec["group_type"],
                    "metric": "EF",
                    "metric_label": "EF",
                    "effect_type": "Hedges_d",
                    "k": k,
                    "n_refs": n_refs,
                    "effect_raw": np.nan,
                    "ci_low_raw": np.nan,
                    "ci_high_raw": np.nan,
                    "se": np.nan,
                    "tau2": np.nan,
                    "effect": np.nan,
                    "ci_low": np.nan,
                    "ci_high": np.nan,
                    "significant": np.nan,
                    "plot_ready": False,
                    "reason_not_plotted": "fewer_than_3_references"
                    if n_refs < 3
                    else "fewer_than_3_effect_sizes",
                }
            )
            continue

        yi = valid["yi"].to_numpy(dtype=float)
        vi = valid["vi"].to_numpy(dtype=float)
        _, effect, se, tau2 = dl_summary(yi, vi)
        ci_low, ci_high = bootstrap_ci(yi, vi)
        rows.append(
            {
                "group": group,
                "group_label": spec["group_label"],
                "group_type": spec["group_type"],
                "metric": "EF",
                "metric_label": "EF",
                "effect_type": "Hedges_d",
                "k": k,
                "n_refs": n_refs,
                "effect_raw": effect,
                "ci_low_raw": ci_low,
                "ci_high_raw": ci_high,
                "se": se,
                "tau2": tau2,
                "effect": effect,
                "ci_low": ci_low,
                "ci_high": ci_high,
                "significant": bool(ci_low > 0 or ci_high < 0),
                "plot_ready": True,
                "reason_not_plotted": np.nan,
            }
        )
    return pd.DataFrame(rows)


def update_summary(ef_summary: pd.DataFrame) -> pd.DataFrame:
    current = pd.read_csv(SUMMARY_FILE)
    out = pd.concat(
        [current.loc[current["metric"] != "EF"].copy(), ef_summary],
        ignore_index=True,
    )
    metric_order = pd.CategoricalDtype(["N2O", "Yield", "YSE", "EF"], ordered=True)
    group_order = pd.CategoricalDtype(GROUP_SPECS["group_label"].tolist(), ordered=True)
    out["metric_order"] = out["metric"].astype(metric_order)
    out["group_order"] = out["group_label"].astype(group_order)
    out = out.sort_values(["metric_order", "group_order"]).drop(
        columns=["metric_order", "group_order"]
    )
    round_cols = [
        "effect_raw",
        "ci_low_raw",
        "ci_high_raw",
        "se",
        "tau2",
        "effect",
        "ci_low",
        "ci_high",
    ]
    for col in round_cols:
        out[col] = pd.to_numeric(out[col], errors="coerce").round(6 if col.endswith("raw") or col in {"se", "tau2"} else 4)
    out.to_csv(SUMMARY_FILE, index=False)
    return out


def load_font(size: int) -> ImageFont.FreeTypeFont | ImageFont.ImageFont:
    candidates = [
        "/Library/Fonts/Arial Unicode.ttf",
        "/System/Library/Fonts/STHeiti Medium.ttc",
        "/System/Library/Fonts/Supplemental/Songti.ttc",
    ]
    for candidate in candidates:
        if Path(candidate).exists():
            return ImageFont.truetype(candidate, size=size)
    return ImageFont.load_default()


def draw_forest(summary: pd.DataFrame) -> None:
    scale = 3
    width, height = 2480 * scale, 1950 * scale
    image = Image.new("RGB", (width, height), "white")
    draw = ImageDraw.Draw(image)
    title_font = load_font(34 * scale)
    panel_title_font = load_font(28 * scale)
    label_font = load_font(23 * scale)
    tick_font = load_font(21 * scale)
    small_font = load_font(20 * scale)

    group_labels = GROUP_SPECS["group_label"].tolist()
    y_positions = np.arange(len(group_labels))[::-1].astype(float)
    y_lookup = dict(zip(group_labels, y_positions))

    panels = [
        ("N2O", "(a)", "N₂O排放量", "变化率 (%)"),
        ("Yield", "(b)", "作物产量", "变化率 (%)"),
        ("YSE", "(c)", "产量尺度N₂O排放", "变化率 (%)"),
        ("EF", "(d)", "EF", "Hedges' d"),
    ]
    colors = {"decrease": "#2166AC", "increase": "#D6604D", "ns": "#666666"}
    panel_boxes = [
        (180 * scale, 210 * scale, 1180 * scale, 885 * scale),
        (1330 * scale, 210 * scale, 2330 * scale, 885 * scale),
        (180 * scale, 1070 * scale, 1180 * scale, 1745 * scale),
        (1330 * scale, 1070 * scale, 2330 * scale, 1745 * scale),
    ]

    def text_center(x: float, y: float, text: str, font: ImageFont.ImageFont, fill: str = "#111111") -> None:
        box = draw.textbbox((0, 0), text, font=font)
        draw.text((x - (box[2] - box[0]) / 2, y - (box[3] - box[1]) / 2), text, fill=fill, font=font)

    def text_right(x: float, y: float, text: str, font: ImageFont.ImageFont, fill: str = "#111111") -> None:
        box = draw.textbbox((0, 0), text, font=font)
        draw.text((x - (box[2] - box[0]), y - (box[3] - box[1]) / 2), text, fill=fill, font=font)

    def nice_ticks(lo: float, hi: float, n_ticks: int = 5) -> tuple[list[float], tuple[float, float]]:
        if not np.isfinite(lo) or not np.isfinite(hi) or math.isclose(lo, hi):
            lo, hi = lo - 1, hi + 1
        raw_step = (hi - lo) / max(n_ticks - 1, 1)
        magnitude = 10 ** math.floor(math.log10(abs(raw_step)))
        step = next(
            candidate * magnitude
            for candidate in [1, 2, 5, 10]
            if candidate * magnitude >= raw_step
        )
        start = math.floor(lo / step) * step
        end = math.ceil(hi / step) * step
        ticks = np.arange(start, end + step * 0.5, step).tolist()
        return ticks, (float(start), float(end))

    def tick_label(value: float) -> str:
        return f"{value:.2f}".rstrip("0").rstrip(".")

    text_center(
        width / 2,
        76 * scale,
        "减量施氮对N₂O排放量、作物产量、YSE和EF的Meta分析森林图",
        title_font,
    )

    for box, (metric, tag, title, xlabel) in zip(panel_boxes, panels):
        x0, y0, x1, y1 = box
        axis_left = x0 + 300 * scale
        axis_right = x1 - 370 * scale
        axis_top = y0 + 115 * scale
        axis_bottom = y1 - 120 * scale
        label_px = x1 - 315 * scale

        sub = summary.loc[summary["metric"] == metric].copy()
        sub["y"] = sub["group_label"].map(y_lookup)
        ready = sub["plot_ready"].astype(str).str.upper() == "TRUE"
        vals = pd.concat([sub.loc[ready, "ci_low"], sub.loc[ready, "ci_high"]])
        vals = pd.to_numeric(vals, errors="coerce").dropna().tolist() + [0]
        lo, hi = min(vals), max(vals)
        span = max(hi - lo, 0.1)
        tick_values, xlim = nice_ticks(lo - span * 0.12, hi + span * 0.12)

        def xmap(value: float) -> float:
            return axis_left + (value - xlim[0]) / (xlim[1] - xlim[0]) * (axis_right - axis_left)

        def ymap(value: float) -> float:
            return axis_bottom - value / (len(group_labels) - 1) * (axis_bottom - axis_top)

        # Border and zero line.
        draw.rectangle((x0, y0, x1, y1), outline="#333333", width=2 * scale)
        zx = xmap(0)
        dash = 12 * scale
        gap = 9 * scale
        y = axis_top
        while y < axis_bottom:
            draw.line((zx, y, zx, min(y + dash, axis_bottom)), fill="#777777", width=2 * scale)
            y += dash + gap

        # Titles and tags.
        text_center((x0 + x1) / 2, y0 + 48 * scale, title, panel_title_font)
        draw.text((x0 + 24 * scale, y0 + 18 * scale), tag, fill="#111111", font=panel_title_font)

        for _, row in sub.iterrows():
            yy = ymap(float(row["y"]))
            is_ready = str(row["plot_ready"]).upper() == "TRUE"
            if is_ready:
                ci_low, ci_high = float(row["ci_low"]), float(row["ci_high"])
                effect = float(row["effect"])
                if ci_high < 0:
                    color = colors["decrease"]
                elif ci_low > 0:
                    color = colors["increase"]
                else:
                    color = colors["ns"]
                draw.line((xmap(ci_low), yy, xmap(ci_high), yy), fill=color, width=5 * scale)
                cx = xmap(effect)
                r = 12 * scale
                if row["group_type"] == "总体":
                    points = [(cx, yy - r), (cx + r, yy), (cx, yy + r), (cx - r, yy)]
                    draw.polygon(points, fill=color if color != colors["ns"] else "white", outline="#222222")
                elif row["group_type"] == "减氮幅度":
                    draw.rectangle((cx - r, yy - r, cx + r, yy + r), fill=color if color != colors["ns"] else "white", outline="#222222", width=2 * scale)
                else:
                    draw.ellipse((cx - r, yy - r, cx + r, yy + r), fill=color if color != colors["ns"] else "white", outline="#222222", width=2 * scale)
                label = (
                    f"{effect:.1f} [{ci_low:.1f}, {ci_high:.1f}]"
                    if row["effect_type"] == "lnRR"
                    else f"{effect:.2f} [{ci_low:.2f}, {ci_high:.2f}]"
                )
            else:
                reason = row.get("reason_not_plotted", "")
                label = (
                    f"文献数<3 (k={int(row['k'])}, refs={int(row['n_refs'])})"
                    if reason == "fewer_than_3_references"
                    else f"效应量<3 (k={int(row['k'])})"
                )
            draw.text((label_px, yy - 13 * scale), label, fill="#222222", font=small_font)

        for group_label in group_labels:
            yy = ymap(y_lookup[group_label])
            text_right(axis_left - 28 * scale, yy, group_label, tick_font)

        for tick in tick_values:
            tx = xmap(float(tick))
            draw.line((tx, axis_bottom, tx, axis_bottom + 14 * scale), fill="#333333", width=2 * scale)
            text_center(tx, axis_bottom + 40 * scale, tick_label(float(tick)), small_font)
        text_center((axis_left + axis_right) / 2, y1 - 45 * scale, xlabel, label_font)

    image.save(PNG_FILE, dpi=(600, 600))
    image.save(TIFF_FILE, compression="tiff_lzw", dpi=(600, 600))


def main() -> None:
    dat = read_table()
    strict_pairs = pd.read_csv(STRICT_PAIRS_FILE)
    ef_pairs = build_ef_pair_set(dat, strict_pairs)
    ef_pairs = add_manuscript_ef(dat, ef_pairs)
    ef_pairs.to_csv(EF_PAIRS_FILE, index=False)
    ef_summary = summarize_ef(ef_pairs)
    summary = update_summary(ef_summary)
    draw_forest(summary)
    print(summary.loc[summary["metric"] == "EF"].to_string(index=False))
    print(f"Wrote {SUMMARY_FILE}")
    print(f"Wrote {EF_PAIRS_FILE}")
    print(f"Wrote {PNG_FILE}")
    print(f"Wrote {TIFF_FILE}")


if __name__ == "__main__":
    main()

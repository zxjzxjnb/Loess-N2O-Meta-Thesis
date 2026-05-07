#!/usr/bin/env Rscript

# Fig 2-1: Site distribution map — publication quality

project_dir <- normalizePath(getwd())
local_lib   <- file.path(project_dir, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c("readxl", "dplyr", "tidyr", "stringr",
                       "ggplot2", "maps", "mapdata", "patchwork", "grid", "scales")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(ggplot2); library(maps); library(mapdata)
  library(patchwork); library(grid); library(scales)
})

# NOTE: Run from RStudio (Source button) for proper CJK rendering.
# The quartz device in RStudio resolves "Heiti SC" via macOS system fonts automatically.

# ── Config ────────────────────────────────────────────────────────────────────
input_xlsx    <- file.path(project_dir, "TableS1.xlsx")
output_dir    <- file.path(project_dir, "output", "figures")
include_potato <- TRUE
font_fam    <- if (Sys.info()[["sysname"]] == "Darwin") "Heiti SC" else "sans"
device_type <- if (Sys.info()[["sysname"]] == "Darwin" && capabilities("aqua")) "quartz" else "cairo"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ── Coordinate parser (unchanged) ─────────────────────────────────────────────
parse_coordinate <- function(x) {
  x <- str_squish(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  out <- suppressWarnings(as.numeric(x))
  needs_parsing <- is.na(out) & !is.na(x)
  if (any(needs_parsing)) {
    out[needs_parsing] <- vapply(x[needs_parsing], function(value) {
      value <- str_replace_all(value, "[°º′'''″\"NSEWnsew\\s]", "")
      value <- str_replace_all(value, "，", ".")
      parts <- str_split(value, "\\.", simplify = TRUE)
      parts <- parts[parts != ""]
      if (length(parts) >= 3 && all(str_detect(parts, "^[0-9]+$"))) {
        return(as.numeric(parts[1]) + as.numeric(paste(parts[-1], collapse = ".")) / 60)
      }
      suppressWarnings(as.numeric(value))
    }, numeric(1))
  }
  out
}

# ── Data ──────────────────────────────────────────────────────────────────────
crop_lookup <- c(
  "spring maize" = "春玉米", "winter wheat" = "冬小麦",
  "spring wheat" = "春小麦", "apple"        = "苹果园",
  "potato"       = "其他"
)
target_keys   <- c("spring maize", "winter wheat", "spring wheat", "apple")
if (include_potato) target_keys <- c(target_keys, "potato")
target_labels <- unname(crop_lookup[target_keys])

raw <- readxl::read_excel(input_xlsx, sheet = 1, guess_max = 10000)

sites_all <- raw %>%
  tidyr::fill(Provice, Latitude, Longitude, Crop, References, .direction = "down") %>%
  mutate(
    crop_key   = str_to_lower(str_squish(Crop)),
    crop_label = unname(crop_lookup[crop_key]),
    latitude   = parse_coordinate(Latitude),
    longitude  = parse_coordinate(Longitude),
    coordinate_note = ""
  ) %>%
  mutate(
    coordinate_override = str_detect(
      References,
      fixed("Effects of different land use patterns on soil properties and N2O emissions",
            ignore_case = TRUE)
    ) &
      crop_key == "spring wheat" &
      near(longitude, 103.52, tol = 1e-6) &
      near(latitude, 34.26, tol = 1e-6),
    longitude = if_else(coordinate_override, 104 + 38 / 60 + 30 / 3600, longitude),
    latitude  = if_else(coordinate_override, 35 + 34 / 60 + 53 / 3600, latitude),
    coordinate_note = if_else(
      coordinate_override,
      "Du et al. (2023) T-field coordinate conflicts with the paper site description; mapped to Dingxi site description.",
      coordinate_note
    )
  ) %>%
  filter(!is.na(References), !is.na(crop_key),
         !is.na(latitude), !is.na(longitude), !is.na(crop_label)) %>%
  distinct(References, crop_key, crop_label, Provice, longitude, latitude, coordinate_note)

sites <- sites_all %>%
  filter(crop_key %in% target_keys) %>%
  group_by(crop_key, crop_label, longitude, latitude) %>%
  summarise(
    Provice = paste(sort(unique(Provice)), collapse = "/"),
    reference_n = n_distinct(References),
    References = paste(sort(unique(References)), collapse = " | "),
    coordinate_note = paste(sort(unique(coordinate_note[coordinate_note != ""])),
                            collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(crop_label = factor(crop_label, levels = target_labels)) %>%
  arrange(longitude, latitude, crop_label, References) %>%
  group_by(longitude, latitude) %>%
  mutate(
    overlap_n = n(),
    overlap_i = row_number(),
    angle     = 2 * pi * (overlap_i - 1) / pmax(overlap_n, 1),
    plot_x    = longitude + if_else(overlap_n > 1, 0.085 * cos(angle), 0),
    plot_y    = latitude  + if_else(overlap_n > 1, 0.085 * sin(angle), 0)
  ) %>%
  ungroup()

excluded <- setdiff(unique(sites_all$crop_key), target_keys)
if (length(excluded) > 0)
  message("Crops excluded from plot: ", paste(excluded, collapse = ", "))

# ── Map polygons ──────────────────────────────────────────────────────────────
china_province <- ggplot2::map_data("china") %>%
  filter(long >= 97, long <= 118, lat >= 31, lat <= 44)
china_full <- ggplot2::map_data("china")

# ── Geographic features ───────────────────────────────────────────────────────
# Prefer the public Loess Plateau Region KMZ boundary when present.
# Source: WANG Zhengxing. Boundary Data of Loess Plateau Region.
read_loess_kmz <- function(path) {
  if (!file.exists(path)) return(NULL)
  kml <- tryCatch(
    paste(readLines(unz(path, "doc.kml"), warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(kml)) return(NULL)
  blocks <- regmatches(
    kml,
    gregexpr("<coordinates>[^<]+</coordinates>", kml, perl = TRUE)
  )[[1]]
  if (length(blocks) == 0) return(NULL)
  coords_text <- blocks[which.max(nchar(blocks))]
  coords_text <- gsub("</?coordinates>", "", coords_text)
  tokens <- unlist(strsplit(str_squish(coords_text), "\\s+"))
  coords <- do.call(rbind, lapply(tokens, function(token) {
    parts <- strsplit(token, ",", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(c(NA_real_, NA_real_))
    c(as.numeric(parts[1]), as.numeric(parts[2]))
  }))
  coords <- coords[stats::complete.cases(coords), , drop = FALSE]
  if (nrow(coords) < 3) return(NULL)
  data.frame(x = coords[, 1], y = coords[, 2])
}

loess_polygon <- read_loess_kmz(
  file.path(project_dir, "data", "geo", "LoessPlateauRegion.kmz")
)
if (is.null(loess_polygon)) {
  loess_polygon <- data.frame(
    x = c(100.7, 101.6, 103.1, 105.3, 107.5, 109.6, 111.8, 114.4,
          114.1, 112.6, 110.5, 108.1, 105.6, 103.0, 101.5, 100.7),
    y = c(35.8,  37.8,  39.6,  40.7,  41.0,  40.5,  39.4,  37.3,
          35.4,  34.3,  33.9,  34.0,  34.4,  35.0,  35.3,  35.8)
  )
}
prov_labs <- data.frame(
  x     = c(107.8, 112.0, 104.5, 106.2, 111.2, 113.8),
  y     = c(35.0,  37.1,  36.2,  38.4,  40.3,  34.5),
  label = c("陕西", "山西", "甘肃", "宁夏", "内蒙古", "河南")
)

# ── Aesthetics ────────────────────────────────────────────────────────────────
# Colorblind-safe palette (RColorBrewer-compatible)
COLORS <- c(
  "春玉米" = "#2166AC",  # blue
  "冬小麦" = "#D6604D",  # warm red
  "春小麦" = "#F59B20",  # amber
  "苹果园" = "#35978F",  # teal
  "其他"   = "#D6CF8A"   # pale olive
)
SHAPES <- c(
  "春玉米" = 21,  # filled circle
  "冬小麦" = 24,  # filled triangle up
  "春小麦" = 22,  # filled square
  "苹果园" = 23,  # filled diamond
  "其他"   = 21   # filled circle
)

# ── Map extent ────────────────────────────────────────────────────────────────
xlim_map <- c(99.6, 115.8)
ylim_map <- c(33.2, 41.9)

# ── Scale bar ─────────────────────────────────────────────────────────────────
# At 34°N: 1° lon ≈ 92.3 km → 100 km ≈ 1.083°, 200 km ≈ 2.165°
sb_x0  <- 100.3;  sb_y_lo <- 34.05;  sb_y_hi <- 34.22
d100   <- 1.083
sb_mid <- sb_x0 + d100
sb_x1  <- sb_x0 + 2 * d100

# ── North arrow ───────────────────────────────────────────────────────────────
na_x      <- 115.05
na_y_base <- 39.5
na_y_tip  <- 40.7

# ── Inset: China overview map ─────────────────────────────────────────────────
inset <- ggplot() +
  geom_polygon(
    data  = subset(china_full, long >= 73 & long <= 136 & lat >= 15 & lat <= 54),
    aes(long, lat, group = group),
    fill  = "#D9D9D9", color = "#BBBBBB", linewidth = 0.12
  ) +
  annotate("rect",
           xmin = xlim_map[1], xmax = xlim_map[2],
           ymin = ylim_map[1], ymax = ylim_map[2],
           fill = NA, color = "#CC2200", linewidth = 0.85) +
  coord_quickmap(xlim = c(73, 136), ylim = c(15, 54), expand = FALSE) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "#E3EFF8", color = "#555555", linewidth = 0.6),
    plot.background  = element_blank()
  )

# ── Main map ──────────────────────────────────────────────────────────────────
p <- ggplot() +

  # Province polygons
  geom_polygon(
    data  = china_province, aes(long, lat, group = group),
    fill  = "#F5F5F0", color = "#C0C0B8", linewidth = 0.2
  ) +

  # Loess Plateau envelope (approximate)
  geom_polygon(
    data  = loess_polygon, aes(x, y),
    fill  = "#F2DEB5", color = "#8B6834", linewidth = 0.65, alpha = 0.55
  ) +

  # Province name labels
  geom_text(
    data   = prov_labs, aes(x, y, label = label),
    family = font_fam, color = "#888880", size = 2.85, fontface = "plain"
  ) +

  # "黄土高原" label
  annotate("text", x = 108.4, y = 37.0,
           label = "黄土高原", family = font_fam,
           color = "#6B4C1E", fontface = "bold", size = 4.0) +

  # Site points
  geom_point(
    data  = sites, aes(plot_x, plot_y, fill = crop_label, shape = crop_label),
    color = "white", size = 3.8, stroke = 0.5, alpha = 0.92
  ) +

  # ── Scale bar (graphic, two-tone) ─────────────────────────────────────────
  annotate("rect",
           xmin = sb_x0, xmax = sb_mid, ymin = sb_y_lo, ymax = sb_y_hi,
           fill = "#222222", color = "#222222", linewidth = 0.25) +
  annotate("rect",
           xmin = sb_mid, xmax = sb_x1, ymin = sb_y_lo, ymax = sb_y_hi,
           fill = "white",   color = "#222222", linewidth = 0.25) +
  annotate("text", x = sb_x0,  y = sb_y_lo - 0.12, label = "0",
           family = font_fam, size = 2.6, color = "#222222", hjust = 0.5) +
  annotate("text", x = sb_mid, y = sb_y_lo - 0.12, label = "100",
           family = font_fam, size = 2.6, color = "#222222", hjust = 0.5) +
  annotate("text", x = sb_x1,  y = sb_y_lo - 0.12, label = "200 km",
           family = font_fam, size = 2.6, color = "#222222", hjust = 0.5) +

  # ── North arrow ─────────────────────────────────────────────────────────────
  annotate("segment",
           x = na_x, xend = na_x, y = na_y_base, yend = na_y_tip,
           arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
           color = "#222222", linewidth = 0.65) +
  annotate("text", x = na_x, y = na_y_tip + 0.26,
           label = "N", family = font_fam,
           size = 3.1, fontface = "bold", color = "#222222") +

  # ── China overview inset ─────────────────────────────────────────────────────
  annotation_custom(
    grob = ggplotGrob(inset),
    xmin = 111.8, xmax = 115.8,
    ymin = 33.2,  ymax = 36.9
  ) +

  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_fill_manual(
    values = COLORS[target_labels], name = "作物体系", drop = FALSE
  ) +
  scale_shape_manual(
    values = SHAPES[target_labels], name = "作物体系", drop = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(100, 115, by = 3),
    labels = function(x) paste0(x, "°E")
  ) +
  scale_y_continuous(
    breaks = seq(34, 42, by = 2),
    labels = function(x) paste0(x, "°N")
  ) +
  coord_quickmap(xlim = xlim_map, ylim = ylim_map, expand = FALSE) +

  # No embedded title — goes in figure caption
  labs(x = NULL, y = NULL) +

  theme_classic(base_family = font_fam, base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "#EFF5FA", color = NA),
    panel.border       = element_rect(color = "#333333", fill = NA, linewidth = 0.55),
    axis.line          = element_blank(),
    axis.ticks         = element_line(color = "#555555", linewidth = 0.3),
    axis.ticks.length  = unit(0.13, "cm"),
    axis.text          = element_text(size = 9, color = "#333333"),

    # Legend: top-left, inside panel (clear of data points)
    legend.position      = c(0.10, 0.80),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = alpha("white", 0.88),
                                        color = "#AAAAAA", linewidth = 0.28),
    legend.title         = element_text(size = 9.5, face = "bold"),
    legend.text          = element_text(size = 9.0),
    legend.key           = element_rect(fill = NA, color = NA),
    legend.key.size      = unit(0.42, "cm"),
    legend.spacing.y     = unit(0.06, "cm"),
    legend.margin        = margin(4, 6, 4, 5),

    plot.margin = margin(6, 6, 6, 6)
  )

# ── Export ────────────────────────────────────────────────────────────────────
png_file  <- file.path(output_dir, "Fig2-1_site_distribution_CN.png")
tiff_file <- file.path(output_dir, "Fig2-1_site_distribution_CN.tiff")
csv_file  <- file.path(output_dir, "Fig2-1_site_points_used.csv")

png(filename = png_file, width = 190, height = 140, units = "mm",
    res = 600, type = device_type, family = font_fam, bg = "white")
print(p)
dev.off()

tiff(filename = tiff_file, width = 190, height = 140, units = "mm",
     res = 600, type = device_type, family = font_fam, bg = "white")
print(p)
dev.off()

sites %>%
  transmute(
    `参考文献`     = References,
    `作物体系`     = as.character(crop_label),
    `省份`         = Provice,
    `经度`         = longitude,
    `纬度`         = latitude,
    `绘图经度`     = plot_x,
    `绘图纬度`     = plot_y,
    `同坐标记录数` = overlap_n,
    `文献数`       = reference_n,
    `坐标备注`     = coordinate_note
  ) %>%
  write.csv(csv_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Output PNG:  ", png_file)
message("Output TIFF: ", tiff_file)
message("Drawn records by crop:")
print(dplyr::count(sites, crop_label, name = "n"))
message("Total: ", nrow(sites))

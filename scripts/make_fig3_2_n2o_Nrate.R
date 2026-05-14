#!/usr/bin/env Rscript

# Fig 3-2: N2O排放 ~ 施氮量线性模型 + 箱线图（主要农业生态系统）
# Publication-quality Chinese figure with command-line reproducible CJK output.

project_dir <- normalizePath(getwd())
local_lib   <- file.path(project_dir, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
  "grid", "showtext", "sysfonts"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
  library(grid)
  library(showtext)
  library(sysfonts)
})

# Config -----------------------------------------------------------------------
input_xlsx <- file.path(project_dir, "data", "tables", "TableS1.xlsx")
output_dir <- file.path(project_dir, "output", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_candidates <- c(
  "/Library/Fonts/Arial Unicode.ttf",
  "/System/Library/Fonts/Supplemental/Arial Unicode.ttf",
  "/System/Library/Fonts/STHeiti Medium.ttc",
  "/System/Library/Fonts/Supplemental/Songti.ttc"
)
font_path <- font_candidates[file.exists(font_candidates)][1]
if (is.na(font_path)) {
  stop("No usable Chinese font found. Checked: ", paste(font_candidates, collapse = ", "),
       call. = FALSE)
}
font_fam <- "fig32_cn"
sysfonts::font_add(family = font_fam, regular = font_path)
showtext::showtext_auto(enable = TRUE)
showtext::showtext_opts(dpi = 600)

device_type <- if (Sys.info()[["sysname"]] == "Darwin" &&
                   isTRUE(capabilities("aqua"))) "quartz" else getOption("bitmapType", "cairo")

set_file_dpi <- function(path, dpi = 600) {
  if (Sys.info()[["sysname"]] != "Darwin") return(invisible(FALSE))
  if (!nzchar(Sys.which("sips")) || !file.exists(path)) return(invisible(FALSE))
  status <- system2(
    "sips",
    c("-s", "dpiWidth", dpi, "-s", "dpiHeight", dpi, path),
    stdout = FALSE, stderr = FALSE
  )
  invisible(identical(status, 0L) || is.null(status))
}

# Data -------------------------------------------------------------------------
raw <- suppressMessages(read_excel(
  input_xlsx, sheet = 1, skip = 2,
  col_names = c("Province", "Latitude", "Longitude", "AnnualTemp", "AnnualPrecip",
                "SOC", "TN", "BD", "pH", "Crop", "Year", "Treatment", "N_source",
                "Farming", "Replicates", "N_rate", "N2O", "N2O_sd", "Yield",
                "Yield_sd", "References")
))

crop_keys <- c("spring maize", "winter wheat", "spring wheat", "apple")
crop_lookup <- c(
  "spring maize" = "春玉米",
  "winter wheat" = "冬小麦",
  "spring wheat" = "春小麦",
  "apple" = "苹果园"
)
crop_levels <- enc2utf8(unname(crop_lookup[crop_keys]))

excluded_pattern <- paste(
  c("biochar", "biocar", "DCD", "slow release", "control release",
    "controlled release", "inhibitor", "enhanced", "EENF", "EEF"),
  collapse = "|"
)

dat <- raw %>%
  tidyr::fill(Crop, .direction = "down") %>%
  mutate(
    N_rate = suppressWarnings(as.numeric(N_rate)),
    N2O = suppressWarnings(as.numeric(N2O)),
    exclusion_text = paste(
      if_else(is.na(Treatment), "", as.character(Treatment)),
      if_else(is.na(N_source), "", as.character(N_source)),
      if_else(is.na(Farming), "", as.character(Farming))
    ),
    is_excluded = grepl(excluded_pattern, exclusion_text, ignore.case = TRUE)
  ) %>%
  filter(
    Crop %in% crop_keys,
    !is.na(N_rate),
    !is.na(N2O),
    !is_excluded
  ) %>%
  mutate(crop_cn = factor(enc2utf8(unname(crop_lookup[Crop])), levels = crop_levels))

if (nrow(dat) == 0) {
  stop("No usable N2O records after filtering.", call. = FALSE)
}

# Aesthetics -------------------------------------------------------------------
COLORS <- c("春玉米" = "#2166AC",
            "冬小麦" = "#D6604D",
            "春小麦" = "#F59B20",
            "苹果园" = "#1B9E77")
SHAPES <- c("春玉米" = 21,
            "冬小麦" = 24,
            "春小麦" = 22,
            "苹果园" = 25)

theme_fig32 <- function() {
  theme_classic(base_family = font_fam, base_size = 9.6) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(color = "#333333", fill = NA, linewidth = 0.5),
      axis.line         = element_blank(),
      axis.ticks        = element_line(color = "#555555", linewidth = 0.28),
      axis.ticks.length = unit(0.11, "cm"),
      axis.text         = element_text(size = 8.7, color = "#222222"),
      axis.title        = element_text(size = 9.3, color = "#222222"),
      plot.tag          = element_text(face = "bold", size = 11.5),
      plot.margin       = margin(5, 6, 5, 5)
    )
}

fmt_p_value <- function(p) {
  if (is.na(p)) "p = NA"
  else if (p < 0.001) "p < 0.001"
  else sprintf("p = %.3f", p)
}

# Linear fits ------------------------------------------------------------------
fit_linear <- function(df) {
  m <- lm(N2O ~ N_rate, data = df)
  sm <- summary(m)
  co <- coef(m)
  list(
    model = m,
    intercept = unname(co[["(Intercept)"]]),
    slope = unname(co[["N_rate"]]),
    r2 = sm$r.squared,
    p_slope = sm$coefficients["N_rate", "Pr(>|t|)"],
    n = nrow(df),
    xmax = max(df$N_rate, na.rm = TRUE)
  )
}

fit_list <- lapply(crop_levels, function(cn) {
  fit_linear(dat %>% filter(crop_cn == cn))
})
names(fit_list) <- crop_levels

curve_df <- bind_rows(lapply(crop_levels, function(cn) {
  f <- fit_list[[cn]]
  xx <- seq(0, f$xmax, length.out = 200)
  pr <- predict(f$model, newdata = data.frame(N_rate = xx),
                interval = "confidence", level = 0.95)
  data.frame(
    crop_cn = factor(cn, levels = crop_levels),
    N_rate  = xx,
    fit     = pr[, "fit"],
    lwr     = pmax(pr[, "lwr"], 0),
    upr     = pr[, "upr"]
  )
}))

annot_df <- data.frame(
  crop_cn = factor(crop_levels, levels = crop_levels),
  label = sapply(crop_levels, function(cn) {
    f <- fit_list[[cn]]
    sprintf(
      "%s: E0 = %.2f; k = %.2f%%; R² = %.3f; %s",
      cn, f$intercept, f$slope * 100, f$r2, fmt_p_value(f$p_slope)
    )
  })
)

# Panel limits are shared for visual comparability.
xmax_panel <- ceiling(max(dat$N_rate, na.rm = TRUE) / 100) * 100
xmax_panel <- max(xmax_panel, 1200)
ymax_panel <- ceiling(max(c(dat$N2O, curve_df$upr), na.rm = TRUE) * 1.10)
ymax_panel <- max(ymax_panel, 10)

annot_df <- annot_df %>%
  mutate(
    x = xmax_panel * 0.985,
    y = ymax_panel * 0.955 - (as.integer(crop_cn) - 1) * ymax_panel * 0.060
  )

pA <- ggplot() +
  geom_ribbon(
    data = curve_df,
    aes(N_rate, ymin = lwr, ymax = upr, fill = crop_cn),
    alpha = 0.13, color = NA
  ) +
  geom_point(
    data = dat,
    aes(N_rate, N2O, fill = crop_cn, shape = crop_cn),
    color = "white", size = 1.72, stroke = 0.22, alpha = 0.64
  ) +
  geom_line(
    data = curve_df,
    aes(N_rate, fit, color = crop_cn),
    linewidth = 0.82
  ) +
  annotate(
    "rect",
    xmin = xmax_panel * 0.43, xmax = xmax_panel * 0.998,
    ymin = min(annot_df$y) - ymax_panel * 0.035,
    ymax = max(annot_df$y) + ymax_panel * 0.025,
    fill = alpha("white", 0.86), color = NA
  ) +
  geom_text(
    data = annot_df,
    aes(x = x, y = y, label = label, color = crop_cn),
    hjust = 1, vjust = 1, family = font_fam, size = 2.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = COLORS, name = "农业生态系统", drop = FALSE) +
  scale_fill_manual(values = COLORS, name = "农业生态系统", drop = FALSE) +
  scale_shape_manual(values = SHAPES, name = "农业生态系统", drop = FALSE) +
  scale_x_continuous(
    limits = c(0, xmax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, xmax_panel, by = 200)
  ) +
  scale_y_continuous(
    limits = c(0, ymax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, ymax_panel, by = 2)
  ) +
  labs(
    x = "施氮量 (kg N ha⁻¹)",
    y = "N₂O排放量 (kg N ha⁻¹)",
    tag = "(a)"
  ) +
  theme_fig32() +
  theme(
    legend.position      = c(0.035, 0.96),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = alpha("white", 0.90),
                                        color = "#AAAAAA", linewidth = 0.25),
    legend.title         = element_text(size = 8.3, face = "bold"),
    legend.text          = element_text(size = 8.0),
    legend.key           = element_rect(fill = NA, color = NA),
    legend.key.size      = unit(0.36, "cm"),
    legend.spacing.y     = unit(0.02, "cm"),
    legend.margin        = margin(3, 5, 3, 4),
    plot.tag.position    = c(0.012, 0.985)
  )

# Panel B: fertilized-treatment boxplot ---------------------------------------
dat_fert <- dat %>% filter(N_rate > 0)

stat_df <- dat_fert %>%
  group_by(crop_cn) %>%
  summarise(
    n = n(),
    mean = mean(N2O),
    median = median(N2O),
    sd = sd(N2O),
    min = min(N2O),
    max = max(N2O),
    cv_percent = sd(N2O) / mean(N2O) * 100,
    .groups = "drop"
  )

make_sig_letters <- function(groups, means, sig) {
  groups <- as.character(groups)
  means <- means[groups]
  ordered_groups <- groups[order(means[groups], decreasing = TRUE)]
  n_groups <- length(ordered_groups)
  pool <- letters[seq_len(max(n_groups, 1))]

  has_common_letter <- function(a, b) {
    length(intersect(strsplit(a, "")[[1]], strsplit(b, "")[[1]])) > 0
  }

  for (k in seq_len(n_groups)) {
    candidate_letters <- unlist(lapply(seq_len(k), function(m) {
      apply(combn(pool[seq_len(k)], m), 2, paste0, collapse = "")
    }))
    grid <- expand.grid(rep(list(candidate_letters), n_groups), stringsAsFactors = FALSE)

    valid <- vapply(seq_len(nrow(grid)), function(i) {
      assignment <- as.character(grid[i, ])
      names(assignment) <- ordered_groups
      for (ii in seq_len(n_groups - 1)) {
        for (jj in (ii + 1):n_groups) {
          g1 <- ordered_groups[ii]
          g2 <- ordered_groups[jj]
          common <- has_common_letter(assignment[g1], assignment[g2])
          if (sig[g1, g2] && common) return(FALSE)
          if (!sig[g1, g2] && !common) return(FALSE)
        }
      }
      TRUE
    }, logical(1))

    if (any(valid)) {
      valid_idx <- which(valid)
      char_counts <- vapply(valid_idx, function(i) sum(nchar(as.character(grid[i, ]))), numeric(1))
      letter_order <- vapply(valid_idx, function(i) paste(as.character(grid[i, ]), collapse = "|"),
                             character(1))
      selected <- valid_idx[order(char_counts, letter_order)[1]]
      out <- as.character(grid[selected, ])
      names(out) <- ordered_groups
      return(out[groups])
    }
  }

  fallback <- letters[seq_along(ordered_groups)]
  names(fallback) <- ordered_groups
  fallback[groups]
}

pairwise_center <- bind_rows(lapply(seq_len(length(crop_levels) - 1), function(i) {
  bind_rows(lapply((i + 1):length(crop_levels), function(j) {
    g1 <- crop_levels[i]
    g2 <- crop_levels[j]
    x1 <- dat_fert %>% filter(crop_cn == g1) %>% pull(N2O)
    x2 <- dat_fert %>% filter(crop_cn == g2) %>% pull(N2O)
    data.frame(
      group1 = g1,
      group2 = g2,
      p_wilcox = suppressWarnings(wilcox.test(x1, x2, exact = FALSE)$p.value),
      stringsAsFactors = FALSE
    )
  }))
})) %>%
  mutate(
    p_wilcox_BH = p.adjust(p_wilcox, method = "BH"),
    significant_center_0.05 = is.finite(p_wilcox_BH) & p_wilcox_BH < 0.05,
    center_test = "pairwise Wilcoxon rank-sum, BH adjusted"
  )

omnibus_tests <- data.frame(
  metric = "N2O",
  p_kruskal = suppressWarnings(kruskal.test(N2O ~ crop_cn, data = dat_fert)$p.value),
  stringsAsFactors = FALSE
)

center_sig <- matrix(FALSE, length(crop_levels), length(crop_levels),
                     dimnames = list(crop_levels, crop_levels))

for (ii in seq_len(nrow(pairwise_center))) {
  g1 <- pairwise_center$group1[ii]
  g2 <- pairwise_center$group2[ii]
  is_sig <- isTRUE(pairwise_center$significant_center_0.05[ii])
  center_sig[g1, g2] <- is_sig
  center_sig[g2, g1] <- is_sig
}

means_named <- setNames(stat_df$mean, as.character(stat_df$crop_cn))
center_letters <- make_sig_letters(crop_levels, means_named, center_sig)
stat_df <- stat_df %>%
  mutate(
    letter_group = unname(center_letters[as.character(crop_cn)]),
    letter_test = "pairwise Wilcoxon rank-sum, BH adjusted",
    label_y = pmin(max + 0.42, ymax_panel - 0.38)
  )

box_x_labels <- setNames(
  sprintf("%s\n(n=%d)", as.character(stat_df$crop_cn), stat_df$n),
  as.character(stat_df$crop_cn)
)

pB <- ggplot(dat_fert, aes(crop_cn, N2O, fill = crop_cn, color = crop_cn)) +
  geom_jitter(
    aes(shape = crop_cn),
    width = 0.16, height = 0, size = 1.23, alpha = 0.45,
    stroke = 0.18, color = "white"
  ) +
  geom_boxplot(
    width = 0.50, alpha = 0.55, outlier.shape = NA,
    linewidth = 0.48, color = "#222222"
  ) +
  stat_summary(
    fun = mean, geom = "point", shape = 23, size = 2.25,
    fill = "white", color = "#222222", stroke = 0.48
  ) +
  geom_text(
    data = stat_df,
    aes(x = crop_cn, y = label_y, label = letter_group),
    inherit.aes = FALSE, family = font_fam, size = 3.15,
    fontface = "bold", color = "#222222"
  ) +
  annotate(
    "text",
    x = 2.5, y = ymax_panel * 0.98,
    hjust = 0.5, vjust = 1,
    label = "Wilcoxon-BH: p < 0.05",
    family = font_fam, size = 2.8, color = "#222222"
  ) +
  scale_fill_manual(values = COLORS, guide = "none") +
  scale_color_manual(values = COLORS, guide = "none") +
  scale_shape_manual(values = SHAPES, guide = "none") +
  scale_x_discrete(labels = box_x_labels) +
  scale_y_continuous(
    limits = c(0, ymax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, ymax_panel, by = 2)
  ) +
  labs(
    x = NULL,
    y = "N₂O排放量 (kg N ha⁻¹)",
    tag = "(b)"
  ) +
  theme_fig32() +
  theme(
    axis.text.x       = element_text(size = 7.9, color = "#222222", lineheight = 0.95),
    plot.tag.position = c(0.025, 0.985),
    plot.margin       = margin(5, 5, 5, 4)
  )

# Combine and export -----------------------------------------------------------
fig <- pA + pB + plot_layout(widths = c(2.05, 1))

png_file  <- file.path(output_dir, "Fig3-2_N2O_Nrate_CN.png")
tiff_file <- file.path(output_dir, "Fig3-2_N2O_Nrate_CN.tiff")
csv_file  <- file.path(output_dir, "Fig3-2_linear_fit_summary.csv")
box_csv_file <- file.path(output_dir, "Fig3-2_boxplot_summary.csv")
pairwise_center_file <- file.path(output_dir, "Fig3-2_boxplot_pairwise_center_tests.csv")
omnibus_file <- file.path(output_dir, "Fig3-2_boxplot_omnibus_tests.csv")

png(filename = png_file, width = 190, height = 110, units = "mm",
    res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(png_file, 600)

tiff(filename = tiff_file, width = 190, height = 110, units = "mm",
     res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(tiff_file, 600)

fit_summary <- data.frame(
  crop = enc2utf8(crop_levels),
  n = unname(sapply(fit_list, function(f) f$n)),
  background_emission_kgN_ha = unname(sapply(fit_list, function(f) round(f$intercept, 4))),
  slope_kgN_ha_per_kgN_ha = unname(sapply(fit_list, function(f) round(f$slope, 6))),
  emission_coefficient_percent = unname(sapply(fit_list, function(f) round(f$slope * 100, 3))),
  R2 = unname(sapply(fit_list, function(f) round(f$r2, 3))),
  p_linear_slope = unname(sapply(fit_list, function(f) signif(f$p_slope, 4))),
  stringsAsFactors = FALSE
)
write.csv(fit_summary, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

box_summary <- stat_df %>%
  transmute(
    crop = enc2utf8(as.character(crop_cn)),
    n,
    mean_N2O_kgN_ha = round(mean, 3),
    median_N2O_kgN_ha = round(median, 3),
    sd_N2O_kgN_ha = round(sd, 3),
    min_N2O_kgN_ha = round(min, 3),
    max_N2O_kgN_ha = round(max, 3),
    cv_percent = round(cv_percent, 1),
    letter_group,
    letter_test,
    kruskal_p = signif(omnibus_tests$p_kruskal[1], 4)
  )
write.csv(box_summary, box_csv_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pairwise_center, pairwise_center_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(omnibus_tests, omnibus_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Chinese font: ", font_path)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Fit summary: ", csv_file)
message("Box summary: ", box_csv_file)
message("Pairwise center tests: ", pairwise_center_file)
message("Omnibus tests: ", omnibus_file)
print(fit_summary)
print(as.data.frame(box_summary))

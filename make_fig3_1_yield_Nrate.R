#!/usr/bin/env Rscript

# Fig 3-1: 作物产量 ~ 施氮量二次模型 + 箱线图（粮食作物）
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

# ── Config ────────────────────────────────────────────────────────────────────
input_xlsx <- file.path(project_dir, "TableS1.xlsx")
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
font_fam <- "fig31_cn"
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

# ── Data ──────────────────────────────────────────────────────────────────────
raw <- suppressMessages(read_excel(
  input_xlsx, sheet = 1, skip = 2,
  col_names = c("Province", "Latitude", "Longitude", "AnnualTemp", "AnnualPrecip",
                "SOC", "TN", "BD", "pH", "Crop", "Year", "Treatment", "N_source",
                "Farming", "Replicates", "N_rate", "N2O", "col18", "Yield", "col20",
                "References")
))

raw <- raw %>%
  tidyr::fill(Crop, .direction = "down") %>%
  mutate(
    N_rate = suppressWarnings(as.numeric(N_rate)),
    Yield  = suppressWarnings(as.numeric(Yield))
  )

cereals <- c("spring maize", "winter wheat", "spring wheat")
crop_lookup <- c(
  "spring maize" = "春玉米",
  "winter wheat" = "冬小麦",
  "spring wheat" = "春小麦"
)
crop_levels <- enc2utf8(unname(crop_lookup[cereals]))

excluded_pattern <- paste(
  c("biochar", "biocar", "DCD", "slow release", "control release",
    "controlled release", "inhibitor", "enhanced", "EENF", "EEF"),
  collapse = "|"
)

dat <- raw %>%
  mutate(
    exclusion_text = paste(
      if_else(is.na(Treatment), "", as.character(Treatment)),
      if_else(is.na(N_source), "", as.character(N_source)),
      if_else(is.na(Farming), "", as.character(Farming))
    ),
    is_excluded = grepl(excluded_pattern, exclusion_text, ignore.case = TRUE)
  ) %>%
  filter(!is.na(Yield), !is.na(N_rate), Crop %in% cereals, !is_excluded) %>%
  mutate(crop_cn = factor(enc2utf8(unname(crop_lookup[Crop])), levels = crop_levels))

# ── Aesthetics ────────────────────────────────────────────────────────────────
COLORS <- c("春玉米" = "#2166AC",
            "冬小麦" = "#D6604D",
            "春小麦" = "#F59B20")
SHAPES <- c("春玉米" = 21,
            "冬小麦" = 24,
            "春小麦" = 22)

theme_fig31 <- function() {
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

# ── Quadratic fits ────────────────────────────────────────────────────────────
fit_quad <- function(df) {
  m  <- lm(Yield ~ N_rate + I(N_rate^2), data = df)
  co <- coef(m)
  a <- co[[1]]
  b <- co[[2]]
  cc <- co[[3]]
  vx <- -b / (2 * cc)
  vy <- a + b * vx + cc * vx^2
  pv <- summary(m)$coefficients[3, 4]
  r2 <- summary(m)$r.squared
  list(model = m, a = a, b = b, c = cc, vx = vx, vy = vy, p = pv, r2 = r2,
       n = nrow(df), xmax = max(df$N_rate, na.rm = TRUE))
}

fit_list <- lapply(crop_levels, function(cn) {
  fit_quad(dat %>% filter(crop_cn == cn))
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
    lwr     = pr[, "lwr"],
    upr     = pr[, "upr"]
  )
}))

vertex_df <- data.frame(
  crop_cn = factor(crop_levels, levels = crop_levels),
  N_rate  = sapply(fit_list, function(f) f$vx),
  Yield   = sapply(fit_list, function(f) f$vy)
)

fmt_p_value <- function(p) {
  if (is.na(p)) "p = NA"
  else if (p < 0.001) "p < 0.001"
  else sprintf("p = %.3f", p)
}

annot_df <- data.frame(
  crop_cn = factor(crop_levels, levels = crop_levels),
  label = sapply(crop_levels, function(cn) {
    f <- fit_list[[cn]]
    sprintf(
      "%s: Nopt = %.0f kg N ha⁻¹; Ymax = %.1f t ha⁻¹; R² = %.3f; %s",
      cn, f$vx, f$vy, f$r2, fmt_p_value(f$p)
    )
  })
)

# ── Panel A: scatter + quadratic fits ────────────────────────────────────────
xmax_panel <- 450
ymax_panel <- ceiling(max(dat$Yield, na.rm = TRUE) * 1.12 / 2) * 2
ymax_panel <- max(ymax_panel, 24)

pA <- ggplot() +
  geom_ribbon(
    data = curve_df,
    aes(N_rate, ymin = lwr, ymax = upr, fill = crop_cn),
    alpha = 0.13, color = NA
  ) +
  geom_point(
    data = dat,
    aes(N_rate, Yield, fill = crop_cn, shape = crop_cn),
    color = "white", size = 1.75, stroke = 0.22, alpha = 0.62
  ) +
  geom_line(
    data = curve_df,
    aes(N_rate, fit, color = crop_cn),
    linewidth = 0.82
  ) +
  geom_segment(
    data = vertex_df,
    aes(x = N_rate, xend = N_rate, y = 0, yend = Yield, color = crop_cn),
    linetype = "dotted", linewidth = 0.38, show.legend = FALSE
  ) +
  geom_point(
    data = vertex_df,
    aes(N_rate, Yield, color = crop_cn),
    shape = 8, size = 2.8, stroke = 0.9, show.legend = FALSE
  ) +
  geom_text(
    data = annot_df,
    aes(
      x = xmax_panel * 0.985,
      y = ymax_panel * 0.972 - (as.integer(crop_cn) - 1) * ymax_panel * 0.055,
      label = label,
      color = crop_cn
    ),
    hjust = 1, vjust = 1, family = font_fam, size = 2.45,
    show.legend = FALSE
  ) +
  scale_color_manual(values = COLORS, name = "作物体系", drop = FALSE) +
  scale_fill_manual(values = COLORS, name = "作物体系", drop = FALSE) +
  scale_shape_manual(values = SHAPES, name = "作物体系", drop = FALSE) +
  scale_x_continuous(
    limits = c(0, xmax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = c(0, 100, 200, 300, 400, 450)
  ) +
  scale_y_continuous(
    limits = c(0, ymax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, ymax_panel, by = 4)
  ) +
  labs(
    x = "施氮量 (kg N ha⁻¹)",
    y = "作物产量 (t ha⁻¹)",
    tag = "(a)"
  ) +
  theme_fig31() +
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

# ── Panel B: fertilized-treatment boxplot ────────────────────────────────────
dat_fert <- dat %>% filter(N_rate > 0)

stat_df <- dat_fert %>%
  group_by(crop_cn) %>%
  summarise(
    n = n(),
    mean = mean(Yield),
    median = median(Yield),
    sd = sd(Yield),
    ymax = max(Yield),
    .groups = "drop"
  )

aov_fit <- aov(Yield ~ crop_cn, data = dat_fert)
aov_p <- summary(aov_fit)[[1]][["Pr(>F)"]][1]
tukey_table <- TukeyHSD(aov_fit, "crop_cn")$crop_cn

make_tukey_letters <- function(groups, means, tukey_table, alpha = 0.05) {
  groups <- as.character(groups)
  means <- means[groups]
  sig <- matrix(FALSE, length(groups), length(groups), dimnames = list(groups, groups))

  if (!is.null(tukey_table) && nrow(tukey_table) > 0) {
    for (rn in rownames(tukey_table)) {
      pair <- strsplit(rn, "-", fixed = TRUE)[[1]]
      if (length(pair) == 2 && all(pair %in% groups)) {
        sig[pair[1], pair[2]] <- tukey_table[rn, "p adj"] < alpha
        sig[pair[2], pair[1]] <- tukey_table[rn, "p adj"] < alpha
      }
    }
  }

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

means_named <- setNames(stat_df$mean, as.character(stat_df$crop_cn))
tukey_letters <- make_tukey_letters(crop_levels, means_named, tukey_table)
stat_df <- stat_df %>%
  mutate(
    tukey_group = unname(tukey_letters[as.character(crop_cn)]),
    label_y = pmin(ymax + 0.75, ymax_panel - 0.7)
  )

box_x_labels <- setNames(
  sprintf("%s\n(n=%d)", as.character(stat_df$crop_cn), stat_df$n),
  as.character(stat_df$crop_cn)
)

pB <- ggplot(dat_fert, aes(crop_cn, Yield, fill = crop_cn, color = crop_cn)) +
  geom_jitter(
    aes(shape = crop_cn),
    width = 0.16, height = 0, size = 1.25, alpha = 0.45,
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
    aes(x = crop_cn, y = label_y, label = tukey_group),
    inherit.aes = FALSE, family = font_fam, size = 3.15,
    fontface = "bold", color = "#222222"
  ) +
  annotate(
    "text",
    x = 2, y = ymax_panel * 0.98,
    hjust = 0.5, vjust = 1,
    label = ifelse(aov_p < 0.001,
                   "ANOVA: p < 0.001",
                   sprintf("ANOVA: p = %.3f", aov_p)),
    family = font_fam, size = 2.95, color = "#222222"
  ) +
  scale_fill_manual(values = COLORS, guide = "none") +
  scale_color_manual(values = COLORS, guide = "none") +
  scale_shape_manual(values = SHAPES, guide = "none") +
  scale_x_discrete(labels = box_x_labels) +
  scale_y_continuous(
    limits = c(0, ymax_panel),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, ymax_panel, by = 4)
  ) +
  labs(
    x = NULL,
    y = "作物产量 (t ha⁻¹)",
    tag = "(b)"
  ) +
  theme_fig31() +
  theme(
    axis.text.x       = element_text(size = 8.4, color = "#222222", lineheight = 0.95),
    plot.tag.position = c(0.025, 0.985),
    plot.margin       = margin(5, 5, 5, 4)
  )

# ── Combine ───────────────────────────────────────────────────────────────────
fig <- pA + pB + plot_layout(widths = c(2.05, 1))

# ── Export ────────────────────────────────────────────────────────────────────
png_file  <- file.path(output_dir, "Fig3-1_yield_Nrate_CN.png")
tiff_file <- file.path(output_dir, "Fig3-1_yield_Nrate_CN.tiff")
csv_file  <- file.path(output_dir, "Fig3-1_quadratic_fit_summary.csv")
box_csv_file <- file.path(output_dir, "Fig3-1_boxplot_summary.csv")

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
  crop          = enc2utf8(crop_levels),
  n             = unname(sapply(fit_list, function(f) f$n)),
  intercept_a   = unname(sapply(fit_list, function(f) round(f$a, 4))),
  linear_b      = unname(sapply(fit_list, function(f) round(f$b, 5))),
  quadratic_c   = unname(sapply(fit_list, function(f) signif(f$c, 4))),
  N_opt_kgN_ha  = unname(sapply(fit_list, function(f) round(f$vx, 1))),
  Y_max_t_ha    = unname(sapply(fit_list, function(f) round(f$vy, 2))),
  R2            = unname(sapply(fit_list, function(f) round(f$r2, 3))),
  p_quadratic   = unname(sapply(fit_list, function(f) signif(f$p, 4))),
  stringsAsFactors = FALSE
)
write.csv(fit_summary, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

box_summary <- stat_df %>%
  transmute(
    crop = enc2utf8(as.character(crop_cn)),
    n,
    mean_yield_t_ha = round(mean, 3),
    median_yield_t_ha = round(median, 3),
    sd_yield_t_ha = round(sd, 3),
    tukey_group,
    anova_p = signif(aov_p, 4)
  )
write.csv(box_summary, box_csv_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Chinese font: ", font_path)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Fit summary: ", csv_file)
message("Box summary: ", box_csv_file)
print(fit_summary)
print(box_summary)

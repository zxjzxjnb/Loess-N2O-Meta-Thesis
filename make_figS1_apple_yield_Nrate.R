#!/usr/bin/env Rscript

# Fig S1: 苹果产量 ~ 施氮量散点图（苹果园）
# Publication-quality Chinese supplementary figure with reproducible CJK output.

project_dir <- normalizePath(getwd())
local_lib   <- file.path(project_dir, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "scales", "grid"
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
  library(scales)
  library(grid)
})

# Config -----------------------------------------------------------------------
input_xlsx <- file.path(project_dir, "TableS1.xlsx")
output_dir <- file.path(project_dir, "output", "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

font_fam <- if (Sys.info()[["sysname"]] == "Darwin") "Arial Unicode MS" else "sans"

device_type <- if (Sys.info()[["sysname"]] == "Darwin" &&
                   isTRUE(capabilities("aqua"))) "quartz" else "cairo"

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

fmt_p_value <- function(p) {
  if (is.na(p)) "NA"
  else if (p < 0.001) "< 0.001"
  else sprintf("= %.3f", p)
}

excluded_pattern <- paste(
  c("biochar", "biocar", "DCD", "slow release", "control release",
    "controlled release", "inhibitor", "enhanced", "EENF", "EEF"),
  collapse = "|"
)

# Data -------------------------------------------------------------------------
raw <- suppressMessages(read_excel(
  input_xlsx, sheet = 1, skip = 2,
  col_names = c("Province", "Latitude", "Longitude", "AnnualTemp", "AnnualPrecip",
                "SOC", "TN", "BD", "pH", "Crop", "Year", "Treatment", "N_source",
                "Farming", "Replicates", "N_rate", "N2O", "N2O_sd", "Yield",
                "Yield_sd", "References")
))

dat <- raw %>%
  tidyr::fill(Crop, .direction = "down") %>%
  mutate(
    N_rate = suppressWarnings(as.numeric(N_rate)),
    Yield  = suppressWarnings(as.numeric(Yield)),
    exclusion_text = paste(
      if_else(is.na(Treatment), "", as.character(Treatment)),
      if_else(is.na(N_source), "", as.character(N_source)),
      if_else(is.na(Farming), "", as.character(Farming))
    ),
    is_excluded = grepl(excluded_pattern, exclusion_text, ignore.case = TRUE)
  ) %>%
  filter(Crop == "apple", !is.na(N_rate), !is.na(Yield), !is_excluded) %>%
  mutate(crop_cn = enc2utf8("苹果园"))

if (nrow(dat) == 0) {
  stop("No usable apple records after filtering.", call. = FALSE)
}

set.seed(20260428)
dat <- dat %>%
  mutate(N_rate_plot = pmin(pmax(N_rate + runif(n(), -16, 16), 0), 1050))

by_n_summary <- dat %>%
  group_by(N_rate) %>%
  summarise(
    n = n(),
    mean_yield_t_ha = mean(Yield),
    sd_yield_t_ha = sd(Yield),
    se_yield_t_ha = if_else(n > 1, sd_yield_t_ha / sqrt(n), 0),
    min_yield_t_ha = min(Yield),
    max_yield_t_ha = max(Yield),
    .groups = "drop"
  ) %>%
  mutate(
    ymin = pmax(mean_yield_t_ha - se_yield_t_ha, 0),
    ymax = mean_yield_t_ha + se_yield_t_ha
  )

# Quadratic trend is descriptive only; do not label its vertex as an optimum.
fit <- lm(Yield ~ N_rate + I(N_rate^2), data = dat)
fit_stats <- summary(fit)
coef_table <- fit_stats$coefficients
p_quadratic <- coef_table["I(N_rate^2)", "Pr(>|t|)"]

curve_df <- {
  xx <- seq(0, 1050, length.out = 260)
  pr <- predict(fit, newdata = data.frame(N_rate = xx),
                interval = "confidence", level = 0.95)
  data.frame(
    N_rate = xx,
    fit = pr[, "fit"],
    lwr = pmax(pr[, "lwr"], 0),
    upr = pr[, "upr"]
  )
}

# Aesthetics -------------------------------------------------------------------
apple_col <- "#1B9E77"
apple_dark <- "#0E6F5C"

theme_figS1 <- function() {
  theme_classic(base_family = font_fam, base_size = 9.4) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(color = "#333333", fill = NA, linewidth = 0.50),
      axis.line         = element_blank(),
      axis.ticks        = element_line(color = "#555555", linewidth = 0.28),
      axis.ticks.length = unit(0.11, "cm"),
      axis.text         = element_text(size = 8.5, color = "#222222"),
      axis.title        = element_text(size = 9.2, color = "#222222"),
      plot.margin       = margin(5, 6, 5, 5)
    )
}

fig <- ggplot() +
  geom_point(
    data = dat,
    aes(N_rate_plot, Yield),
    shape = 21, size = 2.45, stroke = 0.24,
    color = "white", fill = apple_col, alpha = 0.58
  ) +
  geom_errorbar(
    data = by_n_summary,
    aes(N_rate, ymin = ymin, ymax = ymax),
    width = 18, linewidth = 0.45, color = apple_dark
  ) +
  geom_point(
    data = by_n_summary,
    aes(N_rate, mean_yield_t_ha),
    shape = 23, size = 3.15, stroke = 0.58,
    color = apple_dark, fill = "white"
  ) +
  scale_x_continuous(
    limits = c(0, 1050),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, 1000, by = 200)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.01)),
    breaks = seq(0, 100, by = 20)
  ) +
  labs(
    x = "施氮量 (kg N ha⁻¹)",
    y = "苹果产量 (t ha⁻¹)"
  ) +
  theme_figS1()

# Export -----------------------------------------------------------------------
png_file  <- file.path(output_dir, "FigS1_apple_yield_Nrate_CN.png")
tiff_file <- file.path(output_dir, "FigS1_apple_yield_Nrate_CN.tiff")
fit_csv_file <- file.path(output_dir, "FigS1_apple_yield_Nrate_fit_summary.csv")
by_n_csv_file <- file.path(output_dir, "FigS1_apple_yield_Nrate_by_N_summary.csv")

png(filename = png_file, width = 95, height = 80, units = "mm",
    res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(png_file, 600)

tiff(filename = tiff_file, width = 95, height = 80, units = "mm",
     res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(tiff_file, 600)

fit_summary <- data.frame(
  model = "Yield ~ N_rate + I(N_rate^2)",
  crop = enc2utf8("苹果园"),
  n = nrow(dat),
  n_N_levels = dplyr::n_distinct(dat$N_rate),
  intercept_a = round(unname(coef(fit)[["(Intercept)"]]), 4),
  linear_b = round(unname(coef(fit)[["N_rate"]]), 6),
  quadratic_c = signif(unname(coef(fit)[["I(N_rate^2)"]]), 6),
  R2 = round(fit_stats$r.squared, 3),
  adj_R2 = round(fit_stats$adj.r.squared, 3),
  p_linear = signif(coef_table["N_rate", "Pr(>|t|)"], 4),
  p_quadratic = signif(p_quadratic, 4),
  AIC = round(AIC(fit), 3),
  stringsAsFactors = FALSE
)
write.csv(fit_summary, fit_csv_file, row.names = FALSE, fileEncoding = "UTF-8")

by_n_export <- by_n_summary %>%
  transmute(
    N_rate_kgN_ha = N_rate,
    n,
    mean_yield_t_ha = round(mean_yield_t_ha, 3),
    sd_yield_t_ha = round(sd_yield_t_ha, 3),
    se_yield_t_ha = round(se_yield_t_ha, 3),
    min_yield_t_ha = round(min_yield_t_ha, 3),
    max_yield_t_ha = round(max_yield_t_ha, 3)
  )
write.csv(by_n_export, by_n_csv_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Chinese font family: ", font_fam)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Fit summary: ", fit_csv_file)
message("N summary  : ", by_n_csv_file)
print(fit_summary)
print(by_n_export)

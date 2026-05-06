#!/usr/bin/env Rscript

# Fig 3-3: EF + yield-scaled N2O emission boxplots.
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
font_fam <- "fig33_cn"
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

normalize_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- tolower(trimws(x))
  x <- gsub("[-_]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x
}

make_management_key <- function(farming) {
  farming <- normalize_text(farming)
  vapply(farming, function(x) {
    if (!nzchar(x) || x %in% c("na", "nan", "none")) return("bare")
    if (grepl("bio plastic", x, fixed = TRUE) ||
        grepl("bioplastic", x, fixed = TRUE)) {
      return("plastic_film")
    }

    parts <- character()
    if (grepl("plastic film", x, fixed = TRUE) ||
        grepl("film mulching", x, fixed = TRUE)) {
      parts <- c(parts, "plastic_film")
    }
    if (grepl("straw", x, fixed = TRUE)) {
      parts <- c(parts, "straw")
    }
    if (grepl("green manure", x, fixed = TRUE)) {
      parts <- c(parts, "green_manure")
    }
    if (grepl("no till", x, fixed = TRUE) ||
        grepl("notill", x, fixed = TRUE)) {
      parts <- c(parts, "no_till")
    } else if (grepl("tillage", x, fixed = TRUE) || x == "till") {
      parts <- c(parts, "tillage")
    }
    if (grepl("no mulching", x, fixed = TRUE) && length(parts) == 0) {
      parts <- c(parts, "bare")
    }

    if (length(parts) > 0) {
      paste(unique(parts), collapse = "+")
    } else {
      gsub(" ", "_", x)
    }
  }, character(1), USE.NAMES = FALSE)
}

make_treatment_family <- function(treatment) {
  treatment <- normalize_text(treatment)
  treatment <- gsub("[^a-z0-9]+", "", treatment)
  vapply(treatment, function(x) {
    if (!nzchar(x) || x %in% c("na", "nan")) return("")
    if (grepl("^nm", x)) return("nm")
    if (grepl("^pm", x)) return("pm")
    if (grepl("^pr", x)) return("pr")
    if (grepl("^ct", x)) return("ct")
    if (grepl("^(tn|tl|tm|th)", x)) return("t")
    if (grepl("^(rn|rl|rm|rh)", x)) return("r")
    if (grepl("^bn", x)) return("bn")
    if (x == "n" || grepl("^n[0-9]+$", x)) return("n")
    if (x %in% c("ck", "control", "m", "npk", "mnpk")) return("")

    stripped <- gsub("[0-9]+$", "", x)
    if (stripped %in% c("n", "pr", "ct", "bn")) return(stripped)
    stripped
  }, character(1), USE.NAMES = FALSE)
}

dat <- raw %>%
  tidyr::fill(Crop, .direction = "down") %>%
  tidyr::fill(
    Province, Latitude, Longitude, AnnualTemp, AnnualPrecip,
    SOC, TN, BD, pH, Year, References,
    .direction = "down"
  ) %>%
  mutate(
    N_rate = suppressWarnings(as.numeric(N_rate)),
    N2O = suppressWarnings(as.numeric(N2O)),
    Yield = suppressWarnings(as.numeric(Yield)),
    N2O_sd = suppressWarnings(as.numeric(N2O_sd)),
    Yield_sd = suppressWarnings(as.numeric(Yield_sd)),
    Replicates = suppressWarnings(as.numeric(Replicates)),
    exclusion_text = paste(
      if_else(is.na(Treatment), "", as.character(Treatment)),
      if_else(is.na(N_source), "", as.character(N_source)),
      if_else(is.na(Farming), "", as.character(Farming))
    ),
    is_excluded = grepl(excluded_pattern, exclusion_text, ignore.case = TRUE),
    crop_cn = factor(enc2utf8(unname(crop_lookup[Crop])), levels = crop_levels),
    management_key = make_management_key(Farming),
    treatment_family = make_treatment_family(Treatment),
    row_id = row_number()
  ) %>%
  filter(Crop %in% crop_keys, !is_excluded)

match_cols <- c(
  "References", "Crop", "Year", "Province",
  "Latitude", "Longitude", "management_key"
)
family_match_cols <- c(match_cols, "treatment_family")

control_pool <- dat %>%
  filter(!is.na(N_rate), N_rate == 0, !is.na(N2O)) %>%
  mutate(control_row_id = row_id)

control_exact_df <- control_pool %>%
  group_by(across(all_of(family_match_cols))) %>%
  summarise(
    exact_control_N2O = mean(N2O, na.rm = TRUE),
    exact_control_N2O_sd = if_else(n() > 1, sd(N2O, na.rm = TRUE), NA_real_),
    exact_n_control = n(),
    exact_control_rows = paste(control_row_id, collapse = ";"),
    .groups = "drop"
  )

control_fallback_df <- control_pool %>%
  group_by(across(all_of(match_cols))) %>%
  summarise(
    fallback_control_N2O = mean(N2O, na.rm = TRUE),
    fallback_control_N2O_sd = if_else(n() > 1, sd(N2O, na.rm = TRUE), NA_real_),
    fallback_n_control = n(),
    fallback_n_control_families = n_distinct(treatment_family),
    fallback_control_families = paste(sort(unique(treatment_family)), collapse = ";"),
    fallback_control_rows = paste(control_row_id, collapse = ";"),
    .groups = "drop"
  )

ef_candidates <- dat %>%
  filter(!is.na(N_rate), N_rate > 0, !is.na(N2O)) %>%
  left_join(control_exact_df, by = family_match_cols) %>%
  left_join(control_fallback_df, by = match_cols) %>%
  mutate(
    has_exact_control = !is.na(exact_control_N2O),
    use_fallback_control = !has_exact_control &
      !is.na(fallback_control_N2O) &
      fallback_n_control_families <= 1,
    control_N2O = case_when(
      has_exact_control ~ exact_control_N2O,
      use_fallback_control ~ fallback_control_N2O,
      TRUE ~ NA_real_
    ),
    control_N2O_sd = case_when(
      has_exact_control ~ exact_control_N2O_sd,
      use_fallback_control ~ fallback_control_N2O_sd,
      TRUE ~ NA_real_
    ),
    n_control = case_when(
      has_exact_control ~ exact_n_control,
      use_fallback_control ~ fallback_n_control,
      TRUE ~ NA_integer_
    ),
    n_control_families = case_when(
      has_exact_control ~ 1L,
      use_fallback_control ~ fallback_n_control_families,
      TRUE ~ fallback_n_control_families
    ),
    control_rows = case_when(
      has_exact_control ~ exact_control_rows,
      use_fallback_control ~ fallback_control_rows,
      TRUE ~ NA_character_
    ),
    control_match = case_when(
      has_exact_control ~ "same_management_and_treatment_family",
      use_fallback_control ~ "same_management_single_control_family",
      !is.na(fallback_control_N2O) ~ "unmatched_multiple_control_families",
      TRUE ~ "unmatched_no_same_management_control"
    )
  )

ef_dat <- ef_candidates %>%
  filter(!is.na(control_N2O)) %>%
  mutate(
    value = (N2O - control_N2O) / N_rate * 100,
    metric = "EF",
    metric_label = "EF (%)",
    x_pos = as.numeric(crop_cn)
  )

yse_dat <- dat %>%
  filter(!is.na(N_rate), N_rate > 0, !is.na(N2O), !is.na(Yield), Yield > 0) %>%
  mutate(
    control_N2O = NA_real_,
    control_N2O_sd = NA_real_,
    n_control = NA_integer_,
    n_control_families = NA_integer_,
    control_rows = NA_character_,
    control_match = NA_character_,
    value = N2O / Yield,
    metric = "YSE",
    metric_label = "产量尺度N₂O排放 (kg N t⁻¹)",
    x_pos = as.numeric(crop_cn)
  )

if (nrow(ef_dat) == 0) stop("No EF records could be derived.", call. = FALSE)
if (nrow(yse_dat) == 0) stop("No yield-scaled emission records could be derived.", call. = FALSE)

# Aesthetics -------------------------------------------------------------------
COLORS <- c("春玉米" = "#2166AC",
            "冬小麦" = "#D6604D",
            "春小麦" = "#F59B20",
            "苹果园" = "#1B9E77")
SHAPES <- c("春玉米" = 21,
            "冬小麦" = 24,
            "春小麦" = 22,
            "苹果园" = 25)

theme_fig33 <- function() {
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
      plot.margin       = margin(5, 6, 5, 6)
    )
}

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

summarise_metric <- function(df, metric_name) {
  stat_df <- df %>%
    group_by(crop_cn) %>%
    summarise(
      n = n(),
      mean = mean(value),
      median = median(value),
      sd = sd(value),
      min = min(value),
      q1 = quantile(value, 0.25, names = FALSE),
      q3 = quantile(value, 0.75, names = FALSE),
      max = max(value),
      cv_percent = if_else(abs(mean) > .Machine$double.eps, sd / abs(mean) * 100, NA_real_),
      .groups = "drop"
    )

  pairwise_center <- make_pairwise_center_tests(df, metric_name)
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

  stat_df %>%
    mutate(
      metric = metric_name,
      x_pos = as.numeric(crop_cn),
      letter_group = unname(center_letters[as.character(crop_cn)]),
      letter_test = "pairwise Wilcoxon rank-sum, BH adjusted"
    )
}

make_pairwise_center_tests <- function(df, metric_name) {
  out <- bind_rows(lapply(seq_len(length(crop_levels) - 1), function(i) {
    bind_rows(lapply((i + 1):length(crop_levels), function(j) {
      g1 <- crop_levels[i]
      g2 <- crop_levels[j]
      x1 <- df %>% filter(crop_cn == g1) %>% pull(value)
      x2 <- df %>% filter(crop_cn == g2) %>% pull(value)
      p_wilcox <- if (length(x1) > 0 && length(x2) > 0) {
        suppressWarnings(wilcox.test(x1, x2, exact = FALSE)$p.value)
      } else {
        NA_real_
      }
      p_t <- if (length(x1) > 1 && length(x2) > 1) {
        suppressWarnings(t.test(x1, x2)$p.value)
      } else {
        NA_real_
      }
      data.frame(
        metric = metric_name,
        group1 = g1,
        group2 = g2,
        p_wilcox = p_wilcox,
        p_t_test = p_t,
        stringsAsFactors = FALSE
      )
    }))
  }))

  out %>%
    mutate(
      p_wilcox_BH = if_else(
        is.na(p_wilcox),
        NA_real_,
        p.adjust(p_wilcox, method = "BH")
      ),
      significant_center_0.05 = is.finite(p_wilcox_BH) & p_wilcox_BH < 0.05,
      center_test = "pairwise Wilcoxon rank-sum, BH adjusted"
    )
}

make_pairwise_ks <- function(df, metric_name) {
  bind_rows(lapply(seq_len(length(crop_levels) - 1), function(i) {
    bind_rows(lapply((i + 1):length(crop_levels), function(j) {
      g1 <- crop_levels[i]
      g2 <- crop_levels[j]
      x1 <- df %>% filter(crop_cn == g1) %>% pull(value)
      x2 <- df %>% filter(crop_cn == g2) %>% pull(value)
      p_val <- if (length(x1) > 0 && length(x2) > 0) {
        suppressWarnings(ks.test(x1, x2)$p.value)
      } else {
        NA_real_
      }
      data.frame(
        metric = metric_name,
        group1 = g1,
        group2 = g2,
        p_ks = p_val,
        significant_0.05 = is.finite(p_val) && p_val < 0.05,
        stringsAsFactors = FALSE
      )
    }))
  }))
}

make_omnibus_tests <- function(df, metric_name) {
  data.frame(
    metric = metric_name,
    p_kruskal = suppressWarnings(kruskal.test(value ~ crop_cn, data = df)$p.value),
    p_anova = suppressWarnings(summary(aov(value ~ crop_cn, data = df))[[1]][["Pr(>F)"]][1]),
    stringsAsFactors = FALSE
  )
}

make_density_df <- function(df, y_limits, x_offset = 0.055, width = 0.30, adjust = 0.85) {
  bind_rows(lapply(seq_along(crop_levels), function(i) {
    cn <- crop_levels[i]
    vals <- df %>% filter(crop_cn == cn) %>% pull(value)
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2 || length(unique(vals)) < 2) {
      return(data.frame())
    }
    den <- density(vals, from = min(vals), to = max(vals), n = 256,
                   adjust = adjust, na.rm = TRUE)
    keep <- den$x >= y_limits[1] & den$x <= y_limits[2]
    den$x <- den$x[keep]
    den$y <- den$y[keep]
    if (length(den$x) < 2) {
      return(data.frame())
    }
    scaled <- if (max(den$y) > 0) den$y / max(den$y) * width else den$y
    data.frame(
      crop_cn = factor(cn, levels = crop_levels),
      x = c(i + x_offset + scaled, rep(i + x_offset, length(scaled))),
      y = c(den$x, rev(den$x)),
      density_group = paste0(cn, "_density"),
      stringsAsFactors = FALSE
    )
  }))
}

metric_y_limits <- function(values, zero_floor = FALSE) {
  values <- values[is.finite(values)]
  y_min <- min(values)
  y_max <- max(values)
  span <- max(y_max - y_min, 0.1)
  lower <- if (zero_floor) 0 else floor((y_min - span * 0.10) * 10) / 10
  upper <- ceiling((y_max + span * 0.16) * 10) / 10
  c(lower, upper)
}

plot_metric <- function(df, stat_df, y_label, tag, y_limits, zero_line = FALSE) {
  box_shift <- 0.13
  point_shift <- 0.19
  density_df <- make_density_df(df, y_limits = y_limits)
  span <- y_limits[2] - y_limits[1]
  stat_df <- stat_df %>%
    mutate(
      label_y = pmin(max + span * 0.065, y_limits[2] - span * 0.045),
      box_x = x_pos - box_shift
    )
  box_x_labels <- setNames(
    sprintf("%s\n(n=%d)", as.character(stat_df$crop_cn), stat_df$n),
    as.character(seq_along(crop_levels))
  )

  p <- ggplot() +
    geom_polygon(
      data = density_df,
      aes(x = x, y = y, group = density_group, fill = crop_cn),
      color = NA, alpha = 0.18
    ) +
    geom_jitter(
      data = df,
      aes(x = x_pos + point_shift, y = value, fill = crop_cn, shape = crop_cn),
      width = 0.055, height = 0, size = 1.45, alpha = 0.50,
      stroke = 0.18, color = "white"
    ) +
    geom_boxplot(
      data = df,
      aes(x = x_pos - box_shift, y = value, group = crop_cn, fill = crop_cn),
      width = 0.34, alpha = 0.58, outlier.shape = NA,
      linewidth = 0.50, color = "#222222"
    ) +
    stat_summary(
      data = df,
      aes(x = x_pos - box_shift, y = value, group = crop_cn),
      fun = mean, geom = "point", shape = 23, size = 2.45,
      fill = "white", color = "#222222", stroke = 0.52
    ) +
    geom_text(
      data = stat_df,
      aes(x = box_x, y = label_y, label = letter_group),
      inherit.aes = FALSE, family = font_fam, size = 3.35,
      fontface = "bold", color = "#222222"
    ) +
    scale_fill_manual(values = COLORS, guide = "none") +
    scale_shape_manual(values = SHAPES, guide = "none") +
    scale_x_continuous(
      limits = c(0.50, length(crop_levels) + 0.52),
      breaks = seq_along(crop_levels),
      labels = box_x_labels,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = pretty(y_limits, n = 6),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(x = NULL, y = y_label, tag = tag) +
    coord_cartesian(clip = "off") +
    theme_fig33() +
    theme(
      axis.text.x = element_text(size = 8.4, color = "#222222", lineheight = 0.95),
      plot.tag.position = c(0.014, 0.985)
    )

  if (zero_line) {
    p <- p + geom_hline(yintercept = 0, linewidth = 0.28, linetype = "dashed",
                        color = "#777777")
  }
  p
}

# Statistics -------------------------------------------------------------------
ef_stat <- summarise_metric(ef_dat, "EF")
yse_stat <- summarise_metric(yse_dat, "YSE")
pairwise_center <- bind_rows(
  make_pairwise_center_tests(ef_dat, "EF"),
  make_pairwise_center_tests(yse_dat, "YSE")
)
pairwise_ks <- bind_rows(
  make_pairwise_ks(ef_dat, "EF"),
  make_pairwise_ks(yse_dat, "YSE")
)
omnibus_tests <- bind_rows(
  make_omnibus_tests(ef_dat, "EF"),
  make_omnibus_tests(yse_dat, "YSE")
)

ef_limits <- metric_y_limits(ef_dat$value, zero_floor = FALSE)
yse_limits <- metric_y_limits(yse_dat$value, zero_floor = TRUE)

pA <- plot_metric(
  ef_dat, ef_stat,
  y_label = "EF (%)",
  tag = "(a)",
  y_limits = ef_limits,
  zero_line = TRUE
)

pB <- plot_metric(
  yse_dat, yse_stat,
  y_label = "产量尺度N₂O排放 (kg N t⁻¹)",
  tag = "(b)",
  y_limits = yse_limits,
  zero_line = FALSE
)

fig <- pA / pB + plot_layout(heights = c(1, 1.04))

# Export -----------------------------------------------------------------------
png_file <- file.path(output_dir, "Fig3-3_EF_YSE_CN.png")
tiff_file <- file.path(output_dir, "Fig3-3_EF_YSE_CN.tiff")
derived_file <- file.path(output_dir, "Fig3-3_EF_YSE_derived_data.csv")
summary_file <- file.path(output_dir, "Fig3-3_EF_YSE_summary.csv")
pairwise_center_file <- file.path(output_dir, "Fig3-3_EF_YSE_pairwise_center_tests.csv")
pairwise_ks_file <- file.path(output_dir, "Fig3-3_EF_YSE_pairwise_ks.csv")
omnibus_file <- file.path(output_dir, "Fig3-3_EF_YSE_omnibus_tests.csv")
control_audit_file <- file.path(output_dir, "Fig3-3_EF_control_matching_audit.csv")

png(filename = png_file, width = 145, height = 178, units = "mm",
    res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(png_file, 600)

tiff(filename = tiff_file, width = 145, height = 178, units = "mm",
     res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(tiff_file, 600)

derived_data <- bind_rows(
  ef_dat %>%
    transmute(
      metric, crop = Crop, crop_cn = enc2utf8(as.character(crop_cn)),
      value,
      N_rate, N2O, control_N2O, n_control, n_control_families,
      management_key, treatment_family, control_match, control_rows,
      Yield, Treatment, N_source, Farming, Year,
      Province, Latitude, Longitude, References, row_id
    ),
  yse_dat %>%
    transmute(
      metric, crop = Crop, crop_cn = enc2utf8(as.character(crop_cn)),
      value,
      N_rate, N2O, control_N2O, n_control, n_control_families,
      management_key, treatment_family, control_match, control_rows,
      Yield, Treatment, N_source, Farming, Year,
      Province, Latitude, Longitude, References, row_id
    )
)
write.csv(derived_data, derived_file, row.names = FALSE, fileEncoding = "UTF-8")

summary_data <- bind_rows(ef_stat, yse_stat) %>%
  transmute(
    metric,
    crop = enc2utf8(as.character(crop_cn)),
    n,
    mean = round(mean, 4),
    median = round(median, 4),
    sd = round(sd, 4),
    min = round(min, 4),
    q1 = round(q1, 4),
    q3 = round(q3, 4),
    max = round(max, 4),
    cv_percent = round(cv_percent, 1),
    letter_group,
    letter_test
  )
write.csv(summary_data, summary_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pairwise_center, pairwise_center_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(pairwise_ks, pairwise_ks_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(omnibus_tests, omnibus_file, row.names = FALSE, fileEncoding = "UTF-8")

control_audit <- ef_candidates %>%
  transmute(
    crop = Crop,
    crop_cn = enc2utf8(as.character(crop_cn)),
    row_id,
    Year,
    Province,
    Latitude,
    Longitude,
    N_rate,
    N2O,
    Yield,
    Treatment,
    N_source,
    Farming,
    management_key,
    treatment_family,
    control_match,
    control_N2O,
    n_control,
    n_control_families,
    control_rows,
    fallback_control_families,
    References
  )
write.csv(control_audit, control_audit_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Chinese font: ", font_path)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Derived data: ", derived_file)
message("Summary CSV : ", summary_file)
message("Pairwise center tests: ", pairwise_center_file)
message("Pairwise K-S: ", pairwise_ks_file)
message("Omnibus tests: ", omnibus_file)
message("Control audit: ", control_audit_file)
print(summary_data)

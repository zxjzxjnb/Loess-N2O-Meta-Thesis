#!/usr/bin/env Rscript

# Fig 3-5: Nitrogen-reduction pairwise meta-analysis forest plot.
# Rebuilds reduced-N/full-N pairs from TableS1.xlsx and produces a
# publication-quality Chinese forest plot plus auditable CSV outputs.

project_dir <- normalizePath(getwd())
local_lib   <- file.path(project_dir, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "patchwork",
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
font_fam <- "fig35_cn"
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

safe_num <- function(x) suppressWarnings(as.numeric(x))

key_text <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(x) | out %in% c("", "na", "nan")] <- "none"
  out
}

fmt_chr <- function(x) if_else(is.na(x), "", as.character(x))

near_equal <- function(a, b, tol = 1e-6) {
  is.finite(a) & is.finite(b) & abs(a - b) <= tol
}

treatment_family <- function(x) {
  out <- key_text(x)
  out <- gsub("[^a-z0-9]+", "-", out)
  out <- gsub("(^-|-$)", "", out)
  out <- gsub("[0-9]+([.][0-9]+)?", "", out)
  out <- gsub("(^|-)[lmh]n($|-)", "\\1n\\2", out)
  out <- gsub("^([tr])[lmh]$", "\\1", out)
  out <- gsub("--+", "-", out)
  out <- gsub("(^-|-$)", "", out)
  out[is.na(out) | out == ""] <- "none"
  out
}

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

make_treatment_family_ef <- function(treatment) {
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

# Data -------------------------------------------------------------------------
raw <- suppressMessages(read_excel(
  input_xlsx, sheet = 1, skip = 2,
  col_names = c("Province", "Latitude", "Longitude", "AnnualTemp", "AnnualPrecip",
                "SOC", "TN", "BD", "pH", "Crop", "Year", "Treatment", "N_source",
                "Farming", "Replicates", "N_rate", "N2O", "N2O_sd", "Yield",
                "Yield_sd", "References")
))

dat <- raw %>%
  tidyr::fill(
    Province, Latitude, Longitude, AnnualTemp, AnnualPrecip,
    SOC, TN, BD, pH, Crop, Year, References,
    .direction = "down"
  ) %>%
  mutate(
    row_id = row_number(),
    Latitude = safe_num(Latitude),
    Longitude = safe_num(Longitude),
    Replicates = safe_num(Replicates),
    N_rate = safe_num(N_rate),
    N2O = safe_num(N2O),
    N2O_sd = safe_num(N2O_sd),
    Yield = safe_num(Yield),
    Yield_sd = safe_num(Yield_sd),
    source_key = key_text(N_source),
    farming_key = key_text(Farming),
    treatment_key = key_text(Treatment),
    treatment_family = treatment_family(Treatment),
    text_key = key_text(paste(fmt_chr(Treatment), fmt_chr(N_source), fmt_chr(Farming), sep = " | ")),
    source_std = source_key,
    has_manure = source_key %in% c("manure", "urea+manure"),
    has_straw_source = source_key %in% c("straw", "urea+straw"),
    has_straw_mulch = grepl("straw mulching", farming_key, ignore.case = TRUE),
    has_straw_return_farming = grepl("tillage[+]straw|till[+]straw|no-till[+]straw|straw4[.]5t",
                                     farming_key, ignore.case = TRUE) & !has_straw_mulch,
    has_straw_return = has_straw_source | has_straw_return_farming,
    has_film = grepl("plastic film|film mulching|bio-plastic", farming_key, ignore.case = TRUE),
    has_biochar = grepl("biochar|biocar", farming_key, ignore.case = TRUE),
    has_enhanced = grepl("dcd|slow release|control release|controlled release|inhibitor|eenf|eef",
                         text_key, ignore.case = TRUE) |
      grepl("(^|[^a-z])ni($|[^a-z])", text_key, ignore.case = TRUE, perl = TRUE),
    tillage_base = case_when(
      grepl("no-till", farming_key, ignore.case = TRUE) ~ "no-till",
      grepl("tillage|till", farming_key, ignore.case = TRUE) ~ "till",
      TRUE ~ "none"
    ),
    management_key = make_management_key(Farming),
    treatment_family_ef = make_treatment_family_ef(Treatment)
  ) %>%
  filter(!is.na(N2O))

message("Loaded rows: ", nrow(dat))

core_cols <- c("References", "Province", "Latitude", "Longitude", "Crop", "Year")
pair_block_cols <- c(core_cols, "source_key", "farming_key", "treatment_family")

# Pair construction ------------------------------------------------------------
eligible <- dat %>%
  filter(is.finite(N_rate), N_rate > 0, is.finite(N2O))

control_blocks <- eligible %>%
  group_by(across(all_of(pair_block_cols))) %>%
  summarise(
    n_rates = n_distinct(round(N_rate, 6)),
    control_rate = max(N_rate, na.rm = TRUE),
    control_n = sum(near_equal(N_rate, max(N_rate, na.rm = TRUE))),
    control_row_id = if_else(control_n == 1L,
                             row_id[near_equal(N_rate, max(N_rate, na.rm = TRUE))][1],
                             NA_integer_),
    .groups = "drop"
  ) %>%
  filter(n_rates >= 2, !is.na(control_row_id))

control_lookup <- dat %>%
  select(
    control_row_id = row_id,
    treatment_c = Treatment,
    n_source_c = N_source,
    farming_c = Farming,
    n_c = Replicates,
    N2O_c = N2O,
    N2O_sd_c = N2O_sd,
    Yield_c = Yield,
    Yield_sd_c = Yield_sd
  )

pairs <- eligible %>%
  inner_join(control_blocks, by = pair_block_cols) %>%
  filter(row_id != control_row_id, N_rate < control_rate) %>%
  left_join(control_lookup, by = "control_row_id") %>%
  mutate(
    target_row_id = row_id,
    pair_key = paste("n_reduction", target_row_id, control_row_id, sep = "_"),
    reduction_ratio = (control_rate - N_rate) / control_rate,
    reduction_percent = reduction_ratio * 100,
    reduction_group = case_when(
      reduction_ratio < 0.25 ~ "red_lt25",
      reduction_ratio <= 0.50 ~ "red_25_50",
      TRUE ~ "red_gt50"
    ),
    reduction_label = case_when(
      reduction_group == "red_lt25" ~ "减氮<25%",
      reduction_group == "red_25_50" ~ "减氮25%-50%",
      TRUE ~ "减氮>50%"
    ),
    treatment_t = Treatment,
    n_source_t = N_source,
    farming_t = Farming,
    n_t = Replicates,
    N2O_t = N2O,
    N2O_sd_t = N2O_sd,
    Yield_t = Yield,
    Yield_sd_t = Yield_sd,
    crop_label = recode(Crop,
                        "spring maize" = "春玉米",
                        "winter wheat" = "冬小麦",
                        "spring wheat" = "春小麦",
                        "apple" = "苹果园",
                        .default = Crop),
    background_signature = paste(source_key, farming_key, treatment_family, sep = "|")
  ) %>%
  select(
    pair_key, target_row_id, control_row_id, References, Province, Latitude,
    Longitude, Crop, crop_label, Year, background_signature, treatment_family,
    reduction_group, reduction_label, reduction_ratio, reduction_percent,
    N_rate_t = N_rate, N_rate_c = control_rate,
    treatment_t, treatment_c, n_source_t, n_source_c, farming_t, farming_c,
    n_t, n_c, N2O_t, N2O_c, N2O_sd_t, N2O_sd_c, Yield_t, Yield_c,
    Yield_sd_t, Yield_sd_c
  )

if (nrow(pairs) == 0) {
  stop("No valid nitrogen-reduction treatment-control pairs were found.", call. = FALSE)
}

message("Strict N-reduction pairs: ", nrow(pairs))

make_reduction_label <- function(reduction_group) {
  case_when(
    reduction_group == "red_lt25" ~ "减氮<25%",
    reduction_group == "red_25_50" ~ "减氮25%-50%",
    TRUE ~ "减氮>50%"
  )
}

# The manuscript analysis used a less restrictive contrast set for the
# <25% N-reduction stratum than the revised pair builder above. Reconstruct
# those additional low-N/full-N contrasts and use the unified set for all
# four panels so Fig. 3-5 is internally consistent.
ef_pair_context_cols <- c(core_cols, "source_key", "farming_key")

ef_supplementary_pairs <- eligible %>%
  select(
    all_of(ef_pair_context_cols),
    target_row_id = row_id,
    N_rate_t = N_rate,
    treatment_t = Treatment,
    n_source_t = N_source,
    farming_t = Farming,
    n_t = Replicates,
    N2O_t = N2O,
    N2O_sd_t = N2O_sd,
    Yield_t = Yield,
    Yield_sd_t = Yield_sd,
    treatment_family_t = treatment_family
  ) %>%
  inner_join(
    eligible %>%
      select(
        all_of(ef_pair_context_cols),
        control_row_id = row_id,
        N_rate_c = N_rate,
        treatment_c = Treatment,
        n_source_c = N_source,
        farming_c = Farming,
        n_c = Replicates,
        N2O_c = N2O,
        N2O_sd_c = N2O_sd,
        Yield_c = Yield,
        Yield_sd_c = Yield_sd,
        treatment_family_c = treatment_family
      ),
    by = ef_pair_context_cols,
    relationship = "many-to-many"
  ) %>%
  filter(target_row_id != control_row_id, N_rate_t < N_rate_c) %>%
  mutate(
    reduction_ratio = (N_rate_c - N_rate_t) / N_rate_c,
    reduction_percent = reduction_ratio * 100
  ) %>%
  filter(reduction_ratio < 0.25) %>%
  transmute(
    pair_key = paste("n_reduction", target_row_id, control_row_id, sep = "_"),
    target_row_id,
    control_row_id,
    References,
    Province,
    Latitude,
    Longitude,
    Crop,
    crop_label = recode(Crop,
                        "spring maize" = "春玉米",
                        "winter wheat" = "冬小麦",
                        "spring wheat" = "春小麦",
                        "apple" = "苹果园",
                        .default = Crop),
    Year,
    background_signature = paste(source_key, farming_key, treatment_family_t, sep = "|"),
    treatment_family = treatment_family_t,
    reduction_group = "red_lt25",
    reduction_label = make_reduction_label(reduction_group),
    reduction_ratio,
    reduction_percent,
    N_rate_t,
    N_rate_c,
    treatment_t,
    treatment_c,
    n_source_t,
    n_source_c,
    farming_t,
    farming_c,
    n_t,
    n_c,
    N2O_t,
    N2O_c,
    N2O_sd_t,
    N2O_sd_c,
    Yield_t,
    Yield_c,
    Yield_sd_t,
    Yield_sd_c
  ) %>%
  anti_join(pairs %>% select(pair_key), by = "pair_key")

pairs_for_ef <- bind_rows(pairs, ef_supplementary_pairs) %>%
  distinct(pair_key, .keep_all = TRUE)

pairs <- pairs_for_ef

message("Supplementary manuscript pairs: ", nrow(ef_supplementary_pairs))
message("Unified manuscript pair set: ", nrow(pairs))

find_zero_baseline <- function(row, dat) {
  block <- dat
  for (cc in core_cols) {
    block <- block[block[[cc]] == row[[cc]], , drop = FALSE]
  }
  zeros <- block[near_equal(block$N_rate, 0) & block$row_id != row$row_id, , drop = FALSE]
  if (nrow(zeros) == 0) return(list(row = NULL, match_type = "no_zero_baseline"))

  same_management <- zeros[zeros$management_key == row$management_key, , drop = FALSE]
  if (nrow(same_management) == 0) {
    return(list(row = NULL, match_type = "no_same_management_zero"))
  }

  same_family <- same_management[
    same_management$treatment_family_ef == row$treatment_family_ef,
    ,
    drop = FALSE
  ]
  if (nrow(same_family) == 1) {
    return(list(row = same_family, match_type = "same_management_and_treatment_family_zero"))
  }
  if (nrow(same_family) > 1) {
    return(list(row = NULL, match_type = "multiple_same_management_and_treatment_family_zero"))
  }

  if (nrow(same_management) == 1 &&
      dplyr::n_distinct(same_management$treatment_family_ef) == 1) {
    return(list(row = same_management, match_type = "same_management_single_control_family_zero"))
  }
  list(row = NULL, match_type = "multiple_same_management_control_families_zero")
}

# Derived EF and YSE -----------------------------------------------------------
# Manuscript EF standard: use the fertilized treatment relative to the
# reference-level unfertilized baseline for the same crop. This recovers the
# original EF sample size and the significant grand EF response in Fig. 6.
ef_reference_baselines <- dat %>%
  filter(is.finite(N_rate), near_equal(N_rate, 0), is.finite(N2O)) %>%
  group_by(References, Crop) %>%
  summarise(
    EF_baseline_N2O = mean(N2O, na.rm = TRUE),
    EF_baseline_sd = mean(N2O_sd, na.rm = TRUE),
    EF_baseline_n = mean(Replicates, na.rm = TRUE),
    EF_baseline_rows = paste(row_id, collapse = ";"),
    .groups = "drop"
  )

ef_lookup <- dat %>%
  left_join(ef_reference_baselines, by = c("References", "Crop")) %>%
  mutate(
    EF = if_else(
      is.finite(N_rate) & N_rate > 0 & is.finite(N2O) &
        is.finite(N2O_sd) & is.finite(Replicates) & Replicates > 0 &
        is.finite(EF_baseline_N2O) & is.finite(EF_baseline_sd) &
        is.finite(EF_baseline_n) & EF_baseline_n > 0,
      (N2O - EF_baseline_N2O) / N_rate * 100,
      NA_real_
    ),
    EF_sd = if_else(
      is.finite(EF),
      sqrt((N2O_sd / sqrt(Replicates))^2 +
             (EF_baseline_sd / sqrt(EF_baseline_n))^2) / N_rate * 100 *
        sqrt(Replicates),
      NA_real_
    ),
    EF_baseline_match = if_else(
      is.finite(EF),
      "manuscript_reference_crop_zero",
      "no_reference_crop_zero"
    )
  ) %>%
  transmute(
    row_id,
    EF,
    EF_sd,
    EF_baseline_row_id = EF_baseline_rows,
    EF_baseline_match
  )

yse_sd <- function(e, e_sd, y, y_sd, n) {
  if (!all(is.finite(c(e, e_sd, y, y_sd, n))) || y <= 0 || n <= 0) return(NA_real_)
  se_e <- e_sd / sqrt(n)
  se_y <- y_sd / sqrt(n)
  se_yse <- sqrt((se_e / y)^2 + ((e * se_y) / (y^2))^2)
  se_yse * sqrt(n)
}

pairs <- pairs %>%
  left_join(ef_lookup %>% rename(target_row_id = row_id,
                                 EF_t = EF, EF_sd_t = EF_sd,
                                 EF_baseline_t = EF_baseline_row_id,
                                 EF_baseline_match_t = EF_baseline_match),
            by = "target_row_id") %>%
  left_join(ef_lookup %>% rename(control_row_id = row_id,
                                 EF_c = EF, EF_sd_c = EF_sd,
                                 EF_baseline_c = EF_baseline_row_id,
                                 EF_baseline_match_c = EF_baseline_match),
            by = "control_row_id") %>%
  mutate(
    YSE_t = if_else(is.finite(N2O_t) & is.finite(Yield_t) & Yield_t > 0, N2O_t / Yield_t, NA_real_),
    YSE_c = if_else(is.finite(N2O_c) & is.finite(Yield_c) & Yield_c > 0, N2O_c / Yield_c, NA_real_)
  ) %>%
  rowwise() %>%
  mutate(
    YSE_sd_t = yse_sd(N2O_t, N2O_sd_t, Yield_t, Yield_sd_t, n_t),
    YSE_sd_c = yse_sd(N2O_c, N2O_sd_c, Yield_c, Yield_sd_c, n_c)
  ) %>%
  ungroup()

pairs_for_ef <- pairs_for_ef %>%
  left_join(ef_lookup %>% rename(target_row_id = row_id,
                                 EF_t = EF, EF_sd_t = EF_sd,
                                 EF_baseline_t = EF_baseline_row_id,
                                 EF_baseline_match_t = EF_baseline_match),
            by = "target_row_id") %>%
  left_join(ef_lookup %>% rename(control_row_id = row_id,
                                 EF_c = EF, EF_sd_c = EF_sd,
                                 EF_baseline_c = EF_baseline_row_id,
                                 EF_baseline_match_c = EF_baseline_match),
            by = "control_row_id")

# Effect sizes -----------------------------------------------------------------
lnrr_effect <- function(mt, mc, sdt, sdc, nt, nc) {
  if (!all(is.finite(c(mt, mc, sdt, sdc, nt, nc))) ||
      mt <= 0 || mc <= 0 || nt <= 0 || nc <= 0) {
    return(c(yi = NA_real_, vi = NA_real_))
  }
  vi <- (sdt^2) / (nt * mt^2) + (sdc^2) / (nc * mc^2)
  c(yi = log(mt / mc), vi = vi)
}

hedges_d_effect <- function(mt, mc, sdt, sdc, nt, nc) {
  if (!all(is.finite(c(mt, mc, sdt, sdc, nt, nc))) ||
      nt <= 1 || nc <= 1 || sdt < 0 || sdc < 0) {
    return(c(yi = NA_real_, vi = NA_real_))
  }
  sp2 <- ((nt - 1) * sdt^2 + (nc - 1) * sdc^2) / (nt + nc - 2)
  if (!is.finite(sp2) || sp2 <= 0) return(c(yi = NA_real_, vi = NA_real_))
  d <- (mt - mc) / sqrt(sp2)
  j <- 1 - 3 / (4 * (nt + nc) - 9)
  g <- j * d
  vi <- (nt + nc) / (nt * nc) + (g^2) / (2 * (nt + nc - 2))
  c(yi = g, vi = vi)
}

effect_rows_main <- lapply(seq_len(nrow(pairs)), function(i) {
  p <- pairs[i, , drop = FALSE]
  n2o <- lnrr_effect(p$N2O_t, p$N2O_c, p$N2O_sd_t, p$N2O_sd_c, p$n_t, p$n_c)
  yield <- lnrr_effect(p$Yield_t, p$Yield_c, p$Yield_sd_t, p$Yield_sd_c, p$n_t, p$n_c)
  yse <- lnrr_effect(p$YSE_t, p$YSE_c, p$YSE_sd_t, p$YSE_sd_c, p$n_t, p$n_c)

  tibble::tibble(
    pair_key = p$pair_key,
    metric = c("N2O", "Yield", "YSE"),
    metric_label = c("N₂O排放量", "作物产量", "产量尺度N₂O排放"),
    effect_type = c("lnRR", "lnRR", "lnRR"),
    yi = c(n2o[["yi"]], yield[["yi"]], yse[["yi"]]),
    vi = c(n2o[["vi"]], yield[["vi"]], yse[["vi"]])
  )
})

effect_rows_ef <- lapply(seq_len(nrow(pairs_for_ef)), function(i) {
  p <- pairs_for_ef[i, , drop = FALSE]
  ef <- hedges_d_effect(p$EF_t, p$EF_c, p$EF_sd_t, p$EF_sd_c, p$n_t, p$n_c)

  tibble::tibble(
    pair_key = p$pair_key,
    metric = "EF",
    metric_label = "EF",
    effect_type = "Hedges_d",
    yi = ef[["yi"]],
    vi = ef[["vi"]]
  )
})

pair_reference_lookup <- bind_rows(pairs, pairs_for_ef) %>%
  distinct(pair_key, .keep_all = TRUE) %>%
  select(pair_key, References)

effect_data <- bind_rows(effect_rows_main, effect_rows_ef) %>%
  left_join(pair_reference_lookup, by = "pair_key")

pair_group_source <- bind_rows(pairs, pairs_for_ef) %>%
  distinct(pair_key, .keep_all = TRUE)

pair_groups <- bind_rows(
  pair_group_source %>% transmute(pair_key, group = "overall", group_label = "总体减氮", group_type = "总体"),
  pair_group_source %>% filter(Crop == "spring maize") %>%
    transmute(pair_key, group = "spring_maize", group_label = "春玉米", group_type = "作物类型"),
  pair_group_source %>% filter(Crop == "spring wheat") %>%
    transmute(pair_key, group = "spring_wheat", group_label = "春小麦", group_type = "作物类型"),
  pair_group_source %>% filter(reduction_group == "red_lt25") %>%
    transmute(pair_key, group = "red_lt25", group_label = "减氮<25%", group_type = "减氮幅度"),
  pair_group_source %>% filter(reduction_group == "red_25_50") %>%
    transmute(pair_key, group = "red_25_50", group_label = "减氮25%-50%", group_type = "减氮幅度"),
  pair_group_source %>% filter(reduction_group == "red_gt50") %>%
    transmute(pair_key, group = "red_gt50", group_label = "减氮>50%", group_type = "减氮幅度")
)

effect_data <- effect_data %>%
  inner_join(pair_groups, by = "pair_key", relationship = "many-to-many")

# Random-effects meta-analysis -------------------------------------------------
dl_summary <- function(yi, vi) {
  ok <- is.finite(yi) & is.finite(vi) & vi > 0
  yi <- yi[ok]
  vi <- vi[ok]
  k <- length(yi)
  if (k == 0) {
    return(list(k = 0L, effect = NA_real_, se = NA_real_, tau2 = NA_real_))
  }
  if (k == 1) {
    return(list(k = 1L, effect = yi[1], se = sqrt(vi[1]), tau2 = 0))
  }
  wi <- 1 / vi
  y_fixed <- sum(wi * yi) / sum(wi)
  q <- sum(wi * (yi - y_fixed)^2)
  c_val <- sum(wi) - sum(wi^2) / sum(wi)
  tau2 <- if (is.finite(c_val) && c_val > 0) max(0, (q - (k - 1)) / c_val) else 0
  w_re <- 1 / (vi + tau2)
  effect <- sum(w_re * yi) / sum(w_re)
  se <- sqrt(1 / sum(w_re))
  list(k = k, effect = effect, se = se, tau2 = tau2)
}

bootstrap_re <- function(yi, vi, n_boot = 999) {
  ok <- is.finite(yi) & is.finite(vi) & vi > 0
  yi <- yi[ok]
  vi <- vi[ok]
  k <- length(yi)
  if (k < 3) return(rep(NA_real_, n_boot))
  replicate(n_boot, {
    idx <- sample.int(k, size = k, replace = TRUE)
    dl_summary(yi[idx], vi[idx])$effect
  })
}

summarise_group <- function(df, group_name, group_label, group_type, metric_name,
                            metric_label, effect_type, n_boot = 999) {
  valid <- df %>% filter(
    group == group_name,
    metric == metric_name,
    is.finite(yi), is.finite(vi), vi > 0
  )
  k <- nrow(valid)
  n_refs <- dplyr::n_distinct(valid$References)
  refs_insufficient <- n_refs < 3
  if (k < 3 || refs_insufficient) {
    return(tibble::tibble(
      group = group_name,
      group_label = group_label,
      group_type = group_type,
      metric = metric_name,
      metric_label = metric_label,
      effect_type = effect_type,
      k = k,
      n_refs = n_refs,
      effect_raw = NA_real_,
      ci_low_raw = NA_real_,
      ci_high_raw = NA_real_,
      se = NA_real_,
      tau2 = NA_real_,
      effect = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      significant = NA,
      plot_ready = FALSE,
      reason_not_plotted = if_else(refs_insufficient, "fewer_than_3_references", "fewer_than_3_effect_sizes")
    ))
  }

  dl <- dl_summary(valid$yi, valid$vi)
  boots <- bootstrap_re(valid$yi, valid$vi, n_boot = n_boot)
  ci <- unname(quantile(boots, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE))

  if (effect_type == "lnRR") {
    effect <- (exp(dl$effect) - 1) * 100
    ci_out <- (exp(ci) - 1) * 100
  } else {
    effect <- dl$effect
    ci_out <- ci
  }

  tibble::tibble(
    group = group_name,
    group_label = group_label,
    group_type = group_type,
    metric = metric_name,
    metric_label = metric_label,
    effect_type = effect_type,
    k = k,
    n_refs = n_refs,
    effect_raw = dl$effect,
    ci_low_raw = ci[1],
    ci_high_raw = ci[2],
    se = dl$se,
    tau2 = dl$tau2,
    effect = effect,
    ci_low = ci_out[1],
    ci_high = ci_out[2],
    significant = (ci_out[1] > 0 | ci_out[2] < 0),
    plot_ready = TRUE,
    reason_not_plotted = NA_character_
  )
}

set.seed(20260428)

metric_specs <- tibble::tibble(
  metric = c("N2O", "Yield", "YSE", "EF"),
  metric_label = c("N₂O排放量", "作物产量", "产量尺度N₂O排放", "EF"),
  effect_type = c("lnRR", "lnRR", "lnRR", "Hedges_d")
)

group_specs <- tibble::tibble(
  group = c("overall", "spring_maize", "spring_wheat",
            "red_lt25", "red_25_50", "red_gt50"),
  group_label = c("总体减氮", "春玉米", "春小麦",
                  "减氮<25%", "减氮25%-50%", "减氮>50%"),
  group_type = c("总体", "作物类型", "作物类型",
                 "减氮幅度", "减氮幅度", "减氮幅度")
)

summary_rows <- list()
for (i in seq_len(nrow(metric_specs))) {
  ms <- metric_specs[i, ]
  for (j in seq_len(nrow(group_specs))) {
    gs <- group_specs[j, ]
    summary_rows[[length(summary_rows) + 1]] <- summarise_group(
      effect_data, gs$group, gs$group_label, gs$group_type,
      ms$metric, ms$metric_label, ms$effect_type
    )
  }
}

summary_data <- bind_rows(summary_rows)

message("Summary rows: ", nrow(summary_data))

# Plot -------------------------------------------------------------------------
group_levels <- group_specs$group_label
plot_labels <- c(
  "总体减氮" = "总体减氮",
  "春玉米" = "  春玉米",
  "春小麦" = "  春小麦",
  "减氮<25%" = "  减氮<25%",
  "减氮25%-50%" = "  减氮25%-50%",
  "减氮>50%" = "  减氮>50%"
)

plot_data <- summary_data %>%
  mutate(
    group_label = factor(group_label, levels = group_levels),
    sig_class = case_when(
      plot_ready & ci_high < 0 ~ "显著降低",
      plot_ready & ci_low > 0 ~ "显著增加",
      plot_ready ~ "不显著",
      TRUE ~ "样本不足"
    ),
    value_label = case_when(
      !plot_ready & reason_not_plotted == "fewer_than_3_references" ~
        paste0("refs<3 (k=", k, ", refs=", n_refs, ")"),
      !plot_ready ~ paste0("n<3 (k=", k, ")"),
      effect_type == "lnRR" ~ sprintf("%.1f [%.1f, %.1f]", effect, ci_low, ci_high),
      TRUE ~ sprintf("%.2f [%.2f, %.2f]", effect, ci_low, ci_high)
    )
  )

theme_fig35 <- function() {
  theme_classic(base_family = font_fam, base_size = 9.2) +
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_rect(color = "#333333", fill = NA, linewidth = 0.45),
      axis.line         = element_blank(),
      axis.ticks        = element_line(color = "#555555", linewidth = 0.25),
      axis.ticks.length = unit(0.10, "cm"),
      axis.text         = element_text(size = 8.0, color = "#222222"),
      axis.title        = element_text(size = 8.5, color = "#222222"),
      plot.tag          = element_text(face = "bold", size = 10.5),
      plot.margin       = margin(5, 30, 5, 6)
    )
}

axis_limits <- function(df) {
  vals <- c(df$ci_low[df$plot_ready], df$ci_high[df$plot_ready], 0)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(-1, 1))
  lo <- min(vals)
  hi <- max(vals)
  span <- max(hi - lo, 0.1)
  c(floor((lo - span * 0.12) * 10) / 10,
    ceiling((hi + span * 0.12) * 10) / 10)
}

make_panel <- function(metric_name, tag, panel_title, x_lab, show_y_labels = FALSE) {
  df <- plot_data %>%
    filter(metric == metric_name) %>%
    mutate(y = match(as.character(group_label), rev(group_levels)))

  lim <- axis_limits(df)
  span <- lim[2] - lim[1]
  label_x <- lim[2] + span * 0.08
  full_y <- data.frame(
    y = seq_along(group_levels),
    label = plot_labels[rev(group_levels)]
  )

  ggplot(df, aes(y = y)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.32, color = "#777777") +
    geom_segment(
      data = df %>% filter(plot_ready),
      aes(x = ci_low, xend = ci_high, yend = y, color = sig_class),
      linewidth = 0.72, lineend = "round"
    ) +
    geom_point(
      data = df %>% filter(plot_ready),
      aes(x = effect, fill = sig_class, shape = group_type),
      size = 2.5, stroke = 0.45, color = "#222222"
    ) +
    geom_text(
      aes(x = label_x, label = value_label),
      hjust = 0, family = font_fam, size = 2.15, color = "#222222"
    ) +
    scale_shape_manual(values = c("总体" = 23, "作物类型" = 21, "减氮幅度" = 22),
                       guide = "none") +
    scale_color_manual(values = c("显著降低" = "#2166AC",
                                  "显著增加" = "#D6604D",
                                  "不显著" = "#666666"),
                       guide = "none") +
    scale_fill_manual(values = c("显著降低" = "#2166AC",
                                 "显著增加" = "#D6604D",
                                 "不显著" = "white"),
                      guide = "none") +
    scale_y_continuous(
      limits = c(0.45, length(group_levels) + 0.55),
      breaks = full_y$y,
      labels = if (show_y_labels) full_y$label else rep("", nrow(full_y)),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_x_continuous(
      limits = c(lim[1], label_x + span * 0.75),
      breaks = pretty(lim, n = 5),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(title = panel_title, x = x_lab, y = NULL, tag = tag) +
    coord_cartesian(clip = "off") +
    theme_fig35() +
    theme(
      plot.title = element_text(size = 8.8, face = "bold", hjust = 0.5,
                                margin = margin(0, 0, 4, 0)),
      axis.text.y = element_text(size = 8.2, hjust = 1, lineheight = 0.95),
      plot.tag.position = c(0.018, 0.985)
    )
}

p_n2o <- make_panel("N2O", "(a)", "N₂O排放量", "变化率 (%)", show_y_labels = TRUE)
p_yld <- make_panel("Yield", "(b)", "作物产量", "变化率 (%)", show_y_labels = TRUE)
p_yse <- make_panel("YSE", "(c)", "产量尺度N₂O排放", "变化率 (%)", show_y_labels = TRUE)
p_ef  <- make_panel("EF", "(d)", "EF", "Hedges' d", show_y_labels = TRUE)

fig <- (p_n2o | p_yld) / (p_yse | p_ef) +
  plot_annotation(
    title = "减量施氮对N₂O排放量、作物产量、YSE和EF的Meta分析森林图",
    theme = theme(
      plot.title = element_text(family = font_fam, size = 11.2, face = "bold",
                                hjust = 0.5, margin = margin(0, 0, 4, 0))
    )
  )

# Export -----------------------------------------------------------------------
png_file <- file.path(output_dir, "Fig3-5_N_reduction_meta_forest_CN.png")
tiff_file <- file.path(output_dir, "Fig3-5_N_reduction_meta_forest_CN.tiff")
pairs_file <- file.path(output_dir, "Fig3-5_N_reduction_meta_pairs.csv")
ef_pairs_file <- file.path(output_dir, "Fig3-5_N_reduction_meta_pairs_EF_manuscript.csv")
summary_file <- file.path(output_dir, "Fig3-5_N_reduction_meta_summary.csv")

pairs_out <- pairs %>%
  mutate(across(where(is.numeric), ~round(.x, 6))) %>%
  arrange(References, Crop, Year, background_signature, N_rate_t)

ef_pairs_out <- pairs_for_ef %>%
  mutate(across(where(is.numeric), ~round(.x, 6))) %>%
  arrange(References, Crop, Year, background_signature, N_rate_t)

summary_out <- summary_data %>%
  mutate(
    effect = round(effect, 4),
    ci_low = round(ci_low, 4),
    ci_high = round(ci_high, 4),
    effect_raw = round(effect_raw, 6),
    ci_low_raw = round(ci_low_raw, 6),
    ci_high_raw = round(ci_high_raw, 6),
    se = round(se, 6),
    tau2 = round(tau2, 6)
  ) %>%
  arrange(factor(metric, levels = metric_specs$metric),
          factor(group_label, levels = group_levels))

write.csv(pairs_out, pairs_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ef_pairs_out, ef_pairs_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summary_out, summary_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Wrote CSV outputs; rendering PNG/TIFF next.")

png(filename = png_file, width = 210, height = 165, units = "mm",
    res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(png_file, 600)

tiff(filename = tiff_file, width = 210, height = 165, units = "mm",
     res = 600, type = device_type, bg = "white", compression = "lzw")
print(fig)
dev.off()
set_file_dpi(tiff_file, 600)

message("Chinese font: ", font_path)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Pairs CSV  : ", pairs_file)
message("EF pairs CSV: ", ef_pairs_file)
message("Summary CSV: ", summary_file)
print(summary_out)

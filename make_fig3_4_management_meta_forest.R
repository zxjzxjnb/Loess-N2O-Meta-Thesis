#!/usr/bin/env Rscript

# Fig 3-4: Management-practice pairwise meta-analysis forest plot.
# Rebuilds strict treatment-control pairs from TableS1.xlsx and produces a
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
font_fam <- "fig34_cn"
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

trim_lower <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(x) | out %in% c("", "na", "nan")] <- NA_character_
  out
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

near_equal <- function(a, b, tol = 1e-6) {
  is.finite(a) & is.finite(b) & abs(a - b) <= tol
}

fmt_chr <- function(x) if_else(is.na(x), "", as.character(x))

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
    treatment_low = trim_lower(Treatment),
    source_low = trim_lower(N_source),
    farming_low = trim_lower(Farming),
    text_low = trim_lower(paste(fmt_chr(Treatment), fmt_chr(N_source), fmt_chr(Farming), sep = " | ")),
    source_std = case_when(
      is.na(source_low) ~ "none",
      source_low %in% c("urea", "manure", "urea+manure", "straw", "urea+straw",
                        "green manure", "urea+green manure") ~ source_low,
      TRUE ~ source_low
    ),
    has_manure = source_low %in% c("manure", "urea+manure"),
    has_straw_source = source_low %in% c("straw", "urea+straw"),
    has_straw_mulch = grepl("straw mulching", farming_low, ignore.case = TRUE),
    has_straw_return_farming = grepl("tillage[+]straw|till[+]straw|no-till[+]straw|straw4[.]5t",
                                     farming_low, ignore.case = TRUE) & !has_straw_mulch,
    has_straw_return = has_straw_source | has_straw_return_farming,
    has_film = grepl("plastic film|film mulching|bio-plastic", farming_low, ignore.case = TRUE),
    has_no_mulch = grepl("no mulching", farming_low, ignore.case = TRUE),
    has_biochar = grepl("biochar|biocar", farming_low, ignore.case = TRUE),
    has_enhanced = grepl("dcd|slow release|control release|controlled release|inhibitor|eenf|eef",
                         text_low, ignore.case = TRUE) |
      grepl("(^|[^a-z])ni($|[^a-z])", text_low, ignore.case = TRUE, perl = TRUE),
    has_green_manure = source_low %in% c("green manure", "urea+green manure"),
    tillage_base = case_when(
      grepl("no-till", farming_low, ignore.case = TRUE) ~ "no-till",
      grepl("tillage|till", farming_low, ignore.case = TRUE) ~ "till",
      TRUE ~ "none"
    ),
    management_key = make_management_key(Farming),
    treatment_family_ef = make_treatment_family_ef(Treatment)
  ) %>%
  filter(!is.na(N2O))

core_cols <- c("References", "Province", "Latitude", "Longitude", "Crop", "Year")

measure_specs <- tibble::tibble(
  measure = c("manure", "straw_return", "film", "straw_mulching", "biochar", "enhanced"),
  measure_label = c("粪肥施用", "秸秆还田", "地膜覆盖", "秸秆覆盖", "生物炭添加", "增效肥料"),
  parent = c("organic", "organic", "mulching", "mulching", NA_character_, NA_character_)
)

measure_label_lookup <- setNames(measure_specs$measure_label, measure_specs$measure)

is_target <- function(df, measure_name) {
  switch(
    measure_name,
    manure = df$has_manure & !df$has_green_manure,
    straw_return = df$has_straw_return & !df$has_straw_mulch,
    film = df$has_film & !df$has_biochar & !df$has_enhanced &
      !df$has_straw_return & !df$has_straw_mulch & !grepl("straw", df$farming_low, ignore.case = TRUE),
    straw_mulching = df$has_straw_mulch & !df$has_biochar & !df$has_enhanced,
    biochar = df$has_biochar,
    enhanced = df$has_enhanced,
    rep(FALSE, nrow(df))
  )
}

expected_control_source <- function(target_row, measure_name) {
  if (measure_name %in% c("manure", "straw_return")) return("urea")
  target_row$source_std
}

base_treatment <- function(treatment, measure_name) {
  x <- trim_lower(treatment)
  if (is.na(x)) return("none")
  if (measure_name == "enhanced") {
    x <- gsub("[+]dcd|[+]sr|[+]ni", "", x, ignore.case = TRUE)
    x <- gsub("cu$", "u", x, ignore.case = TRUE)
    x <- gsub("c$", "u", x, ignore.case = TRUE)
  }
  x
}

background_signature <- function(row, measure_name) {
  film_sig <- if (measure_name == "film") {
    "film_removed"
  } else if (isTRUE(row$has_film)) {
    "film"
  } else {
    "no_film"
  }

  straw_mulch_sig <- if (measure_name == "straw_mulching") {
    "straw_mulch_removed"
  } else if (isTRUE(row$has_straw_mulch)) {
    "straw_mulch"
  } else {
    "no_straw_mulch"
  }

  biochar_sig <- if (measure_name == "biochar") {
    "biochar_removed"
  } else if (isTRUE(row$has_biochar)) {
    "biochar"
  } else {
    "no_biochar"
  }

  enhanced_sig <- if (measure_name == "enhanced") {
    "enhanced_removed"
  } else if (isTRUE(row$has_enhanced)) {
    "enhanced"
  } else {
    "no_enhanced"
  }

  manure_sig <- if (measure_name == "manure") {
    "manure_removed"
  } else if (isTRUE(row$has_manure)) {
    "manure"
  } else {
    "no_manure"
  }

  straw_return_sig <- if (measure_name == "straw_return") {
    "straw_return_removed"
  } else if (isTRUE(row$has_straw_return)) {
    "straw_return"
  } else {
    "no_straw_return"
  }

  paste(
    film_sig, straw_mulch_sig, biochar_sig, enhanced_sig,
    manure_sig, straw_return_sig,
    if_else(isTRUE(row$has_green_manure), "green_manure", "no_green_manure"),
    row$tillage_base,
    sep = "|"
  )
}

is_control_candidate <- function(candidate, target_row, measure_name) {
  if (!near_equal(candidate$N_rate, target_row$N_rate)) return(FALSE)
  if (!identical(candidate$source_std, expected_control_source(target_row, measure_name))) return(FALSE)
  if (!identical(background_signature(candidate, measure_name),
                 background_signature(target_row, measure_name))) return(FALSE)

  switch(
    measure_name,
    manure = !candidate$has_manure,
    straw_return = !candidate$has_straw_return,
    film = !candidate$has_film | candidate$has_no_mulch,
    straw_mulching = !candidate$has_straw_mulch | candidate$has_no_mulch,
    biochar = !candidate$has_biochar,
    enhanced = !candidate$has_enhanced,
    FALSE
  )
}

select_unique_control <- function(candidates, target_row, measure_name) {
  if (nrow(candidates) == 0) return(candidates)
  if (nrow(candidates) == 1) return(candidates)

  if (measure_name == "enhanced") {
    target_base <- base_treatment(target_row$Treatment, measure_name)
    candidates2 <- candidates %>%
      rowwise() %>%
      mutate(treat_base = base_treatment(Treatment, measure_name)) %>%
      ungroup() %>%
      filter(treat_base == target_base)
    if (nrow(candidates2) == 1) return(candidates2)
  }

  candidates[0, ]
}

add_unmatched <- function(unmatched, target_row, measure_name, reason) {
  bind_rows(unmatched, tibble::tibble(
    measure = measure_name,
    measure_label = unname(measure_label_lookup[[measure_name]]),
    reason = reason,
    target_row_id = target_row$row_id,
    target_reference = target_row$References,
    target_crop = target_row$Crop,
    target_year = target_row$Year,
    target_province = target_row$Province,
    target_treatment = target_row$Treatment,
    target_n_source = target_row$N_source,
    target_farming = target_row$Farming,
    target_n_rate = target_row$N_rate,
    target_N2O = target_row$N2O,
    target_Yield = target_row$Yield
  ))
}

build_pairs <- function(dat, measure_name) {
  targets <- dat[is_target(dat, measure_name), , drop = FALSE]
  pairs <- tibble::tibble()
  unmatched <- tibble::tibble()

  for (i in seq_len(nrow(targets))) {
    target_row <- targets[i, , drop = FALSE]
    block <- dat
    for (cc in core_cols) {
      block <- block[block[[cc]] == target_row[[cc]], , drop = FALSE]
    }
    block <- block[block$row_id != target_row$row_id, , drop = FALSE]

    if (nrow(block) == 0) {
      unmatched <- add_unmatched(unmatched, target_row, measure_name, "no_same_block_control")
      next
    }

    keep <- vapply(seq_len(nrow(block)), function(j) {
      is_control_candidate(block[j, , drop = FALSE], target_row, measure_name)
    }, logical(1))
    candidates <- block[keep, , drop = FALSE]
    selected <- select_unique_control(candidates, target_row, measure_name)

    if (nrow(selected) != 1) {
      reason <- if (nrow(candidates) == 0) "no_unique_strict_control" else "multiple_strict_controls"
      unmatched <- add_unmatched(unmatched, target_row, measure_name, reason)
      next
    }

    pairs <- bind_rows(pairs, tibble::tibble(
      measure = measure_name,
      measure_label = unname(measure_label_lookup[[measure_name]]),
      target_row_id = target_row$row_id,
      control_row_id = selected$row_id,
      pair_key = paste(measure_name, target_row$row_id, selected$row_id, sep = "_"),
      References = target_row$References,
      Province = target_row$Province,
      Latitude = target_row$Latitude,
      Longitude = target_row$Longitude,
      Crop = target_row$Crop,
      Year = target_row$Year,
      N_rate = target_row$N_rate,
      background_signature = background_signature(target_row, measure_name),
      treatment_t = target_row$Treatment,
      treatment_c = selected$Treatment,
      n_source_t = target_row$N_source,
      n_source_c = selected$N_source,
      farming_t = target_row$Farming,
      farming_c = selected$Farming,
      n_t = target_row$Replicates,
      n_c = selected$Replicates,
      N2O_t = target_row$N2O,
      N2O_c = selected$N2O,
      N2O_sd_t = target_row$N2O_sd,
      N2O_sd_c = selected$N2O_sd,
      Yield_t = target_row$Yield,
      Yield_c = selected$Yield,
      Yield_sd_t = target_row$Yield_sd,
      Yield_sd_c = selected$Yield_sd
    ))
  }

  list(pairs = pairs, unmatched = unmatched)
}

pair_result <- lapply(measure_specs$measure, function(m) build_pairs(dat, m))
pairs <- bind_rows(lapply(pair_result, `[[`, "pairs"))
unmatched <- bind_rows(lapply(pair_result, `[[`, "unmatched"))

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
ef_rows <- lapply(seq_len(nrow(dat)), function(i) {
  row <- dat[i, , drop = FALSE]
  if (!is.finite(row$N_rate) || row$N_rate <= 0 ||
      !is.finite(row$N2O) || !is.finite(row$N2O_sd) ||
      !is.finite(row$Replicates) || row$Replicates <= 0) {
    return(tibble::tibble(row_id = row$row_id, EF = NA_real_, EF_sd = NA_real_,
                          EF_baseline_row_id = NA_integer_,
                          EF_baseline_match = "not_fertilized_or_missing"))
  }
  baseline <- find_zero_baseline(row, dat)
  if (is.null(baseline$row)) {
    return(tibble::tibble(row_id = row$row_id, EF = NA_real_, EF_sd = NA_real_,
                          EF_baseline_row_id = NA_integer_,
                          EF_baseline_match = baseline$match_type))
  }

  b <- baseline$row
  if (!is.finite(b$N2O) || !is.finite(b$N2O_sd) ||
      !is.finite(b$Replicates) || b$Replicates <= 0) {
    return(tibble::tibble(row_id = row$row_id, EF = NA_real_, EF_sd = NA_real_,
                          EF_baseline_row_id = b$row_id,
                          EF_baseline_match = "zero_baseline_missing_sd_or_n"))
  }

  se_ef <- sqrt((row$N2O_sd / sqrt(row$Replicates))^2 +
                  (b$N2O_sd / sqrt(b$Replicates))^2) / row$N_rate * 100
  tibble::tibble(
    row_id = row$row_id,
    EF = (row$N2O - b$N2O) / row$N_rate * 100,
    EF_sd = se_ef * sqrt(row$Replicates),
    EF_baseline_row_id = b$row_id,
    EF_baseline_match = baseline$match_type
  )
})

ef_lookup <- bind_rows(ef_rows)

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
  )

yse_sd <- function(e, e_sd, y, y_sd, n) {
  if (!all(is.finite(c(e, e_sd, y, y_sd, n))) || y <= 0 || n <= 0) return(NA_real_)
  se_e <- e_sd / sqrt(n)
  se_y <- y_sd / sqrt(n)
  se_yse <- sqrt((se_e / y)^2 + ((e * se_y) / (y^2))^2)
  se_yse * sqrt(n)
}

pairs <- pairs %>%
  rowwise() %>%
  mutate(
    YSE_sd_t = yse_sd(N2O_t, N2O_sd_t, Yield_t, Yield_sd_t, n_t),
    YSE_sd_c = yse_sd(N2O_c, N2O_sd_c, Yield_c, Yield_sd_c, n_c)
  ) %>%
  ungroup()

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

effect_rows <- lapply(seq_len(nrow(pairs)), function(i) {
  p <- pairs[i, , drop = FALSE]
  n2o <- lnrr_effect(p$N2O_t, p$N2O_c, p$N2O_sd_t, p$N2O_sd_c, p$n_t, p$n_c)
  yield <- lnrr_effect(p$Yield_t, p$Yield_c, p$Yield_sd_t, p$Yield_sd_c, p$n_t, p$n_c)
  yse <- lnrr_effect(p$YSE_t, p$YSE_c, p$YSE_sd_t, p$YSE_sd_c, p$n_t, p$n_c)
  ef <- hedges_d_effect(p$EF_t, p$EF_c, p$EF_sd_t, p$EF_sd_c, p$n_t, p$n_c)

  tibble::tibble(
    pair_key = p$pair_key,
    metric = c("N2O", "Yield", "YSE", "EF"),
    metric_label = c("N₂O排放量", "作物产量", "产量尺度N₂O排放", "EF"),
    effect_type = c("lnRR", "lnRR", "lnRR", "Hedges_d"),
    yi = c(n2o[["yi"]], yield[["yi"]], yse[["yi"]], ef[["yi"]]),
    vi = c(n2o[["vi"]], yield[["vi"]], yse[["vi"]], ef[["vi"]])
  )
})

effect_data <- bind_rows(effect_rows) %>%
  left_join(pairs %>% select(pair_key, measure, measure_label, References), by = "pair_key")

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

summarise_group <- function(df, measure_name, measure_label, metric_name, metric_label,
                            effect_type, n_boot = 999) {
  valid <- df %>% filter(
    measure %in% measure_name,
    metric == metric_name,
    is.finite(yi), is.finite(vi), vi > 0
  )
  k <- nrow(valid)
  n_refs <- dplyr::n_distinct(valid$References)
  refs_insufficient <- n_refs < 3
  if (k < 3 || refs_insufficient) {
    return(tibble::tibble(
      measure = paste(measure_name, collapse = "+"),
      measure_label = measure_label,
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
    measure = paste(measure_name, collapse = "+"),
    measure_label = measure_label,
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

summary_rows <- list()
for (i in seq_len(nrow(metric_specs))) {
  ms <- metric_specs[i, ]
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, c("manure", "straw_return"), "有机物料投入",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "manure", "粪肥施用",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "straw_return", "秸秆还田",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, c("film", "straw_mulching"), "覆盖措施",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "film", "地膜覆盖",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "straw_mulching", "秸秆覆盖",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "biochar", "生物炭添加",
    ms$metric, ms$metric_label, ms$effect_type
  )
  summary_rows[[length(summary_rows) + 1]] <- summarise_group(
    effect_data, "enhanced", "增效肥料",
    ms$metric, ms$metric_label, ms$effect_type
  )
}

summary_data <- bind_rows(summary_rows)

# Plot -------------------------------------------------------------------------
measure_levels <- c("有机物料投入", "粪肥施用", "秸秆还田", "覆盖措施",
                    "地膜覆盖", "秸秆覆盖", "生物炭添加", "增效肥料")
plot_labels <- c(
  "有机物料投入" = "有机物料投入",
  "粪肥施用" = "  粪肥施用",
  "秸秆还田" = "  秸秆还田",
  "覆盖措施" = "覆盖措施",
  "地膜覆盖" = "  地膜覆盖",
  "秸秆覆盖" = "  秸秆覆盖",
  "生物炭添加" = "生物炭添加",
  "增效肥料" = "增效肥料"
)

plot_data <- summary_data %>%
  mutate(
    measure_label = factor(measure_label, levels = measure_levels),
    y = match(as.character(measure_label), rev(measure_levels)),
    row_type = if_else(as.character(measure_label) %in% c("有机物料投入", "覆盖措施"),
                       "总体", "亚组"),
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

theme_fig34 <- function() {
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
  vals <- c(df$ci_low, df$ci_high, 0)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(-1, 1))
  lo <- min(vals)
  hi <- max(vals)
  span <- max(hi - lo, 0.1)
  c(floor((lo - span * 0.12) * 10) / 10,
    ceiling((hi + span * 0.12) * 10) / 10)
}

make_panel <- function(metric_name, tag, panel_title, x_lab, show_y_labels = FALSE) {
  df <- plot_data %>% filter(metric == metric_name)
  panel_levels <- measure_levels[measure_levels %in% as.character(df$measure_label)]
  df <- df %>%
    mutate(y = match(as.character(measure_label), rev(panel_levels)))
  lim <- axis_limits(df)
  span <- lim[2] - lim[1]
  label_x <- lim[2] + span * 0.08
  full_y <- data.frame(
    y = seq_along(panel_levels),
    label = plot_labels[rev(panel_levels)]
  )

  ggplot(df, aes(y = y)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.32, color = "#777777") +
    geom_segment(data = df %>% filter(plot_ready),
                 aes(x = ci_low, xend = ci_high, yend = y, color = sig_class),
                 linewidth = 0.72, lineend = "round") +
    geom_point(data = df %>% filter(plot_ready),
               aes(x = effect, fill = sig_class, shape = row_type),
               size = 2.5, stroke = 0.45, color = "#222222") +
    geom_text(aes(x = label_x, label = value_label),
              hjust = 0, family = font_fam, size = 2.15, color = "#222222") +
    scale_shape_manual(values = c("总体" = 23, "亚组" = 21), guide = "none") +
    scale_color_manual(values = c("显著降低" = "#2166AC",
                                  "显著增加" = "#D6604D",
                                  "不显著" = "#666666"),
                       guide = "none") +
    scale_fill_manual(values = c("显著降低" = "#2166AC",
                                 "显著增加" = "#D6604D",
                                 "不显著" = "white"),
                      guide = "none") +
    scale_y_continuous(
      limits = c(0.45, length(panel_levels) + 0.55),
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
    theme_fig34() +
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
    title = "管理措施对N₂O排放量、作物产量、YSE和EF的Meta分析森林图",
    theme = theme(
      plot.title = element_text(family = font_fam, size = 11.2, face = "bold",
                                hjust = 0.5, margin = margin(0, 0, 4, 0))
    )
  )

# Export -----------------------------------------------------------------------
png_file <- file.path(output_dir, "Fig3-4_management_meta_forest_CN.png")
tiff_file <- file.path(output_dir, "Fig3-4_management_meta_forest_CN.tiff")
pairs_file <- file.path(output_dir, "Fig3-4_management_meta_pairs.csv")
summary_file <- file.path(output_dir, "Fig3-4_management_meta_summary.csv")
unmatched_file <- file.path(output_dir, "Fig3-4_management_meta_unmatched.csv")

png(filename = png_file, width = 210, height = 165, units = "mm",
    res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(png_file, 600)

tiff(filename = tiff_file, width = 210, height = 165, units = "mm",
     res = 600, type = device_type, bg = "white")
print(fig)
dev.off()
set_file_dpi(tiff_file, 600)

pairs_out <- pairs %>%
  mutate(across(where(is.numeric), ~round(.x, 6))) %>%
  arrange(measure, References, Crop, Year, target_row_id)

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
          factor(measure_label, levels = measure_levels))

write.csv(pairs_out, pairs_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summary_out, summary_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(unmatched, unmatched_file, row.names = FALSE, fileEncoding = "UTF-8")

message("Chinese font: ", font_path)
message("Output PNG : ", png_file)
message("Output TIFF: ", tiff_file)
message("Pairs CSV  : ", pairs_file)
message("Summary CSV: ", summary_file)
message("Unmatched  : ", unmatched_file)
print(summary_out)

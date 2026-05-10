#!/usr/bin/env Rscript

# Export manuscript figures as vector graphics without changing the original
# PNG/TIFF figure-generation scripts.

project_dir <- normalizePath(getwd())
local_lib <- file.path(project_dir, "Rlib")
if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))

output_dir <- file.path(project_dir, "output", "manuscript_vector_figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

figures <- data.frame(
  label = c("Fig2-1", "Fig3-1", "Fig3-2", "Fig3-3", "Fig3-4", "Fig3-5", "Fig3-6/S1"),
  script = c(
    "make_fig2_1_site_distribution.R",
    "make_fig3_1_yield_Nrate.R",
    "make_fig3_2_n2o_Nrate.R",
    "make_fig3_3_EF_YSE.R",
    "make_fig3_4_management_meta_forest.R",
    "make_fig3_5_n_reduction_meta_forest.R",
    "make_figS1_apple_yield_Nrate.R"
  ),
  plot_object = c("p", "fig", "fig", "fig", "fig", "fig", "fig"),
  width_mm = c(190, 190, 190, 145, 210, 210, 95),
  height_mm = c(140, 110, 110, 178, 165, 165, 80),
  base_name = c(
    "Fig2-1_site_distribution_CN",
    "Fig3-1_yield_Nrate_CN",
    "Fig3-2_N2O_Nrate_CN",
    "Fig3-3_EF_YSE_CN",
    "Fig3-4_management_meta_forest_CN",
    "Fig3-5_N_reduction_meta_forest_CN",
    "Fig3-6_apple_yield_Nrate_CN"
  ),
  stringsAsFactors = FALSE
)

mm_to_in <- function(x) x / 25.4

save_pdf <- function(fig, file, width_mm, height_mm) {
  grDevices::pdf(
    file = file,
    width = mm_to_in(width_mm),
    height = mm_to_in(height_mm),
    bg = "white",
    onefile = FALSE,
    useDingbats = FALSE
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(fig)
}

save_eps <- function(fig, file, width_mm, height_mm) {
  grDevices::postscript(
    file = file,
    width = mm_to_in(width_mm),
    height = mm_to_in(height_mm),
    onefile = FALSE,
    horizontal = FALSE,
    paper = "special",
    bg = "white"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  print(fig)
}

saved <- character()
pdftocairo <- Sys.which("pdftocairo")
if (!nzchar(pdftocairo)) {
  warning("pdftocairo was not found; SVG files will not be generated.")
}

for (i in seq_len(nrow(figures))) {
  spec <- figures[i, ]
  message("Rendering ", spec$label, " from ", spec$script)

  env <- new.env(parent = globalenv())
  source(file.path(project_dir, spec$script), local = env)

  if (!exists(spec$plot_object, envir = env, inherits = FALSE)) {
    stop(
      "Script did not create an object named `", spec$plot_object, "`: ",
      spec$script,
      call. = FALSE
    )
  }

  fig <- get(spec$plot_object, envir = env, inherits = FALSE)
  pdf_file <- file.path(output_dir, paste0(spec$base_name, ".pdf"))
  eps_file <- file.path(output_dir, paste0(spec$base_name, ".eps"))
  svg_file <- file.path(output_dir, paste0(spec$base_name, ".svg"))

  save_pdf(fig, pdf_file, spec$width_mm, spec$height_mm)
  save_eps(fig, eps_file, spec$width_mm, spec$height_mm)
  if (nzchar(pdftocairo)) {
    status <- system2(pdftocairo, c("-svg", pdf_file, svg_file))
    if (!identical(status, 0L) && !is.null(status)) {
      warning("Failed to convert SVG: ", svg_file)
    }
  }
  saved <- c(saved, pdf_file, eps_file, svg_file)

  if (identical(spec$label, "Fig3-6/S1")) {
    s1_pdf <- file.path(output_dir, "FigS1_apple_yield_Nrate_CN.pdf")
    s1_eps <- file.path(output_dir, "FigS1_apple_yield_Nrate_CN.eps")
    s1_svg <- file.path(output_dir, "FigS1_apple_yield_Nrate_CN.svg")
    file.copy(pdf_file, s1_pdf, overwrite = TRUE)
    file.copy(eps_file, s1_eps, overwrite = TRUE)
    file.copy(svg_file, s1_svg, overwrite = TRUE)
    saved <- c(saved, s1_pdf, s1_eps, s1_svg)
  }
}

message("Vector figures written to: ", output_dir)
message(paste(saved, collapse = "\n"))

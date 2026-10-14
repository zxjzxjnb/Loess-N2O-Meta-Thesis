# Loess Plateau N₂O Meta-Analysis

[中文](README.md) | **English**

Data and analysis scripts for a thesis on agricultural nitrous oxide (N₂O) emissions in croplands and orchards on China's Loess Plateau. The repository brings together the source spreadsheet, geographic boundary data, figure-generation scripts, statistical summaries, treatment–control pairing records, and manuscript vector figures.

The analysis examines how nitrogen application rates and agricultural management relate to N₂O emissions, crop yield, emission factors (EF), and yield-scaled emissions (YSE). It is intended to support review of the thesis analyses and reuse of the data-cleaning, pairing, and figure-generation workflow.

## What is included

- **Source data:** an Excel table of study observations and a Loess Plateau boundary file.
- **Analysis scripts:** nitrogen-rate regressions, EF/YSE calculations, and subgroup meta-analyses of management practices and nitrogen reduction.
- **Traceable outputs:** summary statistics, treatment–control pairs, control-matching audits, and unmatched records.
- **Figures:** Chinese thesis figures, English forest plots, and PDF/SVG/EPS files for manuscript preparation.

## Repository structure

```text
.
├── data/
│   ├── tables/TableS1.xlsx          # Main analysis input
│   └── geo/LoessPlateauRegion.kmz   # Regional boundary
├── scripts/                        # R and Python analysis/plotting scripts
├── output/
│   ├── figures/                    # Raster figures, summaries, and audit CSVs
│   └── manuscript_vector_figures/  # PDF, SVG, and EPS figures
├── loess_n2o_meta.Rproj             # RStudio project
├── README.md                       # Chinese documentation
└── README.en.md                     # English documentation
```

## Setup

Clone the repository and run the R scripts from its root directory:

```sh
git clone https://github.com/zxjzxjnb/Loess-N2O-Meta-Thesis.git
cd Loess-N2O-Meta-Thesis
```

Use R with the following packages. You can also open `loess_n2o_meta.Rproj` in RStudio.

```r
install.packages(c(
  "readxl", "dplyr", "tidyr", "stringr", "ggplot2",
  "patchwork", "scales", "showtext", "sysfonts", "maps", "mapdata"
))
```

The scripts also use `grid`, which is included with R. Package versions are not pinned in this repository.

**Fonts:** several scripts search for fonts at macOS-specific paths and stop if none is available. Before running them on another system, update their `font_candidates` or font-path settings to point to installed fonts. Chinese figures require a font with Chinese character support; the English forest-plot script also needs a valid local font path.

**Vector conversion:** install Poppler and make `pdftocairo` available on your system path to export SVG and EPS files. The export script can still generate PDFs without it.

**Optional Python scripts:** install their dependencies with:

```sh
python -m pip install pandas numpy Pillow openpyxl
```

The Python site-map script additionally expects local R map files at `Rlib/mapdata/mapdata/china.L` and `Rlib/maps/mapdata/world.L`, plus fonts at hard-coded macOS paths. These map files are not included in the repository. Supply them or adapt the paths before using that script.

## Reproduce the main figures

Run the following commands from the repository root:

```sh
Rscript scripts/make_fig2_1_site_distribution.R
Rscript scripts/make_fig3_1_yield_Nrate.R
Rscript scripts/make_fig3_2_n2o_Nrate.R
Rscript scripts/make_fig3_3_EF_YSE.R
Rscript scripts/make_fig3_4_management_meta_forest.R
Rscript scripts/make_fig3_5_n_reduction_meta_forest.R
Rscript scripts/make_figS1_apple_yield_Nrate.R
```

After generating the management and nitrogen-reduction summary CSVs, create the English forest plots:

```sh
Rscript scripts/make_fig5_fig6_meta_forest_EN.R
```

Export the Chinese figures as manuscript vector graphics:

```sh
Rscript scripts/export_manuscript_vector_figures.R
```

The vector-export script reruns the underlying R figure scripts and writes PDF/SVG/EPS files to `output/manuscript_vector_figures/`. Running the workflows regenerates and may overwrite existing figures and CSVs.

## Script guide

| Script in `scripts/` | Analysis or output |
| --- | --- |
| `make_fig2_1_site_distribution.R` | Study-site distribution map and plotted-site CSV. |
| `make_fig3_1_yield_Nrate.R` | Crop yield versus nitrogen application rate; quadratic fits and group comparisons. |
| `make_fig3_2_n2o_Nrate.R` | N₂O emissions versus nitrogen application rate; linear fits and group comparisons. |
| `make_fig3_3_EF_YSE.R` | EF/YSE calculations, derived observations, and EF control-matching audit. |
| `make_fig3_4_management_meta_forest.R` | Management-practice meta-analysis, forest plot, paired records, and unmatched records. |
| `make_fig3_5_n_reduction_meta_forest.R` | Nitrogen-reduction meta-analysis, forest plot, and separate main/EF pairing tables. |
| `make_figS1_apple_yield_Nrate.R` | Supplementary apple-orchard yield analysis. |
| `make_fig5_fig6_meta_forest_EN.R` | English management and nitrogen-reduction forest plots from the summary CSVs. |
| `export_manuscript_vector_figures.R` | Batch export of Chinese figures as PDF/SVG/EPS. |
| `make_fig2_1_site_distribution_refined.py` | Alternative site-map renderer using Pillow and local R map files. |
| `make_fig3_5_n_reduction_meta_forest_manuscript.py` | Recalculates manuscript EF results using existing nitrogen-reduction outputs and redraws the Chinese forest plot. |

## Data and analytical workflow

`data/tables/TableS1.xlsx` is the main input. Its fields include study location, climate and soil properties, crop, year, treatment, nitrogen source, farming practice, replication, nitrogen application rate, N₂O emissions, yield, standard deviations, and references. Several scripts read the first sheet after skipping two header rows and assign column names by position. Preserve this layout or update the import code when modifying the table.

The workflow standardizes text and numeric fields, fills study-level information down through the table, and constructs treatment–control comparisons within defined study contexts. For nitrogen-reduction analyses, lower nitrogen rates are compared with the highest eligible rate within each matching group.

The forest-plot scripts use log response ratios for N₂O, yield, and YSE, and Hedges' *d* for EF. They implement DerSimonian–Laird random-effects pooling and pair-level bootstrap confidence intervals with 999 resamples and a fixed random seed.

**EF pairing differs between analyses.** The nitrogen-reduction R script uses a manuscript-specific reference-by-crop zero-nitrogen baseline and supplementary contrasts for the <25% reduction group. It exports these EF pairs separately as `Fig3-5_N_reduction_meta_pairs_EF_manuscript.csv`. Consult this table and the script when interpreting EF results; the main nitrogen-reduction pairs CSV does not describe every EF comparison.

## Reading the outputs

Files in `output/figures/` provide the records behind the figures:

| Output | Contents |
| --- | --- |
| `*_summary.csv` | Effect estimates, confidence intervals, counts, or descriptive statistics, depending on the analysis. |
| `*_pairs.csv` | Treatment–control comparisons used in the corresponding analysis. |
| `*_pairwise_center_tests.csv` | Pairwise Wilcoxon comparisons with Benjamini–Hochberg adjustment. |
| `*_omnibus_tests.csv` | Overall group comparisons, such as Kruskal–Wallis tests. |
| `Fig3-3_EF_control_matching_audit.csv` | EF baseline matching decisions and reasons for unmatched observations. |
| `Fig3-4_management_meta_unmatched.csv` | Management records for which a valid comparison could not be constructed. |
| `Fig3-5_N_reduction_meta_pairs_EF_manuscript.csv` | Manuscript-specific nitrogen-reduction comparisons used for EF. |

Chinese figure filenames generally end in `_CN`; the English forest plots end in `_EN`. Historical figure variants are also retained. Before using a figure or summary in a manuscript, confirm that its pairing rules and analysis version match the intended interpretation.

## Reusing the workflow

The scripts can serve as examples for spreadsheet standardization, treatment–control matching, subgroup analysis, and coordinated figure/statistical export in other agricultural meta-analysis projects. Adapt the field mappings, units, crop and management classifications, and matching rules to the new dataset, and inspect the pairing and audit tables before interpreting pooled results.

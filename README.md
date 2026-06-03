# Loess N2O Meta Thesis

本仓库是“黄土高原农田/果园 N2O 排放 meta 分析”的 GitHub 复现包，包含核心数据表、地理边界、图表复现脚本、已生成图件、统计汇总表、配对审计表和投稿排版用矢量图。希望能够帮助课题组复核本论文结果，并且其中的数据清洗、处理-对照匹配、meta分析、图件导出流程可供后续类似研究参考。

## Advantages

- 结果可复核：从 `data/tables/TableS1.xlsx` 到论文图件、统计汇总和矢量图的关键步骤都保留在脚本中。
- 方法可迁移：管理措施分类、减氮处理配对、EF/YSE 计算、对照匹配审计等流程可复用于其他农业生态 meta 分析。
- 图件可再生成：中文 PNG 图件、统计 CSV 和投稿用 PDF/SVG/EPS 矢量图均可由脚本重新导出。
- 审计痕迹完整：关键图件同时输出 summary、pairwise tests、pairs、unmatched、control matching audit 等 CSV，便于组内复核或回应审稿问题。

## Project Structure

以下为GitHub仓库的主要内容：

- `data/tables/`: 作图和 meta 分析输入表，核心文件为 `TableS1.xlsx`。
- `data/geo/`: 地理边界文件，例如黄土高原边界 `LoessPlateauRegion.kmz`。
- `scripts/`: 图表、统计汇总和 meta 分析复现脚本。
- `output/figures/`: 论文图件、统计汇总、派生数据和审计 CSV。
- `output/manuscript_vector_figures/`: 排版用 PDF/SVG/EPS 矢量图。
- `loess_n2o_meta.Rproj`: RStudio 项目文件。

## Environment

脚本默认从项目根目录运行。R 脚本主要依赖：

- `readxl`
- `dplyr`
- `tidyr`
- `stringr`
- `ggplot2`
- `patchwork`
- `scales`
- `showtext`
- `sysfonts`
- `maps`
- `mapdata`

Python 地图脚本主要依赖：

- `pandas`
- `numpy`
- `Pillow`

矢量图导出脚本 `scripts/export_manuscript_vector_figures.R` 会调用系统命令 `pdftocairo` 将 PDF 转为 SVG/EPS。如果本机没有安装 `pdftocairo`，PDF 仍可生成，但 SVG/EPS 导出会被跳过。

## Recommended Workflow

建议按下列顺序复现主要论文图件：

```sh
Rscript scripts/make_fig3_1_yield_Nrate.R
Rscript scripts/make_fig3_2_n2o_Nrate.R
Rscript scripts/make_fig3_3_EF_YSE.R
Rscript scripts/make_fig3_4_management_meta_forest.R
Rscript scripts/make_fig3_5_n_reduction_meta_forest.R
Rscript scripts/make_figS1_apple_yield_Nrate.R
Rscript scripts/export_manuscript_vector_figures.R
```

站点分布图可使用 R 或 Python 版本复现：

```sh
Rscript scripts/make_fig2_1_site_distribution.R
python scripts/make_fig2_1_site_distribution_refined.py
```

## Figure Scripts

| Script | Main output | Purpose |
| --- | --- | --- |
| `scripts/make_fig2_1_site_distribution.R` | `Fig2-1_site_distribution_CN.png`, `Fig2-1_site_points_used.csv` | 黄土高原研究点空间分布图。 |
| `scripts/make_fig2_1_site_distribution_refined.py` | `Fig2-1_site_distribution_CN.png` | Python/Pillow 版本站点分布图，适合在 R 地图环境不可用时复现。 |
| `scripts/make_fig3_1_yield_Nrate.R` | `Fig3-1_yield_Nrate_CN.png`, `Fig3-1_quadratic_fit_summary.csv` | 施氮量与作物产量关系及分组统计。 |
| `scripts/make_fig3_2_n2o_Nrate.R` | `Fig3-2_N2O_Nrate_CN.png`, `Fig3-2_linear_fit_summary.csv` | 施氮量与 N2O 排放关系及分组统计。 |
| `scripts/make_fig3_3_EF_YSE.R` | `Fig3-3_EF_YSE_CN.png`, `Fig3-3_EF_YSE_derived_data.csv`, `Fig3-3_EF_control_matching_audit.csv` | EF 和单位产量 N2O 排放量计算，含对照匹配审计。 |
| `scripts/make_fig3_4_management_meta_forest.R` | `Fig3-4_management_meta_forest_CN.png`, `Fig3-4_management_meta_pairs.csv`, `Fig3-4_management_meta_summary.csv` | 管理措施 pairwise meta 分析森林图。 |
| `scripts/make_fig3_5_n_reduction_meta_forest.R` | `Fig3-5_N_reduction_meta_forest_CN.png`, `Fig3-5_N_reduction_meta_pairs.csv`, `Fig3-5_N_reduction_meta_summary.csv` | 减氮处理 pairwise meta 分析森林图。 |
| `scripts/make_figS1_apple_yield_Nrate.R` | `FigS1_apple_yield_Nrate_CN.png`, `FigS1_apple_yield_Nrate_fit_summary.csv` | 苹果园施氮量与产量补充图。 |
| `scripts/export_manuscript_vector_figures.R` | `output/manuscript_vector_figures/*.pdf`, `*.svg`, `*.eps` | 批量导出投稿排版用矢量图。 |

## Key Outputs

`output/figures/` 中不仅包含图片，也包含可复核的统计结果：

- `*_summary.csv`: 各图对应的效应量、置信区间、样本数、文献数或描述统计。
- `*_pairs.csv`: meta 分析中实际使用的处理-对照配对。
- `*_pairwise_center_tests.csv`: 作物组间 pairwise Wilcoxon 检验和 BH 校正结果。
- `*_omnibus_tests.csv`: Kruskal-Wallis 等整体组间检验。
- `Fig3-3_EF_control_matching_audit.csv`: EF 计算时每条施肥处理如何匹配对照，或为什么未匹配。
- `Fig3-4_management_meta_unmatched.csv`: 管理措施 meta 分析中未能构造有效配对的记录。

`output/manuscript_vector_figures/` 中的 PDF/SVG/EPS 文件用于 Word、论文排版或期刊投稿系统，并作为 GitHub 复现包的一部分保留。

## 可迁移流程

以下流程适合课题组在后续 meta 分析项目中复用：

1. **Excel 数据标准化**
   - 对合并单元格风格的原始表进行向下填充。
   - 将坐标、施氮量、N2O、产量、标准差、重复数等字段统一转为数值。
   - 将处理、肥料来源、耕作/覆盖方式等自由文本转成标准分类变量。

2. **处理-对照配对**
   - 在同一文献、同一地点、同一年份、同一作物、同一管理背景下构造处理-对照对。
   - 对无法匹配的记录单独输出 audit/unmatched 表，而不是静默丢弃。
   - 对减氮研究，以最高施氮量作为对照，较低施氮量作为处理，并计算减氮比例。

3. **效应量与分层 meta 分析**
   - 对 N2O、产量和 YSE 使用响应比或百分比变化表达。
   - 对 EF 等指标使用独立计算逻辑。
   - 按作物类型、管理措施、减氮幅度等进行分层汇总。

4. **论文图件自动化**
   - 每张图由脚本从原始表或派生表生成。
   - 图片和统计 CSV 同时输出，确保图中结论可以回溯到数据。
   - 投稿图通过统一脚本批量导出 PDF/SVG/EPS，减少手工改图和格式漂移。

## Notes for Handoff

- 运行脚本前请确认当前工作目录为仓库根目录。
- `TableS1.xlsx` 是核心输入表；若表头、列名或合并单元格结构改变，需要同步修改读取和清洗代码。
- 字体路径依赖本机中文字体配置；如果图中文字无法正常显示，需要调整各脚本中的字体设置。
- `output/figures/` 和 `output/manuscript_vector_figures/` 中的结果文件可以重新生成，但在正式交付或投稿前应确认当前版本是否为最终论文口径。

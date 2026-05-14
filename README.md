# Loess N2O Meta Thesis

毕业论文项目文件，包含正文文档、分析脚本、地理数据、表格和整理后的图表输出。

## Project Structure

- `docs/manuscript/`: 当前毕业论文正文和英文稿件。
- `docs/references/`: 学校格式模板、撰写规范和参考文献著录规则。
- `docs/tables/`: Word 版表格文件。
- `data/tables/`: 作图和 meta 分析输入表。
- `data/geo/`: 地理边界文件。
- `scripts/`: 图表和 meta 分析复现脚本。
- `output/doc/`: 修订后的文档输出。
- `output/figures/`: 论文图件、统计汇总和导出数据。
- `output/manuscript_vector_figures/`: 排版用矢量图。

## Usage

脚本默认从项目根目录运行，例如：

```sh
Rscript scripts/make_fig3_4_management_meta_forest.R
Rscript scripts/export_manuscript_vector_figures.R
python scripts/make_fig2_1_site_distribution_refined.py
```

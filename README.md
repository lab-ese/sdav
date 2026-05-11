# Statistical Data Analysis in R

A comprehensive collection of 26 R scripts covering essential statistical concepts using public datasets.

## Datasets

Datasets are available in the `datasets/` folder:

| Dataset | File | Description |
|---------|------|-------------|
| Iris | `iris.csv` | Flower measurements (150 samples) |
| Titanic | `titanic.csv` | Passenger survival data (891 passengers) |
| mtcars | `mtcars.csv` | Motor Trend car road tests (32 cars) |
| USArrests | `usarrests.csv` | US crime statistics by state (50 states)

## Scripts Overview

| # | Script | Description |
|---|--------|-------------|
| 01 | `01_read_dataset.R` | Read datasets from URL, local files, built-in |
| 02 | `02_if_else_classify.R` | Customer classification using if/else |
| 03 | `03_for_loop.R` | For loop to display classifications |
| 04 | `04_revenue_function.R` | Function to calculate total revenue |
| 05 | `05_missing_values.R` | Handle missing values (mean, median, imputation) |
| 06 | `06_descriptive_stats.R` | Mean, Median, Mode, SD, Variance, Range |
| 07 | `07_quartiles_iqr.R` | Quartiles & IQR, outlier detection |
| 08 | `08_compare_datasets.R` | Compare datasets and conclude |
| 09 | `09_histogram_boxplot.R` | Histogram and Boxplot |
| 10 | `10_bar_pie_charts.R` | Bar chart and Pie chart |
| 11 | `11_scatter_freqpolygon.R` | Scatter plot and Frequency polygon |
| 12 | `12_stem_leaf.R` | Stem-and-leaf plot and interpretation |
| 13 | `13_binomial_prob.R` | Binomial probability P(X=2) |
| 14 | `14_poisson_prob.R` | Poisson probability |
| 15 | `15_normal_prob.R` | Normal probability P(X<60) |
| 16 | `16_confidence_interval.R` | 95% Confidence Interval |
| 17 | `17_one_sample_ttest.R` | One-sample t-test |
| 18 | `18_two_sample_ttest.R` | Two-sample t-test |
| 19 | `19_anova.R` | One-way ANOVA |
| 20 | `20_chi_square.R` | Chi-square test |
| 21 | `21_f_test.R` | F-test for variances |
| 22 | `22_independent_ttest.R` | Independent t-test for means |
| 23 | `23_proportion_test.R` | Proportion test (one & two sample) |
| 24 | `24_z_test.R` | Z-test |
| 25 | `25_ttest_general.R` | General t-test (paired, Welch, one-tailed) |
| 26 | `26_confidence_interval_adv.R` | Advanced CI (bootstrap, proportion, variance) |

## Running the Scripts

```bash
# Run a specific script
Rscript 01_read_dataset.R

# Run all scripts (if you have them in a loop)
for f in *.R; do Rscript "$f"; done
```

## Requirements

- R (version 3.5+)
- Packages: dplyr, ggplot2, moments, readr, zoo

## License

MIT License
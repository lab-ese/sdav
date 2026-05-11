# Statistical Data Analysis in R

37 R scripts covering essential statistical concepts.

## Scripts in Root (11 scripts - New Questions)

| # | Script | Description |
|---|--------|-------------|
| 01 | `01_student_df.R` | Data frame, subsetting, ggplot2 |
| 02 | `02_airline_struct.R` | Dataset structure, summary, mean/median/mode |
| 03 | `03_ecommerce_stats.R` | SD, variance, coefficient of variation |
| 04 | `04_merchandise_plots.R` | Histogram, boxplot, stem-leaf |
| 05 | `05_ttest_merchandise.R` | One-sample t-test |
| 06 | `06_anova_airline.R` | One-way ANOVA |
| 07 | `07_poisson_merchandise.R` | Poisson distribution |
| 08 | `08_chisq_airline.R` | Chi-square distribution |
| 09 | `09_f_test.R` | F-test (2 datasets) |
| 10 | `10_chisq_independence.R` | Chi-square test |
| 11 | `11_function_ifelse.R` | Function + if-else |

## Original Scripts (26 scripts - in extras/)

| # | Script | Description |
|---|--------|-------------|
| 01 | `extras/01_read_dataset.R` | Read datasets |
| 02 | `extras/02_if_else_classify.R` | If/else classification |
| 03 | `extras/03_for_loop.R` | For loop |
| 04 | `extras/04_revenue_function.R` | Revenue function |
| 05 | `extras/05_missing_values.R` | Handle missing values |
| 06 | `extras/06_descriptive_stats.R` | Mean, Median, Mode, SD, Variance |
| 07 | `extras/07_quartiles_iqr.R` | Quartiles & IQR |
| 08 | `extras/08_compare_datasets.R` | Compare datasets |
| 09 | `extras/09_histogram_boxplot.R` | Histogram and Boxplot |
| 10 | `extras/10_bar_pie_charts.R` | Bar and Pie chart |
| 11 | `extras/11_scatter_freqpolygon.R` | Scatter plot |
| 12 | `extras/12_stem_leaf.R` | Stem-and-leaf plot |
| 13 | `extras/13_binomial_prob.R` | Binomial probability |
| 14 | `extras/14_poisson_prob.R` | Poisson probability |
| 15 | `extras/15_normal_prob.R` | Normal probability |
| 16 | `extras/16_confidence_interval.R` | 95% CI |
| 17 | `extras/17_one_sample_ttest.R` | One-sample t-test |
| 18 | `extras/18_two_sample_ttest.R` | Two-sample t-test |
| 19 | `extras/19_anova.R` | One-way ANOVA |
| 20 | `extras/20_chi_square.R` | Chi-square test |
| 21 | `extras/21_f_test.R` | F-test |
| 22 | `extras/22_independent_ttest.R` | Independent t-test |
| 23 | `extras/23_proportion_test.R` | Proportion test |
| 24 | `extras/24_z_test.R` | Z-test |
| 25 | `extras/25_ttest_general.R` | General t-test |
| 26 | `extras/26_confidence_interval_adv.R` | Advanced CI |

## Datasets (`datasets/`)

| Dataset | Description |
|---------|-------------|
| `iris.csv` | Flower measurements |
| `titanic.csv` | Passenger survival |
| `mtcars.csv` | Car road tests |
| `usarrests.csv` | US crime statistics |
| `airline_delays.csv` | Flight delays |
| `ecommerce.csv` | E-commerce transactions |
| `merchandise_sales.csv` | Sales data |

## Run Scripts

```bash
# Run root scripts (new questions)
Rscript 01_student_df.R

# Run all root scripts
for f in 0[1-9]_*.R 1[0-1]_*.R; do Rscript "$f"; done

# Run extras (original 26)
cd extras
for f in *.R; do Rscript "$f"; done
```
# Statistical Data Analysis in R

26 original scripts + 11 new scripts covering essential statistical concepts.

## Original Scripts (Basic Statistics)

| # | Script | Description |
|---|--------|-------------|
| 01 | `01_read_dataset.R` | Read datasets |
| 02 | `02_if_else_classify.R` | If/else classification |
| 03 | `03_for_loop.R` | For loop |
| 04 | `04_revenue_function.R` | Revenue function |
| 05 | `05_missing_values.R` | Handle missing values |
| 06 | `06_descriptive_stats.R` | Mean, Median, Mode, SD, Variance, Range |
| 07 | `07_quartiles_iqr.R` | Quartiles & IQR |
| 08 | `08_compare_datasets.R` | Compare datasets |
| 09 | `09_histogram_boxplot.R` | Histogram and Boxplot |
| 10 | `10_bar_pie_charts.R` | Bar and Pie chart |
| 11 | `11_scatter_freqpolygon.R` | Scatter plot |
| 12 | `12_stem_leaf.R` | Stem-and-leaf plot |
| 13 | `13_binomial_prob.R` | Binomial probability |
| 14 | `14_poisson_prob.R` | Poisson probability |
| 15 | `15_normal_prob.R` | Normal probability |
| 16 | `16_confidence_interval.R` | 95% CI |
| 17 | `17_one_sample_ttest.R` | One-sample t-test |
| 18 | `18_two_sample_ttest.R` | Two-sample t-test |
| 19 | `19_anova.R` | One-way ANOVA |
| 20 | `20_chi_square.R` | Chi-square test |
| 21 | `21_f_test.R` | F-test |
| 22 | `22_independent_ttest.R` | Independent t-test |
| 23 | `23_proportion_test.R` | Proportion test |
| 24 | `24_z_test.R` | Z-test |
| 25 | `25_ttest_general.R` | General t-test |
| 26 | `26_confidence_interval_adv.R` | Advanced CI |

## New Questions (Advanced)

| # | Script | Description |
|---|--------|-------------|
| 01 | `new_questions/01_student_df.R` | Data frame, subsetting, ggplot2 |
| 02 | `new_questions/02_airline_struct.R` | Structure, summary, mean/median/mode |
| 03 | `new_questions/03_ecommerce_stats.R` | SD, variance, CV |
| 04 | `new_questions/04_merchandise_plots.R` | Histogram, boxplot, stem-leaf |
| 05 | `new_questions/05_ttest_merchandise.R` | One-sample t-test |
| 06 | `new_questions/06_anova_airline.R` | One-way ANOVA |
| 07 | `new_questions/07_poisson_merchandise.R` | Poisson distribution |
| 08 | `new_questions/08_chisq_airline.R` | Chi-square distribution |
| 09 | `new_questions/09_f_test.R` | F-test (2 datasets) |
| 10 | `new_questions/10_chisq_independence.R` | Chi-square test |
| 11 | `new_questions/11_function_ifelse.R` | Function + if-else |

## Datasets

| Dataset | File |
|---------|------|
| Iris | `datasets/iris.csv` |
| Titanic | `datasets/titanic.csv` |
| mtcars | `datasets/mtcars.csv` |
| US Arrests | `datasets/usarrests.csv` |
| Airline Delays | `datasets/airline_delays.csv` |
| E-commerce | `datasets/ecommerce.csv` |
| Merchandise Sales | `datasets/merchandise_sales.csv` |

## Run

```bash
# Run all original scripts
Rscript *.R

# Run new questions
cd new_questions
Rscript *.R
```
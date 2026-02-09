# Infer School Year from File Path

Extracts school year from common ADECE budget file name patterns.
Recognizes patterns like "2021-2022", "21-22", and "24-25".

## Usage

``` r
alprek_infer_school_year(path)
```

## Arguments

- path:

  Character. File path or file name.

## Value

Character in "YYYY-YYYY" format, or `NA_character_` if not detected.

## Examples

``` r
alprek_infer_school_year("rptClassBudgets 2021-2022.xlsx")
#> [1] "2021-2022"
# "2021-2022"
alprek_infer_school_year("24-25 FCPK Budgets.xlsx")
#> [1] "2024-2025"
# "2024-2025"
```

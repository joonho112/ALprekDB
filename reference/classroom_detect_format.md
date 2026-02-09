# Detect Classroom File Format

Determines whether a classroom data frame uses the legacy (2021-2024,
~100 columns) or new (2024-2025, ~125 columns) format.

## Usage

``` r
classroom_detect_format(df)
```

## Arguments

- df:

  A data frame (raw, from Excel import with cleaned column names).

## Value

Character string: `"legacy"` or `"new"`.

## Details

Detection logic:

- **New**: Contains "Fund Source" or "Classroom Code Formula" or ncol
  \>= 120.

- **Legacy**: ncol \<= 105 OR contains "Aux. Teacher Ethnicity" (with
  period).

## Examples

``` r
if (FALSE) { # \dontrun{
df <- readxl::read_excel("FCPK Classroom Details 21-22.xlsx", sheet = "rptRIF")
classroom_detect_format(df)
# "legacy"
} # }
```

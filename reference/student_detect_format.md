# Detect Student File Format

Determines whether a student data frame uses the legacy (2021-2024, 202
columns) or new (2024-2025, 270 columns) format.

## Usage

``` r
student_detect_format(df)
```

## Arguments

- df:

  A data frame (raw, from Excel import with cleaned column names).

## Value

Character string: `"legacy"` or `"new"`.

## Details

Detection logic:

- **Reject partial exports**: fewer than 190 columns is too narrow for a
  canonical Student/Child Details file.

- **New**: Contains "Child First Name" or "Modified Schedule" or
  "Student ID" (without "State") or ncol \>= 250.

- **Legacy**: roughly 190-210 columns and no new-format marker columns
  present.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- readxl::read_excel("FCPK Student Details 21-22.xlsx")
student_detect_format(df)
# "legacy"
} # }
```

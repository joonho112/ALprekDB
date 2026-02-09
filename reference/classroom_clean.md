# Clean Classroom Detail Data

Applies comprehensive cleaning to raw classroom data: renames columns
using codebook mappings, standardizes administrative variables,
classifies free-text teacher degree/credential fields, normalizes
demographic variables, and derives new variables.

## Usage

``` r
classroom_clean(raw_obj, include_dob = FALSE)
```

## Arguments

- raw_obj:

  An `alprek_classroom_raw` object from
  [`classroom_read()`](https://joonho112.github.io/ALprekDB/reference/classroom_read.md).

- include_dob:

  Logical. If `TRUE`, includes Date of Birth columns in the output (new
  format only). Default `FALSE` for PII protection.

## Value

An `alprek_classroom_clean` S3 object (list) with elements:

- `data`: tibble of cleaned classroom data.

- `meta`: list of metadata.

- `degree_audit`: tibble of unmatched degree text values.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- classroom_read("FCPK Classroom Details 21-22.xlsx")
clean <- classroom_clean(raw)
clean$data
clean$degree_audit  # check unmatched values
} # }
```

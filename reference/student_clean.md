# Clean Student/Child Detail Data

Transforms raw student data into analysis-ready format by renaming
columns, standardizing demographics, parsing income, cleaning attendance
data, processing assessment scores (GOLD, PPVT, eDECA, ASQ), deriving
indicators (age, IEP2, binary dummies), and optionally removing PII
fields.

## Usage

``` r
student_clean(raw_obj, include_pii = FALSE)
```

## Arguments

- raw_obj:

  An `alprek_student_raw` object from
  [`student_read()`](https://joonho112.github.io/ALprekDB/reference/student_read.md).

- include_pii:

  Logical. If `FALSE` (default), removes personally identifiable
  information (child names, guardian info, state/student IDs). DOB and
  derived variables (age, birth_year) are always retained.

## Value

An `alprek_student_clean` S3 object (list) with elements:

- `data`: Cleaned tibble of student data.

- `meta`: list of metadata.

- `cleaning_log`: list of cleaning statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
raw <- student_read("FCPK Student Details 21-22.xlsx", "2021-2022")
clean <- student_clean(raw)
clean <- student_clean(raw, include_pii = TRUE)
} # }
```

# Apply the applications-classroom linkage to an in-progress classroom_level tibble. Wraps [`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md) – which itself only returns rows for the target school_year – by joining onto classroom_level for that one school_year and leaving other-year rows unchanged.

Apply the applications-classroom linkage to an in-progress
classroom_level tibble. Wraps
[`linkage_applications_classroom()`](https://joonho112.github.io/ALprekDB/reference/linkage_applications_classroom.md)
– which itself only returns rows for the target school_year – by joining
onto classroom_level for that one school_year and leaving other-year
rows unchanged.

## Usage

``` r
.linkage_master_apply_applications(
  classroom_level,
  classroom_panel,
  applications
)
```

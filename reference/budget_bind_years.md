# Combine Multiple Years into a Panel Dataset

Binds multiple single-year `alprek_budget_master` objects into a
longitudinal panel dataset.

## Usage

``` r
budget_bind_years(..., master_list = NULL)
```

## Arguments

- ...:

  `alprek_budget_master` objects to combine.

- master_list:

  Optional list of `alprek_budget_master` objects. Alternative to `...`
  for programmatic use.

## Value

An `alprek_budget_panel` S3 object.

## Examples

``` r
if (FALSE) { # \dontrun{
panel <- budget_bind_years(master_2122, master_2223, master_2324)
panel$data
} # }
```

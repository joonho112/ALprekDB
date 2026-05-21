# Write a geocode panel to DuckDB

Persists `alprek_geocode_panel$data` into the `geocode_panel` table. The
panel's `data` already carries `geocode_run_id` (the row-level
discriminator from
[`geocode_bind_years()`](https://joonho112.github.io/ALprekDB/reference/geocode_bind_years.md)).
One lineage row per run is written via
[`db_write_geocode_lineage()`](https://joonho112.github.io/ALprekDB/reference/db_write_geocode_lineage.md)
using `panel$binding_log`.

## Usage

``` r
db_write_geocode_panel(conn, panel, overwrite = FALSE)
```

## Arguments

- conn:

  A DBI connection from
  [`db_init()`](https://joonho112.github.io/ALprekDB/reference/db_init.md).

- panel:

  An `alprek_geocode_panel` object.

- overwrite:

  Logical. Default `FALSE`.

## Value

Invisible character vector of tables written.

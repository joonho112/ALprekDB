# Read the geocode lineage table

Returns the full `geocode_lineage` table as a tibble.

## Usage

``` r
db_read_geocode_lineage(conn)
```

## Arguments

- conn:

  A DBI connection.

## Value

A tibble (0 rows if the table is absent).

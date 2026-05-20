# Compute SHA-256 Hash of a File

Computes the SHA-256 hash of a file's contents. Used to uniquely
identify a specific delivery of an ADECE input file (so that
re-deliveries can be detected, and processed datasets carry an
unambiguous source fingerprint).

## Usage

``` r
alprek_file_hash(path)
```

## Arguments

- path:

  Character. Path to file.

## Value

Character. The SHA-256 hash as a 64-character hex string, or
`NA_character_` if the file cannot be read or `digest` is unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
path <- file.path("ORIGINAL-DATA", "applications_2026_2027.xlsx")
alprek_file_hash(path)
} # }
```

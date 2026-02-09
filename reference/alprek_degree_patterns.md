# Load Degree Classification Patterns

Returns the regex-based classification patterns used to parse free-text
teacher degree/credential fields into standardized categories. The
codebook contains three pattern types:

- **degree_level**: Maps raw text to ordered education levels (Waiver,
  Doctoral, Ed.S., Master's, Bachelor's, Associate, Coursework)

- **degree_area**: Maps raw text to subject area categories (ECE,
  Elementary Education, Child Development, etc.)

- **degree_area_consolidation**: Reduces 9 area categories to 6 for
  cross-year comparability

## Usage

``` r
alprek_degree_patterns()
```

## Value

A tibble with columns: `pattern_type`, `regex`, `result`, `priority`,
`teacher_role`, `notes`.

## Details

Patterns are applied by priority (highest first). Waiver detection
(priority 100) overrides all other degree classifications. The
`teacher_role` column is currently `"all"` for all patterns but allows
future role-specific patterns.

## Examples

``` r
alprek_degree_patterns()
#> # A tibble: 27 × 6
#>    pattern_type regex                         result priority teacher_role notes
#>    <chr>        <chr>                         <chr>     <int> <chr>        <chr>
#>  1 degree_level "(?i)waiver|wavier"           Waiver      100 all          "Cat…
#>  2 degree_level "(?i)doctorate|ph\\.?d|ed\\.… Docto…       90 all          "Ph.…
#>  3 degree_level "(?i)ed\\.?\\s*s\\.?|educati… Ed.S.        80 all          "EdS…
#>  4 degree_level "(?i)master|(?i)\\bm\\.?a\\.… Maste…       70 all          "Inc…
#>  5 degree_level "(?i)bachelor|bachlor|achelo… Bache…       60 all          "Cat…
#>  6 degree_level "(?i)coursewo|working toward… Colle…       55 all          "In-…
#>  7 degree_level "(?i)associate|\\bcda\\b|chi… Assoc…       50 all          "CDA…
#>  8 degree_area  "(?i)elementary education.*e… Eleme…       90 all          "Bot…
#>  9 degree_area  "(?i)early childhood educati… Early…       80 all          "Inc…
#> 10 degree_area  "(?i)elementary education|ce… Eleme…       70 all          ""   
#> # ℹ 17 more rows

# View degree level patterns only
dp <- alprek_degree_patterns()
dp[dp$pattern_type == "degree_level", ]
#> # A tibble: 7 × 6
#>   pattern_type regex                          result priority teacher_role notes
#>   <chr>        <chr>                          <chr>     <int> <chr>        <chr>
#> 1 degree_level "(?i)waiver|wavier"            Waiver      100 all          Catc…
#> 2 degree_level "(?i)doctorate|ph\\.?d|ed\\.?… Docto…       90 all          Ph.D…
#> 3 degree_level "(?i)ed\\.?\\s*s\\.?|educatio… Ed.S.        80 all          EdS …
#> 4 degree_level "(?i)master|(?i)\\bm\\.?a\\.?… Maste…       70 all          Incl…
#> 5 degree_level "(?i)bachelor|bachlor|achelor… Bache…       60 all          Catc…
#> 6 degree_level "(?i)coursewo|working toward|… Colle…       55 all          In-p…
#> 7 degree_level "(?i)associate|\\bcda\\b|chil… Assoc…       50 all          CDA,…
```

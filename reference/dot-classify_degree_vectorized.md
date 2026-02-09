# Classify degree text into level and area (vectorized)

Classify degree text into level and area (vectorized)

## Usage

``` r
.classify_degree_vectorized(raw_texts, degree_patterns)
```

## Arguments

- raw_texts:

  Character vector of raw degree/credential text.

- degree_patterns:

  Degree patterns codebook tibble.

## Value

List with vectors: degree_level, degree_area, degree_area_simple,
waiver, and a tibble of unmatched values.

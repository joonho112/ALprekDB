# Allocate payroll taxes proportionally to lead/aux teacher benefits

In legacy format, payroll taxes are a single line item. This function
allocates them proportionally based on the ratio of lead vs aux teacher
salary, within each funding source.

## Usage

``` r
.allocate_payroll_taxes(wide)
```

## Arguments

- wide:

  Wide-format tibble with osr_payroll_taxes and other_payroll_taxes.

## Value

Modified tibble with payroll allocated to benefits columns.

# ALprekDB

**Alabama Pre-K Administrative Data Processing and Database Management**
(v0.5.0)

A modular R toolkit for processing, cleaning, validating, and managing
Alabama First Class Pre-K (FCPK) administrative records received from
ADECE (Alabama Department of Early Childhood Education). Transforms raw
Excel files into analysis-ready longitudinal panel datasets covering
**92,500+ students**, **5,900+ classrooms**, and **4 school years**
(2021-22 through 2024-25).

## Author

JoonHo Lee, Ph.D. Assistant Professor, The University of Alabama
<jlee296@ua.edu>

## Modules

| Module        | Purpose                                           | Key Output                           |
|---------------|---------------------------------------------------|--------------------------------------|
| **Budget**    | Per-classroom funding (OSR + Other, 8 categories) | 5,867 rows x 53 cols                 |
| **Classroom** | Teacher demographics, geography, credentials      | 5,888 rows x 125 cols                |
| **Student**   | Demographics, 4 assessments, services, attendance | 92,507 rows x 288 cols               |
| **Transform** | GOLD gains, chronic absence, risk index (27 vars) | Enriched student panel               |
| **Linkage**   | Cross-module joins, 2-level master datasets       | Classroom + Student masters          |
| **Database**  | DuckDB persistent storage, SQL queries            | Instant loading, incremental updates |

## Installation

``` r
# From GitHub
remotes::install_github("joonho112/ALprekDB")
```

## Quick Start

### Synthetic Data (no ADECE files needed)

``` r
library(ALprekDB)

# Generate linked synthetic datasets (same seed = shared classroom codes)
budget    <- alprek_synthetic_budget(n_classrooms = 20, n_years = 2, seed = 42)
classroom <- alprek_synthetic_classroom(n_classrooms = 20, n_years = 2, seed = 42)
student   <- alprek_synthetic_student(n_students = 100, n_classrooms = 20,
                                       n_years = 2, seed = 42)

# Create 2-level master dataset
master <- linkage_create_master(budget, classroom, student)
master$classroom_level  # classroom-year with budget + aggregated student data
master$student_level    # student-year with classroom + budget attributes
```

### Real Data Pipeline

``` r
# Budget: 4-year panel
configs <- list(
  budget_config("2021-2022", "Budget_21-22.xlsx"),
  budget_config("2022-2023", "Budget_22-23.xlsx"),
  budget_config("2023-2024", "Budget_23-24.xlsx"),
  budget_config("2024-2025", "Budget_24-25.xlsx")
)
result <- budget_process_years(configs)
budget_panel <- result$panel

# Same pattern for classroom_process_years() and student_process_years()

# Enrich student data with derived variables
enriched <- student_transform(student_panel)

# Link all three into master
master <- linkage_create_master(budget_panel, classroom_panel, enriched)
```

### DuckDB Storage

``` r
# Store processed data for instant future loading
conn <- db_init("alprekdb.duckdb")
db_write_panel(conn, budget_panel)
db_write_panel(conn, classroom_panel)
db_write_panel(conn, enriched)
db_write_master(conn, master)

# SQL queries directly on the database
db_query(conn, "
  SELECT school_year, delivery_type, COUNT(*) as n,
         AVG(grand_total) as mean_budget
  FROM master_classroom
  GROUP BY school_year, delivery_type
")

db_close(conn)
```

## Pipeline Architecture

    Excel files (.xlsx)
         |
      *_read()        --> alprek_*_raw        (raw data + metadata)
         |
      *_clean()       --> alprek_*_clean      (standardized, typed)
         |
      *_validate()    --> alprek_*_validation (advisory checks)
         |
      *_bind_years()  --> alprek_*_panel      (multi-year longitudinal)
         |
      student_transform()  --> enriched panel (27 derived variables)
         |
      linkage_create_master()  --> 2-level master (classroom + student)
         |
      db_write_*()   --> DuckDB              (persistent, SQL-queryable)
      *_export_*()   --> CSV / RDS / Stata / Excel / Parquet

## Key Features

- **Format auto-detection**: Legacy (2021-2024) vs New (2024-2025) Excel
  layouts
- **Data-driven mappings**: Column names and value standardization via
  CSV codebooks
- **37 validation checks**: Budget reconciliation, demographic ranges,
  assessment consistency
- **Assessment processing**: GOLD (6 domains), PPVT, eDECA (pre/post),
  ASQ
- **Derived variables**: Gain scores, K-readiness transitions, chronic
  absence, risk index
- **Cross-module linkage**: Join diagnostics, orphan detection, match
  rate reporting
- **DuckDB database**: Column-oriented storage, SQL queries, incremental
  year addition
- **Multiple exports**: CSV, RDS, Stata (.dta), Excel, Parquet

## Vignettes

``` r
vignette("getting-started", package = "ALprekDB")
vignette("panel-construction", package = "ALprekDB")
vignette("linkage-and-analysis", package = "ALprekDB")
vignette("reference-architecture", package = "ALprekDB")
```

## Classroom Code Format

Every record is identified by a classroom code: `CCCDNNNNN.NN`

- `CCC` = County code (001-067)
- `D` = Delivery type (P=Public, C=Private Child Care, H=Head Start,
  O=Community, F=Faith-Based, U=University, S=Private School)
- `NNNNN` = Program number
- `NN` = Class number within site

``` r
parse_classroom_codes(c("001P12345.01", "067H54321.02"))
```

## License

MIT

# ALprekDB targets workflow template

This directory is a companion template for rebuilding an ALprekDB processing
workflow with the `targets` package.

The template has two modes:

- synthetic mode, the default, which runs without confidential ADECE files;
- local real-data mode, which runs only when `ALPREKDB_RUN_REALDATA=1` and
  `ALPREKDB_DATA_DIR` points to a local source-data folder.

The template is intentionally shipped under `inst/templates/targets/` rather
than as a package-root `_targets.R`. It should be copied into a private analysis
project or local working folder before running. Do not point it at public
directories or commit local targets caches, DuckDB databases, panel exports, or
raw ADECE source data.

## Files

- `_targets.R`: target graph for synthetic and local real-data modes.
- `R/functions.R`: helper functions used by the target graph.
- `local.env.example`: optional environment-variable template.

## Required packages

Install ALprekDB and the workflow dependency:

```r
install.packages("targets")
```

DuckDB output also requires the package's optional database dependencies:

```r
install.packages(c("duckdb", "DBI"))
```

## Synthetic mode

Synthetic mode is the default and is safe for CI-like checks:

```r
targets::tar_make()
```

This mode generates linked synthetic budget, classroom, and student panels,
builds a linkage master object, writes local RDS outputs, writes aggregate
summary CSV files, and writes DuckDB output when `duckdb` and `DBI` are
installed.

Optional synthetic sizing variables:

```sh
export ALPREKDB_SYNTHETIC_N_CLASSROOMS=20
export ALPREKDB_SYNTHETIC_N_STUDENTS=100
export ALPREKDB_SYNTHETIC_N_YEARS=2
export ALPREKDB_SYNTHETIC_SEED=42
```

## Local real-data mode

Real-data mode is explicit:

```sh
export ALPREKDB_RUN_REALDATA=1
export ALPREKDB_DATA_DIR="/path/to/ALprekDB/ORIGINAL-DATA/ADECE-source-files"
export ALPREKDB_OUTPUT_DIR="output/alprekdb"
```

Then run:

```r
targets::tar_make()
```

The current 0.6.0 coverage policy encoded in the manifest is:

- budget: 2021-22 through 2024-25;
- classroom: 2021-22 through 2025-26;
- student: 2021-22 through 2025-26;
- 2025-26 budget is treated as structurally unavailable, not zero-filled.

Student processing uses `include_pii = FALSE`. The default outputs are local
aggregate summaries only. Row-level RDS outputs and DuckDB writes are disabled
in real-data mode unless explicitly enabled:

```sh
export ALPREKDB_WRITE_OUTPUTS=1
```

Even with `include_pii = FALSE`, student-level outputs remain confidential.
Keep `_targets/`, `output/`, local database files, and copied raw data out of
git and public documentation.

test_that("budget_read errors on nonexistent file", {
  expect_error(budget_read("nonexistent.xlsx"), "File not found")
})

test_that("alprek_infer_school_year works for full year format", {
  expect_equal(
    alprek_infer_school_year("rptClassBudgets 2021-2022.xlsx"),
    "2021-2022"
  )
  expect_equal(
    alprek_infer_school_year("rptClassBudgets 2023-2024.xlsx"),
    "2023-2024"
  )
})

test_that("alprek_infer_school_year works for short year format", {
  expect_equal(
    alprek_infer_school_year("24-25 FCPK Budgets.xlsx"),
    "2024-2025"
  )
  expect_equal(
    alprek_infer_school_year("21-22 Budget.xlsx"),
    "2021-2022"
  )
})

test_that("alprek_infer_school_year returns NA for unrecognizable names", {
  expect_true(is.na(alprek_infer_school_year("budget_data.xlsx")))
  expect_true(is.na(alprek_infer_school_year("report.xlsx")))
})

test_that("alprek_school_year_to_start converts correctly", {
  expect_equal(alprek_school_year_to_start("2024-2025"), 2024L)
  expect_equal(alprek_school_year_to_start("2021-2022"), 2021L)
})

test_that(".extract_year_from_bv extracts year from column name", {
  expect_equal(
    .extract_year_from_bv("Budget Version (latest approved for 2023-2024)"),
    "2023-2024"
  )
  expect_true(is.na(.extract_year_from_bv("Budget Version")))
})

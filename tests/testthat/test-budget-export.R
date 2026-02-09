test_that("budget_export_csv creates file", {
  raw <- make_budget_raw("legacy")
  master <- budget_transform(budget_clean(raw))

  tmp <- tempfile(fileext = ".csv")
  result <- budget_export_csv(master, path = tmp)

  expect_true(file.exists(tmp))
  expect_equal(result, tmp)

  # Read back and verify
  df <- utils::read.csv(tmp)
  expect_equal(nrow(df), 5)
  unlink(tmp)
})

test_that("budget_export_csv long format has expected columns", {
  raw <- make_budget_raw("new")
  master <- budget_transform(budget_clean(raw))

  tmp <- tempfile(fileext = ".csv")
  budget_export_csv(master, path = tmp, format = "long")

  df <- utils::read.csv(tmp)
  expect_true("source_type" %in% names(df))
  expect_true("category_group" %in% names(df))
  expect_true("amount" %in% names(df))
  unlink(tmp)
})

test_that("budget_export_rds roundtrips correctly", {
  raw <- make_budget_raw("legacy")
  master <- budget_transform(budget_clean(raw))

  tmp <- tempfile(fileext = ".rds")
  budget_export_rds(master, path = tmp)

  expect_true(file.exists(tmp))
  loaded <- readRDS(tmp)
  expect_s3_class(loaded, "alprek_budget_master")
  expect_equal(nrow(loaded$data), nrow(master$data))
  unlink(tmp)
})

test_that("budget_export_parquet requires arrow package", {
  raw <- make_budget_raw("legacy")
  master <- budget_transform(budget_clean(raw))

  if (!requireNamespace("arrow", quietly = TRUE)) {
    expect_error(
      budget_export_parquet(master, "test.parquet"),
      "arrow"
    )
  } else {
    tmp <- tempfile(fileext = ".parquet")
    budget_export_parquet(master, path = tmp)
    expect_true(file.exists(tmp))
    unlink(tmp)
  }
})

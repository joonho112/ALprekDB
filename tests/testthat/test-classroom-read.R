test_that("classroom_detect_format identifies legacy format", {
  df <- make_classroom_legacy_raw_df(3)
  format <- classroom_detect_format(df)
  expect_equal(format, "legacy")
})

test_that("classroom_detect_format identifies new format", {
  df <- make_classroom_new_raw_df(3)
  format <- classroom_detect_format(df)
  expect_equal(format, "new")
})

test_that("classroom_detect_format uses Fund Source as new marker", {
  # Create a minimal df with Fund Source column
  df <- tibble::tibble(
    `Classroom Code Static` = "001P00001.01",
    `Fund Source` = "State"
  )
  # Add enough columns to avoid legacy default
  for (i in seq_len(118)) {
    df[[paste0("col_", i)]] <- "x"
  }
  expect_equal(classroom_detect_format(df), "new")
})

test_that("classroom_detect_format detects legacy via column count", {
  df <- make_classroom_legacy_raw_df(2)
  expect_lte(ncol(df), 105)
  expect_equal(classroom_detect_format(df), "legacy")
})

test_that("make_classroom_raw creates valid legacy S3 object", {
  raw <- make_classroom_raw("legacy", 3)
  expect_s3_class(raw, "alprek_classroom_raw")
  expect_equal(raw$meta$format, "legacy")
  expect_equal(raw$meta$school_year, "2023-2024")
  expect_equal(raw$meta$n_rows_clean, 3)
  expect_true(is.data.frame(raw$data))
})

test_that("make_classroom_raw creates valid new S3 object", {
  raw <- make_classroom_raw("new", 4)
  expect_s3_class(raw, "alprek_classroom_raw")
  expect_equal(raw$meta$format, "new")
  expect_equal(raw$meta$school_year, "2024-2025")
  expect_equal(raw$meta$n_rows_clean, 4)
})

test_that("legacy fixture has 100 columns", {
  df <- make_classroom_legacy_raw_df(2)
  expect_equal(ncol(df), 100)
})

test_that("new fixture has more columns than legacy", {
  legacy_df <- make_classroom_legacy_raw_df(2)
  new_df <- make_classroom_new_raw_df(2)
  expect_gt(ncol(new_df), ncol(legacy_df))
})

test_that("school year inference works for classroom files", {
  expect_equal(
    alprek_infer_school_year("FCPK Classroom Details 21-22.xlsx"),
    "2021-2022"
  )
  expect_equal(
    alprek_infer_school_year("24-25 Classroom Details.xlsx"),
    "2024-2025"
  )
  expect_equal(
    alprek_infer_school_year("FCPK Classroom Details 2023-2024.xlsx"),
    "2023-2024"
  )
})

test_that("print method for alprek_classroom_raw works", {
  raw <- make_classroom_raw("legacy", 2)
  expect_output(print(raw), "alprek_classroom_raw")
  expect_output(print(raw), "legacy")
  expect_output(print(raw), "2023-2024")
})

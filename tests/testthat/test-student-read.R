# tests/testthat/test-student-read.R

test_that("student_detect_format identifies legacy format", {
  df <- make_student_legacy_raw_df(5)
  expect_equal(student_detect_format(df), "legacy")
})


test_that("student_detect_format identifies new format", {
  df <- make_student_new_raw_df(5)
  expect_equal(student_detect_format(df), "new")
})


test_that("student_detect_format uses marker columns for detection", {
  # Add Child First Name to a small df -> should detect as new
  df <- tibble::tibble(
    `Child First Name` = "Test",
    `Other Col` = 1
  )
  expect_equal(student_detect_format(df), "new")

  # Modified Schedule marker
  df2 <- tibble::tibble(
    `Modified Schedule` = TRUE,
    `Other Col` = 1
  )
  expect_equal(student_detect_format(df2), "new")
})


test_that("make_student_raw creates valid S3 objects", {
  raw_legacy <- make_student_raw("legacy", n = 5)
  raw_new <- make_student_raw("new", n = 5)

  # S3 class

  expect_s3_class(raw_legacy, "alprek_student_raw")
  expect_s3_class(raw_new, "alprek_student_raw")

  # Data structure
  expect_true(is.data.frame(raw_legacy$data))
  expect_true(is.data.frame(raw_new$data))

  # Meta fields
  expect_equal(raw_legacy$meta$school_year, "2023-2024")
  expect_equal(raw_legacy$meta$format, "legacy")
  expect_equal(raw_new$meta$school_year, "2024-2025")
  expect_equal(raw_new$meta$format, "new")

  # Row counts
  expect_equal(nrow(raw_legacy$data), 5)
  expect_equal(nrow(raw_new$data), 5)
})


test_that("print method for alprek_student_raw works", {
  raw <- make_student_raw("legacy", n = 3)
  expect_output(print(raw), "alprek_student_raw")
  expect_output(print(raw), "2023-2024")
  expect_output(print(raw), "legacy")
})


test_that(".find_student_id_col finds ADECE ID", {
  expect_equal(.find_student_id_col(c("Name", "ADECE ID", "Gender")), "ADECE ID")
  expect_null(.find_student_id_col(c("Name", "Gender")))
})

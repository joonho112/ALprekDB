# tests/testthat/test-student-panel.R

test_that("student_bind_years requires correct input", {
  expect_error(student_bind_years(), "No data to combine")
  expect_error(student_bind_years(data.frame()), "not an alprek_student_clean")
})


test_that("student_bind_years combines multiple years", {
  clean1 <- make_student_clean("legacy", n = 5)
  clean2 <- make_student_clean("legacy", n = 8)
  clean2$meta$school_year <- "2022-2023"
  clean2$data$school_year <- "2022-2023"
  clean2$data$year <- 2022L

  panel <- student_bind_years(clean1, clean2)

  expect_s3_class(panel, "alprek_student_panel")
  expect_equal(nrow(panel$data), 13)  # 5 + 8
  expect_equal(panel$n_total, 13)
  expect_equal(length(panel$years), 2)
  expect_true("2022-2023" %in% panel$years)
  expect_true("2023-2024" %in% panel$years)
})


test_that("student_bind_years works with clean_list", {
  c1 <- make_student_clean("legacy", n = 5)
  c2 <- make_student_clean("legacy", n = 5)
  c2$meta$school_year <- "2022-2023"
  c2$data$school_year <- "2022-2023"
  c2$data$year <- 2022L

  panel <- student_bind_years(clean_list = list(c1, c2))
  expect_s3_class(panel, "alprek_student_panel")
  expect_equal(nrow(panel$data), 10)
})


test_that("panel counts unique students", {
  c1 <- make_student_clean("legacy", n = 5)
  c2 <- make_student_clean("legacy", n = 5)
  c2$meta$school_year <- "2022-2023"
  c2$data$school_year <- "2022-2023"
  c2$data$year <- 2022L

  # First give c2 entirely different IDs
  c2$data$adece_id <- sprintf("STU%06d", 100 + seq_len(5))
  # Then make 2 overlap with c1
  c2$data$adece_id[1:2] <- c1$data$adece_id[1:2]

  panel <- student_bind_years(c1, c2)
  # 5 unique in c1 + 3 unique new in c2 = 8
  expect_equal(panel$n_unique_students, 8)
})


test_that("student_track creates presence matrix", {
  c1 <- make_student_clean("legacy", n = 5)
  c2 <- make_student_clean("legacy", n = 5)
  c2$meta$school_year <- "2022-2023"
  c2$data$school_year <- "2022-2023"
  c2$data$year <- 2022L

  # First give c2 entirely different IDs, then share 2
  c2$data$adece_id <- sprintf("STU%06d", 100 + seq_len(5))
  c2$data$adece_id[1:2] <- c1$data$adece_id[1:2]

  panel <- student_bind_years(c1, c2)
  track <- student_track(panel)

  expect_s3_class(track, "tbl_df")
  expect_true("adece_id" %in% names(track))
  # Should have columns for each year
  expect_true("2022-2023" %in% names(track))
  expect_true("2023-2024" %in% names(track))
  # Total unique students = 8
  expect_equal(nrow(track), 8)
  # Students present in both years
  both <- sum(track$`2022-2023` & track$`2023-2024`)
  expect_equal(both, 2)
})


test_that("student_summary_stats works by school_year", {
  c1 <- make_student_clean("legacy", n = 10)
  c2 <- make_student_clean("legacy", n = 10)
  c2$meta$school_year <- "2022-2023"
  c2$data$school_year <- "2022-2023"
  c2$data$year <- 2022L

  panel <- student_bind_years(c1, c2)
  stats <- student_summary_stats(panel, by = "school_year")

  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 2)  # Two years
  expect_true("n_students" %in% names(stats))
  expect_true("pct_male" %in% names(stats))
})


test_that("student_summary_stats works on single clean object", {
  clean <- make_student_clean("legacy", n = 10)
  stats <- student_summary_stats(clean, by = NULL)
  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 1)
  expect_equal(stats$n_students, 10)
})


test_that("student_summary_stats requires correct input", {
  expect_error(student_summary_stats(data.frame()),
               "Expected alprek_student_clean or alprek_student_panel")
})


test_that("panel handles column union across formats", {
  c1 <- make_student_clean("legacy", n = 5)
  c2 <- make_student_clean("legacy", n = 5)
  c2$meta$school_year <- "2024-2025"
  c2$data$school_year <- "2024-2025"
  c2$data$year <- 2024L
  c2$meta$format <- "new"
  # Add a new-format-only column
  c2$data$monitor_name <- paste0("Monitor ", seq_len(5))

  panel <- student_bind_years(c1, c2)
  # The new column should exist, with NA for year 1
  expect_true("monitor_name" %in% names(panel$data))
  expect_true(all(is.na(panel$data$monitor_name[panel$data$school_year == "2023-2024"])))
})


test_that("print method for panel works", {
  clean <- make_student_clean("legacy", n = 5)
  panel <- student_bind_years(clean)
  expect_output(print(panel), "alprek_student_panel")
  expect_output(print(panel), "Unique students")
})


test_that("by_year metadata is correct", {
  c1 <- make_student_clean("legacy", n = 5)
  c2 <- make_student_clean("legacy", n = 8)
  c2$meta$school_year <- "2022-2023"
  c2$data$school_year <- "2022-2023"

  panel <- student_bind_years(c1, c2)

  expect_true("2023-2024" %in% names(panel$by_year))
  expect_true("2022-2023" %in% names(panel$by_year))
  expect_equal(panel$by_year[["2023-2024"]]$n_students, 5)
  expect_equal(panel$by_year[["2022-2023"]]$n_students, 8)
})

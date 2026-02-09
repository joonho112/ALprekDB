test_that("budget_bind_years combines multiple years", {
  raw1 <- make_budget_raw("legacy")
  raw1$meta$school_year <- "2021-2022"
  raw1$meta$year <- 2021L
  raw1$data$school_year <- "2021-2022"
  raw1$data$year <- 2021L

  raw2 <- make_budget_raw("legacy")
  raw2$meta$school_year <- "2022-2023"
  raw2$meta$year <- 2022L
  raw2$data$school_year <- "2022-2023"
  raw2$data$year <- 2022L

  m1 <- budget_transform(budget_clean(raw1))
  m2 <- budget_transform(budget_clean(raw2))

  panel <- budget_bind_years(m1, m2)

  expect_s3_class(panel, "alprek_budget_panel")
  expect_equal(panel$n_years, 2)
  expect_equal(nrow(panel$data), 10)  # 5 + 5
})

test_that("budget_track_classrooms returns tibble with presence columns", {
  raw1 <- make_budget_raw("legacy")
  raw1$meta$school_year <- "2021-2022"
  raw1$meta$year <- 2021L
  raw1$data$school_year <- "2021-2022"
  raw1$data$year <- 2021L

  raw2 <- make_budget_raw("legacy")
  raw2$meta$school_year <- "2022-2023"
  raw2$meta$year <- 2022L
  raw2$data$school_year <- "2022-2023"
  raw2$data$year <- 2022L

  m1 <- budget_transform(budget_clean(raw1))
  m2 <- budget_transform(budget_clean(raw2))
  panel <- budget_bind_years(m1, m2)

  tracking <- budget_track_classrooms(panel)
  expect_s3_class(tracking, "tbl_df")
  expect_true("n_years_present" %in% names(tracking))
  expect_true("all_years" %in% names(tracking))
})

test_that("budget_yoy_summary returns per-year statistics", {
  raw <- make_budget_raw("legacy")
  master <- budget_transform(budget_clean(raw))
  panel <- budget_bind_years(master)

  yoy <- budget_yoy_summary(panel)
  expect_s3_class(yoy, "tbl_df")
  expect_true("mean_grand_total" %in% names(yoy))
  expect_equal(nrow(yoy), 1)
})

test_that("budget_to_long produces expected columns", {
  raw <- make_budget_raw("new")
  master <- budget_transform(budget_clean(raw))

  long <- budget_to_long(master)
  expect_true(all(c("source_type", "category_group", "amount") %in% names(long)))
  expect_true(all(long$source_type %in% c("osr", "other")))
})

test_that("budget_summary_stats groups by school_year", {
  raw <- make_budget_raw("legacy")
  master <- budget_transform(budget_clean(raw))

  stats <- budget_summary_stats(master)
  expect_s3_class(stats, "tbl_df")
  expect_true("grand_total_mean" %in% names(stats))
})

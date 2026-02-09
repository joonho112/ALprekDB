test_that("classroom_bind_years combines clean objects", {
  c1 <- make_classroom_clean("legacy", 3)
  c1$data$school_year <- "2022-2023"
  c1$meta$school_year <- "2022-2023"

  c2 <- make_classroom_clean("legacy", 4)
  c2$data$school_year <- "2023-2024"
  c2$meta$school_year <- "2023-2024"

  panel <- classroom_bind_years(clean_list = list(c1, c2))

  expect_s3_class(panel, "alprek_classroom_panel")
  expect_equal(panel$n_total, 7)
  expect_equal(length(panel$years), 2)
  expect_true("2022-2023" %in% panel$years)
  expect_true("2023-2024" %in% panel$years)
})

test_that("classroom_bind_years rejects wrong input", {
  expect_error(classroom_bind_years(clean_list = list()),
               "No data to combine")
  expect_error(classroom_bind_years(clean_list = list(list())),
               "not an alprek_classroom_clean")
})

test_that("imputation fills lat/lon within site groups", {
  c1 <- make_classroom_clean("legacy", 2)
  c1$data$school_year <- "2022-2023"
  c1$meta$school_year <- "2022-2023"
  c1$data$site_code <- c("001P00001", "001P00001")
  c1$data$latitude <- c(32.5, NA)
  c1$data$longitude <- c(-86.5, NA)

  c2 <- make_classroom_clean("legacy", 2)
  c2$data$school_year <- "2023-2024"
  c2$meta$school_year <- "2023-2024"
  c2$data$site_code <- c("001P00001", "002C00002")
  c2$data$latitude <- c(NA, 33.0)
  c2$data$longitude <- c(NA, -87.0)

  panel <- classroom_bind_years(clean_list = list(c1, c2))

  # Check that lat/lon were imputed for same site_code
  site1_rows <- panel$data[panel$data$site_code == "001P00001", ]
  expect_true(all(!is.na(site1_rows$latitude)))
  expect_true(all(!is.na(site1_rows$longitude)))
})

test_that("imputation logs track imputed values", {
  c1 <- make_classroom_clean("legacy", 1)
  c1$data$school_year <- "2022-2023"
  c1$meta$school_year <- "2022-2023"
  c1$data$site_code <- "001P00001"
  c1$data$latitude <- 32.5
  c1$data$longitude <- -86.5

  c2 <- make_classroom_clean("legacy", 1)
  c2$data$school_year <- "2023-2024"
  c2$meta$school_year <- "2023-2024"
  c2$data$site_code <- "001P00001"
  c2$data$latitude <- NA_real_
  c2$data$longitude <- NA_real_

  panel <- classroom_bind_years(clean_list = list(c1, c2))

  expect_true(nrow(panel$imputation_log) > 0)
  expect_true("latitude" %in% panel$imputation_log$variable)
  expect_true("longitude" %in% panel$imputation_log$variable)
})

test_that("classroom_track creates presence matrix", {
  c1 <- make_classroom_clean("legacy", 2)
  c1$data$school_year <- "2022-2023"
  c1$meta$school_year <- "2022-2023"
  c1$data$classroom_code <- c("001P00001.01", "002C00002.01")

  c2 <- make_classroom_clean("legacy", 2)
  c2$data$school_year <- "2023-2024"
  c2$meta$school_year <- "2023-2024"
  c2$data$classroom_code <- c("001P00001.01", "003H00003.01")

  panel <- classroom_bind_years(clean_list = list(c1, c2))
  track <- classroom_track(panel)

  expect_s3_class(track, "tbl_df")
  expect_true("classroom_code" %in% names(track))
  expect_true("2022-2023" %in% names(track))
  expect_true("2023-2024" %in% names(track))

  # 001P00001.01 should be present in both years
  row1 <- track[track$classroom_code == "001P00001.01", ]
  expect_true(row1[["2022-2023"]])
  expect_true(row1[["2023-2024"]])

  # 002C00002.01 should only be in first year
  row2 <- track[track$classroom_code == "002C00002.01", ]
  expect_true(row2[["2022-2023"]])
  expect_false(row2[["2023-2024"]])
})

test_that("classroom_summary_stats works on panel", {
  c1 <- make_classroom_clean("legacy", 5)
  c2 <- make_classroom_clean("new", 3)
  c2$data$school_year <- "2024-2025"
  c2$meta$school_year <- "2024-2025"

  panel <- classroom_bind_years(clean_list = list(c1, c2))
  stats <- classroom_summary_stats(panel)

  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 2)  # Two school years
  expect_true("n_classrooms" %in% names(stats))
})

test_that("print method for panel works", {
  c1 <- make_classroom_clean("legacy", 3)
  panel <- classroom_bind_years(clean_list = list(c1))
  expect_output(print(panel), "alprek_classroom_panel")
})

test_that("year_first_funded imputation works", {
  c1 <- make_classroom_clean("legacy", 1)
  c1$data$school_year <- "2022-2023"
  c1$meta$school_year <- "2022-2023"
  c1$data$classroom_code <- "001P00001.01"
  c1$data$year_first_funded <- 2015L

  c2 <- make_classroom_clean("legacy", 1)
  c2$data$school_year <- "2023-2024"
  c2$meta$school_year <- "2023-2024"
  c2$data$classroom_code <- "001P00001.01"
  c2$data$year_first_funded <- NA_integer_

  panel <- classroom_bind_years(clean_list = list(c1, c2))

  yr2 <- panel$data[panel$data$school_year == "2023-2024", ]
  expect_equal(yr2$year_first_funded, 2015L)
})

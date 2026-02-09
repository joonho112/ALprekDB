test_that("parse_classroom_code parses valid code correctly", {
  result <- parse_classroom_code("823P012601.01")
  expect_equal(result$county_code, "823")
  expect_equal(result$delivery_type_code, "P")
  expect_equal(result$program_code, "012601")
  expect_equal(result$class_num, "01")
})

test_that("parse_classroom_code handles all delivery type codes", {
  for (code in c("P", "C", "H", "O", "F", "U", "S")) {
    result <- parse_classroom_code(paste0("001", code, "12345.01"))
    expect_equal(result$delivery_type_code, code)
  }
})

test_that("parse_classroom_code returns NAs for invalid codes", {
  expect_true(is.na(parse_classroom_code("invalid")$county_code))
  expect_true(is.na(parse_classroom_code("")$county_code))
  expect_true(is.na(parse_classroom_code(NA_character_)$county_code))
  expect_true(is.na(parse_classroom_code("Count:")$county_code))
})

test_that("parse_classroom_codes returns tibble with correct columns", {
  codes <- c("823P012601.01", "456C00789.02")
  result <- parse_classroom_codes(codes)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("county_code", "delivery_type_code", "program_code",
                          "class_num", "delivery_type"))
  expect_equal(nrow(result), 2)
})

test_that("parse_classroom_codes maps delivery type names", {
  result <- parse_classroom_codes(c("001P12345.01", "002C67890.01"))
  expect_equal(unname(result$delivery_type), c("Public School", "Private Child Care"))
})

test_that("parse_classroom_codes handles varying program code lengths", {
  result <- parse_classroom_codes(c("001P1.01", "001P123456.01"))
  expect_equal(result$program_code, c("1", "123456"))
})

test_that("parse_classroom_codes returns NAs for NA input", {
  result <- parse_classroom_codes(c(NA_character_, "001P12345.01"))
  expect_true(is.na(result$county_code[1]))
  expect_equal(result$county_code[2], "001")
})

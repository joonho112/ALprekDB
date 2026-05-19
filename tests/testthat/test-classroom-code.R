test_that("parse_classroom_code parses valid code correctly", {
  result <- parse_classroom_code("901P900001.01")
  expect_equal(result$county_code, "901")
  expect_equal(result$delivery_type_code, "P")
  expect_equal(result$program_code, "900001")
  expect_equal(result$class_num, "01")
})

test_that("parse_classroom_code handles all delivery type codes", {
  for (code in c("P", "C", "H", "O", "F", "U", "S")) {
    result <- parse_classroom_code(paste0("901", code, "900001.01"))
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
  codes <- c("901P900001.01", "956C900789.02")
  result <- parse_classroom_codes(codes)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("county_code", "delivery_type_code", "program_code",
                          "class_num", "delivery_type"))
  expect_equal(nrow(result), 2)
})

test_that("parse_classroom_codes maps delivery type names", {
  result <- parse_classroom_codes(c("901P900001.01", "902C900002.01"))
  expect_equal(unname(result$delivery_type), c("Public School", "Private Child Care"))
})

test_that("parse_classroom_codes handles varying program code lengths", {
  result <- parse_classroom_codes(c("901P9.01", "901P900001.01"))
  expect_equal(result$program_code, c("9", "900001"))
})

test_that("parse_classroom_codes returns NAs for NA input", {
  result <- parse_classroom_codes(c(NA_character_, "901P900001.01"))
  expect_true(is.na(result$county_code[1]))
  expect_equal(result$county_code[2], "901")
})

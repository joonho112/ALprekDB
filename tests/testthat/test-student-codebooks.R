# tests/testthat/test-student-codebooks.R

test_that("student race mapping loads correctly", {
  map <- alprek_student_race_mapping()

  expect_s3_class(map, "tbl_df")
  expect_named(map, c("raw_value", "standardized", "factor_order"))

  # All 7 standardized categories present
  std_categories <- c("White", "Black", "Latino/Hispanic", "Asian",
                       "Mixed", "Other", "Unknown")
  expect_true(all(std_categories %in% unique(map$standardized)))

  # Key raw variants exist
  expect_true("Black or African American" %in% map$raw_value)
  expect_true("Black/African American" %in% map$raw_value)
  expect_true("Hispanic" %in% map$raw_value)
  expect_true("Latino/Hispanic" %in% map$raw_value)
  expect_true("Mixed Heritage" %in% map$raw_value)
  expect_true("Mixed heritage" %in% map$raw_value)
  expect_true("Filipino" %in% map$raw_value)
  expect_true("Decline to answer" %in% map$raw_value)

  # Factor order is consistent
  expect_equal(map$factor_order[map$standardized == "White"][1], 1)
  expect_equal(map$factor_order[map$standardized == "Black"][1], 2)
  expect_equal(map$factor_order[map$standardized == "Unknown"][1], 7)

  # No duplicates in raw_value
  expect_false(any(duplicated(map$raw_value)))
})


test_that("student delivery type mapping loads correctly", {
  map <- alprek_student_delivery_mapping()

  expect_s3_class(map, "tbl_df")
  expect_named(map, c("raw_value", "standardized"))

  # All 7 standardized types
  std_types <- c("Public School", "Community Organization", "Private Child Care",
                  "Faith-Based Organization", "Head Start",
                  "University Operated", "Private School")
  expect_true(all(std_types %in% unique(map$standardized)))

  # Key variant mappings exist
  expect_true("public school" %in% map$raw_value)
  expect_true("community organization operated" %in% map$raw_value)
  expect_true("private childcare" %in% map$raw_value)
  expect_true("private child care" %in% map$raw_value)
  expect_true("faith based organization" %in% map$raw_value)
  expect_true("university" %in% map$raw_value)
  expect_true("university operated" %in% map$raw_value)

  # No duplicates
  expect_false(any(duplicated(map$raw_value)))
})


test_that("student column maps load correctly", {
  legacy <- .load_student_column_map("legacy")
  new_map <- .load_student_column_map("new")

  # Structure
  expect_named(legacy, c("raw_column", "standard_name", "type", "notes"))
  expect_named(new_map, c("raw_column", "standard_name", "type", "notes"))

  # Row counts (key structural test)
  # Legacy has more than 190 rows; New has more than 250 rows
  expect_gt(nrow(legacy), 190)
  expect_gt(nrow(new_map), 250)
  expect_gt(nrow(new_map), nrow(legacy))

  # No duplicate standard_names within each map
  expect_false(any(duplicated(legacy$standard_name)))
  expect_false(any(duplicated(new_map$standard_name)))

  # Key columns present in both
  common_cols <- c("school_year", "classroom_code", "adece_id", "gender",
                   "race", "delivery_type", "iep", "gold_literacy_fall_raw")
  expect_true(all(common_cols %in% legacy$standard_name))
  expect_true(all(common_cols %in% new_map$standard_name))

  # New format has additional column types
  expect_true("child_pii" %in% new_map$type)
  expect_true("guardian_pii" %in% new_map$type)
  expect_true("staff_contact" %in% new_map$type)
  expect_true("gold_growth" %in% new_map$type)

  # Invalid format errors
  expect_error(.load_student_column_map("invalid"))
})

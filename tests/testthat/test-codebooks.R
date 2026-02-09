test_that("alprek_category_groups loads without error", {
  cg <- alprek_category_groups()
  expect_s3_class(cg, "tbl_df")
  expect_true(all(c("category_detail", "category_group", "notes") %in% names(cg)))
})

test_that("alprek_category_groups contains all 9 category groups", {
  cg <- alprek_category_groups()
  expected_groups <- c(
    "lead_teacher_salary", "lead_teacher_benefits",
    "aux_teacher_salary", "aux_teacher_benefits",
    "payroll_taxes",
    "instructional_support", "operations_and_maintenance",
    "equipment", "administrative"
  )
  expect_true(all(expected_groups %in% unique(cg$category_group)))
})

test_that("alprek_delivery_types loads with 7 rows", {
  dt <- alprek_delivery_types()
  expect_s3_class(dt, "tbl_df")
  expect_equal(nrow(dt), 7)
  expect_true(all(c("code", "name", "name_short") %in% names(dt)))
})

test_that("alprek_county_codes loads with 67 rows", {
  cc <- alprek_county_codes()
  expect_s3_class(cc, "tbl_df")
  expect_equal(nrow(cc), 67)
  expect_true(all(c("county_code", "county_name", "fips_code") %in% names(cc)))
})

test_that("column maps load for both formats", {
  legacy <- .load_column_map("legacy")
  expect_s3_class(legacy, "tbl_df")
  expect_true(all(c("raw_column", "standard_name", "type") %in% names(legacy)))

  new_map <- .load_column_map("new")
  expect_s3_class(new_map, "tbl_df")
  expect_true(nrow(new_map) > nrow(legacy))
})

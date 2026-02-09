test_that("alprek_degree_patterns() loads correctly", {
  dp <- alprek_degree_patterns()
  expect_s3_class(dp, "tbl_df")
  expect_true(all(c("pattern_type", "regex", "result", "priority",
                     "teacher_role", "notes") %in% names(dp)))
  # Should have all three pattern types

  expect_true("degree_level" %in% dp$pattern_type)
  expect_true("degree_area" %in% dp$pattern_type)
  expect_true("degree_area_consolidation" %in% dp$pattern_type)
  # At least 7 degree level patterns
  expect_gte(sum(dp$pattern_type == "degree_level"), 7)
})

test_that("degree level patterns cover expected levels", {
  dp <- alprek_degree_patterns()
  levels <- dp$result[dp$pattern_type == "degree_level"]
  expect_true("Waiver" %in% levels)
  expect_true("Doctoral degree" %in% levels)
  expect_true("Master's degree" %in% levels)
  expect_true("Bachelor's degree" %in% levels)
  expect_true("Associate degree" %in% levels)
  expect_true("College Coursework" %in% levels)
})

test_that("Waiver has highest priority", {
  dp <- alprek_degree_patterns()
  level_pats <- dp[dp$pattern_type == "degree_level", ]
  waiver_priority <- level_pats$priority[level_pats$result == "Waiver"]
  other_priorities <- level_pats$priority[level_pats$result != "Waiver"]
  expect_true(all(waiver_priority > other_priorities))
})

test_that("alprek_race_mapping() loads correctly", {
  rm <- alprek_race_mapping()
  expect_s3_class(rm, "tbl_df")
  expect_true(all(c("raw_value", "standardized", "factor_order") %in% names(rm)))
  # Should cover major categories
  expect_true("White" %in% rm$standardized)
  expect_true("Black" %in% rm$standardized)
  expect_true("Latino/Hispanic" %in% rm$standardized)
  expect_true("Asian" %in% rm$standardized)
})

test_that("alprek_language_mapping() loads correctly", {
  lm <- alprek_language_mapping()
  expect_s3_class(lm, "tbl_df")
  expect_true(all(c("raw_value", "standardized", "is_null") %in% names(lm)))
  # Should have null variants
  expect_true(any(lm$is_null == TRUE))
  # Should have actual languages
  expect_true(any(lm$is_null == FALSE))
})

test_that("language mapping null variants include common entries", {
  lm <- alprek_language_mapping()
  null_vals <- lm$raw_value[lm$is_null == TRUE]
  expect_true("N/A" %in% null_vals)
  expect_true("no" %in% null_vals)
  expect_true("None" %in% null_vals)
  # English should be null (means no second language)
  expect_true("English" %in% null_vals)
})

test_that("column map loader works for both formats", {
  legacy <- .load_classroom_column_map("legacy")
  expect_s3_class(legacy, "tbl_df")
  expect_true(all(c("raw_column", "standard_name", "type") %in% names(legacy)))
  expect_equal(nrow(legacy), 100)  # Legacy has 100 columns

  new_map <- .load_classroom_column_map("new")
  expect_s3_class(new_map, "tbl_df")
  expect_equal(nrow(new_map), 125)  # New has 125 columns
})

test_that("column map loader rejects invalid format", {
  expect_error(.load_classroom_column_map("invalid"))
})

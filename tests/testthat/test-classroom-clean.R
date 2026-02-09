# ==========================================================================
# Degree classification tests
# ==========================================================================

test_that("classify_degree: Bachelor's level", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Bachelor's in Early Childhood Education", dp)
  expect_equal(as.character(result$degree_level), "Bachelor's degree")
  expect_equal(result$waiver, 0L)
})

test_that("classify_degree: Master's level", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Master's in Elementary Education", dp)
  expect_equal(as.character(result$degree_level), "Master's degree")
})

test_that("classify_degree: CDA maps to Associate", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("CDA", dp)
  expect_equal(as.character(result$degree_level), "Associate degree")
})

test_that("classify_degree: Child Development Associate maps to Associate", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Child Development Associate", dp)
  expect_equal(as.character(result$degree_level), "Associate degree")
})

test_that("classify_degree: Waiver overrides everything", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Waiver", dp)
  expect_equal(as.character(result$degree_level), "Waiver")
  expect_equal(result$waiver, 1L)
  expect_equal(result$degree_area, "Waiver")
  expect_equal(result$degree_area_simple, "Waiver")
})

test_that("classify_degree: misspelled 'wavier' still maps to Waiver", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("wavier", dp)
  expect_equal(as.character(result$degree_level), "Waiver")
  expect_equal(result$waiver, 1L)
})

test_that("classify_degree: 'Waiver to complete CDA' maps to Waiver", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Waiver to complete CDA", dp)
  expect_equal(as.character(result$degree_level), "Waiver")
  expect_equal(result$waiver, 1L)
})

test_that("classify_degree: Ph.D. maps to Doctoral", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Ph.D. in Education", dp)
  expect_equal(as.character(result$degree_level), "Doctoral degree")
})

test_that("classify_degree: Ed.D. maps to Doctoral", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Ed.D.", dp)
  expect_equal(as.character(result$degree_level), "Doctoral degree")
})

test_that("classify_degree: Ed.S. variants", {
  dp <- alprek_degree_patterns()
  for (val in c("Ed.S.", "EdS", "Ed. S.", "Education Specialist")) {
    result <- .classify_degree_vectorized(val, dp)
    expect_equal(as.character(result$degree_level), "Ed.S.",
                 info = paste("Failed for:", val))
  }
})

test_that("classify_degree: 'Interdisciplinary Studies' maps to Bachelor's", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Interdisciplinary Studies", dp)
  expect_equal(as.character(result$degree_level), "Bachelor's degree")
})

test_that("classify_degree: '9 Hours ECE' maps to Coursework", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("9 Hours ECE Coursework", dp)
  expect_equal(as.character(result$degree_level), "College Coursework")
})

test_that("classify_degree: 'Working toward CDA' maps to Coursework", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Working toward CDA", dp)
  expect_equal(as.character(result$degree_level), "College Coursework")
})

test_that("classify_degree: 'High School Diploma' maps to Coursework", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("High School Diploma", dp)
  expect_equal(as.character(result$degree_level), "College Coursework")
})

test_that("classify_degree: NA returns NA", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized(NA_character_, dp)
  expect_true(is.na(result$degree_level))
  expect_true(is.na(result$waiver))
})

test_that("classify_degree: empty string returns NA", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("", dp)
  expect_true(is.na(result$degree_level))
})

# ==========================================================================
# Degree area tests
# ==========================================================================

test_that("classify_degree: ECE area detection", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("Bachelor's in Early Childhood Education", dp)
  expect_equal(result$degree_area, "Early Childhood Education")
  expect_equal(result$degree_area_simple, "Early Childhood Education")
})

test_that("classify_degree: Elementary + ECE composite", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized(
    "Bachelor's in Elementary Education with Early Childhood coursework", dp
  )
  # Should detect the composite
  expect_true(result$degree_area %in% c("Elementary Education with ECE Coursework",
                                        "Elementary Education"))
})

test_that("classify_degree: Psychology consolidates to Child Development", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("B.S. in Psychology", dp)
  expect_equal(result$degree_area, "Psychology")
  expect_equal(result$degree_area_simple, "Child Development")
})

test_that("classify_degree: Family Studies consolidates to Other", {
  dp <- alprek_degree_patterns()
  result <- .classify_degree_vectorized("B.S. Family and Consumer Sciences", dp)
  expect_equal(result$degree_area_simple, "Other")
})

# ==========================================================================
# Vectorized classification test
# ==========================================================================

test_that("classify_degree works with vector input", {
  dp <- alprek_degree_patterns()
  texts <- c("Bachelor's ECE", "Master's Elementary", "CDA", "Waiver", NA)
  result <- .classify_degree_vectorized(texts, dp)
  expect_length(result$degree_level, 5)
  expect_equal(as.character(result$degree_level[1]), "Bachelor's degree")
  expect_equal(as.character(result$degree_level[4]), "Waiver")
  expect_true(is.na(result$degree_level[5]))
})

# ==========================================================================
# Race cleaning tests
# ==========================================================================

test_that("clean_race: standard mappings", {
  rm <- alprek_race_mapping()
  expect_equal(
    as.character(.clean_race("Black or African American", rm)),
    "Black"
  )
  expect_equal(
    as.character(.clean_race("Mixed Heritage", rm)),
    "Mixed"
  )
  expect_equal(
    as.character(.clean_race("Latino", rm)),
    "Latino/Hispanic"
  )
  expect_equal(
    as.character(.clean_race("Filipino", rm)),
    "Asian"
  )
})

test_that("clean_race: NA handling", {
  rm <- alprek_race_mapping()
  expect_true(is.na(.clean_race(NA_character_, rm)))
  expect_true(is.na(.clean_race("", rm)))
})

# ==========================================================================
# Ethnicity cleaning tests
# ==========================================================================

test_that("clean_ethnicity: standard values", {
  expect_equal(as.character(.clean_ethnicity("Hispanic")), "Hispanic")
  expect_equal(as.character(.clean_ethnicity("Non-Hispanic")), "Non-Hispanic")
})

test_that("clean_ethnicity: NA handling", {
  expect_true(is.na(.clean_ethnicity(NA_character_)))
})

# ==========================================================================
# Gender cleaning tests
# ==========================================================================

test_that("clean_gender: standard values", {
  expect_equal(as.character(.clean_gender("Female")), "Female")
  expect_equal(as.character(.clean_gender("Male")), "Male")
})

test_that("clean_gender: first letter matching", {
  expect_equal(as.character(.clean_gender("F")), "Female")
  expect_equal(as.character(.clean_gender("M")), "Male")
})

# ==========================================================================
# Name concatenation tests
# ==========================================================================

test_that("concat_name: standard case", {
  expect_equal(.concat_name("John", "Doe"), "John Doe")
})

test_that("concat_name: NA NA returns NA", {
  expect_true(is.na(.concat_name(NA, NA)))
  expect_true(is.na(.concat_name("NA", "NA")))
})

test_that("concat_name: one NA returns other", {
  expect_equal(.concat_name(NA, "Doe"), "Doe")
  expect_equal(.concat_name("John", NA), "John")
})

# ==========================================================================
# Emergency certification tests
# ==========================================================================

test_that("derive_emergency: emergency keyword", {
  expect_equal(.derive_emergency("emergency-12345"), 1L)
  expect_equal(.derive_emergency("EMERGENCY"), 1L)
})

test_that("derive_emergency: normal cert number", {
  expect_equal(.derive_emergency("12345"), 0L)
  expect_equal(.derive_emergency("LT123456"), 0L)
})

test_that("derive_emergency: NA returns NA", {
  expect_true(is.na(.derive_emergency(NA_character_)))
})

# ==========================================================================
# Language cleaning tests
# ==========================================================================

test_that("clean_fluent_language: null variants return NA", {
  lm <- alprek_language_mapping()
  expect_true(is.na(.clean_fluent_language("N/A", lm)))
  expect_true(is.na(.clean_fluent_language("no", lm)))
  expect_true(is.na(.clean_fluent_language("None", lm)))
  expect_true(is.na(.clean_fluent_language("English", lm)))
})

test_that("clean_fluent_language: valid languages standardized", {
  lm <- alprek_language_mapping()
  expect_equal(.clean_fluent_language("spanish", lm), "Spanish")
  expect_equal(.clean_fluent_language("Spanish", lm), "Spanish")
})

# ==========================================================================
# Title I cleaning test
# ==========================================================================

test_that("title_i cleaned correctly in admin vars", {
  df <- tibble::tibble(
    title_i = c("Y", "N", "NA", NA_character_, ""),
    region = c(1, 2, 3, 4, 5)
  )
  result <- .clean_administrative_vars(df)
  expect_equal(result$title_i_numeric[1], 1)
  expect_equal(result$title_i_numeric[2], 0)
  expect_true(is.na(result$title_i_numeric[3]))
  expect_true(is.na(result$title_i_numeric[4]))
  expect_true(is.na(result$title_i_numeric[5]))
})

# ==========================================================================
# Full classroom_clean pipeline test
# ==========================================================================

test_that("classroom_clean works on legacy fixture", {
  raw <- make_classroom_raw("legacy", 5)
  clean <- classroom_clean(raw)

  expect_s3_class(clean, "alprek_classroom_clean")
  expect_equal(clean$meta$school_year, "2023-2024")
  expect_equal(clean$meta$format, "legacy")
  expect_equal(clean$meta$n_classrooms, 5)
  expect_true(is.data.frame(clean$data))

  # Should have standard columns
  expect_true("classroom_code" %in% names(clean$data))
  expect_true("delivery_type" %in% names(clean$data))
  expect_true("lead_tch_degree_level" %in% names(clean$data))
  expect_true("lead_tch_race" %in% names(clean$data))
})

test_that("classroom_clean works on new fixture", {
  raw <- make_classroom_raw("new", 5)
  clean <- classroom_clean(raw)

  expect_s3_class(clean, "alprek_classroom_clean")
  expect_equal(clean$meta$format, "new")

  # New format should have seat_count column
  expect_true("seat_count" %in% names(clean$data))
})

test_that("classroom_clean excludes DOB by default", {
  raw <- make_classroom_raw("new", 3)
  clean <- classroom_clean(raw, include_dob = FALSE)

  dob_cols <- grep("_dob$|_username$", names(clean$data), value = TRUE)
  expect_length(dob_cols, 0)
})

test_that("classroom_clean includes DOB when requested", {
  raw <- make_classroom_raw("new", 3)
  clean <- classroom_clean(raw, include_dob = TRUE)

  dob_cols <- grep("_dob$", names(clean$data), value = TRUE)
  expect_gt(length(dob_cols), 0)
})

test_that("classroom_clean has degree_audit", {
  raw <- make_classroom_raw("legacy", 5)
  clean <- classroom_clean(raw)
  expect_true(is.data.frame(clean$degree_audit))
})

test_that("delivery_type is a factor with 7 levels", {
  raw <- make_classroom_raw("legacy", 10)
  clean <- classroom_clean(raw)
  expect_true(is.factor(clean$data$delivery_type))
  expect_equal(nlevels(clean$data$delivery_type), 7)
})

test_that("print method for alprek_classroom_clean works", {
  raw <- make_classroom_raw("legacy", 3)
  clean <- classroom_clean(raw)
  expect_output(print(clean), "alprek_classroom_clean")
  expect_output(print(clean), "2023-2024")
})

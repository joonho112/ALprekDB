test_that("budget_clean requires alprek_budget_raw input", {
  expect_error(
    budget_clean(data.frame(x = 1)),
    "alprek_budget_raw"
  )
})

test_that("budget_clean step message interpolates format type", {
  raw <- make_budget_raw("legacy")
  messages <- character()

  withCallingHandlers(
    budget_clean(raw),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  text <- paste(messages, collapse = "\n")
  expect_match(text, "Cleaning legacy format budget data")
  expect_false(grepl("\\{format_type\\}", text))
})

test_that("budget amount parsing handles currency text without warnings", {
  expect_no_warning({
    parsed <- .parse_budget_amount(c("$1,234", "1,234", "", NA, "N/A",
                                     "not available", "(1,234)"))
  })
  expect_equal(parsed, c(1234, 1234, NA, NA, NA, NA, -1234))
})

test_that("legacy budget_clean handles mixed numeric and currency amount columns", {
  raw <- make_budget_raw("legacy", n = 2)
  raw$data[["Lead Teacher Salary From OSR Funds"]] <- c("$1,234", "1,250")
  raw$data[["Lead Teacher Salary From Additional Funds 1"]] <- c("$100", "N/A")
  raw$data[["Total From OSR Funds"]] <- c("$1,234", "1,250")
  raw$data[["Total From Additional Sources"]] <- c("$100", "N/A")
  raw$data[["Grand Total"]] <- c("$1,334", "1,250")

  expect_no_warning(cleaned <- budget_clean(raw))

  lead_osr <- cleaned$long[
    cleaned$long$category_detail == "Lead Teacher Salary" &
      cleaned$long$source_type == "osr",
  ]
  expect_true(1234 %in% lead_osr$amount)
})

test_that("new budget_clean handles mixed numeric and currency amount columns", {
  raw <- make_budget_raw("new", n = 2)
  raw$data[["Lead Teacher Salary OSR"]] <- c("$1,234", "1,250")
  raw$data[["Lead Teacher Salary Other"]] <- c("$100", "N/A")
  raw$data[["Total"]] <- c("$1,334", "1,250")
  raw$data[["Total Federal, Local, and Other Funds used for Other Funds Expenditures"]] <- c("$100", "N/A")

  expect_no_warning(cleaned <- budget_clean(raw))

  lead_osr <- cleaned$long[
    cleaned$long$category_detail == "Lead Teacher Salary" &
      cleaned$long$source_type == "osr",
  ]
  expect_true(1234 %in% lead_osr$amount)
})

test_that("legacy format produces correct long-format structure", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)

  expect_s3_class(cleaned, "alprek_budget_long")
  expect_true(all(c("long", "totals", "meta") %in% names(cleaned)))

  long <- cleaned$long
  expected_cols <- c("school_year", "year", "classroom_code", "classroom_name",
                     "category_detail", "category_group", "source_type",
                     "source_detail", "amount",
                     "county_code", "delivery_type_code", "delivery_type",
                     "program_code", "class_num")
  expect_true(all(expected_cols %in% names(long)))
})

test_that("new format produces correct long-format structure", {
  raw <- make_budget_raw("new")
  cleaned <- budget_clean(raw)

  expect_s3_class(cleaned, "alprek_budget_long")
  long <- cleaned$long
  expect_true("category_group" %in% names(long))
  expect_true("source_type" %in% names(long))
})

test_that("source_type is always osr or other", {
  for (fmt in c("legacy", "new")) {
    raw <- make_budget_raw(fmt)
    cleaned <- budget_clean(raw)
    expect_true(all(cleaned$long$source_type %in% c("osr", "other")))
  }
})

test_that("amounts are numeric and non-NA", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  expect_true(is.numeric(cleaned$long$amount))
  expect_false(any(is.na(cleaned$long$amount)))
})

test_that("classroom code components are correctly parsed", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  long <- cleaned$long

  # All delivery types should be valid
  valid_types <- c("Public School", "Private Child Care", "Head Start",
                   "Community Organization", "Faith-Based Organization",
                   "University Operated", "Private School")
  non_na <- long$delivery_type[!is.na(long$delivery_type)]
  expect_true(all(non_na %in% valid_types))
})

test_that("totals table has reconciliation columns", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)

  expect_true("flag_total_mismatch" %in% names(cleaned$totals))
  expect_true(is.logical(cleaned$totals$flag_total_mismatch))
})

test_that("meta contains expected fields", {
  raw <- make_budget_raw("new")
  cleaned <- budget_clean(raw)

  expect_equal(cleaned$meta$format, "new")
  expect_equal(cleaned$meta$school_year, "2024-2025")
  expect_true(cleaned$meta$n_classrooms > 0)
  expect_true(cleaned$meta$n_long_rows > 0)
})

test_that("zero-amount additional funds rows are excluded in legacy", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  other_rows <- cleaned$long[cleaned$long$source_type == "other", ]
  if (nrow(other_rows) > 0) {
    expect_true(all(other_rows$amount != 0))
  }
})

test_that("category_group fallback works for unmapped categories", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  # All rows should have a category_group (either from codebook or "unmapped")
  expect_false(any(is.na(cleaned$long$category_group)))
})

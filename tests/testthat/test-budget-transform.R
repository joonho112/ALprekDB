test_that("budget_transform requires alprek_budget_long input", {
  expect_error(
    budget_transform(data.frame(x = 1)),
    "alprek_budget_long"
  )
})

test_that("transform produces one row per classroom", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  expect_s3_class(master, "alprek_budget_master")
  expect_equal(nrow(master$data), 5)  # n=5 fixtures
})

test_that("derived total columns sum correctly", {
  raw <- make_budget_raw("new")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  d <- master$data
  # total_{cat} should equal osr_{cat} + other_{cat}
  expect_equal(
    d$total_lead_teacher_salary,
    d$osr_lead_teacher_salary + d$other_lead_teacher_salary
  )
  expect_equal(
    d$total_instructional_support,
    d$osr_instructional_support + d$other_instructional_support
  )
})

test_that("grand_total equals sum of osr_total and other_total", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  d <- master$data
  expect_equal(d$grand_total, d$osr_total + d$other_total, tolerance = 1)
})

test_that("shares sum to approximately 100%", {
  raw <- make_budget_raw("new")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  d <- master$data
  share_sum <- d$share_osr + d$share_other
  expect_true(all(abs(share_sum - 100) < 0.01 | d$grand_total == 0))
})

test_that("delivery type groups are correct", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  d <- master$data
  expect_true("delivery_type_binary" %in% names(d))
  expect_true("delivery_type_3way" %in% names(d))
  expect_true(all(d$delivery_type_binary %in%
                    c("Public School", "Non-Public Provider", NA)))
})

test_that("zero-budget classrooms get share = 0, not NaN", {
  raw <- make_budget_raw("new")
  # Force a zero-budget classroom
  raw$data[1, grep("Salary|Benefits|Support|Maintenance|Equipment|Admin",
                   names(raw$data))] <- 0
  raw$data$Total[1] <- 0

  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  # Should not produce NaN
  expect_false(any(is.nan(master$data$share_osr)))
})

test_that("payroll allocation adjusts benefits in legacy format", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)

  # With allocation: payroll taxes are allocated to benefits and removed
  master_alloc <- budget_transform(cleaned, allocate_payroll = TRUE)
  expect_false("osr_payroll_taxes" %in% names(master_alloc$data))

  # Without allocation: payroll taxes remain as separate column
  master_noalloc <- budget_transform(cleaned, allocate_payroll = FALSE)

  # Allocated version should have higher benefits than non-allocated
  expect_true(all(
    master_alloc$data$total_lead_teacher_benefits >=
      master_noalloc$data$total_lead_teacher_benefits
  ))
})

test_that("meta contains expected fields", {
  raw <- make_budget_raw("legacy")
  cleaned <- budget_clean(raw)
  master <- budget_transform(cleaned)

  expect_true(master$meta$has_payroll_allocation)
  expect_true(master$meta$has_shares)
  expect_true(master$meta$has_groups)
})

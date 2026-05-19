# test-linkage-master.R
# Tests for linkage_create_master(), linkage_validate(), linkage_summary_stats()

test_that("linkage_create_master requires correct S3 classes", {
  fixtures <- make_linkage_fixtures()
  expect_error(linkage_create_master("bad", fixtures$classroom_panel, fixtures$student_panel))
  expect_error(linkage_create_master(fixtures$budget_panel, "bad", fixtures$student_panel))
  expect_error(linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel, "bad"))
})

test_that("linkage_create_master returns alprek_linkage_master", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  expect_s3_class(master, "alprek_linkage_master")
  expect_true(is.data.frame(master$classroom_level))
  expect_true(is.data.frame(master$student_level))
  expect_true(is.list(master$diagnostics))
  expect_true(is.list(master$meta))
})

test_that("linkage_create_master classroom-level has correct rows", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  # Classroom-level should have same rows as classroom panel
  expect_equal(nrow(master$classroom_level), nrow(fixtures$classroom_panel$data))
})

test_that("linkage_create_master student-level has correct rows", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  # Student-level should have same rows as student panel
  expect_equal(nrow(master$student_level), nrow(fixtures$student_panel$data))
})

test_that("linkage_create_master has per_child_budget", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  expect_true("per_child_budget" %in% names(master$classroom_level))

  # per_child_budget should be grand_total / n_children
  cl <- master$classroom_level
  has_both <- !is.na(cl$grand_total) & !is.na(cl$n_children) & cl$n_children > 0
  if (any(has_both)) {
    expected <- round(cl$grand_total[has_both] / cl$n_children[has_both], 2)
    actual <- cl$per_child_budget[has_both]
    expect_equal(actual, expected)
  }
})

test_that("linkage_create_master has student aggregate columns", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  # Student aggregates should be in classroom-level
  expect_true("n_children" %in% names(master$classroom_level))
  expect_true("pct_male" %in% names(master$classroom_level))
  expect_true("mean_days_absent" %in% names(master$classroom_level))
})

test_that("linkage_create_master student-level has budget + classroom columns", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  # Student-level should include classroom columns
  expect_true("latitude" %in% names(master$student_level))
  expect_true("longitude" %in% names(master$student_level))

  # Student-level should include budget columns (where matched)
  expect_true("grand_total" %in% names(master$student_level) ||
              "osr_total" %in% names(master$student_level))
})

test_that("linkage_create_master avoids .x/.y columns at both levels", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  cl_names <- names(master$classroom_level)
  st_names <- names(master$student_level)
  expect_false(any(grepl("\\.x$|\\.y$", cl_names)),
               info = "Classroom-level has .x/.y columns")
  expect_false(any(grepl("\\.x$|\\.y$", st_names)),
               info = "Student-level has .x/.y columns")
})

test_that("linkage_create_master records asymmetric budget coverage", {
  fixtures <- make_asymmetric_linkage_fixtures(n_classrooms = 5)
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)

  coverage <- master$meta$coverage
  expect_equal(coverage$missing_budget_years, fixtures$extra_year)
  expect_true(is.data.frame(coverage$by_year))

  extra_row <- coverage$by_year[coverage$by_year$school_year == fixtures$extra_year, ]
  expect_equal(nrow(extra_row), 1L)
  expect_false(extra_row$has_budget)
  expect_true(extra_row$has_classroom)
  expect_true(extra_row$has_student)
  expect_equal(extra_row$budget_status, "missing_budget")
  expect_equal(extra_row$n_budget_rows, 0L)

  missing_year_rows <- master$classroom_level$school_year == fixtures$extra_year
  expect_true(all(is.na(master$classroom_level$grand_total[missing_year_rows])))
  expect_true(all(is.na(master$classroom_level$per_child_budget[missing_year_rows])))

  student_missing_year_rows <- master$student_level$school_year == fixtures$extra_year
  expect_true(all(is.na(master$student_level$grand_total[student_missing_year_rows])))
  expect_true(all(is.na(master$student_level$osr_total[student_missing_year_rows])))
})

test_that("linkage_create_master has print method", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  expect_output(print(master), "alprek_linkage_master")
})


# --- linkage_validate tests ---

test_that("linkage_validate passes for clean fixture data", {
  # Use more classrooms so match rate > 95% (1 orphan out of 20 = 95%)
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  cb <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  val <- linkage_validate(cb)

  expect_s3_class(val, "alprek_linkage_validation")
  expect_true(val$passed)
  expect_equal(val$n_errors, 0L)
})

test_that("linkage_validate works for student-classroom join", {
  fixtures <- make_linkage_fixtures()
  sc <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  val <- linkage_validate(sc)

  expect_s3_class(val, "alprek_linkage_validation")
  expect_true(val$passed)
})

test_that("linkage_validate works for master object", {
  # Use more classrooms so match rate > 95%
  fixtures <- make_linkage_fixtures(n_classrooms = 20)
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  val <- linkage_validate(master)

  expect_s3_class(val, "alprek_linkage_validation")
  expect_true(val$passed)
})

test_that("linkage_validate treats missing budget years as coverage info", {
  fixtures <- make_asymmetric_linkage_fixtures(n_classrooms = 20)
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  val <- linkage_validate(master)

  expect_true(val$passed)

  checks <- val$checks
  match_rate <- checks[checks$check_name == "match_rate", ]
  budget_orphans <- checks[checks$check_name == "budget_overlap_orphans", ]
  budget_coverage <- checks[checks$check_name == "budget_missing_coverage", ]
  na_intro <- checks[checks$check_name == "na_introduced", ]
  coverage <- checks[checks$check_name == "year_coverage", ]

  expect_equal(match_rate$status, "PASS")
  expect_equal(budget_orphans$status, "WARN")
  expect_equal(budget_coverage$status, "INFO")
  expect_equal(na_intro$status, "PASS")
  expect_equal(coverage$status, "INFO")
  expect_match(coverage$details, fixtures$extra_year)
})

test_that("linkage_validate errors when student year lacks classroom coverage", {
  fixtures <- make_student_classroom_coverage_fixtures(
    n_classrooms = 20,
    extra_side = "student"
  )
  sc <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  val <- linkage_validate(sc)
  checks <- val$checks
  missing_coverage <- checks[checks$check_name == "student_classroom_missing_coverage", ]
  overlap_orphans <- checks[checks$check_name == "student_classroom_overlap_orphans", ]

  expect_false(val$passed)
  expect_equal(missing_coverage$status, "ERROR")
  expect_equal(overlap_orphans$status, "PASS")
  expect_match(missing_coverage$details, fixtures$extra_year)
})

test_that("linkage_validate warns when classroom year lacks student coverage", {
  fixtures <- make_student_classroom_coverage_fixtures(
    n_classrooms = 20,
    extra_side = "classroom"
  )
  sc <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  val <- linkage_validate(sc)
  checks <- val$checks
  missing_coverage <- checks[checks$check_name == "student_classroom_missing_coverage", ]
  empty_classrooms <- checks[checks$check_name == "empty_classrooms", ]

  expect_true(val$passed)
  expect_equal(missing_coverage$status, "WARN")
  expect_equal(empty_classrooms$status, "INFO")
  expect_match(missing_coverage$details, fixtures$extra_year)
})

test_that("linkage_validate surfaces student-classroom overlap orphan severity", {
  fixtures <- make_linkage_fixtures(n_classrooms = 5, n_students_per = 3)
  fixtures$student_panel$data$classroom_code[1] <- "999P999999.99"
  sc <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  val <- linkage_validate(sc)
  checks <- val$checks
  overlap_orphans <- checks[checks$check_name == "student_classroom_overlap_orphans", ]

  expect_false(val$passed)
  expect_equal(overlap_orphans$status, "ERROR")
  expect_match(overlap_orphans$details, "1 student classroom code")

  master <- linkage_create_master(
    fixtures$budget_panel,
    fixtures$classroom_panel,
    fixtures$student_panel
  )
  master_val <- linkage_validate(master)
  master_overlap <- master_val$checks[
    master_val$checks$check_name == "student_classroom_overlap_orphans",
  ]

  expect_false(master_val$passed)
  expect_equal(master_overlap$status, "ERROR")
})

test_that("linkage_validate has print method", {
  fixtures <- make_linkage_fixtures()
  cb <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  val <- linkage_validate(cb)
  expect_output(print(val), "alprek_linkage_validation")
})


# --- linkage_summary_stats tests ---

test_that("linkage_summary_stats works for master object", {
  fixtures <- make_linkage_fixtures()
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  stats <- linkage_summary_stats(master)

  expect_true(is.data.frame(stats))
  expect_true("n" %in% names(stats))
})

test_that("linkage_summary_stats reports zero budget coverage for missing budget years", {
  fixtures <- make_asymmetric_linkage_fixtures(n_classrooms = 5)
  master <- linkage_create_master(fixtures$budget_panel, fixtures$classroom_panel,
                                   fixtures$student_panel)
  stats <- linkage_summary_stats(master)

  missing_year_stats <- stats[stats$school_year == fixtures$extra_year, ]
  expect_equal(nrow(missing_year_stats), 1L)
  expect_equal(missing_year_stats$pct_with_budget, 0)
  expect_true(is.na(missing_year_stats$mean_grand_total))
  expect_true(is.na(missing_year_stats$mean_per_child_budget))
  expect_false(is.nan(missing_year_stats$mean_grand_total))
  expect_false(is.nan(missing_year_stats$mean_per_child_budget))
})

test_that("linkage_summary_stats works for classroom join", {
  fixtures <- make_linkage_fixtures()
  cb <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  stats <- linkage_summary_stats(cb)

  expect_true(is.data.frame(stats))
  expect_true(nrow(stats) >= 1)
})

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

test_that("linkage_summary_stats works for classroom join", {
  fixtures <- make_linkage_fixtures()
  cb <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  stats <- linkage_summary_stats(cb)

  expect_true(is.data.frame(stats))
  expect_true(nrow(stats) >= 1)
})

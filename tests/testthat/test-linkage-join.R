# test-linkage-join.R
# Tests for linkage_classroom_budget() and linkage_student_classroom()

test_that("linkage_classroom_budget requires correct S3 classes", {
  fixtures <- make_linkage_fixtures()
  expect_error(linkage_classroom_budget("not_panel", fixtures$budget_panel))
  expect_error(linkage_classroom_budget(fixtures$classroom_panel, "not_panel"))
})

test_that("linkage_classroom_budget returns alprek_linkage_classroom", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  expect_s3_class(result, "alprek_linkage_classroom")
  expect_true(is.data.frame(result$data))
  expect_true(is.list(result$diagnostics))
  expect_true(is.list(result$meta))
})

test_that("linkage_classroom_budget preserves all classroom rows (left join)", {

  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  # Left join preserves all classroom rows

  expect_equal(nrow(result$data), nrow(fixtures$classroom_panel$data))
})

test_that("linkage_classroom_budget adds budget-only columns", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)

  # Budget-specific columns should be present
  expect_true("grand_total" %in% names(result$data))
  expect_true("osr_total" %in% names(result$data))
  expect_true("share_osr" %in% names(result$data))
})

test_that("linkage_classroom_budget detects orphan correctly", {
  fixtures <- make_linkage_fixtures()
  # First classroom has no budget, so it should be an orphan
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)

  # Should have 1 classroom orphan (first classroom has no budget)
  expect_equal(result$diagnostics$n_left_orphan, 1L)
  expect_equal(result$diagnostics$n_right_orphan, 0L)

  # The orphan classroom should have NA for budget columns
  orphan_code <- fixtures$shared_codes[1]
  orphan_row <- result$data[result$data$classroom_code == orphan_code, ]
  expect_true(is.na(orphan_row$grand_total))
})

test_that("linkage_classroom_budget avoids duplicate columns (.x/.y)", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)

  # No .x or .y suffixed columns
  col_names <- names(result$data)
  expect_false(any(grepl("\\.x$|\\.y$", col_names)),
               info = paste("Found .x/.y columns:", paste(col_names[grepl("\\.x$|\\.y$", col_names)], collapse = ", ")))
})

test_that("linkage_classroom_budget diagnostics are correct", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  d <- result$diagnostics

  expect_equal(d$join_type, "classroom_budget")
  expect_equal(d$n_left, nrow(fixtures$classroom_panel$data))
  expect_equal(d$n_right, nrow(fixtures$budget_panel$data))
  expect_equal(d$n_result_rows, nrow(fixtures$classroom_panel$data))
  expect_true(d$match_rate > 0 && d$match_rate <= 1)
})

test_that("linkage_classroom_budget has print method", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_classroom_budget(fixtures$classroom_panel, fixtures$budget_panel)
  expect_output(print(result), "alprek_linkage_classroom")
})


# --- linkage_student_classroom tests ---

test_that("linkage_student_classroom requires correct S3 classes", {
  fixtures <- make_linkage_fixtures()
  expect_error(linkage_student_classroom("not_panel", fixtures$classroom_panel))
  expect_error(linkage_student_classroom(fixtures$student_panel, "not_panel"))
})

test_that("linkage_student_classroom returns alprek_linkage_student", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  expect_s3_class(result, "alprek_linkage_student")
  expect_true(is.data.frame(result$data))
})

test_that("linkage_student_classroom preserves all student rows", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  expect_equal(nrow(result$data), nrow(fixtures$student_panel$data))
})

test_that("linkage_student_classroom adds classroom-only columns", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)

  # Classroom-specific columns should be present
  expect_true("latitude" %in% names(result$data))
  expect_true("longitude" %in% names(result$data))
  expect_true("total_grant" %in% names(result$data))
  expect_true("region" %in% names(result$data))
})

test_that("linkage_student_classroom detects empty classrooms", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)

  # Last classroom has no students
  expect_true(result$diagnostics$n_classroom_orphan >= 1)
})

test_that("linkage_student_classroom avoids duplicate columns", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)

  col_names <- names(result$data)
  expect_false(any(grepl("\\.x$|\\.y$", col_names)),
               info = paste("Found .x/.y columns:", paste(col_names[grepl("\\.x$|\\.y$", col_names)], collapse = ", ")))
})

test_that("linkage_student_classroom has print method", {
  fixtures <- make_linkage_fixtures()
  result <- linkage_student_classroom(fixtures$student_panel, fixtures$classroom_panel)
  expect_output(print(result), "alprek_linkage_student")
})

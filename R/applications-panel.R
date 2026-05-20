#' Combine Multiple Cycles into an Applications Panel
#'
#' @description Stacks two or more `alprek_applications_master` objects
#'   (one per cycle) into a longitudinal `alprek_applications_panel`. Mirrors
#'   `budget_bind_years()`. Preserves the **two-grain** structure of the
#'   applications module: per-application rows go to `$data`, per-site
#'   capacity rows go to `$capacity_data`.
#'
#'   Each cycle's `cycle_year` (from `master$meta$cycle_year`) is asserted to
#'   be present and unique across the inputs; the panel rows are sorted by
#'   `cycle_year` then by `application_id` (or `site_code` for capacity).
#'
#' @param ... `alprek_applications_master` objects to combine.
#' @param master_list Optional list of `alprek_applications_master`
#'   objects. Alternative to `...` for programmatic use.
#'
#' @return An `alprek_applications_panel` S3 list with elements:
#'   * `data`: applications-grain long panel (one row per application-cycle)
#'   * `capacity_data`: capacity-grain long panel or NULL if no inputs had capacity
#'   * `cycle_years`: sorted vector of distinct cycle_year values
#'   * `n_cycles`: number of cycles in the panel
#'   * `by_cycle`: per-cycle summary list (`cycle_year`, `n_apps`,
#'     `n_capacity`, `n_buckets`)
#'   * `meta`: `binded_at` timestamp + `tier_bands` (from first master)
#'
#' @examples
#' \dontrun{
#' mst_2526 <- applications_transform(rec_2526, cap_2526)
#' mst_2627 <- applications_transform(rec_2627, cap_2627)
#' panel <- applications_bind_years(mst_2526, mst_2627)
#' panel
#' }
#'
#' @importFrom dplyr bind_rows arrange
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang .data
#' @export
applications_bind_years <- function(..., master_list = NULL) {

  if (is.null(master_list)) master_list <- list(...)
  if (length(master_list) == 0L) {
    stop("No data to combine. Provide alprek_applications_master objects.",
         call. = FALSE)
  }

  # ---- Validate ----
  for (i in seq_along(master_list)) {
    if (!inherits(master_list[[i]], "alprek_applications_master")) {
      stop("Element ", i,
           " is not an alprek_applications_master object.", call. = FALSE)
    }
    if (is.null(master_list[[i]]$meta$cycle_year) ||
        is.na(master_list[[i]]$meta$cycle_year)) {
      stop("Element ", i, " has no cycle_year in meta.", call. = FALSE)
    }
  }

  cycle_years_in <- vapply(master_list,
                            function(m) m$meta$cycle_year, character(1))
  if (anyDuplicated(cycle_years_in)) {
    dups <- cycle_years_in[duplicated(cycle_years_in)]
    stop("Duplicate cycle_year(s) supplied: ",
         paste(unique(dups), collapse = ", "),
         ". Combine cycle versions before binding into a panel.",
         call. = FALSE)
  }

  # ---- Combine applications grain ----
  data_list <- lapply(master_list, function(m) {
    d <- m$data
    if (!"cycle_year" %in% names(d)) {
      d$cycle_year <- m$meta$cycle_year
    }
    d
  })
  combined_apps <- dplyr::bind_rows(data_list)
  if ("application_id" %in% names(combined_apps)) {
    combined_apps <- dplyr::arrange(combined_apps,
                                      .data$cycle_year, .data$application_id)
  } else {
    combined_apps <- dplyr::arrange(combined_apps, .data$cycle_year)
  }

  # ---- Combine capacity grain (optional) ----
  cap_list <- lapply(master_list, function(m) {
    if (is.null(m$capacity_data)) return(NULL)
    d <- m$capacity_data
    if (!"cycle_year" %in% names(d)) {
      d$cycle_year <- m$meta$cycle_year
    }
    d
  })
  cap_list <- Filter(Negate(is.null), cap_list)
  combined_cap <- if (length(cap_list) > 0L) {
    out <- dplyr::bind_rows(cap_list)
    if ("site_code" %in% names(out)) {
      out <- dplyr::arrange(out, .data$cycle_year, .data$site_code)
    } else {
      out <- dplyr::arrange(out, .data$cycle_year)
    }
    out
  } else NULL

  # ---- Per-cycle summary ----
  by_cycle <- lapply(master_list, function(m) {
    bk <- if ("bucket" %in% names(m$data))
              table(factor(m$data$bucket,
                             levels = c("A", "B", "C", "D", "unknown")))
          else integer(0)
	    list(
	      cycle_year = m$meta$cycle_year,
	      n_apps     = nrow(m$data),
	      n_capacity = if (is.null(m$capacity_data)) 0L else nrow(m$capacity_data),
	      n_buckets  = as.list(bk),
	      file_sha256 = m$meta$file_sha256 %||% NA_character_,
	      git_sha = m$meta$git_sha %||% NA_character_,
	      reconciled_at = m$meta$reconciled_at %||% NA_character_,
	      transformed_at = m$meta$transformed_at %||% NA_character_,
	      fuzzy_threshold = m$meta$fuzzy_threshold %||% NA_real_,
	      seed = m$meta$seed %||% NA_integer_
	    )
	  })
  names(by_cycle) <- cycle_years_in

  cycle_years_sorted <- sort(unique(cycle_years_in))

  # tier_bands from first master (should be consistent across cycles)
  tier_bands <- master_list[[1L]]$meta$tier_bands

  structure(list(
    data          = tibble::as_tibble(combined_apps),
    capacity_data = if (!is.null(combined_cap))
                       tibble::as_tibble(combined_cap) else NULL,
    cycle_years   = cycle_years_sorted,
    n_cycles      = length(cycle_years_sorted),
    by_cycle      = by_cycle,
    meta = list(
      binded_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      tier_bands = tier_bands
    )
  ), class = "alprek_applications_panel")
}


#' Print method for `alprek_applications_panel`
#' @param x An `alprek_applications_panel` object.
#' @param ... Ignored.
#' @export
print.alprek_applications_panel <- function(x, ...) {
  cat("<alprek_applications_panel>\n")
  cat("  Cycles:        ", paste(x$cycle_years, collapse = ", "), "\n", sep = "")
  cat("  Apps rows:     ", nrow(x$data), "\n", sep = "")
  if (!is.null(x$capacity_data)) {
    cat("  Capacity rows: ", nrow(x$capacity_data), "\n", sep = "")
  } else {
    cat("  Capacity rows: -\n", sep = "")
  }
  for (yr in names(x$by_cycle)) {
    info <- x$by_cycle[[yr]]
    bk <- info$n_buckets
    bk_str <- if (length(bk) > 0L)
                paste(sprintf("%s=%d", names(bk), as.integer(unlist(bk))),
                       collapse = " ")
              else "-"
    cat("    ", yr, ": ", info$n_apps, " apps, ",
        info$n_capacity, " cap, buckets [", bk_str, "]\n", sep = "")
  }
  cat("  Binded at:     ", x$meta$binded_at, "\n", sep = "")
  invisible(x)
}


#' Track Classroom Presence Across Application Cycles
#'
#' @description For each unique classroom (by `matched_classroom_code` for
#'   bucket A/B/C, by composite key for D), reports which cycles it applied
#'   in. Mirrors `budget_track_classrooms()`.
#'
#' @param panel An `alprek_applications_panel` object.
#' @return A tibble with one row per classroom + logical columns per cycle +
#'   `n_cycles_present`, `all_cycles`, `first_cycle`, `last_cycle`.
#'
#' @importFrom dplyr distinct mutate group_by summarise across n_distinct
#' @importFrom tidyr pivot_wider replace_na
#' @export
applications_track_classrooms <- function(panel) {
  if (!inherits(panel, "alprek_applications_panel")) {
    stop("Expected an alprek_applications_panel object.", call. = FALSE)
  }

  d <- panel$data
  # Build classroom key: prefer matched_classroom_code; fall back to a hash
  # of organization_name+project_name+county for bucket D rows.
  if ("matched_classroom_code" %in% names(d)) {
    key <- ifelse(!is.na(d$matched_classroom_code),
                   d$matched_classroom_code,
                   sprintf("NEW::%s::%s::%s",
                            d$organization_name %||% "",
                            d$project_name %||% "",
                            d$county %||% ""))
  } else {
    key <- d$application_id
  }

  presence <- tibble::tibble(
    classroom_key = key,
    cycle_year    = d$cycle_year,
    bucket        = d$bucket %||% NA_character_
  )

  wide <- presence |>
    dplyr::distinct(.data$classroom_key, .data$cycle_year) |>
    dplyr::mutate(present = TRUE) |>
    tidyr::pivot_wider(names_from = "cycle_year",
                         values_from = "present",
                         values_fill = FALSE)

  year_cols <- intersect(panel$cycle_years, names(wide))
  wide$n_cycles_present <- rowSums(wide[year_cols])
  wide$all_cycles       <- wide$n_cycles_present == panel$n_cycles

  # First / last cycle present
  wide$first_cycle <- apply(wide[year_cols], 1L, function(rr) {
    yrs <- year_cols[as.logical(rr)]
    if (length(yrs) > 0L) min(yrs) else NA_character_
  })
  wide$last_cycle <- apply(wide[year_cols], 1L, function(rr) {
    yrs <- year_cols[as.logical(rr)]
    if (length(yrs) > 0L) max(yrs) else NA_character_
  })

  wide
}

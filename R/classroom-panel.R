#' Combine Multiple Years into a Classroom Panel Dataset
#'
#' @description Binds multiple single-year `alprek_classroom_clean` objects into
#'   a longitudinal panel dataset. Applies forward-fill imputation for geographic
#'   coordinates and year_first_funded within site/classroom groups.
#'
#' @param ... `alprek_classroom_clean` objects to combine.
#' @param clean_list Optional list of `alprek_classroom_clean` objects.
#'   Alternative to `...` for programmatic use.
#'
#' @return An `alprek_classroom_panel` S3 object (list) with elements:
#'   - `data`: tibble of combined panel data.
#'   - `years`: character vector of school years included.
#'   - `n_total`: total number of classroom-year observations.
#'   - `by_year`: list with per-year metadata.
#'   - `imputation_log`: tibble of imputed values.
#'
#' @examples
#' \dontrun{
#' panel <- classroom_bind_years(clean_2122, clean_2223, clean_2324)
#' panel$data
#' panel$imputation_log
#' }
#'
#' @importFrom dplyr bind_rows arrange group_by ungroup mutate
#' @export
classroom_bind_years <- function(..., clean_list = NULL) {
  if (is.null(clean_list)) {
    clean_list <- list(...)
  }

  if (length(clean_list) == 0) {
    stop("No data to combine. Provide alprek_classroom_clean objects.", call. = FALSE)
  }

  # Validate inputs
  for (i in seq_along(clean_list)) {
    if (!inherits(clean_list[[i]], "alprek_classroom_clean")) {
      stop("Element ", i, " is not an alprek_classroom_clean object.", call. = FALSE)
    }
  }

  msg_info("Building classroom panel from {length(clean_list)} year(s)")

  # Combine data
  combined <- dplyr::bind_rows(lapply(clean_list, function(m) m$data))
  combined <- combined |>
    dplyr::arrange(.data$school_year, .data$classroom_code)

  # Apply imputation
  impute_result <- .impute_classroom_panel(combined)
  combined <- impute_result$data
  imputation_log <- impute_result$log

  if (nrow(imputation_log) > 0) {
    msg_info("Imputed {nrow(imputation_log)} value(s) via forward-fill")
  }

  years <- sort(unique(combined$school_year))
  by_year <- lapply(clean_list, function(m) {
    list(
      school_year = m$meta$school_year,
      format = m$meta$format,
      n_classrooms = m$meta$n_classrooms
    )
  })
  names(by_year) <- sapply(clean_list, function(m) m$meta$school_year)

  result <- structure(
    list(
      data = combined,
      years = years,
      n_total = nrow(combined),
      by_year = by_year,
      imputation_log = imputation_log
    ),
    class = "alprek_classroom_panel"
  )

  msg_success("Panel: {nrow(combined)} classroom-years across {length(years)} year(s)")
  result
}


#' Print method for alprek_classroom_panel
#' @param x An alprek_classroom_panel object.
#' @param ... Ignored.
#' @export
print.alprek_classroom_panel <- function(x, ...) {
  cat("<alprek_classroom_panel>\n")
  cat("  Years:", paste(x$years, collapse = ", "), "\n")
  cat("  Total observations:", x$n_total, "\n")
  cat("  Columns:", ncol(x$data), "\n")
  for (yr in names(x$by_year)) {
    info <- x$by_year[[yr]]
    cat("    ", yr, ":", info$n_classrooms, "classrooms (",
        info$format, ")\n")
  }
  if (nrow(x$imputation_log) > 0) {
    cat("  Imputed values:", nrow(x$imputation_log), "\n")
  }
  invisible(x)
}


#' Track Classroom Presence Across Years
#'
#' @description Creates a binary matrix showing which classrooms are present
#'   in which school years.
#'
#' @param panel An `alprek_classroom_panel` object.
#'
#' @return A tibble with classroom_code and one logical column per school year.
#'
#' @examples
#' \dontrun{
#' panel <- classroom_bind_years(clean_list = list(c1, c2, c3))
#' track <- classroom_track(panel)
#' # Count classrooms present in all years
#' sum(rowSums(track[, -1]) == ncol(track) - 1)
#' }
#'
#' @importFrom tidyr pivot_wider
#' @export
classroom_track <- function(panel) {
  if (!inherits(panel, "alprek_classroom_panel")) {
    stop("Expected an 'alprek_classroom_panel' object.", call. = FALSE)
  }

  df <- panel$data
  presence <- unique(df[, c("classroom_code", "school_year")])
  presence$present <- TRUE

  wide <- tidyr::pivot_wider(
    presence,
    names_from = "school_year",
    values_from = "present",
    values_fill = FALSE
  )

  # Sort columns by year
  year_cols <- setdiff(names(wide), "classroom_code")
  year_cols <- sort(year_cols)
  wide[, c("classroom_code", year_cols)]
}


#' Summary Statistics for Classroom Data
#'
#' @description Computes summary statistics for classroom data, either
#'   by school year (default) or overall.
#'
#' @param x An `alprek_classroom_clean` or `alprek_classroom_panel` object.
#' @param by Character. Grouping variable. Default `"school_year"`.
#'   Use `NULL` for overall statistics.
#'
#' @return A tibble of summary statistics.
#'
#' @importFrom dplyr group_by summarise n across
#' @export
classroom_summary_stats <- function(x, by = "school_year") {
  if (inherits(x, "alprek_classroom_clean")) {
    df <- x$data
  } else if (inherits(x, "alprek_classroom_panel")) {
    df <- x$data
  } else {
    stop("Expected alprek_classroom_clean or alprek_classroom_panel.", call. = FALSE)
  }

  if (!is.null(by) && by %in% names(df)) {
    stats <- df |>
      dplyr::group_by(.data[[by]]) |>
      dplyr::summarise(
        n_classrooms = dplyr::n(),
        mean_total_grant = if ("total_grant" %in% names(df)) mean(.data$total_grant, na.rm = TRUE) else NA_real_,
        median_total_grant = if ("total_grant" %in% names(df)) stats::median(.data$total_grant, na.rm = TRUE) else NA_real_,
        pct_public_school = if ("delivery_type" %in% names(df)) {
          mean(as.character(.data$delivery_type) == "Public School", na.rm = TRUE) * 100
        } else NA_real_,
        lead_degree_coverage = if ("lead_tch_degree_level" %in% names(df)) {
          mean(!is.na(.data$lead_tch_degree_level), na.rm = TRUE) * 100
        } else NA_real_,
        .groups = "drop"
      )
  } else {
    stats <- df |>
      dplyr::summarise(
        n_classrooms = dplyr::n(),
        mean_total_grant = if ("total_grant" %in% names(df)) mean(.data$total_grant, na.rm = TRUE) else NA_real_,
        median_total_grant = if ("total_grant" %in% names(df)) stats::median(.data$total_grant, na.rm = TRUE) else NA_real_
      )
  }

  stats
}


# ==========================================================================
# Internal helpers
# ==========================================================================

#' Forward-fill imputation for panel data
#' @return List with `data` and `log` (tibble of imputed values).
#' @keywords internal
.impute_classroom_panel <- function(df) {
  log_entries <- list()

  # 1. Latitude/Longitude: fill within site_code groups
  if (all(c("site_code", "latitude", "longitude") %in% names(df))) {
    df <- df |>
      dplyr::group_by(.data$site_code) |>
      dplyr::mutate(
        .lat_before = .data$latitude,
        .lon_before = .data$longitude,
        latitude = .fill_within_group(.data$latitude),
        longitude = .fill_within_group(.data$longitude)
      ) |>
      dplyr::ungroup()

    # Log imputed lat values
    lat_imputed <- which(is.na(df$.lat_before) & !is.na(df$latitude))
    if (length(lat_imputed) > 0) {
      for (idx in lat_imputed) {
        log_entries[[length(log_entries) + 1]] <- tibble::tibble(
          classroom_code = df$classroom_code[idx],
          school_year = df$school_year[idx],
          variable = "latitude",
          imputed_value = as.character(df$latitude[idx]),
          method = "forward_fill_site"
        )
      }
    }
    lon_imputed <- which(is.na(df$.lon_before) & !is.na(df$longitude))
    if (length(lon_imputed) > 0) {
      for (idx in lon_imputed) {
        log_entries[[length(log_entries) + 1]] <- tibble::tibble(
          classroom_code = df$classroom_code[idx],
          school_year = df$school_year[idx],
          variable = "longitude",
          imputed_value = as.character(df$longitude[idx]),
          method = "forward_fill_site"
        )
      }
    }

    df$.lat_before <- NULL
    df$.lon_before <- NULL
  }

  # 2. Year first funded: fill within classroom_code groups
  if ("year_first_funded" %in% names(df)) {
    df <- df |>
      dplyr::group_by(.data$classroom_code) |>
      dplyr::mutate(
        .yff_before = .data$year_first_funded,
        year_first_funded = .fill_within_group(.data$year_first_funded)
      ) |>
      dplyr::ungroup()

    yff_imputed <- which(is.na(df$.yff_before) & !is.na(df$year_first_funded))
    if (length(yff_imputed) > 0) {
      for (idx in yff_imputed) {
        log_entries[[length(log_entries) + 1]] <- tibble::tibble(
          classroom_code = df$classroom_code[idx],
          school_year = df$school_year[idx],
          variable = "year_first_funded",
          imputed_value = as.character(df$year_first_funded[idx]),
          method = "forward_fill_classroom"
        )
      }
    }

    df$.yff_before <- NULL
  }

  imputation_log <- if (length(log_entries) > 0) {
    dplyr::bind_rows(log_entries)
  } else {
    tibble::tibble(
      classroom_code = character(),
      school_year = character(),
      variable = character(),
      imputed_value = character(),
      method = character()
    )
  }

  list(data = df, log = imputation_log)
}


#' Fill NA values within a group using non-NA values from the same group
#' @keywords internal
.fill_within_group <- function(x) {
  non_na <- x[!is.na(x)]
  if (length(non_na) > 0) {
    # Use the most recent non-NA value (last non-NA)
    x[is.na(x)] <- non_na[length(non_na)]
  }
  x
}

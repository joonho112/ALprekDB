#' Derive Advanced Analysis Variables for Student Data
#'
#' @description Computes derived variables for research analysis, including
#'   GOLD assessment gain scores, K-readiness transition indicators, chronic
#'   absence flags, service density indices, and eDECA pre-post gains.
#'
#' @param clean_obj An `alprek_student_clean` or `alprek_student_panel` object.
#' @param add_gold_gains Logical. Compute GOLD fall-to-spring gain scores
#'   (6 domains x raw + scale = 12 columns)? Default `TRUE`.
#' @param add_kready_transitions Logical. Compute K-readiness transition
#'   indicators (6 domains)? Default `TRUE`.
#' @param add_chronic_absence Logical. Compute chronic absence flag and
#'   percentage? Default `TRUE`.
#' @param add_service_density Logical. Compute service count and risk index?
#'   Default `TRUE`.
#' @param add_edeca_gains Logical. Compute eDECA pre-post T-score gains
#'   (5 constructs)? Default `TRUE`.
#' @param chronic_absence_threshold Numeric. Days absent threshold for chronic
#'   absence flag. Default `18` (~10% of 180 school days).
#' @param school_days Numeric. Total school days for absence percentage
#'   calculation. Default `180`.
#'
#' @return The same S3 class as the input object, with additional derived
#'   columns in `$data`. A `$transform_log` element is added to track which
#'   variables were created.
#'
#' @details
#' This function is designed to be called **after** [student_clean()] and
#' optionally **before** [student_bind_years()]. It does not modify the S3
#' class, so downstream functions (export, validate, linkage) continue to
#' work without modification.
#'
#' The function is idempotent: running it twice produces the same result
#' (existing derived columns are overwritten).
#'
#' Source columns that do not exist in the data are silently skipped with
#' an informational message.
#'
#' @section Derived Variables:
#' \describe{
#'   \item{GOLD Gains (12 cols)}{`gold_[domain]_gain_raw` and
#'     `gold_[domain]_gain_scale` for 6 domains: literacy, math, se,
#'     physical, cognitive, language. Computed as spring - fall.}
#'   \item{K-Readiness Transitions (6 cols)}{`gold_[domain]_kready_improved`:
#'     1 if fall = Emerging and spring = Accomplished, else 0.}
#'   \item{Chronic Absence (2 cols)}{`chronic_absence` (0/1 flag) and
#'     `chronic_absence_pct` (days_absent_total / school_days * 100).}
#'   \item{Service Density (2 cols)}{`n_services` (count of binary service
#'     indicators) and `risk_index` (count of risk indicators).}
#'   \item{eDECA Gains (5 cols)}{`edeca_[construct]_gain`: post T-score
#'     minus pre T-score for initiative, self_reg, attachment, tpf, behavior.}
#' }
#'
#' @examples
#' \dontrun{
#' clean <- student_clean(student_read("data/FCPK Student Details 23-24.xlsx"))
#' enriched <- student_transform(clean)
#' # Or with panel:
#' panel <- student_process_years(configs)$panel
#' enriched_panel <- student_transform(panel)
#' }
#'
#' @export
student_transform <- function(clean_obj,
                               add_gold_gains = TRUE,
                               add_kready_transitions = TRUE,
                               add_chronic_absence = TRUE,
                               add_service_density = TRUE,
                               add_edeca_gains = TRUE,
                               chronic_absence_threshold = 18,
                               school_days = 180) {

  # --- Input validation ---
  is_clean <- inherits(clean_obj, "alprek_student_clean")
  is_panel <- inherits(clean_obj, "alprek_student_panel")

  if (!is_clean && !is_panel) {
    stop("Expected an 'alprek_student_clean' or 'alprek_student_panel' object.",
         call. = FALSE)
  }

  df <- clean_obj$data
  n_before <- ncol(df)
  log <- list(
    gold_gains_added = 0L,
    kready_transitions_added = 0L,
    chronic_absence_added = 0L,
    service_density_added = 0L,
    edeca_gains_added = 0L,
    n_cols_added = 0L,
    skipped = character()
  )

  msg_info("Deriving advanced analysis variables for {nrow(df)} students")

  # --- Category 1: GOLD Gain Scores ---
  if (add_gold_gains) {
    result <- .derive_gold_gains(df)
    df <- result$df
    log$gold_gains_added <- result$n_added
    if (length(result$skipped) > 0) {
      log$skipped <- c(log$skipped, result$skipped)
    }
  }

  # --- Category 2: K-Readiness Transitions ---
  if (add_kready_transitions) {
    result <- .derive_kready_transitions(df)
    df <- result$df
    log$kready_transitions_added <- result$n_added
    if (length(result$skipped) > 0) {
      log$skipped <- c(log$skipped, result$skipped)
    }
  }

  # --- Category 3: Chronic Absence ---
  if (add_chronic_absence) {
    result <- .derive_chronic_absence(df, chronic_absence_threshold, school_days)
    df <- result$df
    log$chronic_absence_added <- result$n_added
    if (length(result$skipped) > 0) {
      log$skipped <- c(log$skipped, result$skipped)
    }
  }

  # --- Category 4: Service Density ---
  if (add_service_density) {
    result <- .derive_service_density(df)
    df <- result$df
    log$service_density_added <- result$n_added
    if (length(result$skipped) > 0) {
      log$skipped <- c(log$skipped, result$skipped)
    }
  }

  # --- Category 5: eDECA Gains ---
  if (add_edeca_gains) {
    result <- .derive_edeca_gains(df)
    df <- result$df
    log$edeca_gains_added <- result$n_added
    if (length(result$skipped) > 0) {
      log$skipped <- c(log$skipped, result$skipped)
    }
  }

  log$n_cols_added <- ncol(df) - n_before

  # --- Update object ---
  clean_obj$data <- df
  clean_obj$meta$transformed_at <- Sys.time()
  clean_obj$meta$n_cols <- ncol(df)
  clean_obj$transform_log <- log

  total_added <- log$gold_gains_added + log$kready_transitions_added +
    log$chronic_absence_added + log$service_density_added +
    log$edeca_gains_added
  msg_success("Added {total_added} derived columns ({ncol(df)} total columns)")

  if (length(log$skipped) > 0) {
    msg_info("Skipped {length(log$skipped)} domain(s) due to missing source columns")
  }

  clean_obj
}


# ===========================================================================
# Internal Helpers
# ===========================================================================

#' Derive GOLD Assessment Gain Scores
#' @noRd
.derive_gold_gains <- function(df) {
  domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  score_types <- c("raw", "scale")
  n_added <- 0L
  skipped <- character()

  for (domain in domains) {
    for (stype in score_types) {
      fall_col <- paste0("gold_", domain, "_fall_", stype)
      spring_col <- paste0("gold_", domain, "_spring_", stype)
      gain_col <- paste0("gold_", domain, "_gain_", stype)

      if (fall_col %in% names(df) && spring_col %in% names(df)) {
        df[[gain_col]] <- ifelse(
          !is.na(df[[fall_col]]) & !is.na(df[[spring_col]]),
          df[[spring_col]] - df[[fall_col]],
          NA_real_
        )
        n_added <- n_added + 1L
      } else {
        skipped <- c(skipped, paste0("gold_", domain, "_gain_", stype))
      }
    }
  }

  list(df = df, n_added = n_added, skipped = skipped)
}


#' Derive K-Readiness Transition Indicators
#' @noRd
.derive_kready_transitions <- function(df) {
  domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  n_added <- 0L
  skipped <- character()

  for (domain in domains) {
    fall_col <- paste0("gold_", domain, "_fall_kready")
    spring_col <- paste0("gold_", domain, "_spring_kready")
    improved_col <- paste0("gold_", domain, "_kready_improved")

    if (fall_col %in% names(df) && spring_col %in% names(df)) {
      fall_vals <- as.character(df[[fall_col]])
      spring_vals <- as.character(df[[spring_col]])

      df[[improved_col]] <- ifelse(
        !is.na(fall_vals) & !is.na(spring_vals),
        as.integer(fall_vals == "Emerging" & spring_vals == "Accomplished"),
        NA_integer_
      )
      n_added <- n_added + 1L
    } else {
      skipped <- c(skipped, improved_col)
    }
  }

  list(df = df, n_added = n_added, skipped = skipped)
}


#' Derive Chronic Absence Indicators
#' @noRd
.derive_chronic_absence <- function(df, threshold, school_days) {
  n_added <- 0L
  skipped <- character()


  if ("days_absent_total" %in% names(df)) {
    df$chronic_absence <- ifelse(
      !is.na(df$days_absent_total),
      as.integer(df$days_absent_total >= threshold),
      NA_integer_
    )
    df$chronic_absence_pct <- ifelse(
      !is.na(df$days_absent_total),
      round(df$days_absent_total / school_days * 100, 2),
      NA_real_
    )
    n_added <- 2L
  } else {
    skipped <- c("chronic_absence", "chronic_absence_pct")
  }

  list(df = df, n_added = n_added, skipped = skipped)
}


#' Derive Service Density and Risk Index
#' @noRd
.derive_service_density <- function(df) {
  n_added <- 0L
  skipped <- character()

  # Service count: sum of available binary service indicators

  service_cols <- c("childcare_subsidy", "tanf", "wic", "free_reduced_lunch",
                    "snap", "foster_care", "homeless", "head_start_dum",
                    "center_care_dum")
  available_svc <- intersect(service_cols, names(df))

  if (length(available_svc) > 0) {
    svc_matrix <- as.matrix(df[, available_svc, drop = FALSE])
    # rowSums with na.rm: if ALL are NA, result is 0; we convert to NA instead
    row_sums <- rowSums(svc_matrix, na.rm = TRUE)
    all_na <- rowSums(!is.na(svc_matrix)) == 0
    df$n_services <- ifelse(all_na, NA_integer_, as.integer(row_sums))
    n_added <- n_added + 1L
  } else {
    skipped <- c(skipped, "n_services")
  }

  # Risk index: sum of risk indicators
  risk_cols <- c("poverty_dum", "single_parent", "english_learner",
                 "homeless", "foster_care")
  available_risk <- intersect(risk_cols, names(df))

  if (length(available_risk) > 0) {
    risk_matrix <- as.matrix(df[, available_risk, drop = FALSE])
    row_sums <- rowSums(risk_matrix, na.rm = TRUE)
    all_na <- rowSums(!is.na(risk_matrix)) == 0
    df$risk_index <- ifelse(all_na, NA_integer_, as.integer(row_sums))
    n_added <- n_added + 1L
  } else {
    skipped <- c(skipped, "risk_index")
  }

  list(df = df, n_added = n_added, skipped = skipped)
}


#' Derive eDECA Pre-Post T-Score Gains
#' @noRd
.derive_edeca_gains <- function(df) {
  constructs <- c("initiative", "self_reg", "attachment", "tpf", "behavior")
  n_added <- 0L
  skipped <- character()

  for (construct in constructs) {
    pre_col <- paste0("edeca_pre_", construct, "_tscore")
    post_col <- paste0("edeca_post_", construct, "_tscore")
    gain_col <- paste0("edeca_", construct, "_gain")

    if (pre_col %in% names(df) && post_col %in% names(df)) {
      df[[gain_col]] <- ifelse(
        !is.na(df[[pre_col]]) & !is.na(df[[post_col]]),
        df[[post_col]] - df[[pre_col]],
        NA_real_
      )
      n_added <- n_added + 1L
    } else {
      skipped <- c(skipped, gain_col)
    }
  }

  list(df = df, n_added = n_added, skipped = skipped)
}


#' Print method for transform log
#' @param x The transform log list.
#' @param ... Ignored.
#' @noRd
.print_transform_log <- function(x, ...) {
  cat("  Transform Log:\n")
  cat("    GOLD gains:          ", x$gold_gains_added, "columns\n")
  cat("    K-ready transitions: ", x$kready_transitions_added, "columns\n")

  cat("    Chronic absence:     ", x$chronic_absence_added, "columns\n")
  cat("    Service density:     ", x$service_density_added, "columns\n")
  cat("    eDECA gains:         ", x$edeca_gains_added, "columns\n")
  cat("    Total added:         ", x$n_cols_added, "columns\n")
  if (length(x$skipped) > 0) {
    cat("    Skipped:             ", length(x$skipped), "variables\n")
  }
  invisible(x)
}

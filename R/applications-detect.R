#' Detect ADECE Applications Data Format
#'
#' @description Inspects raw column names to determine which cycle's schema
#'   a particular sheet conforms to. Returns one of `"cycle1"` (2026-2027
#'   combined-file layout), `"cycle0"` (2025-2026 separate-file layout), or
#'   `"unknown"`. Used by `applications_clean()` to pick the correct column
#'   mapping.
#'
#' @param x Either an `alprek_applications_raw` object (the recommended input)
#'   or a character vector of raw column names.
#' @param kind Character. The data kind being inspected — affects which
#'   marker columns are checked. One of `"renewals"`, `"new_apps"`,
#'   `"non_renewals"`, `"capacity"`. If `x` is an `alprek_applications_raw`,
#'   this is inferred from `x$meta$kind`.
#' @return A character scalar: `"cycle1"`, `"cycle0"`, or `"unknown"`.
#'
#' @examples
#' \dontrun{
#' raw <- applications_read_renewals(path, cycle_year = "2026-2027")
#' applications_detect_format(raw)        # likely "cycle1"
#' applications_detect_format(c("Classroom Code", "Site Code", "Region of Classroom"),
#'                              kind = "renewals")  # likely "cycle0"
#' }
#' @export
applications_detect_format <- function(x, kind = NULL) {

  # Extract column names + kind
  if (inherits(x, "alprek_applications_raw")) {
    col_names <- x$meta$col_names
    if (is.null(kind)) kind <- x$meta$kind
  } else if (is.character(x)) {
    col_names <- x
    if (is.null(kind)) {
      stop("kind must be supplied when x is a character vector.", call. = FALSE)
    }
  } else {
    stop("x must be an alprek_applications_raw object or character vector of column names.",
         call. = FALSE)
  }

  kind <- match.arg(kind, c("renewals", "new_apps", "non_renewals", "capacity"))
  col_names_norm <- tolower(trimws(col_names))

  # Marker columns per kind/cycle (lowercased trimmed)
  markers <- list(
    renewals = list(
      cycle1 = c("process name", "tier-adjustment", "26/27 funding",
                  "25/26 project name", "26-27 **draft** base award"),
      cycle0 = c("classroom code", "site code", "program code",
                  "funding (applicant)", "expenditures reported",
                  "2024-25 award amount")
    ),
    new_apps = list(
      cycle1 = c("process name", "26-27 awards", "total 26-27 awards",
                  "new classroom award", "type of program"),
      cycle0 = c("application contact person", "street address",
                  "city", "zip code 4")
    ),
    non_renewals = list(
      # non_renewals is a cycle-1-only kind; headerless sheet.
      # cycle1 is detected by ABSENCE of headers + positional cols starting with col_1
      cycle1 = c("col_1", "col_2", "col_3"),
      cycle0 = character(0)
    ),
    capacity = list(
      cycle1 = c("site code", "site name", "# of classrooms at site",
                  "current site enrollment"),
      cycle0 = character(0)
    )
  )

  cycle1_markers <- markers[[kind]]$cycle1
  cycle0_markers <- markers[[kind]]$cycle0

  has_c1 <- if (length(cycle1_markers) == 0L) 0L else
              sum(cycle1_markers %in% col_names_norm)
  has_c0 <- if (length(cycle0_markers) == 0L) 0L else
              sum(cycle0_markers %in% col_names_norm)

  # Decision: cycle with more matches wins; tie or 0 matches → "unknown"
  # For kinds that exist only in cycle1 (non_renewals, capacity), cycle0 = 0
  # so cycle1 wins as long as any marker matches.
  if (has_c1 == 0L && has_c0 == 0L) {
    return("unknown")
  }
  if (has_c1 > has_c0) return("cycle1")
  if (has_c0 > has_c1) return("cycle0")
  # tie (rare): prefer cycle1 (more recent)
  "cycle1"
}

#' Infer School Year from File Path
#'
#' @description Extracts school year from common ADECE budget file name patterns.
#'   Recognizes patterns like "2021-2022", "21-22", and "24-25".
#'
#' @param path Character. File path or file name.
#' @return Character in "YYYY-YYYY" format, or `NA_character_` if not detected.
#'
#' @examples
#' alprek_infer_school_year("rptClassBudgets 2021-2022.xlsx")
#' # "2021-2022"
#' alprek_infer_school_year("24-25 FCPK Budgets.xlsx")
#' # "2024-2025"
#'
#' @export
alprek_infer_school_year <- function(path) {
  fname <- basename(path)


  # Pattern 1: Full year "2021-2022"
  m <- regmatches(fname, regexpr("(20\\d{2})-(20\\d{2})", fname))
  if (length(m) == 1 && nchar(m) > 0) {
    return(m)
  }

  # Pattern 2: Short year "24-25", "21-22"

  m <- regmatches(fname, regexpr("(\\d{2})-(\\d{2})", fname))
  if (length(m) == 1 && nchar(m) > 0) {
    parts <- strsplit(m, "-")[[1]]
    y1 <- as.integer(parts[1])
    y2 <- as.integer(parts[2])
    # Ensure it looks like consecutive years
    if (y2 == y1 + 1 || (y1 == 99 && y2 == 0)) {
      century <- if (y1 >= 90) "19" else "20"
      century2 <- if (y2 == 0 && y1 == 99) "20" else century
      return(paste0(century, sprintf("%02d", y1), "-", century2, sprintf("%02d", y2)))
    }
  }

  NA_character_
}


#' Convert School Year to Start Year
#'
#' @description Extracts the first year from a "YYYY-YYYY" school year string.
#'
#' @param school_year Character. School year in "YYYY-YYYY" format.
#' @return Integer start year.
#'
#' @examples
#' alprek_school_year_to_start("2024-2025")
#' # 2024L
#'
#' @export
alprek_school_year_to_start <- function(school_year) {
  as.integer(substr(school_year, 1, 4))
}


# ---- Internal helpers ----

#' Clean column names by squishing whitespace
#' @param x Character vector of column names.
#' @return Character vector with squeezed whitespace.
#' @keywords internal
alprek_clean_colnames <- function(x) {
  stringr::str_squish(x)
}


#' Convert currency strings to numeric
#' @param x Character or numeric vector. Handles "$1,234.56" format.
#' @return Numeric vector.
#' @keywords internal
currency_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- gsub("[$,]", "", as.character(x))
  suppressWarnings(as.numeric(x))
}


#' Evaluate "yes" variants as logical
#' @param x Character or logical vector.
#' @return Logical vector.
#' @keywords internal
is_yes <- function(x) {
  if (is.logical(x)) return(x)
  tolower(as.character(x)) %in% c("y", "yes", "true", "1")
}


#' Safe row sums with NA handling
#' @param df Data frame.
#' @param cols Character vector of column names.
#' @return Numeric vector of row sums.
#' @keywords internal
safe_row_sum <- function(df, cols) {
  existing <- intersect(cols, names(df))
  if (length(existing) == 0) return(rep(0, nrow(df)))
  rowSums(df[existing], na.rm = TRUE)
}

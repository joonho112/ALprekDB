#' Parse a Single Classroom Code
#'
#' @description Parses an Alabama Pre-K classroom code in "XXXAYYYYY.ZZ" format
#'   into its component parts.
#'
#' @param code Character. A single classroom code string.
#' @return A named list with elements: `county_code`, `delivery_type_code`,
#'   `program_code`, `class_num`. Returns NAs for invalid codes.
#'
#' @examples
#' parse_classroom_code("823P012601.01")
#' # list(county_code = "823", delivery_type_code = "P",
#' #      program_code = "012601", class_num = "01")
#'
#' @export
parse_classroom_code <- function(code) {
  na_result <- list(
    county_code = NA_character_,
    delivery_type_code = NA_character_,
    program_code = NA_character_,
    class_num = NA_character_
  )

  if (is.na(code) || !is.character(code) || nchar(code) == 0) {
    return(na_result)
  }

  m <- regexec("^(\\d{3})([A-Z])(\\d+)\\.(\\d+)$", code)
  groups <- regmatches(code, m)[[1]]

  if (length(groups) < 5) {
    return(na_result)
  }

  list(
    county_code = groups[2],
    delivery_type_code = groups[3],
    program_code = groups[4],
    class_num = groups[5]
  )
}


#' Parse Multiple Classroom Codes
#'
#' @description Vectorized version of [parse_classroom_code()]. Parses a vector
#'   of classroom codes and returns a tibble with parsed components plus the
#'   delivery type name.
#'
#' @param codes Character vector of classroom codes.
#' @return A tibble with columns: `county_code`, `delivery_type_code`,
#'   `delivery_type`, `program_code`, `class_num`.
#'
#' @examples
#' parse_classroom_codes(c("823P012601.01", "456C789.02"))
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' @export
parse_classroom_codes <- function(codes) {
  parsed <- lapply(codes, parse_classroom_code)

  dt_map <- .delivery_type_map()

  result <- tibble::tibble(
    county_code = purrr::map_chr(parsed, "county_code"),
    delivery_type_code = purrr::map_chr(parsed, "delivery_type_code"),
    program_code = purrr::map_chr(parsed, "program_code"),
    class_num = purrr::map_chr(parsed, "class_num")
  )

  result$delivery_type <- dt_map[result$delivery_type_code]
  result$delivery_type[is.na(result$delivery_type_code)] <- NA_character_

  result
}


#' Delivery Type Code to Name Map
#' @return Named character vector mapping single-letter codes to full names.
#' @keywords internal
.delivery_type_map <- function() {
  c(
    P = "Public School",
    C = "Private Child Care",
    H = "Head Start",
    O = "Community Organization",
    F = "Faith-Based Organization",
    U = "University Operated",
    S = "Private School"
  )
}

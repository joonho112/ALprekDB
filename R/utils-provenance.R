#' Compute SHA-256 Hash of a File
#'
#' @description Computes the SHA-256 hash of a file's contents. Used to
#'   uniquely identify a specific delivery of an ADECE input file (so that
#'   re-deliveries can be detected, and processed datasets carry an
#'   unambiguous source fingerprint).
#'
#' @param path Character. Path to file.
#' @return Character. The SHA-256 hash as a 64-character hex string, or
#'   `NA_character_` if the file cannot be read or `digest` is unavailable.
#'
#' @examples
#' \dontrun{
#' path <- file.path("ORIGINAL-DATA", "applications_2026_2027.xlsx")
#' alprek_file_hash(path)
#' }
#' @export
alprek_file_hash <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    warning("Package 'digest' not installed; cannot compute file hash.",
            call. = FALSE)
    return(NA_character_)
  }
  if (is.null(path) || !is.character(path) || length(path) != 1L) {
    return(NA_character_)
  }
  if (!file.exists(path)) {
    return(NA_character_)
  }
  digest::digest(file = path, algo = "sha256")
}


#' Get Current Git SHA of ALprekDB
#'
#' @description Returns the current HEAD commit SHA of the ALprekDB git
#'   repository. Used as part of dataset lineage tracking. Returns
#'   `NA_character_` if git is unavailable, this is not a git repository,
#'   or the call fails.
#'
#' @param repo_path Character. Path to the git repository. Default `"."`
#'   (current working directory).
#' @return Character. Full SHA (40 chars), or `NA_character_`.
#'
#' @examples
#' \dontrun{
#' alprek_git_sha()
#' }
#' @export
alprek_git_sha <- function(repo_path = ".") {
  if (requireNamespace("gert", quietly = TRUE)) {
    out <- tryCatch(gert::git_log(repo = repo_path, max = 1L)$commit[1],
                     error = function(e) NA_character_)
    return(out)
  }
  # Fallback: shell out to git CLI
  out <- tryCatch(
    suppressWarnings(system2("git",
                              c("-C", repo_path, "rev-parse", "HEAD"),
                              stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_
  )
  if (length(out) >= 1L && nzchar(out[1])) out[1] else NA_character_
}


#' Construct a Provenance Record
#'
#' @description Bundles the standard provenance fields (file hash, receipt
#'   date, cycle year, geocoding source, git SHA, timestamp) into a single
#'   tibble row suitable for appending to `applications_lineage` and other
#'   lineage tables.
#'
#' @param file_path Character. Source file path. Hashed via [alprek_file_hash()].
#' @param cycle_year Character. Cycle year label (e.g., `"2026-2027"`).
#' @param receipt_date Date or character. Date the source file was received.
#'   Default `Sys.Date()`.
#' @param sheet Character. Sheet name within the xlsx (optional).
#' @param geocoding_source Character. Geocoding service used (only relevant
#'   downstream; default `NA_character_` for the applications module).
#' @param repo_path Character. Path to git repository. Default `"."`.
#' @return A tibble row with provenance fields.
#'
#' @examples
#' \dontrun{
#' alprek_provenance_record(
#'   file_path = "Copy of 2026-27 ... (003).xlsx",
#'   cycle_year = "2026-2027",
#'   receipt_date = "2026-05-19",
#'   sheet = "26-27 requests_TW"
#' )
#' }
#' @importFrom tibble tibble
#' @export
alprek_provenance_record <- function(file_path,
                                       cycle_year,
                                       receipt_date = Sys.Date(),
                                       sheet = NA_character_,
                                       geocoding_source = NA_character_,
                                       repo_path = ".") {
  if (inherits(receipt_date, "Date")) {
    receipt_date <- format(receipt_date, "%Y-%m-%d")
  }
  tibble::tibble(
    file_path = file_path,
    file_basename = basename(file_path),
    file_sha256 = alprek_file_hash(file_path),
    cycle_year = cycle_year,
    receipt_date = receipt_date,
    sheet = sheet,
    geocoding_source = geocoding_source,
    git_sha = alprek_git_sha(repo_path),
    captured_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}


#' Build stable row-level lineage IDs
#' @keywords internal
#' @noRd
.alprek_lineage_id <- function(file_sha256, sheet, raw_row_index, cycle_year) {
  key <- paste(file_sha256, sheet, raw_row_index, cycle_year, sep = "||")
  vapply(key, digest::digest, character(1), algo = "sha256", serialize = FALSE)
}

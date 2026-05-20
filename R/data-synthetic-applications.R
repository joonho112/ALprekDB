#' Generate Synthetic Applications Data
#'
#' @description Creates synthetic ADECE classroom applications data covering the
#'   four canonical input kinds (renewals, new applications, non-renewals,
#'   capacity). Output mirrors cycle-1 (2026-2027) standardized schema using
#'   fake `9xx`-prefix classroom codes and `9xxxxx` program codes so that
#'   examples cannot be confused with confidential source records.
#'
#'   Designed for vignette, tests, and demonstrations. Shares classroom/site
#'   codes with `alprek_synthetic_budget()` / `alprek_synthetic_classroom()` /
#'   `alprek_synthetic_student()` when called with the same `seed`.
#'
#' @param n_renewals Integer. Number of renewal classrooms. Default `100`.
#' @param n_new Integer. Number of new classroom applications. Default `30`.
#' @param n_non_renewals Integer. Number of non-renewals. Default `5`.
#' @param n_capacity_sites Integer. Number of site capacity rows. Default
#'   `120` (covers both renewals and new).
#' @param cycle_year Character. Cycle year label, e.g., `"2026-2027"`. Default
#'   `"2026-2027"`.
#' @param seed Integer. Random seed for reproducibility. Default `42L`.
#'
#' @return A list with four tibbles in standardized cycle-1 schema:
#'   * `renewals` (n_renewals rows): process_name, region, county,
#'     organization_name, project_name, funding_type, program_type,
#'     project_name_prior, funding_type_prior, award_prior,
#'     total_funding_request, draft_base_award, tier_adjustment, draft_award,
#'     notes
#'   * `new_apps` (n_new rows): process_name, region, county,
#'     organization_name, project_name, funding_type, program_type,
#'     total_funding_request, award_other, new_classroom_award, total_award
#'   * `non_renewals` (n_non_renewals rows): region, county, organization_name,
#'     project_name, prior_funding_amount, prior_funding_type, notes
#'   * `capacity` (n_capacity_sites rows): site_code, site_name, n_classrooms,
#'     enrollment, capacity, waitlist, spaces_available_with_waitlist
#'
#' @examples
#' apps <- alprek_synthetic_applications(n_renewals = 20, n_new = 5, seed = 42)
#' head(apps$renewals)
#' head(apps$new_apps)
#'
#' @importFrom tibble tibble
#' @export
alprek_synthetic_applications <- function(n_renewals = 100L,
                                            n_new = 30L,
                                            n_non_renewals = 5L,
                                            n_capacity_sites = 120L,
                                            cycle_year = "2026-2027",
                                            seed = 42L) {

  # ---- validation ----
  stopifnot(
    is.numeric(n_renewals), n_renewals >= 0,
    is.numeric(n_new), n_new >= 0,
    is.numeric(n_non_renewals), n_non_renewals >= 0,
    is.numeric(n_capacity_sites), n_capacity_sites >= 0,
    is.character(cycle_year), length(cycle_year) == 1L,
    grepl("^\\d{4}-\\d{4}$", cycle_year),
    is.numeric(seed), length(seed) == 1L
  )

  n_renewals <- as.integer(n_renewals)
  n_new <- as.integer(n_new)
  n_non_renewals <- as.integer(n_non_renewals)
  n_capacity_sites <- as.integer(n_capacity_sites)
  seed <- as.integer(seed)

  set.seed(seed)

  # cycle_year_prev: e.g., "2025-2026" derived from "2026-2027"
  yr_a <- as.integer(substr(cycle_year, 1, 4))
  yr_b <- as.integer(substr(cycle_year, 6, 9))
  cycle_year_prev <- sprintf("%d-%d", yr_a - 1L, yr_b - 1L)

  # ---- helpers ----
  fake_region <- function(n) sprintf("Region %d", sample.int(9L, n, replace = TRUE))
  fake_county <- function(n) sample(
    c("Madison", "Jefferson", "Mobile", "Shelby", "Tuscaloosa", "Lee",
      "Wilcox", "Sumter", "Greene", "Perry", "Lowndes", "Macon"),
    n, replace = TRUE
  )
  fake_org <- function(n, prefix = "Synthetic") sprintf(
    "%s %s", prefix,
    sample(c("Daycare LLC", "Academy", "Learning Center", "Preschool",
              "Christian School", "Public Schools", "Head Start"),
            n, replace = TRUE)
  )
  fake_program_type <- function(n) sample(
    c("Public School", "Private Child Care", "Head Start",
      "Faith-Based Organization", "Community Organization",
      "University Operated"),
    n, replace = TRUE
  )
  fake_funding_type <- function(n, weights = c(0.85, 0.10, 0.05)) sample(
    c("Classroom Funding", "Supplemental Funding", "Reduced Capacity Funding"),
    n, replace = TRUE, prob = weights
  )

  # ---- renewals ----
  process_renewal <- sprintf("%d - %d First Class Pre-K Classroom Renewal",
                              yr_a, yr_b)
  renewals <- tibble::tibble(
    process_name = rep(process_renewal, n_renewals),
    region = fake_region(n_renewals),
    county = fake_county(n_renewals),
    organization_name = fake_org(n_renewals),
    project_name = sprintf("Classroom %s Pre-K %d",
                            sample(LETTERS, n_renewals, replace = TRUE),
                            sample.int(5L, n_renewals, replace = TRUE)),
    funding_type = fake_funding_type(n_renewals),
    program_type = fake_program_type(n_renewals),
    project_name_prior = NA_character_,
    funding_type_prior = fake_funding_type(n_renewals),
    award_prior = round(runif(n_renewals, 100000, 150000), 0),
    total_funding_request = round(runif(n_renewals, 110000, 160000), 0),
    draft_base_award = round(runif(n_renewals, 100000, 130000), 0),
    tier_adjustment = round(runif(n_renewals, 0, 10000), 0),
    draft_award = NA_real_,
    notes = sample(c(NA_character_, "Reviewed", "Pending"), n_renewals,
                    replace = TRUE, prob = c(0.7, 0.2, 0.1))
  )
  # project_name_prior = project_name with cycle prefix
  renewals$project_name_prior <- renewals$project_name
  renewals$draft_award <- renewals$draft_base_award + renewals$tier_adjustment

  # ---- new apps ----
  process_new <- sprintf("%d - %d First Class Pre-K New Classroom Application",
                          yr_a, yr_b)
  new_apps <- tibble::tibble(
    process_name = rep(process_new, n_new),
    region = fake_region(n_new),
    county = fake_county(n_new),
    organization_name = fake_org(n_new, prefix = "Synthetic New"),
    project_name = sprintf("New Classroom %s Pre-K %d",
                            sample(LETTERS, n_new, replace = TRUE),
                            sample.int(5L, n_new, replace = TRUE)),
    funding_type = fake_funding_type(n_new),
    program_type = fake_program_type(n_new),
    total_funding_request = round(runif(n_new, 130000, 200000), 0),
    award_other = round(runif(n_new, 0, 20000), 0),
    new_classroom_award = round(runif(n_new, 100000, 150000), 0),
    total_award = NA_real_
  )
  new_apps$total_award <- new_apps$award_other + new_apps$new_classroom_award

  # ---- non_renewals ----
  non_renewals <- tibble::tibble(
    region = fake_region(n_non_renewals),
    county = fake_county(n_non_renewals),
    organization_name = fake_org(n_non_renewals, prefix = "Synthetic NonRen"),
    project_name = sprintf("NonRen %s Pre-K %d",
                            sample(LETTERS, n_non_renewals, replace = TRUE),
                            sample.int(5L, n_non_renewals, replace = TRUE)),
    prior_funding_amount = round(runif(n_non_renewals, 90000, 130000), 0),
    prior_funding_type = fake_funding_type(n_non_renewals),
    notes = NA_character_
  )

  # ---- capacity ----
  fake_site_code <- function(n) {
    sprintf("9%02d%s%05d",
            sample.int(99L, n, replace = TRUE),
            sample(c("P","C","H","O","F","U","S"), n, replace = TRUE),
            sample(1e3:99999L, n, replace = TRUE))
  }
  capacity <- tibble::tibble(
    site_code = fake_site_code(n_capacity_sites),
    site_name = sprintf("Synthetic Site %d", seq_len(n_capacity_sites)),
    n_classrooms = sample.int(8L, n_capacity_sites, replace = TRUE),
    enrollment = round(runif(n_capacity_sites, 0, 100), 0),
    capacity = NA_integer_,
    waitlist = round(runif(n_capacity_sites, 0, 30), 0),
    spaces_available_with_waitlist = sample(0:5, n_capacity_sites, replace = TRUE)
  )
  # capacity >= enrollment for realism
  capacity$capacity <- capacity$enrollment + sample.int(20L,
                                                          n_capacity_sites,
                                                          replace = TRUE)

  list(
    renewals = renewals,
    new_apps = new_apps,
    non_renewals = non_renewals,
    capacity = capacity,
    meta = list(
      cycle_year = cycle_year,
      cycle_year_prev = cycle_year_prev,
      seed = seed,
      counts = c(renewals = n_renewals, new_apps = n_new,
                  non_renewals = n_non_renewals, capacity = n_capacity_sites)
    )
  )
}

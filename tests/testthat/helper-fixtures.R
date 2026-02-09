# Shared test fixture builders for ALprekDB
# These create synthetic data that mirrors real ADECE budget file structures

#' Create a synthetic legacy-format budget data frame
#' @param n Number of classrooms to generate.
#' @return A tibble resembling a raw legacy Excel import.
make_legacy_raw_df <- function(n = 5) {
  codes <- paste0(
    sprintf("%03d", sample(1:133, n, replace = TRUE)),
    sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n)),
    ".",
    sprintf("%02d", sample(1:5, n, replace = TRUE))
  )

  names_vec <- paste0("Test Site ", seq_len(n))

  # Create amounts
  set.seed(42)
  lt_sal_osr <- round(runif(n, 25000, 50000), 0)
  lt_ben_osr <- round(runif(n, 5000, 15000), 0)
  at_sal_osr <- round(runif(n, 15000, 30000), 0)
  at_ben_osr <- round(runif(n, 3000, 10000), 0)
  payroll_osr <- round(runif(n, 2000, 6000), 0)
  subs_osr <- round(runif(n, 500, 3000), 0)
  bg_osr <- round(runif(n, 100, 500), 0)
  maint_osr <- round(runif(n, 1000, 5000), 0)
  furn_osr <- round(runif(n, 500, 3000), 0)
  admin_osr <- round(runif(n, 1000, 4000), 0)
  acct_osr <- round(runif(n, 200, 1000), 0)

  total_osr <- lt_sal_osr + lt_ben_osr + at_sal_osr + at_ben_osr +
    payroll_osr + subs_osr + bg_osr + maint_osr + furn_osr + admin_osr + acct_osr

  # Some additional funds (smaller, sparse)
  lt_sal_add1 <- round(runif(n, 0, 5000), 0)
  total_add <- lt_sal_add1

  grand_total <- total_osr + total_add

  df <- tibble::tibble(
    `Classroom Name` = names_vec,
    `Classroom Code` = codes,
    `Budget Version (latest approved for 2023-2024)` = paste("Budget", seq_len(n)),
    `Lead Teacher Salary From OSR Funds` = lt_sal_osr,
    `Lead Teacher Salary From Additional Funds 1` = lt_sal_add1,
    `Lead Teacher Salary From Additional Funds 2` = rep(0, n),
    `Lead Teacher Salary Additional Source 1` = ifelse(lt_sal_add1 > 0, "Local School", ""),
    `Lead Teacher Salary Additional Source 2` = rep("", n),
    `Lead Teacher Benefits From OSR Funds` = lt_ben_osr,
    `Lead Teacher Benefits From Additional Funds 1` = rep(0, n),
    `Lead Teacher Benefits From Additional Funds 2` = rep(0, n),
    `Aux Teacher Salary From OSR Funds` = at_sal_osr,
    `Aux Teacher Salary From Additional Funds 1` = rep(0, n),
    `Aux Teacher Salary From Additional Funds 2` = rep(0, n),
    `Aux Teacher Benefits From OSR Funds` = at_ben_osr,
    `Aux Teacher Benefits From Additional Funds 1` = rep(0, n),
    `Aux Teacher Benefits From Additional Funds 2` = rep(0, n),
    `Payroll Taxes From OSR Funds` = payroll_osr,
    `Payroll Taxes From Additional Funds 1` = rep(0, n),
    `Payroll Taxes From Additional Funds 2` = rep(0, n),
    `Substitutes From OSR Funds` = subs_osr,
    `Substitutes From Additional Funds 1` = rep(0, n),
    `Substitutes From Additional Funds 2` = rep(0, n),
    `Background Checks From OSR Funds` = bg_osr,
    `Background Checks From Additional Funds 1` = rep(0, n),
    `Background Checks From Additional Funds 2` = rep(0, n),
    `Classroom Maintenance And Cleaning From OSR Funds` = maint_osr,
    `Classroom Maintenance And Cleaning From Additional Funds 1` = rep(0, n),
    `Classroom Maintenance And Cleaning From Additional Funds 2` = rep(0, n),
    `Classroom Furnishings And Equipment From OSR Funds` = furn_osr,
    `Classroom Furnishings And Equipment From Additional Funds 1` = rep(0, n),
    `Classroom Furnishings And Equipment From Additional Funds 2` = rep(0, n),
    `Administrator From OSR Funds` = admin_osr,
    `Administrator From Additional Funds 1` = rep(0, n),
    `Administrator From Additional Funds 2` = rep(0, n),
    `Accounting From OSR Funds` = acct_osr,
    `Accounting From Additional Funds 1` = rep(0, n),
    `Accounting From Additional Funds 2` = rep(0, n),
    `Total From OSR Funds` = total_osr,
    `Total From Additional Sources` = total_add,
    `Grand Total` = grand_total
  )

  df
}


#' Create a synthetic new-format budget data frame
#' @param n Number of classrooms to generate.
#' @return A tibble resembling a raw new-format (2024-2025+) Excel import.
make_new_raw_df <- function(n = 5) {
  codes <- paste0(
    sprintf("%03d", sample(1:133, n, replace = TRUE)),
    sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n)),
    ".",
    sprintf("%02d", sample(1:5, n, replace = TRUE))
  )

  names_vec <- paste0("Test Site ", seq_len(n))

  set.seed(42)
  lt_sal_osr <- round(runif(n, 25000, 50000), 0)
  lt_ben_osr <- round(runif(n, 7000, 20000), 0)
  at_sal_osr <- round(runif(n, 15000, 30000), 0)
  at_ben_osr <- round(runif(n, 5000, 12000), 0)
  is_osr <- round(runif(n, 2000, 8000), 0)
  om_osr <- round(runif(n, 1000, 5000), 0)
  eq_osr <- round(runif(n, 500, 3000), 0)
  ad_osr <- round(runif(n, 1000, 4000), 0)

  lt_sal_oth <- round(runif(n, 0, 3000), 0)
  lt_ben_oth <- round(runif(n, 0, 1000), 0)
  at_sal_oth <- rep(0, n)
  at_ben_oth <- rep(0, n)
  is_oth <- round(runif(n, 0, 2000), 0)
  om_oth <- rep(0, n)
  eq_oth <- rep(0, n)
  ad_oth <- rep(0, n)

  osr_total <- lt_sal_osr + lt_ben_osr + at_sal_osr + at_ben_osr +
    is_osr + om_osr + eq_osr + ad_osr
  oth_total <- lt_sal_oth + lt_ben_oth + at_sal_oth + at_ben_oth +
    is_oth + om_oth + eq_oth + ad_oth
  grand <- osr_total + oth_total

  tibble::tibble(
    `Classroom Name` = names_vec,
    `Classroom Code` = codes,
    `Budget Version (latest approved for 2024-2025)` = paste("Budget", seq_len(n)),
    `Lead Teacher Salary OSR` = lt_sal_osr,
    `Lead Teacher Salary Other` = lt_sal_oth,
    `Lead Teacher Benefits OSR (including Payroll Taxes)` = lt_ben_osr,
    `Lead Teacher Benefits Other` = lt_ben_oth,
    `Aux Teacher Salary OSR` = at_sal_osr,
    `Aux Teacher Salary Other` = at_sal_oth,
    `Aux Teacher Benefits OSR (including Payroll Taxes)` = at_ben_osr,
    `Aux Teacher Benefits Other` = at_ben_oth,
    `Instructional Support OSR` = is_osr,
    `Instructional Support Other` = is_oth,
    `Operations and Maintenance OSR` = om_osr,
    `Operations  and Maintenance Other` = om_oth,
    `Equipment OSR` = eq_osr,
    `Equipment Other` = eq_oth,
    `Administrative OSR` = ad_osr,
    `Administrative Other` = ad_oth,
    `Total` = grand,
    `OSR Grant Amount` = osr_total,
    `Proration Total` = rep(0, n),
    `Federal Funds used for Other Funds Expenditures` = round(oth_total * 0.5, 0),
    `Local Funds used for Other Funds Expenditures` = round(oth_total * 0.3, 0),
    `Other Funds used for Other Funds Expenditures` = oth_total - round(oth_total * 0.5, 0) - round(oth_total * 0.3, 0),
    `Total Federal, Local, and Other Funds used for Other Funds Expenditures` = oth_total,
    `Total Additional OSR Funds` = rep(0, n),
    `Total of ALL OSR Grants` = osr_total
  )
}


#' Create an alprek_budget_raw S3 object from synthetic data
#' @param format Either "legacy" or "new".
#' @param n Number of classrooms.
#' @return An alprek_budget_raw object.
make_budget_raw <- function(format = "legacy", n = 5) {
  if (format == "legacy") {
    df <- make_legacy_raw_df(n)
    sy <- "2023-2024"
  } else {
    df <- make_new_raw_df(n)
    sy <- "2024-2025"
  }

  # Add school_year and year columns (normally added by budget_read)
  yr <- alprek_school_year_to_start(sy)
  df$school_year <- sy
  df$year <- yr

  structure(
    list(
      data = df,
      meta = list(
        path = paste0("test_budget_", format, ".xlsx"),
        school_year = sy,
        year = alprek_school_year_to_start(sy),
        format = format,
        sheet = "Sheet1",
        n_rows_raw = nrow(df),
        n_rows_clean = nrow(df),
        read_at = Sys.time()
      )
    ),
    class = "alprek_budget_raw"
  )
}


# ==========================================================================
# Classroom Module Fixtures
# ==========================================================================

#' Helper: create a character column of length n with last `n_na` entries as NA
#' @param n Total length.
#' @param gen Function(k) that generates k non-NA values.
#' @param n_na Number of trailing NAs.
#' @return Character vector of length n.
.make_sparse_col <- function(n, gen, n_na = 2) {
  k <- max(0, n - n_na)
  if (k > 0) {
    c(gen(k), rep(NA_character_, min(n_na, n)))
  } else {
    rep(NA_character_, n)
  }
}

.make_sparse_col_int <- function(n, gen, n_na = 2) {
  k <- max(0, n - n_na)
  if (k > 0) {
    c(gen(k), rep(NA_integer_, min(n_na, n)))
  } else {
    rep(NA_integer_, n)
  }
}

#' Sample degree text values for testing classification
classroom_degree_samples <- c(
 "Bachelor's in Early Childhood Education",
 "Master's in Elementary Education",
 "CDA",
 "Waiver",
 "wavier",
 "Ph.D. in Education",
 "Ed.D.",
 "Ed.S.",
 "EdS Early Childhood",
 "Associate in Child Development",
 "Bachelor of Science",
 "B.S. in Psychology",
 "B.A. Interdisciplinary Studies",
 "9 Hours ECE Coursework",
 "Working toward CDA",
 "Waiver to complete CDA",
 "Master's in Special Education",
 "Doctorate in Curriculum and Instruction",
 "High School Diploma",
 "In process of obtaining Bachelor's",
 "College Coursework",
 "Bachelor's in Elementary Education with Early Childhood coursework",
 "Master of Education",
 "B.S. Family and Consumer Sciences",
 NA_character_,
 "",
 "Child Development Associate",
 "Registered in school",
 "Technical Education",
 "Master's in Child Development"
)


#' Create a synthetic legacy-format classroom data frame (100 cols)
#' @param n Number of classrooms to generate.
#' @return A tibble with raw Excel column names (legacy format).
make_classroom_legacy_raw_df <- function(n = 5) {
  set.seed(123)

  codes <- paste0(
    sprintf("%03d", sample(1:67, n, replace = TRUE)),
    sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n)),
    ".",
    sprintf("%02d", sample(1:5, n, replace = TRUE))
  )

  site_codes <- paste0(
    sprintf("%03d", sample(1:67, n, replace = TRUE)),
    sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n))
  )

  delivery_types <- sample(c("Public School", "Private Child Care", "Head Start",
                             "Community Organization", "Faith-Based Organization",
                             "University Operated", "Private School"),
                           n, replace = TRUE)

  degree_vals <- sample(classroom_degree_samples, n, replace = TRUE)
  races <- sample(c("White", "Black or African American", "Latino",
                     "Asian", "Mixed Heritage", "Unknown"), n, replace = TRUE)
  ethnicities <- sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE)
  genders <- sample(c("Female", "Male"), n, replace = TRUE)
  languages <- sample(c("Spanish", "N/A", "no", "None", NA), n, replace = TRUE)

  tibble::tibble(
    `Classroom Name` = paste0("Test Classroom ", seq_len(n)),
    `School Year` = rep("2023-2024", n),
    `Classroom Code Static` = codes,
    `Region` = sample(1:11, n, replace = TRUE),
    `County #` = sprintf("%03d", sample(1:67, n, replace = TRUE)),
    `Delivery Type` = delivery_types,
    `Program Code` = sprintf("%05d", sample(10000:99999, n)),
    `Site #` = sprintf("%02d", sample(1:10, n, replace = TRUE)),
    `Class #` = sprintf("%02d", sample(1:5, n, replace = TRUE)),
    `Title I` = sample(c("Y", "N", "NA"), n, replace = TRUE),
    `Turnaround School` = sample(c("Y", "N"), n, replace = TRUE),
    `Class Type` = rep("Full Day", n),
    `Year First Funded` = sample(2005:2020, n, replace = TRUE),
    `Description` = rep("Pre-K", n),
    `Classroom Grant Amount` = round(runif(n, 80000, 150000), 0),
    `Enhancement Grant Amount` = round(runif(n, 0, 5000), 0),
    `Salary Enhancement Grant Amount` = round(runif(n, 0, 3000), 0),
    `Equity Grant Amount` = round(runif(n, 0, 2000), 0),
    `Total Grant Amount` = round(runif(n, 85000, 155000), 0),
    `Vendor Code` = sprintf("V%04d", sample(1000:9999, n)),
    `Address Code` = sprintf("A%04d", sample(1000:9999, n)),
    `Program Name` = paste0("Program ", seq_len(n)),
    `Program Street Address` = paste0(sample(100:999, n), " Main St"),
    `Program City` = sample(c("Birmingham", "Montgomery", "Tuscaloosa", "Huntsville"), n, replace = TRUE),
    `Program State` = rep("AL", n),
    `Program Zip Code` = sprintf("%05d", sample(35000:36999, n)),
    `GOLD Country ID` = rep("US", n),
    `Program Phone` = paste0("(205) ", sprintf("%03d-%04d", sample(100:999, n), sample(1000:9999, n))),
    `Site Name` = paste0("Site ", seq_len(n)),
    `Site Code Static` = site_codes,
    `County Name` = sample(c("Jefferson", "Montgomery", "Tuscaloosa", "Madison"), n, replace = TRUE),
    `Site Street Address` = paste0(sample(100:999, n), " Oak Ave"),
    `Site City` = sample(c("Birmingham", "Montgomery", "Tuscaloosa", "Huntsville"), n, replace = TRUE),
    `Site State` = rep("AL", n),
    `Site Zip Code` = sprintf("%05d", sample(35000:36999, n)),
    `Latitude` = round(runif(n, 30.2, 35.0), 6),
    `Longitude` = round(runif(n, -88.5, -85.0), 6),
    `Site Phone` = paste0("(205) ", sprintf("%03d-%04d", sample(100:999, n), sample(1000:9999, n))),
    `Senate District` = sample(1:35, n, replace = TRUE),
    `House District` = sample(1:105, n, replace = TRUE),
    # Staff: RD
    `RD First Name` = paste0("RDFirst", seq_len(n)),
    `RD Last Name` = paste0("RDLast", seq_len(n)),
    `RD Email` = paste0("rd", seq_len(n), "@test.com"),
    # Staff: Director
    `Director FName` = paste0("DirFirst", seq_len(n)),
    `Director LName` = paste0("DirLast", seq_len(n)),
    `Director Email` = paste0("dir", seq_len(n), "@test.com"),
    # Staff: Administrator
    `Administrator FName` = paste0("AdminFirst", seq_len(n)),
    `Administrator LName` = paste0("AdminLast", seq_len(n)),
    `Administrator Email` = paste0("admin", seq_len(n), "@test.com"),
    # Staff: Principal
    `Principal FName` = paste0("PrinFirst", seq_len(n)),
    `Principal LName` = paste0("PrinLast", seq_len(n)),
    `Principal Email` = paste0("prin", seq_len(n), "@test.com"),
    # Staff: Asst Principal (note double space in LName and Email columns)
    `Assistant Principal FName` = paste0("AsstFirst", seq_len(n)),
    `Assistant Principal LName` = paste0("AsstLast", seq_len(n)),
    `Assistant Principal Email` = paste0("asst", seq_len(n), "@test.com"),
    # Staff: CFO
    `CFO FName` = paste0("CFOFirst", seq_len(n)),
    `CFO LName` = paste0("CFOLast", seq_len(n)),
    `CFO Email` = paste0("cfo", seq_len(n), "@test.com"),
    # Staff: Registrar
    `Registrar FName` = paste0("RegFirst", seq_len(n)),
    `Registrar LName` = paste0("RegLast", seq_len(n)),
    `Registrar Email` = paste0("reg", seq_len(n), "@test.com"),
    # Lead Teacher
    `Lead Teacher TCert Num` = paste0("LT", sprintf("%06d", sample(100000:999999, n))),
    `Lead Teacher FName` = paste0("LeadFirst", seq_len(n)),
    `Lead Teacher LName` = paste0("LeadLast", seq_len(n)),
    `Lead Teacher Email` = paste0("lead", seq_len(n), "@test.com"),
    `Lead Teacher Degree/Credential` = degree_vals,
    `Lead Teacher Years OSR Exp` = sample(0:30, n, replace = TRUE),
    `Lead Teacher Total Years Exp` = sample(0:35, n, replace = TRUE),
    `Lead Teacher Race` = races,
    `Lead Teacher Ethnicity` = ethnicities,
    `Lead Teacher Gender` = genders,
    `Lead Teacher Other Fluent Language` = languages,
    # Aux Teacher
    `Aux Teacher TCert Num` = paste0("AT", sprintf("%06d", sample(100000:999999, n))),
    `Aux Teacher FName` = paste0("AuxFirst", seq_len(n)),
    `Aux Teacher LName` = paste0("AuxLast", seq_len(n)),
    `Aux Teacher Email` = paste0("aux", seq_len(n), "@test.com"),
    `Aux Teacher Degree/Credential` = sample(classroom_degree_samples, n, replace = TRUE),
    `Aux Teacher Years OSR Exp` = sample(0:20, n, replace = TRUE),
    `Aux Teacher Total Years Exp` = sample(0:25, n, replace = TRUE),
    `Aux Teacher Race` = sample(c("White", "Black or African American", "Unknown"), n, replace = TRUE),
    `Aux. Teacher Ethnicity` = sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE),
    `Aux Teacher Gender` = sample(c("Female", "Male"), n, replace = TRUE),
    `Aux Teacher Other Fluent Language` = sample(c("Spanish", "N/A", NA), n, replace = TRUE),
    # 2nd Aux Teacher — last 2 rows always NA to simulate sparse data
    `2nd Aux Teacher TCert Num` = .make_sparse_col(n, function(k) paste0("2AT", sprintf("%05d", sample(10000:99999, k))), 2),
    `2nd Aux Teacher FName` = .make_sparse_col(n, function(k) paste0("SecFirst", seq_len(k)), 2),
    `2nd Aux Teacher LName` = .make_sparse_col(n, function(k) paste0("SecLast", seq_len(k)), 2),
    `2nd Aux Teacher Email` = .make_sparse_col(n, function(k) paste0("sec", seq_len(k), "@test.com"), 2),
    `2nd Aux Teacher Degree/Credential` = .make_sparse_col(n, function(k) sample(classroom_degree_samples[!is.na(classroom_degree_samples)], k, replace = TRUE), 2),
    `2nd Aux Teacher Years OSR Exp` = .make_sparse_col_int(n, function(k) sample(0:10, k, replace = TRUE), 2),
    `2nd Aux Teacher Total Years Exp` = .make_sparse_col_int(n, function(k) sample(0:15, k, replace = TRUE), 2),
    `2nd Aux Teacher Race` = .make_sparse_col(n, function(k) sample(c("White", "Black or African American"), k, replace = TRUE), 2),
    `2nd Aux Teacher Ethnicity` = .make_sparse_col(n, function(k) sample(c("Hispanic", "Non-Hispanic"), k, replace = TRUE), 2),
    `2nd Aux Teacher Gender` = .make_sparse_col(n, function(k) sample(c("Female", "Male"), k, replace = TRUE), 2),
    `2nd Aux Teacher Other Fluent Language` = .make_sparse_col(n, function(k) sample(c("Spanish", NA), k, replace = TRUE), 2),
    # Staff: Monitor
    `Monitor FName` = paste0("MonFirst", seq_len(n)),
    `Monitor LName` = paste0("MonLast", seq_len(n)),
    `Monitor Email` = paste0("mon", seq_len(n), "@test.com"),
    # Staff: Coach
    `Coach FName` = paste0("CoachFirst", seq_len(n)),
    `Coach LName` = paste0("CoachLast", seq_len(n)),
    `Coach Email` = paste0("coach", seq_len(n), "@test.com")
  )
}


#' Create a synthetic new-format classroom data frame (125 cols)
#' @param n Number of classrooms to generate.
#' @return A tibble with raw Excel column names (new format, 2024-2025).
make_classroom_new_raw_df <- function(n = 5) {
  # Start with legacy columns
  df <- make_classroom_legacy_raw_df(n)

  # Add new format-specific columns
  df$`Classroom Code Formula` <- df$`Classroom Code Static`
  df$`Seat Count` <- sample(18:22, n, replace = TRUE)
  df$`Gold Curriculum Type` <- sample(c("TS GOLD", "Other"), n, replace = TRUE)
  df$`Gold Class Level` <- sample(c("Pre-K", "Infant/Toddler"), n, replace = TRUE)
  df$`Fund Source` <- sample(c("State", "Federal", "Mixed"), n, replace = TRUE)
  df$`Site Code Formula` <- paste0(substr(df$`Classroom Code Static`, 1, 9))
  df$`Site Affiliation ID` <- sprintf("SA%04d", sample(1000:9999, n))

  # Add DOB and UserName columns for staff and teachers
  df$`Director Date of Birth` <- rep(NA, n)
  df$`Director UserName` <- paste0("dir_user_", seq_len(n))
  df$`Administrator Date of Birth` <- rep(NA, n)
  df$`Administrator UserName` <- paste0("admin_user_", seq_len(n))
  df$`Principal Date of Birth` <- rep(NA, n)
  df$`Principal UserName` <- paste0("prin_user_", seq_len(n))
  df$`Assistant Principal Date of Birth` <- rep(NA, n)
  df$`Assistant Principal UserName` <- paste0("asst_user_", seq_len(n))
  df$`CFO Date of Birth` <- rep(NA, n)
  df$`CFO UserName` <- paste0("cfo_user_", seq_len(n))
  df$`Registrar Date of Birth` <- rep(NA, n)
  df$`Registrar UserName` <- paste0("reg_user_", seq_len(n))
  df$`Lead Teacher Date of Birth` <- rep(NA, n)
  df$`Lead Teacher UserName` <- paste0("lead_user_", seq_len(n))
  df$`Aux Teacher Date of Birth` <- rep(NA, n)
  df$`Aux Teacher User Name` <- paste0("aux_user_", seq_len(n))
  df$`2nd Aux Teacher Date of Birth` <- rep(NA, n)
  df$`2nd Aux Teacher User Name` <- .make_sparse_col(n, function(k) paste0("sec_user_", seq_len(k)), 2)

  # Update School Year
  df$`School Year` <- rep("2024-2025", n)

  # Rename Aux. Teacher Ethnicity to match new format (still has period in raw data)
  df
}


#' Create an alprek_classroom_raw S3 object from synthetic data
#' @param format Either "legacy" or "new".
#' @param n Number of classrooms.
#' @return An alprek_classroom_raw object.
make_classroom_raw <- function(format = "legacy", n = 5) {
  if (format == "legacy") {
    df <- make_classroom_legacy_raw_df(n)
    sy <- "2023-2024"
  } else {
    df <- make_classroom_new_raw_df(n)
    sy <- "2024-2025"
  }

  structure(
    list(
      data = df,
      meta = list(
        path = paste0("test_classroom_", format, ".xlsx"),
        school_year = sy,
        year = alprek_school_year_to_start(sy),
        format = format,
        sheet = "rptRIF",
        n_rows_raw = nrow(df),
        n_rows_clean = nrow(df),
        n_cols = ncol(df),
        col_names = names(df),
        read_at = Sys.time()
      )
    ),
    class = "alprek_classroom_raw"
  )
}


#' Create a pre-cleaned classroom object for testing downstream steps
#' @param format Either "legacy" or "new".
#' @param n Number of classrooms.
#' @return An alprek_classroom_clean object with minimal cleaned data.
make_classroom_clean <- function(format = "legacy", n = 5) {
  set.seed(456)
  sy <- if (format == "legacy") "2023-2024" else "2024-2025"
  year <- alprek_school_year_to_start(sy)

  codes <- paste0(
    sprintf("%03d", sample(1:67, n, replace = TRUE)),
    sample(c("P", "C", "H", "O", "F", "U", "S"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n)),
    ".",
    sprintf("%02d", sample(1:5, n, replace = TRUE))
  )

  df <- tibble::tibble(
    classroom_code = codes,
    classroom_name = paste0("Test Classroom ", seq_len(n)),
    school_year = rep(sy, n),
    year = rep(year, n),
    region = sample(1:11, n, replace = TRUE),
    county_code = sprintf("%03d", sample(1:67, n, replace = TRUE)),
    county_name = sample(c("Jefferson", "Montgomery", "Tuscaloosa"), n, replace = TRUE),
    delivery_type = factor(
      sample(c("Public School", "Private Child Care", "Head Start"), n, replace = TRUE),
      levels = c("Public School", "Private Child Care", "Head Start",
                 "Community Organization", "Faith-Based Organization",
                 "University Operated", "Private School")
    ),
    title_i = sample(c("Y", "N", NA), n, replace = TRUE),
    title_i_numeric = ifelse(is.na(title_i), NA_real_, ifelse(title_i == "Y", 1, 0)),
    total_grant = round(runif(n, 85000, 155000), 0),
    latitude = round(runif(n, 30.2, 35.0), 6),
    longitude = round(runif(n, -88.5, -85.0), 6),
    site_code = substr(codes, 1, nchar(codes) - 3),
    # Lead teacher
    lead_tch_name = paste0("Lead", seq_len(n), " Teacher", seq_len(n)),
    lead_tch_degree_raw = sample(c("Bachelor's ECE", "Master's Elementary", "CDA", "Waiver"), n, replace = TRUE),
    lead_tch_degree_level = factor(
      sample(c("Bachelor's degree", "Master's degree", "Associate degree", "Waiver"), n, replace = TRUE),
      levels = c("Waiver", "Doctoral degree", "Ed.S.", "Master's degree",
                 "Bachelor's degree", "Associate degree", "College Coursework")
    ),
    lead_tch_degree_area = sample(c("Early Childhood Education", "Elementary Education", "Child Development"), n, replace = TRUE),
    lead_tch_degree_area_simple = sample(c("Early Childhood Education", "Elementary Education", "Child Development"), n, replace = TRUE),
    lead_tch_waiver = as.integer(lead_tch_degree_level == "Waiver"),
    lead_tch_race = factor(
      sample(c("White", "Black", "Latino/Hispanic"), n, replace = TRUE),
      levels = c("White", "Black", "Latino/Hispanic", "Asian", "Mixed", "Other", "Unknown")
    ),
    lead_tch_ethnicity = factor(
      sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE),
      levels = c("Hispanic", "Non-Hispanic")
    ),
    lead_tch_gender = factor(
      sample(c("Female", "Male"), n, replace = TRUE),
      levels = c("Female", "Male")
    ),
    lead_tch_osr_exp = sample(0:30, n, replace = TRUE),
    lead_tch_total_exp = sample(0:35, n, replace = TRUE)
  )

  structure(
    list(
      data = df,
      meta = list(
        school_year = sy,
        year = year,
        format = format,
        n_classrooms = n,
        include_dob = FALSE,
        cleaned_at = Sys.time()
      )
    ),
    class = "alprek_classroom_clean"
  )
}


# ==========================================================================
# Student Module Fixtures
# ==========================================================================

#' Create a synthetic legacy-format student data frame
#' @param n Number of students to generate.
#' @return A tibble with raw Excel column names matching legacy format (202 cols).
#'   For testing, only key columns are populated; remaining assessment/
#'   service columns are included as NA.
make_student_legacy_raw_df <- function(n = 10) {
  set.seed(789)

  # -- ID columns --
  adece_ids <- sprintf("STU%06d", seq_len(n))

  classroom_codes <- paste0(
    sprintf("%03d", sample(1:67, n, replace = TRUE)),
    sample(c("P", "C", "H"), n, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n)),
    ".",
    sprintf("%02d", sample(1:3, n, replace = TRUE))
  )

  # -- Demographics --
  genders <- sample(c("M", "F"), n, replace = TRUE)
  races <- sample(c("White", "Black or African American", "Hispanic",
                     "Asian", "Mixed Heritage"), n, replace = TRUE)
  ethnicities <- sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE)
  state_ids <- sprintf("AL%08d", sample(10000000:99999999, n))
  # DOB: kids aged 3-5 for 2023-2024 school year
  dobs <- as.Date("2018-01-15") + sample(0:730, n, replace = TRUE)

  # -- Family variables --
  poverty <- sample(c("Yes", "No"), n, replace = TRUE)
  delivery <- sample(c("Public School", "Private Child Care", "Head Start",
                        "Community Organization", "Faith-Based Organization"),
                     n, replace = TRUE)
  title_one <- sample(c("Y", "N"), n, replace = TRUE)
  language <- sample(c("English", "Spanish", "Arabic", "Vietnamese"), n,
                     replace = TRUE, prob = c(0.7, 0.15, 0.1, 0.05))
  lives_with <- sample(c("Both", "Parent/guardian 1", "Parent/guardian 2", NA),
                       n, replace = TRUE, prob = c(0.5, 0.3, 0.1, 0.1))
  num_house <- sample(c("3", "4", "5", "6", "Four", "Seven", ";"), n,
                      replace = TRUE, prob = c(0.3, 0.25, 0.15, 0.1, 0.05, 0.05, 0.1))
  # Income: mix of range formats
  incomes <- sample(c("$0 - $34,840", "$34,841 - $47,110", "$47,111 - $59,380",
                       "less than $20,000", "$80,000 or more", NA),
                    n, replace = TRUE)
  pov_lvl <- sample(c("0 to 200%", "201 to 300%", "301 to 400%", NA),
                    n, replace = TRUE, prob = c(0.4, 0.2, 0.1, 0.3))
  monthly_fees <- round(runif(n, 0, 300), 2)

  # -- Services --
  yes_no <- function(n) sample(c("Yes", "No"), n, replace = TRUE)
  yn <- function(n) sample(c("Yes", "No", NA), n, replace = TRUE, prob = c(0.2, 0.7, 0.1))
  dur_svc <- function(n) sample(c("No", "Yes, less than 1 year", "Yes, 1 year",
                                   "Yes, more than 1 year", NA),
                                n, replace = TRUE, prob = c(0.5, 0.2, 0.1, 0.1, 0.1))

  # -- Attendance --
  abs1 <- sample(c(0:30, -2), n, replace = TRUE, prob = c(rep(1/31, 31), 0.05/31*31))
  abs2 <- sample(0:25, n, replace = TRUE)
  tardy1 <- sample(c(0:15, -1), n, replace = TRUE, prob = c(rep(1/16, 16), 0.05/16*16))
  tardy2 <- sample(0:10, n, replace = TRUE)

  # -- GOLD scores (literacy fall only populated for testing) --
  gold_lit_fall_raw <- sample(c(round(runif(max(n, 2), 10, 80), 0), NA), n, replace = TRUE)
  gold_lit_fall_scale <- sample(c(round(runif(max(n, 2), 200, 700), 0), NA), n, replace = TRUE)
  gold_lit_fall_whe <- sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE)
  gold_lit_fall_nn <- sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE)
  gold_lit_fall_kready <- sample(c("Emerging", "Accomplished", NA), n, replace = TRUE)

  # -- IEP referral --
  iep_outcomes <- sample(c("Found eligible for services", "Ineligible",
                            "Referral pending", "Approved for IEP", NA),
                         n, replace = TRUE)

  # Build the tibble with key columns (202 total in real data)
  df <- tibble::tibble(
    `School Year` = rep("2023-2024", n),
    `Classroom Code` = classroom_codes,
    `Classroom Name` = paste0("Classroom ", seq_len(n)),
    `Region #` = sample(1:11, n, replace = TRUE),
    `Site Code` = substr(classroom_codes, 1, nchar(classroom_codes) - 3),
    `Site Name` = paste0("Site ", seq_len(n)),
    `Program Code` = sprintf("%05d", sample(10000:99999, n)),
    `Program Name` = paste0("Program ", seq_len(n)),
    `ADECE ID` = adece_ids,
    `Gender` = genders,
    `Race` = races,
    `Ethnicity` = ethnicities,
    `State Student ID` = state_ids,
    `DOB` = dobs,
    `Poverty Indicator` = poverty,
    `Delivery Type` = delivery,
    `Title One` = title_one,
    `Language` = language,
    `Lives With` = lives_with,
    `Number In House` = num_house,
    `Gross Income` = incomes,
    `Poverty Level Percentage` = pov_lvl,
    `Monthly Fees` = monthly_fees,
    `Childcare Subsidy` = yn(n),
    `TANF` = yn(n),
    `WIC` = yn(n),
    `Free Reduced Lunch` = yn(n),
    `SNAP` = yn(n),
    `IEP` = yn(n),
    `School System Providing Services` = rep(NA, n),
    `Head Start` = dur_svc(n),
    `Center Based Childcare` = dur_svc(n),
    `Home Based Childcare` = dur_svc(n),
    `Home Visiting` = dur_svc(n),
    `Foster Care` = yn(n),
    `Homeless` = yn(n),
    `Allergies / Medical` = sample(c(NA, "Peanuts", "None"), n, replace = TRUE),
    `School Services` = rep(NA, n),
    `Intervention Services` = yn(n),
    `Behavior Support Requested` = sample(c(0, 1, 2, NA), n, replace = TRUE),
    `Class Level ID` = rep(5, n),
    `Days Absent Semester 1` = abs1,
    `Days Absent Semester 2` = abs2,
    `Days Tardy Semester 1` = tardy1,
    `Days Tardy Semester 2` = tardy2,
    `Family Hours` = rep(NA, n),
    `August Family Hours` = rep(NA, n),
    `September Family Hours` = rep(NA, n),
    `October Family Hours` = rep(NA, n),
    `November Family Hours` = rep(NA, n),
    `December Family Hours` = rep(NA, n),
    `Semester 1 Family Hours` = round(runif(n, 0, 20), 1),
    `January Family Hours` = rep(NA, n),
    `February Family Hours` = rep(NA, n),
    `March Family Hours` = rep(NA, n),
    `April Family Hours` = rep(NA, n),
    `May Family Hours` = rep(NA, n),
    `Semester 2 Family Hours` = round(runif(n, 0, 20), 1),
    `IEP Referral Date` = sample(c(as.Date("2024-01-15"), NA), n, replace = TRUE),
    `IEP Referral Outcome` = iep_outcomes,
    `Reason for Referral` = sample(c("Speech", "Developmental", NA), n, replace = TRUE),
    `Part B Entry Date` = rep(NA, n),
    `Part B Entry Tool` = rep(NA, n),
    `Enrolled Date` = as.Date("2023-08-15") + sample(0:30, n, replace = TRUE),
    `Migrant Family` = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.05, 0.95)),
    `Resides on Indian Lands` = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.02, 0.98)),
    `Military-connected Family` = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.1, 0.9)),
    # GOLD Literacy
    `GOLD Literacy Fall Raw Score` = gold_lit_fall_raw,
    `GOLD Literacy Fall Scale Score` = gold_lit_fall_scale,
    `GOLD Literacy Fall WHE Status` = gold_lit_fall_whe,
    `GOLD Literacy Fall NN Status` = gold_lit_fall_nn,
    `GOLD Literacy Fall K-Readiness` = gold_lit_fall_kready,
    `GOLD Literacy Spring Raw Score` = sample(c(round(runif(max(n, 2), 20, 90), 0), NA), n, replace = TRUE),
    `GOLD Literacy Spring Scale Score` = sample(c(round(runif(max(n, 2), 250, 750), 0), NA), n, replace = TRUE),
    `GOLD Literacy Spring WHE Status` = sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE),
    `GOLD Literacy Spring NN Status` = sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE),
    `GOLD Literacy Spring K-Readiness` = sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
    # GOLD Math (mostly NA for simplicity)
    `GOLD Math Fall Raw Score` = sample(c(round(runif(max(n, 2), 10, 50), 0), NA), n, replace = TRUE),
    `GOLD Math Fall Scale Score` = sample(c(round(runif(max(n, 2), 200, 600), 0), NA), n, replace = TRUE),
    `GOLD Math Fall WHE Status` = sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE),
    `GOLD Math Fall NN Status` = sample(c("Below", "Meet", "Exceed", NA), n, replace = TRUE),
    `GOLD Math Fall K-Readiness` = sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
    `GOLD Math Spring Raw Score` = rep(NA, n),
    `GOLD Math Spring Scale Score` = rep(NA, n),
    `GOLD Math Spring WHE Status` = rep(NA, n),
    `GOLD Math Spring NN Status` = rep(NA, n),
    `GOLD Math Spring K-Readiness` = rep(NA, n),
    # GOLD Social-Emotional
    `GOLD Social-Emotional Fall Raw Score` = rep(NA, n),
    `GOLD Social-Emotional Fall Scale Score` = rep(NA, n),
    `GOLD Social-Emotional Fall WHE Status` = rep(NA, n),
    `GOLD Social-Emotional Fall NN Status` = rep(NA, n),
    `GOLD Social-Emotional Fall K-Readiness` = rep(NA, n),
    `GOLD Social-Emotional Spring Raw Score` = rep(NA, n),
    `GOLD Social-Emotional Spring Scale Score` = rep(NA, n),
    `GOLD Social-Emotional Spring WHE Status` = rep(NA, n),
    `GOLD Social-Emotional Spring NN Status` = rep(NA, n),
    `GOLD Social-Emotional Spring K-Readiness` = rep(NA, n),
    # GOLD Physical
    `GOLD Physical Fall Raw Score` = rep(NA, n),
    `GOLD Physical Fall Scale Score` = rep(NA, n),
    `GOLD Physical Fall WHE Status` = rep(NA, n),
    `GOLD Physical Fall NN Status` = rep(NA, n),
    `GOLD Physical Fall K-Readiness` = rep(NA, n),
    `GOLD Physical Spring Raw Score` = rep(NA, n),
    `GOLD Physical Spring Scale Score` = rep(NA, n),
    `GOLD Physical Spring WHE Status` = rep(NA, n),
    `GOLD Physical Spring NN Status` = rep(NA, n),
    `GOLD Physical Spring K-Readiness` = rep(NA, n),
    # GOLD Cognitive
    `GOLD Cognitive Fall Raw Score` = rep(NA, n),
    `GOLD Cognitive Fall Scale Score` = rep(NA, n),
    `GOLD Cognitive Fall WHE Status` = rep(NA, n),
    `GOLD Cognitive Fall NN Status` = rep(NA, n),
    `GOLD Cognitive Fall K-Readiness` = rep(NA, n),
    `GOLD Cognitive Spring Raw Score` = rep(NA, n),
    `GOLD Cognitive Spring Scale Score` = rep(NA, n),
    `GOLD Cognitive Spring WHE Status` = rep(NA, n),
    `GOLD Cognitive Spring NN Status` = rep(NA, n),
    `GOLD Cognitive Spring K-Readiness` = rep(NA, n),
    # GOLD Language
    `GOLD Language Fall Raw Score` = rep(NA, n),
    `GOLD Language Fall Scale Score` = rep(NA, n),
    `GOLD Language Fall WHE Status` = rep(NA, n),
    `GOLD Language Fall NN Status` = rep(NA, n),
    `GOLD Language Fall K-Readiness` = rep(NA, n),
    `GOLD Language Spring Raw Score` = rep(NA, n),
    `GOLD Language Spring Scale Score` = rep(NA, n),
    `GOLD Language Spring WHE Status` = rep(NA, n),
    `GOLD Language Spring NN Status` = rep(NA, n),
    `GOLD Language Spring K-Readiness` = rep(NA, n),
    # PPVT
    `PPVT Fall Standard Score` = rep(NA, n),
    `PPVT Spring Standard Score` = rep(NA, n),
    `PPVT Fall Percentile Rank` = rep(NA, n),
    `PPVT Spring Percentile Rank` = rep(NA, n),
    `PPVT Fall Stanine` = rep(NA, n),
    `PPVT Spring Stanine` = rep(NA, n),
    `PPVT Fall GSV` = rep(NA, n),
    `PPVT Spring GSV` = rep(NA, n),
    `PPVT Category` = rep(NA, n),
    # eDECA Pre
    `eDECA Entry Date (Pre)` = rep(NA, n),
    `eDECA Rater Type (Pre)` = sample(c("T", "P", NA), n, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    `eDECA Rater Description (Pre)` = rep(NA, n),
    `eDECA Rating Status (Pre)` = rep(NA, n),
    `eDECA Rating Date (Pre)` = rep(NA, n),
    `eDECA Rating Period (Pre)` = rep(NA, n),
    `eDECA Initiative Raw Score (Pre)` = sample(c(round(runif(max(n, 2), 20, 60), 0), NA), n, replace = TRUE),
    `eDECA Initiative TScore (Pre)` = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    `eDECA Initiative Percentile (Pre)` = sample(c(round(runif(max(n, 2), 5, 95), 0), NA), n, replace = TRUE),
    `eDECA Initiative Description (Pre)` = rep(NA, n),
    `eDECA Self Regulation Raw Score (Pre)` = rep(NA, n),
    `eDECA Self Regulation TScore (Pre)` = rep(NA, n),
    `eDECA Self Regulation Percentile (Pre)` = rep(NA, n),
    `eDECA Self Regulation Description (Pre)` = rep(NA, n),
    `eDECA Attachment Raw Score (Pre)` = rep(NA, n),
    `eDECA Attachment TScore (Pre)` = rep(NA, n),
    `eDECA Attachment Percentile (Pre)` = rep(NA, n),
    `eDECA Attachment Description (Pre)` = rep(NA, n),
    `eDECA Total Protective Factors Raw Score (Pre)` = rep(NA, n),
    `eDECA Total Protective Factors TScore (Pre)` = rep(NA, n),
    `eDECA Total Protective Factors Percentile (Pre)` = rep(NA, n),
    `eDECA Total Protective Factors Description (Pre)` = rep(NA, n),
    `eDECA Behavior Concerns Raw Score (Pre)` = rep(NA, n),
    `eDECA Behavior Concerns TScore (Pre)` = rep(NA, n),
    `eDECA Behavior Concerns Percentile (Pre)` = rep(NA, n),
    `eDECA Behavior Concerns Description (Pre)` = rep(NA, n),
    # eDECA Post
    `eDECA Entry Date (Post)` = rep(NA, n),
    `eDECA Rater Type (Post)` = rep(NA, n),
    `eDECA Rater Description (Post)` = rep(NA, n),
    `eDECA Rating Status (Post)` = rep(NA, n),
    `eDECA Rating Date (Post)` = rep(NA, n),
    `eDECA Rating Period (Post)` = rep(NA, n),
    `eDECA Initiative Raw Score (Post)` = rep(NA, n),
    `eDECA Initiative TScore (Post)` = rep(NA, n),
    `eDECA Initiative Percentile (Post)` = rep(NA, n),
    `eDECA Initiative Description (Post)` = rep(NA, n),
    `eDECA Self Regulation Raw Score (Post)` = rep(NA, n),
    `eDECA Self Regulation TScore (Post)` = rep(NA, n),
    `eDECA Self Regulation Percentile (Post)` = rep(NA, n),
    `eDECA Self Regulation Description (Post)` = rep(NA, n),
    `eDECA Attachment Raw Score (Post)` = rep(NA, n),
    `eDECA Attachment TScore (Post)` = rep(NA, n),
    `eDECA Attachment Percentile (Post)` = rep(NA, n),
    `eDECA Attachment Description (Post)` = rep(NA, n),
    `eDECA Total Protective Factors Raw Score (Post)` = rep(NA, n),
    `eDECA Total Protective Factors TScore (Post)` = rep(NA, n),
    `eDECA Total Protective Factors Percentile (Post)` = rep(NA, n),
    `eDECA Total Protective Factors Desc (Post)` = rep(NA, n),
    `eDECA Behavior Concerns Raw Score (Post)` = rep(NA, n),
    `eDECA Behavior Concerns TScore (Post)` = rep(NA, n),
    `eDECA Behavior Concerns Percentile (Post)` = rep(NA, n),
    `eDECA Behavior Concerns Description (Post)` = rep(NA, n),
    # ASQ
    `ASQ ID` = rep(NA, n),
    `ASQ Provider` = rep(NA, n),
    `ASQ Interval` = rep(NA, n),
    `ASQ Type` = rep(NA, n),
    `ASQ Screening Date` = rep(NA, n),
    `ASQ Entry Date` = rep(NA, n),
    `ASQ Completed By` = rep(NA, n),
    `ASQ Communication Results` = sample(c("Below", "Monitoring", "Above", NA), n, replace = TRUE),
    `ASQ Gross Motor Results` = sample(c("Below", "Monitoring", "Above", NA), n, replace = TRUE),
    `ASQ Fine Motor Results` = sample(c("Below", "Monitoring", "Above", NA), n, replace = TRUE),
    `ASQ Problem Solving Results` = sample(c("Below", "Monitoring", "Above", NA), n, replace = TRUE),
    `ASQ Personal Social Results` = sample(c("Below", "Monitoring", "Above", NA), n, replace = TRUE),
    `ASQ Overall Concern Count` = sample(c(0, 1, 2, NA), n, replace = TRUE),
    `ASQ Overall No Concern Count` = sample(c(3, 4, 5, NA), n, replace = TRUE)
  )

  df
}


#' Create a synthetic new-format student data frame
#' @param n Number of students to generate.
#' @return A tibble with new format columns (270 cols). Adds child PII, guardian,
#'   staff contact, schedule, and GOLD growth columns to legacy base.
make_student_new_raw_df <- function(n = 10) {
  # Start with legacy base
  df <- make_student_legacy_raw_df(n)

  # Update school year
  df$`School Year` <- rep("2024-2025", n)

  # Add new-format child PII columns (inserted near start in real data)
  df$`Child First Name` <- paste0("First", seq_len(n))
  df$`Child Last Name` <- paste0("Last", seq_len(n))
  df$`Middle Name` <- sample(c("A", "B", NA), n, replace = TRUE)
  df$`Preferred Name` <- rep(NA, n)
  df$`Student ID` <- sprintf("%09d", sample(100000000:999999999, n))

  # Remove State Student ID (legacy only)
  df$`State Student ID` <- NULL

  # Add guardian columns
  df$`Guardian 1 First Name` <- paste0("G1First", seq_len(n))
  df$`Guardian 1 Last Name` <- paste0("G1Last", seq_len(n))
  df$`Guardian 1Phone 1` <- paste0("205-555-", sprintf("%04d", sample(1000:9999, n)))
  df$`Guardian 1Phone 2` <- rep(NA, n)
  df$`Guardian 1 Email` <- paste0("g1_", seq_len(n), "@test.com")
  df$`Guardian 1 Address 1` <- paste0(sample(100:999, n), " Main St")
  df$`Guardian 1 Address 2` <- rep(NA, n)
  df$`Guardian 1 City` <- sample(c("Birmingham", "Montgomery"), n, replace = TRUE)
  df$`Guardian 1 State` <- rep("AL", n)
  df$`Guardian 1 Zip Code` <- sprintf("%05d", sample(35000:36999, n))
  df$`Guardian 2 First Name` <- sample(c(paste0("G2First", seq_len(n)), rep(NA, n)), n)
  df$`Guardian 2 Last Name` <- sample(c(paste0("G2Last", seq_len(n)), rep(NA, n)), n)
  df$`Guardian 2Phone 1` <- rep(NA, n)
  df$`Guardian 2Phone 2` <- rep(NA, n)
  df$`Guardian 2 Email` <- rep(NA, n)
  df$`Guardian 2 Address 1` <- rep(NA, n)
  df$`Guardian 2 Address 2` <- rep(NA, n)
  df$`Guardian 2 City` <- rep(NA, n)
  df$`Guardian 2 State` <- rep(NA, n)
  df$`Guardian 2 Zip Code` <- rep(NA, n)

  # Add staff contacts
  df$`Monitor First Name` <- paste0("MonFirst", seq_len(n))
  df$`Monitor Last Name` <- paste0("MonLast", seq_len(n))
  df$`Monitor Email` <- paste0("mon", seq_len(n), "@adece.gov")
  df$`Coach First Name` <- paste0("CoachFirst", seq_len(n))
  df$`Coach Last Name` <- paste0("CoachLast", seq_len(n))
  df$`Coach Email` <- paste0("coach", seq_len(n), "@adece.gov")

  # Add schedule columns
  df$`Modified Schedule` <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.1, 0.9))
  df$`Modified Schedule Date` <- rep(NA, n)

  # Mix delivery type capitalization (realistic for 2024-25)
  df$`Delivery Type` <- sample(c("public school", "Public School",
                                  "community organization operated",
                                  "private child care", "Head Start",
                                  "faith based organization",
                                  "university operated"),
                                n, replace = TRUE)

  # Add GOLD Growth columns (all empty in real data currently)
  gold_domains <- c("Literacy", "Math", "Social-Emotional", "Physical",
                     "Cognitive", "Language")
  gold_growth_suffixes <- c("Expected Growth Amount", "Avg./Actual Growth Amount",
                             "Met Growth Status", "Fall Administration Date",
                             "Spring Administration Date",
                             "Below/Meeting/Exceeding Widely Held Expectations Status",
                             "Below/Meeting/Exceeding Nationally Normed Status")
  for (domain in gold_domains) {
    for (suffix in gold_growth_suffixes) {
      col_name <- paste0("GOLD ", domain, " ", suffix)
      df[[col_name]] <- rep(NA, n)
    }
  }

  df
}


#' Create an alprek_student_raw S3 object from synthetic data
#' @param format Either "legacy" or "new".
#' @param n Number of students.
#' @return An alprek_student_raw object.
make_student_raw <- function(format = "legacy", n = 10) {
  if (format == "legacy") {
    df <- make_student_legacy_raw_df(n)
    sy <- "2023-2024"
  } else {
    df <- make_student_new_raw_df(n)
    sy <- "2024-2025"
  }

  structure(
    list(
      data = df,
      meta = list(
        path = paste0("test_student_", format, ".xlsx"),
        school_year = sy,
        year = alprek_school_year_to_start(sy),
        format = format,
        sheet = "rptChildren_Excel",
        n_rows_raw = nrow(df),
        n_rows_clean = nrow(df),
        n_cols = ncol(df),
        col_names = names(df),
        read_at = Sys.time()
      )
    ),
    class = "alprek_student_raw"
  )
}


#' Create a pre-cleaned student object for testing downstream steps
#' @param format Either "legacy" or "new".
#' @param n Number of students.
#' @return An alprek_student_clean object with minimal cleaned data.
# ==========================================================================
# Linkage Module Fixtures
# ==========================================================================

#' Create linked test fixtures for linkage module testing
#' @description Creates budget_panel, classroom_panel, and student_panel objects
#'   with shared classroom_codes so join tests work correctly.
#' @param n_classrooms Number of classrooms (default 5).
#' @param n_students_per Number of students per classroom (default 3).
#' @return A list with `budget_panel`, `classroom_panel`, `student_panel`.
make_linkage_fixtures <- function(n_classrooms = 5, n_students_per = 3) {
  set.seed(999)

  # Generate shared classroom codes (all 3 modules will use these)
  shared_codes <- paste0(
    sprintf("%03d", sample(1:67, n_classrooms, replace = TRUE)),
    sample(c("P", "C", "H"), n_classrooms, replace = TRUE),
    sprintf("%05d", sample(10000:99999, n_classrooms)),
    ".", sprintf("%02d", sample(1:3, n_classrooms, replace = TRUE))
  )

  sy <- "2023-2024"
  year <- 2023L

  delivery_types <- sample(c("Public School", "Private Child Care", "Head Start",
                              "Community Organization"), n_classrooms, replace = TRUE)
  county_codes <- sprintf("%03d", sample(1:67, n_classrooms, replace = TRUE))
  program_codes <- sprintf("%05d", sample(10000:99999, n_classrooms))
  classroom_names <- paste0("Test Classroom ", seq_len(n_classrooms))

  # ---- Budget Panel ----
  # First classroom has no budget (orphan test)
  n_budget <- n_classrooms - 1
  budget_df <- tibble::tibble(
    school_year = rep(sy, n_budget),
    year = rep(year, n_budget),
    classroom_code = shared_codes[2:n_classrooms],
    classroom_name = classroom_names[2:n_classrooms],
    county_code = county_codes[2:n_classrooms],
    delivery_type = factor(
      delivery_types[2:n_classrooms],
      levels = c("Public School", "Private Child Care", "Head Start",
                 "Community Organization", "Faith-Based Organization",
                 "University Operated", "Private School")
    ),
    program_code = program_codes[2:n_classrooms],
    class_num = sprintf("%02d", sample(1:3, n_budget, replace = TRUE)),
    delivery_type_code = substr(shared_codes[2:n_classrooms], 4, 4),
    delivery_type_binary = ifelse(delivery_types[2:n_classrooms] == "Public School",
                                  "Public", "Non-Public"),
    delivery_type_3way = sample(c("Public", "Private", "Head Start"), n_budget, replace = TRUE),
    osr_lead_teacher_salary = round(runif(n_budget, 25000, 50000), 0),
    osr_lead_teacher_benefits = round(runif(n_budget, 5000, 15000), 0),
    osr_aux_teacher_salary = round(runif(n_budget, 15000, 30000), 0),
    osr_aux_teacher_benefits = round(runif(n_budget, 3000, 10000), 0),
    osr_instructional_support = round(runif(n_budget, 2000, 8000), 0),
    osr_operations_maintenance = round(runif(n_budget, 1000, 5000), 0),
    osr_equipment = round(runif(n_budget, 500, 3000), 0),
    osr_administrative = round(runif(n_budget, 1000, 4000), 0),
    other_lead_teacher_salary = round(runif(n_budget, 0, 3000), 0),
    other_lead_teacher_benefits = round(runif(n_budget, 0, 1000), 0),
    other_aux_teacher_salary = rep(0, n_budget),
    other_aux_teacher_benefits = rep(0, n_budget),
    other_instructional_support = round(runif(n_budget, 0, 2000), 0),
    other_operations_maintenance = rep(0, n_budget),
    other_equipment = rep(0, n_budget),
    other_administrative = rep(0, n_budget),
    osr_total = NA_real_,
    other_total = NA_real_,
    total_teacher_compensation = NA_real_,
    grand_total = NA_real_,
    share_osr = NA_real_,
    share_teacher_compensation = NA_real_
  )

  # Compute totals
  osr_cols <- grep("^osr_", names(budget_df), value = TRUE)
  osr_cols <- setdiff(osr_cols, c("osr_total"))
  other_cols <- grep("^other_", names(budget_df), value = TRUE)
  other_cols <- setdiff(other_cols, c("other_total"))

  budget_df$osr_total <- rowSums(budget_df[osr_cols], na.rm = TRUE)
  budget_df$other_total <- rowSums(budget_df[other_cols], na.rm = TRUE)
  budget_df$grand_total <- budget_df$osr_total + budget_df$other_total
  budget_df$total_teacher_compensation <- budget_df$osr_lead_teacher_salary +
    budget_df$osr_lead_teacher_benefits + budget_df$osr_aux_teacher_salary +
    budget_df$osr_aux_teacher_benefits + budget_df$other_lead_teacher_salary +
    budget_df$other_lead_teacher_benefits + budget_df$other_aux_teacher_salary +
    budget_df$other_aux_teacher_benefits
  budget_df$share_osr <- budget_df$osr_total / budget_df$grand_total
  budget_df$share_teacher_compensation <- budget_df$total_teacher_compensation /
    budget_df$grand_total

  budget_panel <- structure(
    list(
      data = budget_df,
      years = sy,
      n_years = 1L,
      by_year = stats::setNames(
        list(list(school_year = sy, format = "legacy", n_classrooms = n_budget)),
        sy
      )
    ),
    class = "alprek_budget_panel"
  )

  # ---- Classroom Panel ----
  classroom_df <- tibble::tibble(
    classroom_code = shared_codes,
    classroom_name = classroom_names,
    school_year = rep(sy, n_classrooms),
    year = rep(year, n_classrooms),
    region = sample(1:11, n_classrooms, replace = TRUE),
    county_code = county_codes,
    county_name = sample(c("Jefferson", "Montgomery", "Tuscaloosa"), n_classrooms, replace = TRUE),
    delivery_type = factor(
      delivery_types,
      levels = c("Public School", "Private Child Care", "Head Start",
                 "Community Organization", "Faith-Based Organization",
                 "University Operated", "Private School")
    ),
    program_code = program_codes,
    site_name = paste0("Site ", seq_len(n_classrooms)),
    site_code = substr(shared_codes, 1, nchar(shared_codes) - 3),
    class_num = sprintf("%02d", sample(1:3, n_classrooms, replace = TRUE)),
    title_i = sample(c("Y", "N", NA), n_classrooms, replace = TRUE),
    title_i_numeric = NA_real_,
    total_grant = round(runif(n_classrooms, 85000, 155000), 0),
    enhancement_grant = round(runif(n_classrooms, 0, 5000), 0),
    latitude = round(runif(n_classrooms, 30.2, 35.0), 6),
    longitude = round(runif(n_classrooms, -88.5, -85.0), 6),
    year_first_funded = sample(2005:2020, n_classrooms, replace = TRUE),
    class_type = rep("Full Day", n_classrooms),
    senate_dist = sample(1:35, n_classrooms, replace = TRUE),
    house_dist = sample(1:105, n_classrooms, replace = TRUE),
    seat_count = sample(c(18:22, NA), n_classrooms, replace = TRUE),
    lead_tch_name = paste0("Lead", seq_len(n_classrooms), " Teacher"),
    lead_tch_degree_raw = sample(c("Bachelor's ECE", "Master's Elementary"), n_classrooms, replace = TRUE),
    lead_tch_degree_level = factor(
      sample(c("Bachelor's degree", "Master's degree"), n_classrooms, replace = TRUE),
      levels = c("Waiver", "Doctoral degree", "Ed.S.", "Master's degree",
                 "Bachelor's degree", "Associate degree", "College Coursework")
    ),
    lead_tch_race = factor(
      sample(c("White", "Black"), n_classrooms, replace = TRUE),
      levels = c("White", "Black", "Latino/Hispanic", "Asian", "Mixed", "Other", "Unknown")
    ),
    lead_tch_gender = factor(
      sample(c("Female", "Male"), n_classrooms, replace = TRUE),
      levels = c("Female", "Male")
    ),
    lead_tch_osr_exp = sample(0:30, n_classrooms, replace = TRUE),
    lead_tch_total_exp = sample(0:35, n_classrooms, replace = TRUE),
    aux_tch_name = paste0("Aux", seq_len(n_classrooms), " Teacher"),
    aux_tch_degree_raw = sample(c("CDA", "Associate"), n_classrooms, replace = TRUE),
    aux_tch_race = factor(
      sample(c("White", "Black"), n_classrooms, replace = TRUE),
      levels = c("White", "Black", "Latino/Hispanic", "Asian", "Mixed", "Other", "Unknown")
    ),
    aux_tch_gender = factor(
      sample(c("Female", "Male"), n_classrooms, replace = TRUE),
      levels = c("Female", "Male")
    ),
    monitor_name = paste0("Monitor ", seq_len(n_classrooms)),
    monitor_email = paste0("mon", seq_len(n_classrooms), "@test.com"),
    coach_name = paste0("Coach ", seq_len(n_classrooms)),
    coach_email = paste0("coach", seq_len(n_classrooms), "@test.com")
  )

  classroom_df$title_i_numeric <- ifelse(is.na(classroom_df$title_i), NA_real_,
                                          ifelse(classroom_df$title_i == "Y", 1, 0))

  classroom_panel <- structure(
    list(
      data = classroom_df,
      years = sy,
      n_total = n_classrooms,
      by_year = stats::setNames(
        list(list(school_year = sy, format = "legacy", n_classrooms = n_classrooms)),
        sy
      ),
      imputation_log = tibble::tibble(
        classroom_code = character(), school_year = character(),
        variable = character(), imputed_value = character(), method = character()
      )
    ),
    class = "alprek_classroom_panel"
  )

  # ---- Student Panel ----
  # Last classroom has no students (empty classroom test)
  n_with_students <- n_classrooms - 1
  n_total_students <- n_with_students * n_students_per

  student_codes <- rep(shared_codes[1:n_with_students], each = n_students_per)

  student_df <- tibble::tibble(
    school_year = rep(sy, n_total_students),
    year = rep(year, n_total_students),
    classroom_code = student_codes,
    classroom_name = paste0("Classroom ", match(student_codes, shared_codes)),
    adece_id = sprintf("STU%06d", seq_len(n_total_students)),
    region_num = sample(1:11, n_total_students, replace = TRUE),
    site_code = substr(student_codes, 1, nchar(student_codes) - 3),
    site_name = paste0("Site ", match(student_codes, shared_codes)),
    program_code = program_codes[match(student_codes, shared_codes)],
    program_name = paste0("Program ", match(student_codes, shared_codes)),
    delivery_type = factor(
      delivery_types[match(student_codes, shared_codes)],
      levels = c("Public School", "Private Child Care", "Head Start",
                 "Faith-Based Organization", "Community Organization",
                 "University Operated", "Private School")
    ),
    gender = factor(sample(c("Male", "Female"), n_total_students, replace = TRUE),
                    levels = c("Male", "Female")),
    race = factor(
      sample(c("White", "Black", "Latino/Hispanic", "Asian", "Mixed"),
             n_total_students, replace = TRUE),
      levels = c("White", "Black", "Latino/Hispanic", "Asian", "Other", "Mixed", "Unknown")
    ),
    ethnicity = factor(sample(c("Hispanic", "Non-Hispanic"), n_total_students, replace = TRUE),
                       levels = c("Hispanic", "Non-Hispanic")),
    age = sample(3:5, n_total_students, replace = TRUE),
    dob = as.Date("2018-01-15") + sample(0:730, n_total_students, replace = TRUE),
    poverty_dum = sample(0:1, n_total_students, replace = TRUE),
    iep = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    iep2 = NA_integer_,
    days_absent_sem1 = round(runif(n_total_students, 0, 15), 0),
    days_absent_sem2 = round(runif(n_total_students, 0, 15), 0),
    days_absent_total = NA_real_,
    gross_income_midpoint = sample(c(17420, 40976, 53245, NA), n_total_students, replace = TRUE),
    # --- GOLD: 6 domains x (raw + scale + kready) x (fall + spring) ---
    gold_literacy_fall_raw = sample(c(round(runif(5, 10, 80), 0), NA), n_total_students, replace = TRUE),
    gold_literacy_spring_raw = sample(c(round(runif(5, 20, 90), 0), NA), n_total_students, replace = TRUE),
    gold_literacy_fall_scale = sample(c(round(runif(5, 200, 600), 0), NA), n_total_students, replace = TRUE),
    gold_literacy_spring_scale = sample(c(round(runif(5, 250, 700), 0), NA), n_total_students, replace = TRUE),
    gold_literacy_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_literacy_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),
    gold_math_fall_raw = sample(c(round(runif(5, 10, 50), 0), NA), n_total_students, replace = TRUE),
    gold_math_spring_raw = sample(c(round(runif(5, 20, 60), 0), NA), n_total_students, replace = TRUE),
    gold_math_fall_scale = sample(c(round(runif(5, 200, 500), 0), NA), n_total_students, replace = TRUE),
    gold_math_spring_scale = sample(c(round(runif(5, 250, 600), 0), NA), n_total_students, replace = TRUE),
    gold_math_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                   levels = c("Emerging", "Accomplished")),
    gold_math_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                     levels = c("Emerging", "Accomplished")),
    gold_se_fall_raw = sample(c(round(runif(5, 10, 60), 0), NA), n_total_students, replace = TRUE),
    gold_se_spring_raw = sample(c(round(runif(5, 20, 70), 0), NA), n_total_students, replace = TRUE),
    gold_se_fall_scale = sample(c(round(runif(5, 200, 500), 0), NA), n_total_students, replace = TRUE),
    gold_se_spring_scale = sample(c(round(runif(5, 250, 600), 0), NA), n_total_students, replace = TRUE),
    gold_se_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                 levels = c("Emerging", "Accomplished")),
    gold_se_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                   levels = c("Emerging", "Accomplished")),
    gold_physical_fall_raw = sample(c(round(runif(5, 10, 60), 0), NA), n_total_students, replace = TRUE),
    gold_physical_spring_raw = sample(c(round(runif(5, 20, 70), 0), NA), n_total_students, replace = TRUE),
    gold_physical_fall_scale = sample(c(round(runif(5, 200, 500), 0), NA), n_total_students, replace = TRUE),
    gold_physical_spring_scale = sample(c(round(runif(5, 250, 600), 0), NA), n_total_students, replace = TRUE),
    gold_physical_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_physical_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),
    gold_cognitive_fall_raw = sample(c(round(runif(5, 10, 60), 0), NA), n_total_students, replace = TRUE),
    gold_cognitive_spring_raw = sample(c(round(runif(5, 20, 70), 0), NA), n_total_students, replace = TRUE),
    gold_cognitive_fall_scale = sample(c(round(runif(5, 200, 500), 0), NA), n_total_students, replace = TRUE),
    gold_cognitive_spring_scale = sample(c(round(runif(5, 250, 600), 0), NA), n_total_students, replace = TRUE),
    gold_cognitive_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                        levels = c("Emerging", "Accomplished")),
    gold_cognitive_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                          levels = c("Emerging", "Accomplished")),
    gold_language_fall_raw = sample(c(round(runif(5, 10, 60), 0), NA), n_total_students, replace = TRUE),
    gold_language_spring_raw = sample(c(round(runif(5, 20, 70), 0), NA), n_total_students, replace = TRUE),
    gold_language_fall_scale = sample(c(round(runif(5, 200, 500), 0), NA), n_total_students, replace = TRUE),
    gold_language_spring_scale = sample(c(round(runif(5, 250, 600), 0), NA), n_total_students, replace = TRUE),
    gold_language_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_language_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n_total_students, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),
    # --- eDECA Pre + Post T-scores ---
    edeca_pre_initiative_tscore = sample(c(round(runif(5, 30, 70), 0), NA), n_total_students, replace = TRUE),
    edeca_post_initiative_tscore = sample(c(round(runif(5, 35, 75), 0), NA), n_total_students, replace = TRUE),
    edeca_pre_self_reg_tscore = sample(c(round(runif(5, 30, 70), 0), NA), n_total_students, replace = TRUE),
    edeca_post_self_reg_tscore = sample(c(round(runif(5, 35, 75), 0), NA), n_total_students, replace = TRUE),
    edeca_pre_attachment_tscore = sample(c(round(runif(5, 30, 70), 0), NA), n_total_students, replace = TRUE),
    edeca_post_attachment_tscore = sample(c(round(runif(5, 35, 75), 0), NA), n_total_students, replace = TRUE),
    edeca_pre_tpf_tscore = sample(c(round(runif(5, 30, 70), 0), NA), n_total_students, replace = TRUE),
    edeca_post_tpf_tscore = sample(c(round(runif(5, 35, 75), 0), NA), n_total_students, replace = TRUE),
    edeca_pre_behavior_tscore = sample(c(round(runif(5, 30, 70), 0), NA), n_total_students, replace = TRUE),
    edeca_post_behavior_tscore = sample(c(round(runif(5, 35, 75), 0), NA), n_total_students, replace = TRUE),
    # --- Service binary indicators ---
    childcare_subsidy = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    tanf = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.8, 0.1, 0.1)),
    wic = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    free_reduced_lunch = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
    snap = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    foster_care = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.95, 0.05)),
    homeless = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.95, 0.05)),
    head_start_dum = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.7, 0.3)),
    center_care_dum = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.6, 0.4)),
    english_learner = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.85, 0.15)),
    single_parent = sample(c(0L, 1L, NA), n_total_students, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    military_family = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.95, 0.05)),
    migrant_family = sample(c(0L, 1L), n_total_students, replace = TRUE, prob = c(0.97, 0.03)),
    monitor_name = paste0("Mon ", match(student_codes, shared_codes)),
    monitor_email = paste0("mon", match(student_codes, shared_codes), "@test.com"),
    coach_name = paste0("Coach ", match(student_codes, shared_codes)),
    coach_email = paste0("coach", match(student_codes, shared_codes), "@test.com")
  )

  student_df$days_absent_total <- student_df$days_absent_sem1 + student_df$days_absent_sem2
  student_df$iep2 <- student_df$iep

  student_panel <- structure(
    list(
      data = student_df,
      years = sy,
      n_total = n_total_students,
      n_unique_students = n_total_students,
      by_year = stats::setNames(
        list(list(school_year = sy, format = "legacy",
                  n_students = n_total_students, n_cols = ncol(student_df))),
        sy
      )
    ),
    class = "alprek_student_panel"
  )

  list(
    budget_panel = budget_panel,
    classroom_panel = classroom_panel,
    student_panel = student_panel,
    shared_codes = shared_codes
  )
}


make_student_clean <- function(format = "legacy", n = 10) {
  set.seed(101)
  sy <- if (format == "legacy") "2023-2024" else "2024-2025"
  year <- alprek_school_year_to_start(sy)

  adece_ids <- sprintf("STU%06d", seq_len(n))

  df <- tibble::tibble(
    school_year = rep(sy, n),
    year = rep(year, n),
    classroom_code = paste0(
      sprintf("%03d", sample(1:67, n, replace = TRUE)),
      sample(c("P", "C", "H"), n, replace = TRUE),
      sprintf("%05d", sample(10000:99999, n)),
      ".", sprintf("%02d", sample(1:3, n, replace = TRUE))
    ),
    adece_id = adece_ids,
    gender = factor(sample(c("Male", "Female"), n, replace = TRUE),
                    levels = c("Male", "Female")),
    race = factor(sample(c("White", "Black", "Latino/Hispanic", "Asian"),
                         n, replace = TRUE),
                  levels = c("White", "Black", "Latino/Hispanic", "Asian",
                             "Other", "Mixed", "Unknown")),
    ethnicity = factor(sample(c("Hispanic", "Non-Hispanic"), n, replace = TRUE),
                       levels = c("Hispanic", "Non-Hispanic")),
    age = sample(3:5, n, replace = TRUE),
    delivery_type = factor(
      sample(c("Public School", "Head Start", "Private Child Care"),
             n, replace = TRUE),
      levels = c("Public School", "Private Child Care", "Head Start",
                 "Faith-Based Organization", "Community Organization",
                 "University Operated", "Private School")
    ),
    poverty_dum = sample(0:1, n, replace = TRUE),
    iep = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
    days_absent_sem1 = round(runif(n, 0, 15), 0),
    days_absent_sem2 = round(runif(n, 0, 15), 0),
    days_absent_total = NA_real_,
    gross_income = sample(c("$0 - $34,840", "$34,841 - $47,110", NA),
                          n, replace = TRUE),
    gross_income_midpoint = sample(c(17420, 40976, NA), n, replace = TRUE),

    # --- GOLD: 6 domains x (raw + scale + kready) x (fall + spring) = 36 cols ---
    gold_literacy_fall_raw = sample(c(round(runif(max(n, 2), 10, 80), 0), NA), n, replace = TRUE),
    gold_literacy_spring_raw = sample(c(round(runif(max(n, 2), 20, 90), 0), NA), n, replace = TRUE),
    gold_literacy_fall_scale = sample(c(round(runif(max(n, 2), 200, 600), 0), NA), n, replace = TRUE),
    gold_literacy_spring_scale = sample(c(round(runif(max(n, 2), 250, 700), 0), NA), n, replace = TRUE),
    gold_literacy_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_literacy_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),
    gold_math_fall_raw = sample(c(round(runif(max(n, 2), 10, 50), 0), NA), n, replace = TRUE),
    gold_math_spring_raw = sample(c(round(runif(max(n, 2), 20, 60), 0), NA), n, replace = TRUE),
    gold_math_fall_scale = sample(c(round(runif(max(n, 2), 200, 500), 0), NA), n, replace = TRUE),
    gold_math_spring_scale = sample(c(round(runif(max(n, 2), 250, 600), 0), NA), n, replace = TRUE),
    gold_math_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                   levels = c("Emerging", "Accomplished")),
    gold_math_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                     levels = c("Emerging", "Accomplished")),
    gold_se_fall_raw = sample(c(round(runif(max(n, 2), 10, 60), 0), NA), n, replace = TRUE),
    gold_se_spring_raw = sample(c(round(runif(max(n, 2), 20, 70), 0), NA), n, replace = TRUE),
    gold_se_fall_scale = sample(c(round(runif(max(n, 2), 200, 500), 0), NA), n, replace = TRUE),
    gold_se_spring_scale = sample(c(round(runif(max(n, 2), 250, 600), 0), NA), n, replace = TRUE),
    gold_se_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                 levels = c("Emerging", "Accomplished")),
    gold_se_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                   levels = c("Emerging", "Accomplished")),
    gold_physical_fall_raw = sample(c(round(runif(max(n, 2), 10, 60), 0), NA), n, replace = TRUE),
    gold_physical_spring_raw = sample(c(round(runif(max(n, 2), 20, 70), 0), NA), n, replace = TRUE),
    gold_physical_fall_scale = sample(c(round(runif(max(n, 2), 200, 500), 0), NA), n, replace = TRUE),
    gold_physical_spring_scale = sample(c(round(runif(max(n, 2), 250, 600), 0), NA), n, replace = TRUE),
    gold_physical_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_physical_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),
    gold_cognitive_fall_raw = sample(c(round(runif(max(n, 2), 10, 60), 0), NA), n, replace = TRUE),
    gold_cognitive_spring_raw = sample(c(round(runif(max(n, 2), 20, 70), 0), NA), n, replace = TRUE),
    gold_cognitive_fall_scale = sample(c(round(runif(max(n, 2), 200, 500), 0), NA), n, replace = TRUE),
    gold_cognitive_spring_scale = sample(c(round(runif(max(n, 2), 250, 600), 0), NA), n, replace = TRUE),
    gold_cognitive_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                        levels = c("Emerging", "Accomplished")),
    gold_cognitive_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                          levels = c("Emerging", "Accomplished")),
    gold_language_fall_raw = sample(c(round(runif(max(n, 2), 10, 60), 0), NA), n, replace = TRUE),
    gold_language_spring_raw = sample(c(round(runif(max(n, 2), 20, 70), 0), NA), n, replace = TRUE),
    gold_language_fall_scale = sample(c(round(runif(max(n, 2), 200, 500), 0), NA), n, replace = TRUE),
    gold_language_spring_scale = sample(c(round(runif(max(n, 2), 250, 600), 0), NA), n, replace = TRUE),
    gold_language_fall_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                       levels = c("Emerging", "Accomplished")),
    gold_language_spring_kready = factor(sample(c("Emerging", "Accomplished", NA), n, replace = TRUE),
                                         levels = c("Emerging", "Accomplished")),

    # --- eDECA Pre + Post T-scores (5 constructs x 2 phases = 10 cols) ---
    edeca_pre_initiative_tscore = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    edeca_post_initiative_tscore = sample(c(round(runif(max(n, 2), 35, 75), 0), NA), n, replace = TRUE),
    edeca_pre_self_reg_tscore = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    edeca_post_self_reg_tscore = sample(c(round(runif(max(n, 2), 35, 75), 0), NA), n, replace = TRUE),
    edeca_pre_attachment_tscore = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    edeca_post_attachment_tscore = sample(c(round(runif(max(n, 2), 35, 75), 0), NA), n, replace = TRUE),
    edeca_pre_tpf_tscore = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    edeca_post_tpf_tscore = sample(c(round(runif(max(n, 2), 35, 75), 0), NA), n, replace = TRUE),
    edeca_pre_behavior_tscore = sample(c(round(runif(max(n, 2), 30, 70), 0), NA), n, replace = TRUE),
    edeca_post_behavior_tscore = sample(c(round(runif(max(n, 2), 35, 75), 0), NA), n, replace = TRUE),

    # --- Service binary indicators ---
    childcare_subsidy = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    tanf = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.8, 0.1, 0.1)),
    wic = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    free_reduced_lunch = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.4, 0.5, 0.1)),
    snap = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    foster_care = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.95, 0.05)),
    homeless = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.95, 0.05)),
    head_start_dum = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.7, 0.3)),
    center_care_dum = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.6, 0.4)),
    english_learner = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.85, 0.15)),
    single_parent = sample(c(0L, 1L, NA), n, replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    military_family = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.95, 0.05)),
    migrant_family = sample(c(0L, 1L), n, replace = TRUE, prob = c(0.97, 0.03))
  )

  df$days_absent_total <- df$days_absent_sem1 + df$days_absent_sem2

  structure(
    list(
      data = df,
      meta = list(
        school_year = sy,
        year = year,
        format = format,
        n_students = n,
        n_cols = ncol(df),
        cleaned_at = Sys.time()
      ),
      cleaning_log = list(
        n_negative_absent = 0L,
        n_extreme_absent = 0L,
        n_negative_tardy = 0L,
        n_income_parsed = 7L,
        n_num_house_text = 1L,
        n_delivery_type_standardized = 0L,
        n_pii_removed = 0L
      )
    ),
    class = "alprek_student_clean"
  )
}

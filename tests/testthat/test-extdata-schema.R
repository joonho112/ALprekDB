test_that("all expected extdata CSV files are present", {
  ext_dir <- system.file("extdata", package = "ALprekDB", mustWork = TRUE)
  csv_files <- sort(gsub(paste0("^", ext_dir, "/"), "",
                         list.files(ext_dir, pattern = "\\.csv$",
                                    recursive = TRUE, full.names = TRUE)))

  expected <- sort(c(
    # Geocode module (v0.8.0)
    "codebooks/geocode_al_fips_counties.csv",
    "codebooks/geocode_column_map_melissa_v1.csv",
    "codebooks/geocode_edge_cases.csv",
    "codebooks/geocode_source_manifest.csv",
    "codebooks/melissa_errorcode_codes.csv",
    "codebooks/melissa_resultcode_codes.csv",
    "codebooks/melissa_statuscode_codes.csv",
    # Applications module (v0.7.0)
    "codebooks/applications_edge_cases.csv",
    "codebooks/applications_funding_types.csv",
    "codebooks/applications_source_manifest.csv",
    "codebooks/applications_status_codes.csv",
    "mappings/applications_column_map_capacity_cycle1.csv",
    "mappings/applications_column_map_new_cycle1.csv",
    "mappings/applications_column_map_nonrenewals_cycle1.csv",
    "mappings/applications_column_map_renewals_cycle1.csv",
    # Pre-existing modules (v0.6.0)
    "codebooks/budget_category_groups.csv",
    "codebooks/classroom_degree_patterns.csv",
    "codebooks/classroom_language_mapping.csv",
    "codebooks/classroom_race_mapping.csv",
    "codebooks/county_codes.csv",
    "codebooks/delivery_type_codes.csv",
    "codebooks/student_delivery_type_mapping.csv",
    "codebooks/student_race_mapping.csv",
    "mappings/budget_column_map_legacy.csv",
    "mappings/budget_column_map_new.csv",
    "mappings/classroom_column_map_legacy.csv",
    "mappings/classroom_column_map_new.csv",
    "mappings/student_column_map_legacy.csv",
    "mappings/student_column_map_new.csv"
  ))

  expect_setequal(csv_files, expected)
})


test_that("column mapping files have stable schema and keys", {
  maps <- list(
    budget_legacy = list(data = .load_column_map("legacy"), n = 6,
                         types = c("id", "total"),
                         required = c("classroom_name", "classroom_code", "budget_version")),
    budget_new = list(data = .load_column_map("new"), n = 12,
                      types = c("id", "total"),
                      required = c("classroom_name", "classroom_code", "budget_version")),
    classroom_legacy = list(data = .load_classroom_column_map("legacy"), n = 100,
                            types = c("admin", "contact", "grant", "id", "location",
                                      "school_char", "staff", "teacher_aux",
                                      "teacher_lead", "teacher_second_aux"),
                            required = c("classroom_name", "classroom_code",
                                         "school_year", "delivery_type", "county_name")),
    classroom_new = list(data = .load_classroom_column_map("new"), n = 125,
                         types = c("admin", "contact", "grant", "id", "location",
                                   "school_char", "staff", "staff_pii",
                                   "teacher_aux", "teacher_lead", "teacher_pii",
                                   "teacher_second_aux"),
                         required = c("classroom_name", "classroom_code",
                                      "school_year", "delivery_type", "county_name",
                                      "classroom_code_formula", "seat_count")),
    student_legacy = list(data = .load_student_column_map("legacy"), n = 202,
                          types = c("asq", "attendance", "demographic_flags",
                                    "demographics", "edeca_post", "edeca_pre",
                                    "enrollment", "family", "gold", "id",
                                    "iep_referral", "ppvt", "services"),
                          required = c("school_year", "classroom_code",
                                       "classroom_name", "adece_id", "gender",
                                       "race", "delivery_type")),
    student_new = list(data = .load_student_column_map("new"), n = 270,
                       types = c("asq", "attendance", "child_pii",
                                 "demographic_flags", "demographics", "edeca_post",
                                 "edeca_pre", "enrollment", "family", "gold",
                                 "gold_growth", "guardian_pii", "id",
                                 "iep_referral", "ppvt", "schedule", "services",
                                 "staff_contact"),
                       required = c("school_year", "classroom_code",
                                    "classroom_name", "adece_id", "gender",
                                    "race", "delivery_type", "child_first_name",
                                    "student_id"))
  )

  for (name in names(maps)) {
    df <- maps[[name]]$data

    expect_named(df, c("raw_column", "standard_name", "type", "notes"),
                 info = name)
    expect_equal(nrow(df), maps[[name]]$n, info = name)
    expect_false(any(is.na(df$raw_column) | trimws(df$raw_column) == ""),
                 info = paste(name, "raw_column"))
    expect_false(any(is.na(df$standard_name) | trimws(df$standard_name) == ""),
                 info = paste(name, "standard_name"))
    expect_false(any(is.na(df$type) | trimws(df$type) == ""),
                 info = paste(name, "type"))
    expect_false(any(duplicated(df$raw_column)), info = paste(name, "raw_column"))
    expect_false(any(duplicated(df$standard_name)), info = paste(name, "standard_name"))
    expect_true(all(grepl("^[a-z][a-z0-9_]*$", df$standard_name)),
                info = paste(name, "standard_name naming"))
    expect_equal(sort(unique(df$type)), sort(maps[[name]]$types),
                 info = paste(name, "type"))
    expect_true(all(maps[[name]]$required %in% df$standard_name), info = name)
  }
})


test_that("codebook files have required schemas and unique keys", {
  codebooks <- list(
    budget_category_groups = list(
      data = alprek_category_groups(),
      columns = c("category_detail", "category_group", "notes"),
      key = "category_detail",
      n = 38,
      nonblank = c("category_detail", "category_group")
    ),
    delivery_type_codes = list(
      data = alprek_delivery_types(),
      columns = c("code", "name", "name_short"),
      key = "code",
      n = 7,
      nonblank = c("code", "name", "name_short")
    ),
    county_codes = list(
      data = alprek_county_codes(),
      columns = c("county_code", "county_name", "fips_code"),
      key = "county_code",
      n = 67,
      nonblank = c("county_code", "county_name", "fips_code")
    ),
    classroom_degree_patterns = list(
      data = alprek_degree_patterns(),
      columns = c("pattern_type", "regex", "result", "priority",
                  "teacher_role", "notes"),
      key = "regex",
      n = 27,
      nonblank = c("pattern_type", "regex", "result", "priority", "teacher_role")
    ),
    classroom_race_mapping = list(
      data = alprek_race_mapping(),
      columns = c("raw_value", "standardized", "factor_order"),
      key = "raw_value",
      n = 16,
      nonblank = c("raw_value", "standardized", "factor_order")
    ),
    classroom_language_mapping = list(
      data = alprek_language_mapping(),
      columns = c("raw_value", "standardized", "is_null"),
      key = "raw_value",
      n = 39,
      nonblank = c("raw_value", "is_null")
    ),
    student_race_mapping = list(
      data = alprek_student_race_mapping(),
      columns = c("raw_value", "standardized", "factor_order"),
      key = "raw_value",
      n = 15,
      nonblank = c("raw_value", "standardized", "factor_order")
    ),
    student_delivery_type_mapping = list(
      data = alprek_student_delivery_mapping(),
      columns = c("raw_value", "standardized"),
      key = "raw_value",
      n = 16,
      nonblank = c("raw_value", "standardized")
    )
  )

  for (name in names(codebooks)) {
    df <- codebooks[[name]]$data
    key <- codebooks[[name]]$key

    expect_named(df, codebooks[[name]]$columns, info = name)
    expect_equal(nrow(df), codebooks[[name]]$n, info = name)
    expect_false(any(is.na(df[[key]]) | trimws(as.character(df[[key]])) == ""),
                 info = paste(name, key))
    expect_false(any(duplicated(df[[key]])), info = paste(name, key))
    for (col in codebooks[[name]]$nonblank) {
      expect_false(any(is.na(df[[col]]) | trimws(as.character(df[[col]])) == ""),
                   info = paste(name, col))
    }
  }
})


test_that("codebook values cover canonical factor domains", {
  delivery <- alprek_delivery_types()
  expect_setequal(delivery$code, c("P", "C", "H", "O", "F", "U", "S"))

  expected_delivery <- c("Public School", "Private Child Care", "Head Start",
                         "Community Organization", "Faith-Based Organization",
                         "University Operated", "Private School")
  expect_setequal(delivery$name, expected_delivery)
  expect_setequal(names(.delivery_type_map()), delivery$code)
  expect_equal(unname(.delivery_type_map()[delivery$code]), delivery$name)

  expected_race <- c("White", "Black", "Latino/Hispanic", "Asian",
                     "Mixed", "Other", "Unknown")
  expect_setequal(unique(alprek_race_mapping()$standardized), expected_race)
  expect_setequal(unique(alprek_student_race_mapping()$standardized), expected_race)

  category_groups <- c(
    "lead_teacher_salary", "lead_teacher_benefits",
    "aux_teacher_salary", "aux_teacher_benefits", "payroll_taxes",
    "instructional_support", "operations_and_maintenance",
    "equipment", "administrative"
  )
  expect_setequal(unique(alprek_category_groups()$category_group), category_groups)
  expect_setequal(unique(alprek_student_delivery_mapping()$standardized),
                  expected_delivery)
})


test_that("codebook values have semantic integrity", {
  counties <- alprek_county_codes()
  expect_true(all(grepl("^\\d{3}$", counties$county_code)))
  expect_true(all(grepl("^01\\d{3}$", counties$fips_code)))
  expect_false(any(duplicated(counties$fips_code)))

  degree_patterns <- alprek_degree_patterns()
  expect_setequal(unique(degree_patterns$pattern_type),
                  c("degree_level", "degree_area",
                    "degree_area_consolidation"))
  expect_true(all(!is.na(degree_patterns$priority)))
  expect_true(all(degree_patterns$teacher_role == "all"))
  regex_ok <- vapply(degree_patterns$regex, function(rx) {
    tryCatch({
      grepl(rx, "", perl = TRUE)
      TRUE
    }, error = function(e) FALSE)
  }, logical(1))
  expect_true(all(regex_ok), info = "degree pattern regexes compile")

  race_maps <- list(
    classroom = alprek_race_mapping(),
    student = alprek_student_race_mapping()
  )
  for (name in names(race_maps)) {
    map <- race_maps[[name]]
    per_domain <- tapply(map$factor_order, map$standardized, function(x) {
      length(unique(x))
    })
    expect_true(all(per_domain == 1), info = paste(name, "factor_order"))
  }

  languages <- alprek_language_mapping()
  null_flag <- as.logical(languages$is_null)
  expect_false(any(is.na(null_flag)))
  expect_true(all(is.na(languages$standardized[null_flag]) |
                    trimws(languages$standardized[null_flag]) == ""))
  expect_false(any(is.na(languages$standardized[!null_flag]) |
                     trimws(languages$standardized[!null_flag]) == ""))
})


test_that("geocode codebooks have semantic integrity", {
  rc <- alprek_geocode_resultcode_meaning()
  tier <- stats::setNames(rc$precision_tier, rc$code)
  expect_equal(unname(tier["GS01"]), "zip4")
  expect_equal(unname(tier["GS03"]), "zip5")
  expect_equal(unname(tier["GS05"]), "rooftop")
  expect_equal(unname(tier["GS06"]), "parcel")

  acceptable <- stats::setNames(rc$acceptable_for_master, rc$code)
  expect_setequal(names(acceptable)[acceptable], c("GS01", "GS05", "GS06"))
  expect_false(any(is.na(rc$acceptable_for_master)))

  sc <- alprek_geocode_statuscode_meaning()
  expect_false(any(duplicated(sc$code)))
  expect_false(any(is.na(sc$paired_resultcode_in_v080) |
                     !nzchar(sc$paired_resultcode_in_v080)))
  expect_true(all(sc$paired_resultcode_in_v080 %in%
                    rc$code[rc$observed_in_v080_input]))

  manifest <- alprek_geocode_source_manifest()
  expect_equal(as.character(manifest$delivery_date[1]), "2026-03-04")
  expect_match(manifest$example_path[1], "2026-03-04")
  expect_equal(manifest$n_cols_expected[1], 29L)

  al_fips <- alprek_geocode_al_fips_counties()
  expect_equal(nrow(al_fips), 67L)
  expect_true(all(grepl("^01\\d{3}$", al_fips$fips_full)))
  expect_false(any(duplicated(al_fips$fips_full)))
})


test_that("column maps cover assessment and teacher domains", {
  classroom_new <- .load_classroom_column_map("new")
  teacher_fields <- c(
    "lead_tch_degree_raw", "lead_tch_race_raw", "lead_tch_ethnicity_raw",
    "lead_tch_gender_raw", "lead_tch_fluent_lang_raw",
    "aux_tch_degree_raw", "aux_tch_race_raw", "aux_tch_ethnicity_raw",
    "aux_tch_gender_raw", "aux_tch_fluent_lang_raw",
    "second_aux_degree_raw", "second_aux_race_raw",
    "second_aux_ethnicity_raw", "second_aux_gender_raw",
    "second_aux_fluent_lang_raw"
  )
  expect_true(all(teacher_fields %in% classroom_new$standard_name))

  domains <- c("literacy", "math", "se", "physical", "cognitive", "language")
  gold_score_fields <- as.vector(outer(
    domains,
    c("fall_raw", "fall_scale", "fall_whe", "fall_nn", "fall_kready",
      "spring_raw", "spring_scale", "spring_whe", "spring_nn",
      "spring_kready"),
    function(domain, suffix) paste0("gold_", domain, "_", suffix)
  ))
  student_legacy <- .load_student_column_map("legacy")
  student_new <- .load_student_column_map("new")
  expect_true(all(gold_score_fields %in% student_legacy$standard_name))
  expect_true(all(gold_score_fields %in% student_new$standard_name))

  gold_growth_fields <- as.vector(outer(
    domains,
    c("growth_start", "growth_end", "growth_amount", "growth_range",
      "growth_type", "growth_pct"),
    function(domain, suffix) paste0("gold_", domain, "_", suffix)
  ))
  expect_true(all(gold_growth_fields %in% student_new$standard_name))

  edeca_domains <- c("initiative", "self_reg", "attachment", "tpf", "behavior")
  edeca_fields <- as.vector(outer(
    c("pre", "post"),
    edeca_domains,
    function(wave, domain) paste0("edeca_", wave, "_", domain, "_tscore")
  ))
  expect_true(all(edeca_fields %in% student_legacy$standard_name))
  expect_true(all(edeca_fields %in% student_new$standard_name))
})

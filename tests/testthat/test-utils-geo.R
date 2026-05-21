# tests/testthat/test-utils-geo.R
#
# Tests for alprek_haversine_m() / .geocode_haversine_m(): vectorized
# haversine great-circle distance utility used by the geocode reconciliation
# pipeline (Step 4.x) to compare ADECE-supplied vs Melissa-returned site
# coordinates.
#
# Reference coordinates used below are city centroids (approximate); known
# great-circle distances are taken from public references. Tolerances are
# loose enough that any reasonable Earth radius / haversine variant passes,
# but tight enough that gross bugs (wrong units, swapped lat/lon, missing
# degree-to-radian conversion) fail loudly.


# ===========================================================================
# Assertion 1: Identical points return 0
# ===========================================================================
test_that("identical points return distance 0", {
  # Use a few AL-ish coordinates to make sure we are not accidentally
  # short-circuiting only at the origin.
  d <- alprek_haversine_m(
    lat1 = c(33.5186,  30.6954,  0),
    lon1 = c(-86.8104, -88.0399, 0),
    lat2 = c(33.5186,  30.6954,  0),
    lon2 = c(-86.8104, -88.0399, 0)
  )
  expect_length(d, 3L)
  expect_true(all(abs(d) < 1e-6))
})


# ===========================================================================
# Assertion 2: Birmingham <-> Mobile is ~335 km (+/- 5 km)
# ===========================================================================
test_that("Birmingham <-> Mobile is ~335 km", {
  # Birmingham, AL: 33.5186, -86.8104
  # Mobile, AL:     30.6954, -88.0399
  # Haversine great-circle distance with mean R = 6,371,000 m is ~334.6 km.
  # (Driving distance is ~410 km; "320 km" is sometimes quoted for different
  # endpoint pairs. The haversine identity on these exact coordinates is
  # 334.6 km, which we verify here.)
  d <- alprek_haversine_m(33.5186, -86.8104, 30.6954, -88.0399)
  expect_length(d, 1L)
  expect_true(is.numeric(d))
  expect_true(abs(d - 335000) < 5000)  # within 5 km of 335 km
})


# ===========================================================================
# Assertion 3: Tuscaloosa <-> Birmingham is ~80 km (+/- 2 km)
# ===========================================================================
test_that("Tuscaloosa <-> Birmingham is ~80 km", {
  # Tuscaloosa, AL: 33.2098, -87.5692
  # Birmingham, AL: 33.5186, -86.8104
  # Published great-circle distance ~80 km.
  d <- alprek_haversine_m(33.2098, -87.5692, 33.5186, -86.8104)
  expect_true(abs(d - 80000) < 2000)  # within 2 km of 80 km
})


# ===========================================================================
# Assertion 4: Antipodal points are ~pi * R apart (+/- 100 m)
# ===========================================================================
test_that("antipodal points are ~pi * R apart", {
  # (lat=0, lon=0) and (lat=0, lon=180) are exact antipodes on the equator.
  # Great-circle distance = pi * R with R = 6,371,000 m.
  d <- alprek_haversine_m(0, 0, 0, 180)
  expect_true(abs(d - pi * 6371000) < 100)  # within 100 m
})


# ===========================================================================
# Assertion 5: Vectorized -- length-3 input returns length-3 output
# ===========================================================================
test_that("function is vectorized over equal-length inputs", {
  d <- alprek_haversine_m(
    lat1 = c(33.5186, 33.2098, 0),
    lon1 = c(-86.8104, -87.5692, 0),
    lat2 = c(30.6954, 33.5186, 0),
    lon2 = c(-88.0399, -86.8104, 180)
  )
  expect_length(d, 3L)
  expect_true(is.numeric(d))
  # Element 1: Birmingham <-> Mobile ~335 km (haversine on stated coords)
  expect_true(abs(d[1] - 335000) < 5000)
  # Element 2: Tuscaloosa <-> Birmingham ~80 km
  expect_true(abs(d[2] - 80000) < 2000)
  # Element 3: antipodal points ~pi*R
  expect_true(abs(d[3] - pi * 6371000) < 100)
})


# ===========================================================================
# Assertion 6: NA input -> NA output (defensive)
# ===========================================================================
test_that("NA inputs propagate to NA outputs", {
  d <- alprek_haversine_m(
    lat1 = c(33.5186, NA_real_, 33.2098, NA_real_),
    lon1 = c(-86.8104, -86.8104, NA_real_, NA_real_),
    lat2 = c(30.6954, 30.6954, 33.5186, 33.5186),
    lon2 = c(-88.0399, -88.0399, -86.8104, -86.8104)
  )
  expect_length(d, 4L)
  expect_false(is.na(d[1]))             # both points known -> finite
  expect_true(is.na(d[2]))              # lat1 NA -> NA
  expect_true(is.na(d[3]))              # lon1 NA -> NA
  expect_true(is.na(d[4]))              # both NA -> NA
})


# ===========================================================================
# Assertion 7: Matches geosphere::distHaversine() within 1 m (when available)
# ===========================================================================
test_that("matches geosphere::distHaversine() within 1 m on AL-bound points", {
  skip_if_not_installed("geosphere")

  # 10 random pairs of points loosely bounded inside / near Alabama.
  # AL spans roughly lat [30.2, 35.0], lon [-88.5, -84.9].
  set.seed(20260520L)
  n <- 10L
  lat1 <- stats::runif(n, 30.2, 35.0)
  lon1 <- stats::runif(n, -88.5, -84.9)
  lat2 <- stats::runif(n, 30.2, 35.0)
  lon2 <- stats::runif(n, -88.5, -84.9)

  ours <- alprek_haversine_m(lat1, lon1, lat2, lon2)
  ref  <- geosphere::distHaversine(
    cbind(lon1, lat1), cbind(lon2, lat2), r = 6371000
  )
  expect_length(ours, n)
  expect_true(max(abs(ours - ref)) < 1.0)  # within 1 m
})


# ===========================================================================
# Assertion 8: Argument length mismatch -> error
# ===========================================================================
test_that("argument length mismatch errors out", {
  # lat1 length-3, lon1 length-2
  expect_error(
    alprek_haversine_m(
      lat1 = c(33, 34, 35),
      lon1 = c(-86, -87),
      lat2 = c(33, 34, 35),
      lon2 = c(-86, -87, -88)
    )
  )
  # lat1 length-3, lat2 length-2
  expect_error(
    alprek_haversine_m(
      lat1 = c(33, 34, 35),
      lon1 = c(-86, -87, -88),
      lat2 = c(33, 34),
      lon2 = c(-86, -87)
    )
  )
})


# ===========================================================================
# Assertion 9 (bonus): Internal alias .geocode_haversine_m() agrees
# ===========================================================================
test_that(".geocode_haversine_m() internal alias produces identical results", {
  d_pub <- alprek_haversine_m(
    lat1 = c(33.5186, 33.2098),
    lon1 = c(-86.8104, -87.5692),
    lat2 = c(30.6954, 33.5186),
    lon2 = c(-88.0399, -86.8104)
  )
  d_int <- ALprekDB:::.geocode_haversine_m(
    lat1 = c(33.5186, 33.2098),
    lon1 = c(-86.8104, -87.5692),
    lat2 = c(30.6954, 33.5186),
    lon2 = c(-88.0399, -86.8104)
  )
  expect_identical(d_pub, d_int)
})


# ===========================================================================
# Assertion 10 (bonus): Empty input -> empty numeric output
# ===========================================================================
test_that("length-0 input returns length-0 numeric output", {
  d <- alprek_haversine_m(
    lat1 = numeric(0), lon1 = numeric(0),
    lat2 = numeric(0), lon2 = numeric(0)
  )
  expect_length(d, 0L)
  expect_true(is.numeric(d))
})


# ===========================================================================
# Assertion 11 (bonus): Vendored fallback path matches geosphere path
# ===========================================================================
# Even when geosphere is available, we want to confirm the vendored fallback
# produces identical-to-mm results. We force the fallback branch by
# temporarily masking requireNamespace() inside the function's environment.
test_that("vendored fallback path agrees with geosphere path within 1 m", {
  skip_if_not_installed("geosphere")

  # Compute via the public function (geosphere path).
  d_geosphere <- alprek_haversine_m(
    lat1 = c(33.5186, 33.2098, 0),
    lon1 = c(-86.8104, -87.5692, 0),
    lat2 = c(30.6954, 33.5186, 0),
    lon2 = c(-88.0399, -86.8104, 180)
  )

  # Build a copy of alprek_haversine_m() with requireNamespace masked to
  # always return FALSE, forcing the vendored branch.
  fn <- alprek_haversine_m
  env <- new.env(parent = environment(fn))
  env$requireNamespace <- function(...) FALSE
  environment(fn) <- env

  d_fallback <- fn(
    lat1 = c(33.5186, 33.2098, 0),
    lon1 = c(-86.8104, -87.5692, 0),
    lat2 = c(30.6954, 33.5186, 0),
    lon2 = c(-88.0399, -86.8104, 180)
  )

  expect_length(d_fallback, 3L)
  expect_true(max(abs(d_fallback - d_geosphere)) < 1.0)
})

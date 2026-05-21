# Geographic distance utilities for ALprekDB.
#
# Provides a vectorized haversine great-circle distance function used by the
# geocode reconciliation pipeline (Step 4.x) to compare ADECE-supplied and
# Melissa-returned coordinates for the same site. Exposed publicly as
# alprek_haversine_m() for downstream user convenience and aliased
# internally as .geocode_haversine_m().


#' Haversine Great-Circle Distance in Meters
#'
#' @description
#' Vectorized great-circle distance between pairs of points on the surface of
#' a sphere using the haversine formula. Uses mean Earth radius 6,371,000 m.
#'
#' If the `geosphere` package is available, the function delegates to
#' [geosphere::distHaversine()] as a sanity-checked implementation (geosphere
#' uses a slightly different mean radius and a numerically stable form);
#' otherwise it falls back to a vendored haversine that uses
#' `2 * R * asin(min(1, sqrt(a)))` to remain numerically stable near
#' antipodal points.
#'
#' @param lat1 Numeric vector. Latitude of point 1, in decimal degrees.
#' @param lon1 Numeric vector. Longitude of point 1, in decimal degrees.
#' @param lat2 Numeric vector. Latitude of point 2, in decimal degrees.
#' @param lon2 Numeric vector. Longitude of point 2, in decimal degrees.
#'
#' @details
#' All four arguments must have the same length. `NA` inputs propagate to
#' `NA` outputs. The function does not validate that latitudes are in
#' `[-90, 90]` or longitudes in `[-180, 180]`; callers should pre-validate
#' if strict bounds are required.
#'
#' @return Numeric vector of distances in meters, same length as the inputs.
#'
#' @examples
#' # Birmingham, AL <-> Mobile, AL is ~320 km
#' alprek_haversine_m(33.5186, -86.8104, 30.6954, -88.0399)
#'
#' # Vectorized over multiple pairs
#' alprek_haversine_m(
#'   lat1 = c(33.5186, 33.2098),
#'   lon1 = c(-86.8104, -87.5692),
#'   lat2 = c(30.6954, 33.5186),
#'   lon2 = c(-88.0399, -86.8104)
#' )
#'
#' @export
alprek_haversine_m <- function(lat1, lon1, lat2, lon2) {
  stopifnot(
    length(lat1) == length(lon1),
    length(lat2) == length(lon2),
    length(lat1) == length(lat2)
  )

  # Empty-input fast path (length-0 inputs return length-0 output).
  if (length(lat1) == 0L) return(numeric(0))

  # Sanity path: if geosphere is installed, delegate to its implementation.
  # geosphere expects (lon, lat) order in 2-column matrices and uses
  # WGS84-derived constants by default.
  if (requireNamespace("geosphere", quietly = TRUE)) {
    p1 <- cbind(as.numeric(lon1), as.numeric(lat1))
    p2 <- cbind(as.numeric(lon2), as.numeric(lat2))
    # Force the same mean radius we document so the two paths agree.
    return(as.numeric(geosphere::distHaversine(p1, p2, r = 6371000)))
  }

  # Vendored haversine fallback.
  R <- 6371000
  to_rad <- function(d) d * pi / 180
  phi1 <- to_rad(lat1)
  phi2 <- to_rad(lat2)
  dphi <- to_rad(lat2 - lat1)
  dlam <- to_rad(lon2 - lon1)
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}


# Internal alias for haversine distance.
#
# Thin internal alias of alprek_haversine_m() used inside the geocode
# reconciliation pipeline. Kept stable so internal callers can be refactored
# without touching the public name. Plain comment (not roxygen) to avoid
# generating an .Rd file for an unexported helper.
.geocode_haversine_m <- function(lat1, lon1, lat2, lon2) {
  alprek_haversine_m(lat1, lon1, lat2, lon2)
}

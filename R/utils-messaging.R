# Consistent CLI-based messaging across the package
# Internal functions -- not exported

#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom glue glue
NULL

msg_info <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_info(glue::glue(msg, .envir = .envir))
}

msg_success <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_success(glue::glue(msg, .envir = .envir))
}

msg_warn <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_warning(glue::glue(msg, .envir = .envir))
}

msg_error <- function(msg, .envir = parent.frame()) {
  cli::cli_alert_danger(glue::glue(msg, .envir = .envir))
}

msg_step <- function(step_num, total, description) {
  cli::cli_alert_info("Step {step_num}/{total}: {description}")
}

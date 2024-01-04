# Standalone file: do not edit by hand
# Source: <https://github.com/elipousson/standaloner/blob/main/R/standalone-settoken.R>
# ----------------------------------------------------------------------
#
# ---
# repo: elipousson/standaloner
# file: standalone-settoken.R
# last-updated: 2023-10-10
# license: https://opensource.org/license/mit/
# imports: [rlang (>= 1.0.0), cli (>= 2.5.0)]
# ---
#
# ## Changelog
#
# 2023-10-10:
# * Rename package from settoken to more general name: standaloner.
#
# 2023-08-13:
# * Create file with `set_r_environ_token()` and `get_r_environ_token()`
#
# nocov start
#
# set_r_environ_token is based on the MIT-licensed tidycensus::census_api_key()
# function.
#
# <https://github.com/walkerke/tidycensus/blob/master/LICENSE>
#
# YEAR: 2017
# COPYRIGHT HOLDER: Kyle Walker
#
#' Set or get a token from your  `.Renviron` file
#'
#' @author Kyle Walker \email{kyle@walker-data.com}
#'
#'   Eli Pousson \email{eli.pousson@gmail.com}
#'   ([ORCID](https://orcid.org/0000-0001-8280-1706))
#'
#' [set_r_environ_token()] can set an API key or personal access token (PAT) as
#' a local environment variable temporarily for the current session or saved for
#' future sessions.
#'
#' [get_r_environ_token()] can return an environment variable or error if the
#' token is missing or if the token does not match a supplied pattern.
#'
#' @param token A personal access token, API key, or other environment variable.
#'   Optional for [get_r_environ_token()].
#' @param install If `TRUE`, this function adds your token to your `.Renviron`
#'   for use in future sessions. Defaults to `FALSE`.
#' @param overwrite If `TRUE`, overwrite any existing token in `.Renviron` using
#'   the same environment variable name. Defaults to `FALSE`.
#' @param default Default name used for environment variable where the token
#'   is saved.
#' @param quiet If `TRUE`, suppress messages when setting token by locally
#'   setting the `cli.default_handler` option to [suppressMessages()]. Defaults
#'   to `FALSE`.
#' @inheritParams rlang::args_error_context
#' @returns [set_r_environ_token()] invisibly returns a string supplied to
#'   `token`.
#'
#' @source Adapted from the [tidycensus](https://walker-data.com/tidycensus/)
#'   function [tidycensus::census_api_key()].
#'
#' @keywords internal
#'
#' @importFrom rlang caller_env is_true local_options current_env is_false
#'   caller_call call_name is_null
#' @importFrom cli cli_bullets cli_alert_success
#' @importFrom utils read.table write.table
set_r_environ_token <- function(token,
                                install = FALSE,
                                overwrite = FALSE,
                                default = "TOKEN",
                                quiet = FALSE,
                                call = caller_env()) {
  if (is_true(quiet)) {
    local_options(
      "cli.default_handler" = suppressMessages,
      .frame = current_env()
    )
  }

  settoken_check_string(default, call = call)

  if (is_false(install)) {
    caller_name <- "set_r_environ_token"
    caller <- caller_call()
    if (!is_null(caller)) {
      caller_name <- call_name(caller)
    }

    cli_bullets(
      c(
        "v" = "{.envvar {default}} set to {.val {token}} with {.fn Sys.setenv}.",
        "*" = "To use this token in future sessions, call
        {.fn {caller_name}} using {.arg install = TRUE}."
      )
    )
    Sys.setenv(default = token)
    return(invisible(token))
  }

  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (file.exists(renv)) {
    default_match <- grepl(paste0("^", default, "(?=\\=)"),
      readLines(renv),
      perl = TRUE
    )

    has_default <- any(default_match)

    if (has_default && !overwrite) {
      cli_abort(
        c("{.envvar {default}} already exists in your {.file .Renviron}.",
          "*" = "Set {.arg overwrite = TRUE} to replace this token."
        ),
        call = call
      )
    }
    backup <- file.path(home, ".Renviron_backup")
    file.copy(renv, backup)
    cli_alert_success("{.file .Renviron} backed up to {.path {backup}}.")

    if (has_default) {
      oldenv <- utils::read.table(renv, stringsAsFactors = FALSE)
      newenv <- oldenv[!default_match, ]
      utils::write.table(
        newenv, renv,
        quote = FALSE,
        sep = "\n", col.names = FALSE, row.names = FALSE
      )
    }
  } else {
    file.create(renv)
  }

  write(paste0(default, '="', token, '"'), renv, sep = "\n", append = TRUE)

  cli_bullets(
    c(
      "v" = "{.val {token}} saved to {.file .Renviron} variable {.envvar {default}}.",
      "*" = "Restart R or run {.code readRenviron(\"~/.Renviron\")} then use
      {.code Sys.getenv(\"{default}\")} to access the token."
    )
  )

  invisible(token)
}

#' @rdname set_r_environ_token
#' @name get_r_environ_token
#' @param message Optional error message to use if token can't be found.
#' @param pattern Optional pattern passed to [grepl()] and used to validate the
#'   stored token. If pattern is supplied, the returned token must be a string.
#' @param perl Should Perl-compatible regexps be used when checking `pattern`?
#'   Defaults to `TRUE`.
#' @param strict If `TRUE` (default), error if no environment variable with the
#'   supplied name is found. If `FALSE`, warn instead of error.
#' @returns [get_r_environ_token()] returns a string supplied to `token` or
#'   obtained from the environment variable named with `default`.
#'
#' @keywords internal
#'
#' @importFrom rlang caller_arg %||% is_empty is_null
#' @importFrom cli cli_abort
get_r_environ_token <- function(token = NULL,
                                default = "TOKEN",
                                message = NULL,
                                pattern = NULL,
                                perl = TRUE,
                                strict = TRUE,
                                call = caller_env(),
                                ...) {
  settoken_check_string(default, call = call)

  token <- token %||% Sys.getenv(default)

  if (!is_empty(token) && !identical(token, "")) {
    if (is_null(pattern)) {
      return(token)
    }

    settoken_check_string(pattern, call = call)
    settoken_check_string(token, call = call)

    if (grepl(pattern, token, perl = perl)) {
      return(token)
    }

    message <- "{.arg token} must match the supplied pattern: {.val {pattern}}"
  }

  message <- message %||%
    "{.arg token} is empty and {.envvar {default}} can't be found in {.file .Renviron}"

  if (!strict) {
    cli_warn(
      message = message,
      ...
    )

    return(invisible(NULL))
  }

  cli_abort(
    message = message,
    ...,
    call = call
  )
}

#' Check if x object is a string and error if not
#'
#' @noRd
#' @importFrom rlang caller_arg caller_env is_string
#' @importFrom cli cli_abort
settoken_check_string <- function(x,
                                  ...,
                                  allow_empty = FALSE,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
  if (is_string(x) && (allow_empty || !is_string(x, ""))) {
    return(invisible(NULL))
  }

  cli_abort(
    "{.arg {arg}} must be a string, not {.obj_type_friendly {x}}",
    ...,
    call = call
  )
}

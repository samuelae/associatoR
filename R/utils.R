#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL



#' Generic print for class associatoR
#'
#' @param x an object of class \code{associatoR}
#' @param n (optional) use \code{print(n = )} to show more rows of each tibble.
#' @param ... additional arguments passed to \code{print()}
#'
#' @method print associatoR
#' @export
#'
print.associatoR <- function(x, n = 3, ...) {

  cli::cli_h1("An associatoR object")
  cat("\n")

  cli::cli_text(cli::style_bold("Participants"))
  if(exists("participants", where = x)) {
    print(x$participants, n = n, ...)
  } else {
    cli::cli_warn("use `ar_import()` to import participants")
  }
  cat("\n")

  cli::cli_text(cli::style_bold("Cues"))
  if(exists("cues", where = x)) {
    print(x$cues, n = n, ...)
  } else {
    cli::cli_warn("use `ar_import()` to import cues")
  }
  cat("\n")

  cli::cli_text(cli::style_bold("Responses"))
  if(exists("responses", where = x)) {
    print(x$responses, n = n, ...)
  } else {
    cli::cli_warn("use `ar_import()` to import responses")
  }
  cat("\n")

  cli::cli_text(cli::style_bold("Targets"))
  if(exists("targets", where = x)) {
    print(x$targets, n = n, ...)
  } else {
    cli::cli_warn("use `ar_define_target()` to define targets")
  }
  cat("\n")
}



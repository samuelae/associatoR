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
  print(x$participants, n = n, ...)
  cat("\n")

  cli::cli_text(cli::style_bold("Cues"))
  print(x$cues, n = n, ...)
  cat("\n")

  cli::cli_text(cli::style_bold("Responses"))
  print(x$responses, n = n, ...)
  cat("\n")

  if("targets" %in% names(x)){
    cli::cli_text(cli::style_bold("Targets"))
    print(x$targets, n = n, ...)
    cat("\n")
  }

  }



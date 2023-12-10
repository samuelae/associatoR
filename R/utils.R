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
print.associatoR <- function(x, n = 5, ...) {

  cli::cli_h1("An associatoR object")
  cat("\n")

  cli::cli_text(cli::style_bold("participants"))
  print(x$participants, n = n, ...)
  cat("\n")

  cli::cli_text(cli::style_bold("cues"))
  print(x$cues, n = n, ...)
  cat("\n")

  cli::cli_text(cli::style_bold("responses"))
  print(x$responses, n = n, ...)
  cat("\n")

  if("targets" %in% names(x)){
    cli::cli_text(cli::style_bold("targets"))
    print(x$targets, n = n, ...)
    cat("\n")
  }

  if("target_embedding" %in% names(x)){
    cli::cli_text(cli::style_bold("target_embedding"))
    print(x$target_embedding, n = n, ..., width = 70)
    cat("\n")
  }

}




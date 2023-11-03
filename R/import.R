#' Import R data into associatoR object
#'
#' \code{ar_import} processes R data and generates an associatoR object
#'
#' @param data a \code{data.frame} or \code{tibble} object containing the association data.
#' @param participant_labels a \code{character} vector ... .
#'
#' @return The function returns the \code{associatoR} object, which contains a list with the following elements, each a \code{tibble}:
#' \describe{
#'  \item{participant}{Index of the element in \code{text} where match was found. Formatted as a factor with the number of levels matching the original number of documents.}
#'  \item{cues}{Label of the SDG found in document.}
#'  \item{responses}{The name of the ensemble system that produced the match.}
#'  \item{hit}{Index of hit for the Ensemble model.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#'
#' @examples
#'
#' # run sdg detection
#' data <- ar_impot(ai_asso)
#'
#' @export

ar_import <- function(data){

  print(data)

  }

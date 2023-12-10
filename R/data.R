#' AI association data
#'
#' \code{ar_import} processes R data and generates an associatoR object
#'
#' @format The function returns the \code{associatoR} object, which contains a list with the following elements, each a \code{tibble}:
#' \describe{
#'  \item{participant}{Index of the element in \code{text} where match was found. Formatted as a factor with the number of levels matching the original number of documents.}
#'  \item{cues}{Label of the SDG found in document.}
#'  \item{responses}{The name of the ensemble system that produced the match.}
#'  \item{hit}{Index of hit for the Ensemble model.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#' @source \url{https://ap-unsdsn.org/regional-initiatives/universities-sdgs/}
"ai_asso"

#' AI association data
#'
#' \code{ar_import} processes R data and generates an associatoR object
#'
#' @format The function returns the \code{associatoR} object, which contains a list with the following elements, each a \code{tibble}:
#' \describe{
#'  \item{participant}{Index of the element in \code{text} where match was found. Formatted as a factor with the number of levels matching the original number of documents.}
#'  \item{cues}{Label of the SDG found in document.}
#'  \item{responses}{The name of the ensemble system that produced the match.}
#'  \item{hit}{Index of hit for the Ensemble model.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#' @source \url{https://ap-unsdsn.org/regional-initiatives/universities-sdgs/}
"ai_asso_processed"

#' Risk association data
#'
#' \code{ar_import} processes R data and generates an associatoR object
#'
#' @format The function returns the \code{associatoR} object, which contains a list with the following elements, each a \code{tibble}:
#' \describe{
#'  \item{participant}{Index of the element in \code{text} where match was found. Formatted as a factor with the number of levels matching the original number of documents.}
#'  \item{cues}{Label of the SDG found in document.}
#'  \item{responses}{The name of the ensemble system that produced the match.}
#'  \item{hit}{Index of hit for the Ensemble model.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#' @source \url{https://ap-unsdsn.org/regional-initiatives/universities-sdgs/}
"risk_asso"

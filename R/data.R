#' GPT-4-generated free associations to 'intelligence'
#'
#' Inspect and use the data by calling \code{intelligence} in your R session.
#'
#' @format ## `intelligence`
#' A tibble with 29,820 rows and 7 coulumns:
#' \describe{
#'  \item{participant_id}{Unique id for each participant.}
#'  \item{gender}{Gender of the participant.}
#'  \item{education}{Education level of the participant}
#'  \item{cue}{Cue word for this trial.}
#'  \item{response}{Response given by the participant in this trial.}
#'  \item{response_position}{Position of the response given (1. response to cue, 2. response to cue, etc.).}
#'  \item{response_level}{Response level of the trial. (1: first-level response to the cue 'intelligence', 2: second-level response to first-level responses of the same participant as a cue).}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (in preparation). associatoR.
#' @source \url{https://doi.org}
"intelligence"

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

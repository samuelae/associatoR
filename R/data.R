#' GPT-4-generated free associations to 'intelligence'
#'
#' A data set of free associations generated by GPT-4. This data set includes free associations to the cue word 'intelligence' (response_level == 1), as well as associations to each of the responses by the same participant (response_level == 2). The data set further includes two participant covariables (gender and education) simulated by using a respective prompt when generating the data with OpenAI's GPT-4.
#'
#' Inspect and use the data by calling \code{intelligence} in your R session.
#'
#' @format `intelligence`:
#' A tibble with 29,882 rows and 7 coulumns:
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
#' @references Aeschbach, S., Mata, R., & Wulff, D. U. (2024, March 22). Mapping the Mind With Free Associations: A Tutorial Using the R Package associatoR. \url{https://doi.org/10.31234/osf.io/ra87s}
#' @source \url{https://github.com/samuelae/GPT4-intelligence/tree/main}
"intelligence"

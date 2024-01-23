#' Import association data
#'
#' \code{ar_import} processes tabular association data into an associatoR object.
#'
#'
#' @param data a \code{data.frame} or \code{tibble} containing the association data.
#' @param participant variable name in \code{data} identifying the participants.
#' @param cue variable name in \code{data} or \code{character} string identifying the cue(s).
#' @param response variable name in \code{data} identifying the responses.
#' @param participant_vars optional variable names in \code{data} identifying participant attributes (e.g., demographics).
#' @param cue_vars optional variable names in \code{data} identifying cue attributes.
#' @param response_vars optional variable names in \code{data}  identifying response attributes.
#'
#' @return Returns an \code{associatoR} object containing a list of tibbles:
#' \describe{
#'  \item{participants}{A tibble of participants including a participant \code{id} and potential participant attributes.}
#'  \item{cues}{A tibble of cues including a \code{cue} variable and potential cue attributes.}
#'  \item{responses}{A tibble of responses including a participant id, the cues, the responses, the response level, and additional response attributes.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (in progress)
#'
#' @examples
#' ar_import(intelligence,
#'           participant = participant_id,
#'           cue = cue,
#'           response = response,
#'           participant_vars = c(gender, education),
#'           response_vars = c(response_position, response_level))
#'
#' @export
ar_import <- function(data,
                      participant,
                      cue,
                      response,
                      participant_vars = NULL,
                      cue_vars = NULL,
                      response_vars = NULL) {

  # check arguments ----

  # data
  chk::chk_data(data)

  # enquo arguments ----
  participant = dplyr::enquo(participant)
  response = dplyr::enquo(response)
  cue = dplyr::enquo(cue)
  participant_vars = dplyr::enquo(participant_vars)
  response_vars = dplyr::enquo(response_vars)
  cue_vars = dplyr::enquo(cue_vars)

  #print(rlang::expr_text(cue))

  # check types
  check_tidy(data, participant)
  check_tidy(data, response)
  check_tidy(data, participant_vars)
  check_tidy(data, response_vars)
  check_tidy(data, cue_vars)

  # handle protected variables ----

  # participant and response
  data = data %>%
    dplyr::rename(id = !!participant) %>%
    dplyr::rename(response = !!response)

  # add cue
  data = data %>% dplyr::mutate(cue = !!cue)

  # participants ----

  # all use-cases
  participants <- data %>%
    dplyr::select(id, !!participant_vars) %>%
    dplyr::distinct()

  # check for duplicates
  if(any(duplicated(participants$id))){
    stop("Found duplicate participant ids. Make sure that the participant id is uniquely identified and to only include variables as participant attributes that vary between (not within) participants.")
    }

  # cues ----

  # create cues
  cues <- data %>%
    dplyr::select(cue, !!cue_vars) %>%
    dplyr::distinct()

  # check for duplicates
  if(any(duplicated(cues$cue))){
    stop("Found duplicate cues. Make sure that the cue variable is uniquely identified and to only include variables as cue attributes that vary between (not within) cues.")
    }

  # responses ----

  # create responses
  responses <- data %>%
    dplyr::select(id, cue, response, !!response_vars)


  # check if unique
  if(nrow(responses) > nrow(dplyr::distinct(responses))){
    stop("Responses are not uniquely identified. Add response attribute(s) (e.g., trial, repetition, level) to uniquely identify responses.")
    }

  # compose associatoR object ----

  output <- list(participants = participants,
                 cues = cues,
                 responses = responses)
  class(output) <- "associatoR"
  output

}

#' Import association data
#'
#' \code{ar_import} processes tabular association data into an associatoR object.
#'
#'
#' @param data a \code{data.frame} or \code{tibble} containing the association data.
#' @param participant a \code{character} scalar of the variable name in \code{data} identifying participants.
#' @param cue a \code{character} scalar of the variable name in \code{data} identifying the cues.
#' @param cue_manual an optional \code{character} scalar specifying the cue for all responses. Overrides argument \code{cue}.
#' @param response a \code{character} scalar of the variable name in \code{data} identifying responses.
#' @param response_level an optional \code{character} scalar of the variable name in \code{data} identifying response levels.
#' @param participant_vars an optional \code{character} vector containing all additional variable names in \code{data} that are attributes of participants.
#' @param cue_vars an optional \code{character} vector containing all additional variable names in \code{data} that are attributes of cues.
#' @param response_vars an optional \code{character} vector containing all additional variable names in \code{data} that are attributes of responses
#' @param verbose an \code{logical} specifying whether to show messages.
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
#' @export
#'
#' @examples
#' ai_asso_imported <- ar_import(ai_asso,
#'                               participant = "id",
#'                               response = "association_correct",
#'                               cue_manual = "AI",
#'                               participant_vars = c("age", "gender", "use", "expertise"),
#'                               response_vars = c("association", "trial"))
#'
ar_import <- function(data,
                      participant,
                      cue = NULL,
                      cue_manual = NULL,
                      response,
                      response_level = NULL,
                      participant_vars = NULL,
                      cue_vars = NULL,
                      response_vars = NULL,
                      verbose = FALSE) {

  # check arguments ----

  # data
  chk::chk_data(data)

  # check types
  chk::chk_string(participant)
  if(!chk::vld_null(cue)) chk::chk_string(cue)
  if(!chk::vld_null(cue_manual)) chk::chk_string(cue_manual)
  chk::chk_string(response)
  if(!chk::vld_null(response_level)) chk::chk_string(response_level)
  if(!chk::vld_null(participant_vars)) chk::chk_character(participant_vars)
  if(!chk::vld_null(cue_vars)) chk::chk_character(cue_vars)
  if(!chk::vld_null(response_vars)) chk::chk_character(response_vars)
  chk::chk_logical(verbose)

  # check existence
  chk::chk_subset(participant, names(data))
  chk::chk_subset(cue, names(data))
  chk::chk_subset(response, names(data))
  chk::chk_subset(response_level, names(data))
  chk::chk_subset(participant_vars, names(data))
  chk::chk_subset(cue_vars, names(data))
  chk::chk_subset(response_vars, names(data))

  # handle protected variables ----

  # participant and response
  data = data %>%
    dplyr::rename(id = {{participant}}) %>%
    dplyr::rename(response = {{response}})

  # cue
  if(!chk::vld_null(cue_manual)){
    data = data %>% dplyr::mutate(cue = cue_manual)
    } else {
    data = data %>% dplyr::rename(cue = {{cue}})
    }

  # level
  if(chk::vld_null(response_level)){
    data = data %>% dplyr::mutate(level = 1)
    } else {
    data = data %>% dplyr::rename(level = {{response_level}})
    }

  # participants ----

  # all use-cases
  participants <- data %>%
    dplyr::select(id, tidyselect::all_of(participant_vars)) %>%
    dplyr::distinct()

  # check for duplicates
  if(any(duplicated(participants$id))){
    stop("Found duplicate participant ids. Make sure that the participant id is uniquely identified and to only include variables as participant attributes that vary between (not within) participants.")
    }

  # cues ----

  # create cues
  cues <- data %>%
    dplyr::select(cue, tidyselect::all_of(cue_vars)) %>%
    dplyr::distinct()

  # check for duplicates
  if(any(duplicated(cues$cue))){
    stop("Found duplicate cues. Make sure that the cue variable is uniquely identified and to only include variables as cue attributes that vary between (not within) cues.")
    }

  # responses ----

  # create responses
  responses <- data %>%
    dplyr::select(id, cue, response, level,
                  tidyselect::all_of(c(response_vars)))


  # check if unique
  if(nrow(responses) > nrow(dplyr::distinct(responses))){
    stop("Responses are not uniquely identified. Add response level or attribute (e.g., trial or repetition) to uniquely identify responses.")
    }

  # compose associatoR object ----

  output <- list(participants = participants,
                 cues = cues,
                 responses = responses)
  class(output) <- "associatoR"
  output

  }

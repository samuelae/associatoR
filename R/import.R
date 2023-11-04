#' Import association data into an associatoR object
#'
#' \code{ar_import} processes tabular association data into an associatoR object.
#'
#'
#' @param data a \code{data.frame} or \code{tibble} containing the association data.
#' @param participant_id a \code{character} scalar of the variable name in \code{data} identifying participants.
#' @param response_col a \code{character} scalar of the variable name in \code{data} identifying responses.
#' @param cue_type one of \code{"manual"} for one cue or \code{"col"} for a column of cues, if \code{"manual"}, the argument \code{cue_manual} must be supplied, if \code{"col"}, argument \code{cue_col} must be supplied.
#' @param cue_col (if \code{cue_type = "manual"}) a \code{character} scalar of the variable name in \code{data} identifying cues.
#' @param cue_manual (if \code{cue_type = "col"}) a \code{character} scalar specifying the cue for all responses.
#' @param response_level (optional) a \code{character} scalar of the variable name in \code{data} identifying response levels.
#' @param participant_attributes (optional) a \code{character} vector containing all additional variable names in \code{data} that are attributes of participants.
#' @param cue_attributes (optional) a \code{character} vector containing all additional variable names in \code{data} that are attributes of cues.
#' @param response_attributes (optional) a \code{character} vector containing all additional variable names in \code{data} that are attributes of responses
#'
#' @return Returns an \code{associatoR} object containing a list of tibbles:
#' \describe{
#'  \item{participants}{A tibble of all participants including \code{p_id} to identify participants, and potentially additional participant attributes \code{p_attr_*}.}
#'  \item{cues}{A tibble of all cues \code{c_cue} and potentially additional cue attributes \code{c_attr_*}.}
#'  \item{responses}{A tibble of all participant's \code{p_id} cue \code{r_cue} and response \code{r_response} pairs including response levels \code{r_level}, as well as potentially additional responseattributes \code{r_attr_*}.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (in progress)
#'
#' @export
#'
#' @examples
#' ai_asso_imported <- ar_import(ai_asso, participant_id = "id",
#'                               response_col = "association_correct",
#'                               cue_type = "manual", cue_manual = "artificial intelligence",
#'                               participant_attributes = c("age", "gender", "use", "expertise"),
#'                               response_attributes = c("association", "trial"))
#'
ar_import <- function(data, participant_id, response_col, cue_type,
                      cue_col = NULL,
                      cue_manual = NULL,
                      response_level = NULL,
                      participant_attributes = NULL,
                      cue_attributes = NULL,
                      response_attributes = NULL) {
  # check argumets -------------------------------------------------------------

  # data
  chk::chk_data(data)

  # participant_id
  chk::chk_character(participant_id)

  # response_col
  chk::chk_character(response_col)

  # cue_type
  chk::chk_character(cue_type)
  chk::chk_subset(cue_type, c("manual", "col"))

  # cue_col
  if (cue_type == "col") {
    chk::chk_character(cue_col)
    chk::chk_subset(cue_col, names(data))
  } else {
    chk::chk_null(cue_col)
  }

  # cue_manual
  if (cue_type == "manual") {
    chk::chk_character(cue_manual)
  } else {
    chk::chk_null(cue_manual)
  }

  # response_level
  if (!is.null(response_level)) {
    chk::chk_character(response_level)
    chk::chk_subset(response_level, names(data))
  }

  # participant_attributes
  if (!is.null(participant_attributes)) {
    chk::chk_character(participant_attributes)
    chk::chk_subset(participant_attributes, names(data))
  }

  # cue_attributes
  if (!is.null(cue_attributes)) {
    chk::chk_character(cue_attributes)
    chk::chk_subset(cue_attributes, names(data))
  }

  # response_attributes
  if (!is.null(response_attributes)) {
    chk::chk_character(response_attributes)
    chk::chk_subset(response_attributes, names(data))
  }

  # participants ---------------------------------------------------------------

  # all use-cases
  participants <- data %>%
    dplyr::select(dplyr::all_of(c(participant_id, participant_attributes))) %>%
    dplyr::distinct()

  # handle participant_attributes
  if(length(names(participants)) > 1) {
    names(participants) <- c("p_id", paste0("p_attr_", names(participants)[-1]))
  } else {
    names(participants) <- c("p_id")
  }

  # cues -----------------------------------------------------------------------

  switch (cue_type,
          "manual" = {
            # cue is a string, single cue case, e.g. AInet
            cues <- tibble::tibble(c_cue = cue_manual)
          },
          "col" = {
            # cue is a variable name in data
            if(is.null(response_level)) {
              # get cues from cue variable
              cues <- data %>%
                dplyr::select(dplyr::all_of(c(cue_col, cue_attributes))) %>%
                dplyr::distinct()
            } else {
              # get cues from cue variable where response_level is 1
              cues <- data %>%
                dplyr::filter(eval(parse(text = response_level)) == 1) %>%
                dplyr::select(dplyr::all_of(c(cue_col, cue_attributes))) %>%
                dplyr::distinct()
            }
          }
  )

  # handle cue_attributes
  if(length(names(cues)) > 1) {
    names(cues) <- c("c_cue", paste0("c_attr_", names(cues)[-1]))
  } else {
    names(cues) <- c("c_cue")
  }

  # responses ------------------------------------------------------------------

  switch (cue_type,
          "manual" = {
            responses <- data %>%
              dplyr::mutate(r_cue = cue_manual, r_level = 1) %>%
              dplyr::select(dplyr::all_of(c(participant_id, "r_cue", response_col, "r_level", response_attributes)))
          },
          "col" = {
            if (is.null(response_level)) {
              responses <- data %>%
                dplyr::mutate(r_level = 1) %>%
                dplyr::select(dplyr::all_of(c(participant_id, cue_col, response_col, "r_level", response_attributes)))
            } else {
              responses <- data %>%
                dplyr::select(dplyr::all_of(c(participant_id, cue_col, response_col, response_level, response_attributes)))

            }
          })

  # handle response_attributes
  if(length(names(responses)) > 4) {
    names(responses) <- c("p_id", "r_cue", "r_response", "r_level", paste0("r_attr_", names(responses)[-(1:4)]))
  } else {
    names(responses) <- c("p_id", "r_cue", "r_response", "r_level")
  }

  # compose associatoR object --------------------------------------------------

  output <- list(participants = participants,
                 cues = cues,
                 responses = responses)
  class(output) <- "associatoR"

  output

}

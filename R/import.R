#' Title
#'
#' @param data
#' @param participant_id
#' @param response_col
#' @param cue_type
#' @param cue_col
#' @param cue_manual
#' @param response_level
#' @param participant_attributes
#' @param cue_attributes
#' @param response_attributes
#'
#' @return
#' @export
#'
#' @examples
#'
#'
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

#' ar_import_single_cue_single_response_level
#'
#' @param data a \code{data.frame} or \code{tibble} containing the association data.
#' @param cue a \code{character} scalar to specify a cue that was used for all responses.
#' @param participant_id a \code{character} scalar of the variable name in \code{data} identifying individual participants.
#' @param participant_attributes a \code{character} vector containing all additional variable names in \code{data} that are attributes of participants.
#' @param response a \code{character} scalar of the variable name in \code{data} identifying individual responses.
#' @param response_attributes a \code{character} vector containing all additional variable names in \code{data} that are attributes of responses.
#'
#' @return
#' @export
#'
#' @examples
#'
#' ar_import_single_cue_single_response_level(data = ai_asso, cue = "AI",
#'                                            participant_id = "id",
#'                                            participant_attributes = c("age", "gender", "use", "expertise"),
#'                                            response = "association_correct",
#'                                            response_attributes = c("association", "trial"))
#'
ar_import_single_cue_single_response_level <- function(data,
                                                       cue,
                                                       participant_id,
                                                       participant_attributes,
                                                       response,
                                                       response_attributes) {

  # input checks ---------------------------------------------------------------


  # participants ---------------------------------------------------------------

  participants <- data %>%
    dplyr::select(dplyr::all_of(c(participant_id, participant_attributes))) %>%
    dplyr::distinct()
  if(length(names(participants)) > 1) {
    names(participants) <- c("p_id", paste0("p_attr_", names(participants)[-1]))
  } else {
    names(participants) <- c("p_id")
  }

  # cues -----------------------------------------------------------------------

  cues <- tibble::tibble(c_cue = cue)

  # responses ------------------------------------------------------------------

  responses <- data %>%
    dplyr::mutate(r_cue = cue, r_level = 1) %>%
    dplyr::select(dplyr::all_of(c(participant_id, "r_cue", response, "r_level",
                                  response_attributes)))
  if(length(names(responses)) > 4) {
    names(responses) <- c("p_id", "r_cue", "r_response", "r_level",
                          paste0("r_attr_", names(responses)[-(1:4)]))
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

#' ar_import_multiple_cues_single_response_level
#'
#' @param data
#' @param cue
#' @param cue_attributes
#' @param participant_id
#' @param participant_attributes
#' @param response
#' @param response_attributes
#'
#' @return
#' @export
#'
#' @examples
#'
#' ar_import_multiple_cues_single_response_level(data = fa_data,
#'                                               cue = "cue",
#'                                               cue_attributes = c(),
#'                                               participant_id = "participantID",
#'                                               participant_attributes = c("age", "gender"),
#'                                               response = "response",
#'                                               response_attributes = c("pos", "created_at"))
#'
ar_import_multiple_cues_single_response_level <- function(data,
                                                          cue,
                                                          cue_attributes = c(),
                                                          participant_id,
                                                          participant_attributes = c(),
                                                          response,
                                                          response_attributes = c()) {

  # input checks ---------------------------------------------------------------


  # participants ---------------------------------------------------------------

  participants <- data %>%
    dplyr::select(dplyr::all_of(c(participant_id, participant_attributes))) %>%
    dplyr::distinct()
  if(length(names(participants)) > 1) {
    names(participants) <- c("p_id", paste0("p_attr_", names(participants)[-1]))
  } else {
    names(participants) <- c("p_id")
  }

  # cues -----------------------------------------------------------------------

  cues <- data %>%
    dplyr::select(dplyr::all_of(c(cue, cue_attributes))) %>%
    dplyr::distinct()
  if(length(names(cues)) > 1) {
    names(cues) <- c("c_cue", paste0("c_attr_", names(cues)[-1]))
  } else {
    names(cues) <- c("c_cue")
  }

  # responses ------------------------------------------------------------------

  responses <- data %>%
    dplyr::mutate(r_level = 1) %>%
    dplyr::select(dplyr::all_of(c(participant_id, cue, response, "r_level", response_attributes)))
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

#' ar_import_single_cue_multiple_response_levels
#'
#' @param data
#' @param cue
#' @param cue_attributes
#' @param participant_id
#' @param participant_attributes
#' @param response
#' @param response_level
#' @param response_attributes
#'
#' @return
#' @export
#'
#' @examples
#'
#' ar_import_single_cue_multiple_response_levels(data = risk_net,
#'                                               cue = "from_correct",
#'                                               cue_attributes = c(),
#'                                               participant_id = "subject_no",
#'                                               participant_attributes = c("gender", "age"),
#'                                               response = "to_correct",
#'                                               response_level = "to_correct_level",
#'                                               response_attributes = c("trial"))
#'
#'
ar_import_single_cue_multiple_response_levels <- function(data,
                                                          cue,
                                                          cue_attributes = c(),
                                                          participant_id,
                                                          participant_attributes = c(),
                                                          response,
                                                          response_level,
                                                          response_attributes = c()) {

  # input checks ---------------------------------------------------------------


  # participants ---------------------------------------------------------------

  participants <- data %>%
    dplyr::select(dplyr::all_of(c(participant_id, participant_attributes))) %>%
    dplyr::distinct()
  if(length(names(participants)) > 1) {
    names(participants) <- c("p_id", paste0("p_attr_", names(participants)[-1]))
  } else {
    names(participants) <- c("p_id")
  }

  # cues -----------------------------------------------------------------------

  cues <- data %>%
    dplyr::filter(eval(parse(text = response_level)) == 1) %>%
    dplyr::select(dplyr::all_of(c(cue, cue_attributes))) %>%
    dplyr::distinct()
  if(length(names(cues)) > 1) {
    names(cues) <- c("c_cue", paste0("c_attr_", names(cues)[-1]))
  } else {
    names(cues) <- c("c_cue")
  }

  # responses ------------------------------------------------------------------

  responses <- data %>%
    dplyr::select(dplyr::all_of(c(participant_id, cue, response, response_level, response_attributes)))
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

#' Import association data into associatoR object
#'
#' \code{ar_import} processes tabular association data into an associatoR object.
#'
#' @param data a \code{data.frame} or \code{tibble} containing the association data.
#' @param part_id a \code{character} scalar of the variable name in \code{data} identifying individual participants.
#' @param part_attr a \code{character} vector containing all additional variable names in \code{data} that are attributes of participants.
#' @param cue_type one of \code{c("single", "multiple")}; if \code{cue_type = "single"}, the parameter \code{cue_single} must be specified, if \code{cue_type = "multiple"}, the parameter \code{cue_id} must be specified.
#' @param cue_single a \code{character} scalar to specify a cue that was used for all responses.
#' @param cue_text a \code{character} scalar of the variable name in \code{data} identifying individual cues.
#' @param cue_attr a \code{character} vector containing all additional variable names in \code{data} that are attributes of cues.
#' @param resp_text a \code{character} scalar of the variable name in \code{data} identifying individual responses.
#' @param resp_attr a \code{character} vector containing all additional variable names in \code{data} that are attributes of responses.
#'
#'
#' @return Returns an \code{associatoR} object containing a list of \code{tibble}s:
#' \describe{
#'  \item{participants}{A tibble of all participants including \code{part_id}, and potentially additional participant-specific attributes \code{part_*}.}
#'  \item{cues}{A tibble of all cues including \code{cue_id}, \code{cue_text} and potentially additional cue-specific attributes \code{cue_*}.}
#'  \item{responses}{A tibble of all responses including \code{resp_id}, \code{resp_text} and potentially additional response-specific attributes \code{resp_*}.}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (in progress). associatoR. psyArXiv
#'
#' @examples
#'
#' # import association data object
#' ar_obj <- ar_import(data = ai_asso, part_id = "id",
#'                     part_attr = c("age", "gender", "use", "expertise"),
#'                     cue_type = "single", cue_single = "AI",
#'                     resp_text = "association_correct",
#'                     resp_attr = c("association", "trial"))
#'
#' @export

ar_import <- function(data,
                      part_id,
                      part_attr = c(),
                      cue_type = c("single", "multiple"),
                      cue_single = NULL,
                      cue_text = NULL,
                      cue_attr = c(),
                      resp_text,
                      resp_attr = c()) {

  # attribute checks -----------------------------------------------------------

  # check if data is tibble or data.frame
  if(!is.data.frame(data)) {
    stop(paste0("data is not an object of type data.frame"))
  } else {
    data <- tibble::as_tibble(data)
  }

  # check if part_id variable exists in data
  if(!(part_id %in% colnames(data))) {
    stop(paste0("part_id ", part_id, " is not a column in data"))
  }

  # check if all part_attr variables exists in data
  for (part_att in part_attr) {
    if(!(part_att %in% colnames(data))) {
      stop(paste0("part_attr ", part_att, " is not a column in data"))
    }
  }

  # checks depending on cue_type
  if(!(length(cue_type) == 1)) {
    stop("cue_type must be (exactly) one of c(\"single\", \"multiple\")")
  }

  if(cue_type == "single") {

    if(is.null(cue_single)) {
      stop("cue_single must be specified when cue_type == \"single\"")
    }
    if(!length(cue_single) == 1) {
      stop("cue_single must have a length of one; if cue_type = \"single\", only one cue can be defined")
    }

  } else if(cue_type == "multiple") {

    # check if cue_id variable exists in data
    if(!(cue_text %in% colnames(data))) {
      stop(paste0("cue_text ", cue_text, " is not a column in data"))
    }

    # check if all part_attr variables exists in data
    for (cue_att in cue_attr) {
      if(!(cue_att %in% colnames(data))) {
        stop(paste0("cue_attr ", cue_att, " is not a column in data"))
      }
    }

  } else {
    stop("cue_type must be one of: c(\"single\", \"multiple\")")
  }

  # check if part_id variable exists in data
  if(!(resp_text %in% colnames(data))) {
    stop(paste0("resp_text ", resp_text, " is not a column in data"))
  }

  # check if all part_attr variables exists in data
  for (resp_att in resp_attr) {
    if(!(resp_att %in% colnames(data))) {
      stop(paste0("resp_attr ", resp_att, " is not a column in data"))
    }
  }

  # handle single/multiple cues ------------------------------------------------

  # add cue columns to data if they were not there originally
  if(cue_type == "single") {
    data <- data %>%
      mutate(cue_id = 1,
             cue_text = cue_single)
    cue_text <- "cue_text"
    cue_id <- "cue_id"
    cue_attr <- c()
  }

  # create praticipants tibble -------------------------------------------------

  # extract participants tibble
  participants <- data %>%
    dplyr::select(all_of(c(part_id, part_attr))) %>%
    dplyr::distinct()
  # prepend part_ prefix to variable names
  if(length(names(participants)) > 1) {
    names(participants) <- c("part_id", paste0("part_", names(participants)[-1]))
  } else {
    names(participants) <- c("part_id")
  }

  # create cues tibble ---------------------------------------------------------

  # generate cues tibble from data for multiple cues
  cues <- data %>%
    dplyr::select(all_of(c(cue_text, cue_attr))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(cue_id = 1:n()) %>%
    dplyr::rename(cue_text = all_of(cue_text)) %>%
    dplyr::select(all_of(c("cue_id", "cue_text", cue_attr)))

  if(length(names(cues)) > 2) {
    names(cues) <- c("cue_id", "cue_text", paste0("cue_", names(cues)[-(1:2)]))
  } else {
    names(cues) <- c("cue_id", "cue_text")
  }

  # create responses tibble ----------------------------------------------------

  # extract participants tibble
  responses <- data %>%
    dplyr::select(all_of(c(resp_text, part_id, cue_text, resp_attr))) %>%
    dplyr::mutate(resp_id = 1:n()) %>%
    dplyr::rename(resp_text = all_of(resp_text)) %>%
    dplyr::select(all_of(c("resp_id", "resp_text", part_id, cue_text, resp_attr)))

  # prepend part_ prefix to variable names
  if(length(names(responses)) > 4) {
    names(responses) <- c("resp_id", "resp_text", "part_id", "cue_text", paste0("resp_", names(responses)[-(1:4)]))
  } else {
    names(responses) <- c("resp_id", "resp_text", "part_id", "cue_text")
  }

  # join cue_id from cues
  responses <- responses %>%
    dplyr::left_join(cues, by = c("cue_text")) %>%
    dplyr::select(all_of(c("resp_id", "resp_text", "part_id", "cue_id", paste0("resp_", resp_attr))))


  # create associatoR object ---------------------------------------------------

  output <- list(participants = participants,
                 cues = cues,
                 responses = responses)
  class(output) <- "associatoR"

  output

}

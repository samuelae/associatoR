#' Normalize responses manually
#'
#' \code{ar_normalize_manual} offers an interface to add self-defined normalization steps for responses (and cues) in \code{associatoR} objects.
#'
#' @param associations an \code{associatoR} object containing association data as generated by \link[associatoR]{ar_import}.
#' @param fun a vectorized \code{function} applied to all responses and cues in the response matrix, excluding cues that exist in the .
#' @param ... additional arguments passed to \code{fun}.
#' @param process_cues a \code{logical} indicating if cues should be processed (i.e. changed by \code{fun}) or not.
#'
#' @return Returns an \code{associatoR} object containing a list of tibbles:
#'
#' \describe{
#'  \item{participants}{A tibble of participants including a participant \code{id} and potential participant attributes.}
#'  \item{cues}{A tibble of cues including a \code{cue} variable and potential cue attributes.}
#'  \item{responses}{A tibble of responses including a participant id, the cues, the responses, the response level, and additional response attributes. All responses are passed through \code{fun}}
#' }
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (in progress)
#'
#' @examples
#'
#' ar_import(ai_asso, participant = id, cue = "AI",
#'           response = association_correct,
#'           participant_vars = c(age, gender, use, expertise),
#'           response_vars = c(trial)) %>%
#'   ar_normalize_manual(trimws, which = "left")
#'
#'@export

ar_normalize_manual <- function(associations, fun, ..., process_cues = FALSE) {

  # checks
  chk::chk_s3_class(associations, "associatoR")
  chk::chk_function(fun)
  chk::chk_logical(process_cues)

  # normalize ----

  # process cues (conditional on `process_cues`)
  if (process_cues) {
    associations$cues$cue <- fun(associations$cues$cue, ...)
    associations$responses$cue <- fun(associations$responses$cue, ...)
  }

  # process responses
  associations$responses$response = fun(associations$responses$response, ...)

  # out ----
  associations

  }

#' Normalize responses
#'
#' \code{ar_normalize} performs normalization steps for responses (and cues) and wraps around \link[associatoR]{ar_normalize_manual}.
#'
#' @param associations an \code{associatoR} object containing association data as generated by \link[associatoR]{ar_import}.
#' @param case a \code{character} specifying the normalization of cases. Must be one of \code{c("lower", "upper", "sentence")}. Setting \code{case = "lower"} normalizes all responses to lower case, \code{case = "lower"} to upper case, and \code{case = "sentence"} to sentence case.
#' @param punct a \code{character} specifying the normalization of punctuation. Must be one of \code{c("end", "all")}. \code{punct = "end"} replaces all punctuation at the end (including following whitespaces) with \code{punct_replacement} (default is a single white space). \code{punct = "all"} replaces all punctuation.
#' @param punct_replacement a \code{character} used as replacement for punctuation.
#' @param whitespace a \code{character} specifying the normalization of white spaces. Must be one of \code{c("squish", "trim")}. Setting \code{whitespace = "squish"} removes additional white spaces at the start, the end, and in-between words, whereas setting \code{whitespace = "trim"} removes white space from the start and end.
#' @param process_cues a \code{logical} specifying, if cues should be processed as well. Defaults to \code{FALSE}.
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
#'
#' ar_import(ai_asso, participant = id, cue = "AI",
#'           response = association_correct,
#'           participant_vars = c(age, gender, use, expertise),
#'           response_vars = c(trial)) %>%
#'   ar_normalize(case = "original", punct = "all")
#'
#' @export

ar_normalize <- function(associations,
                         case = "lower",
                         punct = "end",
                         punct_replacement = " ",
                         whitespace = "squish",
                         process_cues = FALSE) {

  # checks ----

  # associations
  chk::chk_s3_class(associations, "associatoR")
  chk::chk_string(case)
  chk::chk_subset(case, c("lower", "upper", "sentence"))

  # punct
  chk::chk_string(punct)
  chk::chk_subset(punct, c("end", "all"))

  # whitespace
  chk::chk_string(whitespace)
  chk::chk_subset(whitespace, c("squish", "trim"))

  # cues
  chk::chk_logical(process_cues)

  # case ----

  if(!is.null(case)){
    if(case == "lower"){
      associations <- ar_normalize_manual(associations, stringr::str_to_lower,
                                          process_cues = process_cues)
      }

    if(case == "upper"){
      associations <- ar_normalize_manual(associations, stringr::str_to_upper,
                                          process_cues = process_cues)
      }

    if(case == "sentence"){
      associations <- ar_normalize_manual(associations, stringr::str_to_sentence,
                                          process_cues = process_cues)
      }
    }

  # punct ----

  if(!is.null(punct)){
    if(punct == "all"){
      associations <- ar_normalize_manual(associations,
                                          stringr::str_replace_all,
                                          pattern = "[:punct:]+",
                                          replacement = punct_replacement,
                                          process_cues = process_cues)
    }

    if(punct == "end"){
      associations <- ar_normalize_manual(associations,
                                          stringr::str_replace_all,
                                          pattern = "[:punct:]+[:blank:]*$",
                                          replacement = punct_replacement,
                                          process_cues = process_cues)
      }
    }


  # whitespace ----

  if(!is.null(whitespace)){
    if(punct == "trim"){
      associations <- ar_normalize_manual(associations, stringr::str_trim,
                                          process_cues = process_cues)
      }
    if(punct == "squish"){
      associations <- ar_normalize_manual(associations, stringr::str_squish,
                                          process_cues = process_cues)
      }
    }

  # out ----
  associations

  }

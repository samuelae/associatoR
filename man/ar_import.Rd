% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{ar_import}
\alias{ar_import}
\title{Import association data}
\usage{
ar_import(
  data,
  participant,
  cue,
  response,
  participant_vars = NULL,
  cue_vars = NULL,
  response_vars = NULL
)
}
\arguments{
\item{data}{a \code{data.frame} or \code{tibble} containing the association data.}

\item{participant}{variable name in \code{data} identifying the participants.}

\item{cue}{variable name in \code{data} or \code{character} string identifying the cue(s).}

\item{response}{variable name in \code{data} identifying the responses.}

\item{participant_vars}{optional variable names in \code{data} identifying participant attributes (e.g., demographics).}

\item{cue_vars}{optional variable names in \code{data} identifying cue attributes.}

\item{response_vars}{optional variable names in \code{data}  identifying response attributes.}
}
\value{
Returns an \code{associatoR} object containing a list of tibbles:
\describe{
 \item{participants}{A tibble of participants including a participant \code{id} and potential participant attributes.}
 \item{cues}{A tibble of cues including a \code{cue} variable and potential cue attributes.}
 \item{responses}{A tibble of responses including a participant id, the cues, the responses, the response level, and additional response attributes.}
}
}
\description{
\code{ar_import} processes tabular association data into an associatoR object.
}
\examples{
ar_import(intelligence,
          participant = participant_id,
          cue = cue,
          response = response,
          participant_vars = c(gender, education),
          response_vars = c(response_position, response_level))

}
\references{
Aeschbach, S., Mata, R., Wulff, D. U. (in progress)
}

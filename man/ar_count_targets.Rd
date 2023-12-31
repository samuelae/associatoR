% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/target.R
\name{ar_count_targets}
\alias{ar_count_targets}
\title{Count target occurences in responses}
\usage{
ar_count_targets(associations, ...)
}
\arguments{
\item{associations}{an \code{associatoR} object containing association data as generated by \link[associatoR]{ar_import}.}

\item{...}{optional \code{logical} comparisons filtering the responses before computing the frequencies.}
}
\value{
Returns an \code{associatoR} object containing a list of tibbles, with \code{targets} gaining a \code{frequency} column:
\describe{
 \item{participants}{A tibble of participants including a participant \code{id} and potential participant attributes.}
 \item{cues}{A tibble of cues including a \code{cue} variable and potential cue attributes.}
 \item{responses}{A tibble of responses including a participant id, the cues, the responses, the response level, and additional response attributes.}
 \item{targets}{A tibble of targets including the specified analysis target}
}
}
\description{
Append the response \code{frequency} of each target to the \code{targets} table in the \code{associatoR} object.
}
\examples{

ar_import(ai_asso, participant = "id",
          response = "association_correct",
          cue_manual = "AI",
          participant_vars = c("age", "gender", "use", "expertise"),
          response_vars = c("association", "trial")) \%>\%
  ar_normalize() \%>\%
  ar_set_targets(target_set = "responses") \%>\%
  ar_count_targets()

}

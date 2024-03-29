% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embed.R
\name{ar_embed_targets}
\alias{ar_embed_targets}
\title{Embed associations}
\usage{
ar_embed_targets(
  associations,
  method = "ppmi-svd",
  min_count = 5,
  n_dim = 100,
  model = NULL,
  token = NULL,
  context = NULL
)
}
\arguments{
\item{associations}{an \code{associatoR} object including targets.}

\item{method}{a \code{character} specifying the type of embedding. One of \code{c("counts","ppmi","ppmi-svd","huggingface")}. Default is \code{"ppmi-svd"}.}

\item{min_count}{an \code{integer} value specifying the minimum response count for responses to be considered in the embedding for \code{method = c("counts","ppmi","ppmi-svd")}. Default is \code{5}.}

\item{n_dim}{an \code{integer} value specifying the number of dimensions generated in \code{method = "ppmi-svd"}. Default is \code{300}.}

\item{model}{a \code{character} specifying the model label. Must match the name on \href{https://huggingface.co/models}{huggingface.co/models}.}

\item{token}{a \code{character} string specifying the access token for the hugging face API. Must be obtained from \href{https://huggingface.co/inference-api}{huggingface.co/inference-api}.}

\item{context}{an optional \code{character} string specifying a common lead text that may help the language model interpret the associations. Defaults to \code{"Free association: "}}
}
\value{
The function returns the \code{associatoR} object including a new
  \code{matrix} element called \code{target_embeddings} containing the target embeddings.
}
\description{
\code{ar_embed_targets} generates target embeddings.
}
\examples{
ar_import(intelligence,
          participant = participant_id,
          cue = cue,
          response = response,
          participant_vars = c(gender, education),
          response_vars = c(response_position, response_level)) \%>\%
  ar_set_targets(targets = "cues") \%>\%
  ar_embed_targets()

}
\references{
Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
}

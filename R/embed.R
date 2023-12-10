#' Embed associations
#'
#' \code{ar_embed_targets} generates target embeddings
#'
#' @param data an \code{ar_object} including targets.
#' @param type a \code{character} specifying the type of embedding. One of \code{c("counts","ppmi","ppmi-svd")}. Default is \code{"model"}.
#' @param model a \code{character} specifying the model label. Must match the
#'   model names on the corresponding APIs. See, \href{https://huggingface.co/models}{huggingface.co/models}, \href{https://platform.openai.com/docs/models/embeddings}{platform.openai.com/docs/models/embeddings}, \href{https://cohere.com/embeddings}{cohere.com/embeddings}. Defaults to XX for \code{api = "huggingface"}, to \code{"text-embedding-ada-002"} for \code{api = "openai"}, and to \code{"embed-english-v3.0"} for \code{api = "cohere"}.
#' @param api a \code{character} specifying the api One of \code{c("huggingface","openai")}. Default is \code{"huggingface"}.
#'
#' @return The function returns the \code{associatoR} object including a new
#'   \code{matrix} element containing the embeddings.
#'
#' @references Aeschbach, S., Mata, R., Wulff, D. U. (2024). associatoR. psyArXiv
#'
#' @examples
#'
#' # run embedding
#' ar_embed(ai_asso_processed)
#'
#' @export

ar_embed <- function(data, api, model) {

  # check types
  chk::chk_s3_class(associations, "associatoR")

  # do targets exist
  chk::chk_subset(names(data), c("targets"))




}



#' Modela tópicos a partir de uma DTM usando LDA, STM ou NMF
#'
#' Recebe uma DocumentTermMatrix e aplica o modelo escolhido.
#'
#' @param dtm Uma DocumentTermMatrix (DTM), tipicamente criada via `tm`.
#' @param k Número de tópicos.
#' @param method Algoritmo de modelagem: `"lda"`, `"stm"` ou `"nmf"`.
#' @param metadados (opcional) Data frame com metadados, obrigatório se `method = "stm"`.
#'
#' @return O objeto do modelo ajustado.
#' @export
modela_topicos <- function(dtm, k = 10, method = c("lda", "stm", "nmf"), metadados = NULL) {
  method <- match.arg(method)

  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("A entrada deve ser uma DocumentTermMatrix.")
  }

  if (method == "lda") {
    if (!requireNamespace("topicmodels", quietly = TRUE)) stop("Pacote 'topicmodels' não instalado.")
    modelo <- topicmodels::LDA(dtm, k = k, control = list(seed = 1234))
    return(modelo)
  }

  if (method == "stm") {
    if (!requireNamespace("stm", quietly = TRUE)) stop("Pacote 'stm' não instalado.")
    if (is.null(metadados)) stop("Para usar STM, você deve fornecer `metadados`.")

    dtm_stm <- stm::convert(dtm, to = "stm")
    modelo <- stm::stm(documents = dtm_stm$documents,
                       vocab = dtm_stm$vocab,
                       data = metadados,
                       K = k,
                       seed = 1234)
    return(modelo)
  }

  if (method == "nmf") {
    if (!requireNamespace("textmineR", quietly = TRUE)) stop("Pacote 'textmineR' não instalado.")

    dtm_mat <- as.matrix(dtm)
    modelo <- textmineR::FitNmfModel(dtm_mat, k = k, init.method = "nndsvd", return_all = TRUE)
    return(modelo)
  }

  stop("Método não reconhecido.")
}

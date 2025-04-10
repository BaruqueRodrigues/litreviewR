#' Agrupa tópicos por área temática ou corrente teórica
#'
#' Este módulo permite dois métodos de agrupamento dos tópicos extraídos de uma modelagem:
#' (1) Manual, a partir de anotação externa em CSV; ou (2) Automático, via embeddings e clustering.
#'
#' @param topicos Data frame com colunas `topico` e `texto` (concatenação de termos representativos).
#' @param metodo Modo de agrupamento: `"manual"` ou `"cluster"`.
#' @param arquivo_csv Caminho do CSV anotado manualmente (obrigatório se metodo = "manual").
#' @param n_clusters Número de grupos para clustering automático.
#' @param return_embeddings Se TRUE, retorna também a matriz de embeddings.
#'
#' @return Um data.frame com colunas `topico`, `texto`, `grupo` e, opcionalmente, embeddings.
#' @export
agrupa_topicos <- function(topicos, metodo = c("manual", "cluster"),
                           arquivo_csv = NULL, n_clusters = 4, return_embeddings = FALSE) {
  metodo <- match.arg(metodo)

  if (!all(c("topico", "texto") %in% names(topicos))) {
    stop("O objeto `topicos` deve conter colunas `topico` e `texto`.")
  }

  if (metodo == "manual") {
    if (is.null(arquivo_csv)) stop("Você deve fornecer o caminho para o arquivo CSV anotado.")
    anotacoes <- readr::read_csv(arquivo_csv)
    resultado <- dplyr::left_join(topicos, anotacoes, by = "topico")
    return(resultado)
  }

  if (metodo == "cluster") {
    if (!requireNamespace("text2vec", quietly = TRUE)) {
      stop("O pacote 'text2vec' é necessário para clustering. Instale com install.packages('text2vec').")
    }

    it <- text2vec::itoken(topicos$texto, progressbar = FALSE)
    vectorizer <- text2vec::vocab_vectorizer(text2vec::create_vocabulary(it))
    dtm <- text2vec::create_dtm(it, vectorizer)

    embeddings <- text2vec::normalize(text2vec::TfIdf$new()$fit_transform(dtm), "l2")

    kmeans_res <- stats::kmeans(embeddings, centers = n_clusters)
    topicos$grupo <- paste0("Grupo_", kmeans_res$cluster)

    if (return_embeddings) {
      return(list(topicos = topicos, embeddings = embeddings))
    } else {
      return(topicos)
    }
  }
}


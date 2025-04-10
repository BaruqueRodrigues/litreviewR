#' Roda o pipeline completo de análise de tópicos
#'
#' Esta função automatiza a leitura de PDFs, criação da DTM,
#' modelagem de tópicos (LDA/STM/NMF), e retorno dos tópicos com seus termos.
#'
#' @param pasta_pdfs Caminho da pasta com arquivos PDF.
#' @param k Número de tópicos.
#' @param method Algoritmo: "lda", "stm", "nmf".
#' @param idioma Idioma para stopwords (ex: "portuguese", "english").
#' @param modo Modo de retorno dos termos: "agrupado" ou "detalhado".
#' @param metadados (opcional) Data frame com metadados (necessário para STM).
#' @param min_freq Filtro de frequência mínima dos termos. Default: 2.
#'
#' @return Uma lista com: `topicos`, `modelo`, `dtm` e (opcional) `metadados`.
#' @export
roda_analise_topicos <- function(pasta_pdfs,
                                 k = 10,
                                 method = c("lda", "stm", "nmf"),
                                 idioma = "portuguese",
                                 modo = c("agrupado", "detalhado"),
                                 metadados = NULL,
                                 min_freq = 2) {
  method <- match.arg(method)
  modo <- match.arg(modo)

  dtm <- cria_dtm(pasta_pdfs, idioma = idioma, min_freq = min_freq)

  modelo <- modela_topicos(dtm = dtm, k = k, method = method, metadados = metadados)

  # Extrai os tópicos
  if (method == "lda") {
    termos <- tidytext::tidy(modelo, matrix = "beta") %>%
      dplyr::group_by(topic) %>%
      dplyr::slice_max(beta, n = 10) %>%
      dplyr::ungroup()

    if (modo == "detalhado") {
      topicos <- dplyr::rename(termos, topico = topic)
    } else {
      topicos <- termos %>%
        dplyr::group_by(topic) %>%
        dplyr::summarise(texto = paste(term, collapse = " "), .groups = "drop") %>%
        dplyr::rename(topico = topic)
    }
  }

  if (method == "stm") {
    top_words <- stm::labelTopics(modelo, n = 10)$prob
    topicos <- purrr::map_chr(1:nrow(top_words), ~ paste(top_words[., ], collapse = " "))
    topicos <- tibble::tibble(topico = seq_along(topicos), texto = topicos)
    if (modo == "detalhado") warning("STM: apenas modo 'agrupado' suportado por ora.")
  }

  if (method == "nmf") {
    topicos <- tibble::tibble(
      topico = seq_len(ncol(modelo$phi)),
      texto = apply(modelo$phi, 2, function(p) {
        termos <- names(sort(p, decreasing = TRUE))[1:10]
        paste(termos, collapse = " ")
      })
    )
    if (modo == "detalhado") warning("NMF: apenas modo 'agrupado' suportado por ora.")
  }

  return(list(
    topicos = topicos,
    modelo = modelo,
    dtm = dtm,
    metadados = metadados
  ))
}

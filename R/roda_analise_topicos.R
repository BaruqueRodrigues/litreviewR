#' Executa o pipeline de análise de tópicos
#'
#' Esta função automatiza a leitura de PDFs ou textos, criação da DTM com
#' rastreamento de exclusões, ajuste de LDA/STM e extração de tópicos. NMF está
#' suspenso até que um backend mantido seja validado.
#'
#' @param pasta_pdfs Caminho da pasta com arquivos PDF (opcional se `textos` ou `dtm` for informado).
#' @param textos Vetor de textos alternativo (opcional).
#' @param doc_ids IDs alinhados com `textos`, quando estes não estão nomeados.
#' @param dtm DocumentTermMatrix pré-computada (opcional).
#' @param k Número de tópicos. Default = 10.
#' @param method Algoritmo: `"lda"` ou `"stm"`. `"nmf"` retorna mensagem de indisponibilidade.
#' @param idioma Idioma para stopwords (ex: "portuguese", "english").
#' @param modo Modo de retorno dos termos: "agrupado" ou "detalhado".
#' @param metadados Data frame de metadados para STM.
#' @param formula_prevalence Fórmula de covariáveis de prevalência para STM.
#' @param formula_content Fórmula de covariáveis de conteúdo para STM.
#' @param min_freq Filtro de frequência mínima dos termos. Default = 2.
#' @param seed Semente para reprodutibilidade. Default = 1234.
#'
#' @return Uma lista com tópicos, modelo, DTM, exclusões, metadados e matriz
#'   `associacao_documento_topico` quando disponível.
#' @export
roda_analise_topicos <- function(pasta_pdfs = NULL,
                                 textos = NULL,
                                 doc_ids = NULL,
                                 dtm = NULL,
                                 k = 10,
                                 method = c("lda", "stm", "nmf"),
                                 idioma = "portuguese",
                                 modo = c("agrupado", "detalhado"),
                                 metadados = NULL,
                                 formula_prevalence = NULL,
                                 formula_content = NULL,
                                 min_freq = 2,
                                 seed = 1234) {
  method <- match.arg(method, c("lda", "stm", "nmf"))
  modo <- match.arg(modo)

  if (is.null(dtm)) {
    dtm <- cria_dtm(
      pasta_pdfs = pasta_pdfs,
      textos = textos,
      doc_ids = doc_ids,
      idioma = idioma,
      min_freq = min_freq
    )
  }

  exclusoes <- dtm_obter_exclusoes(dtm)

  modelo <- modela_topicos(
    dtm = dtm,
    k = k,
    method = method,
    metadados = metadados,
    formula_prevalence = formula_prevalence,
    formula_content = formula_content,
    seed = seed
  )

  # Extrai os tópicos conforme o método
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
  } else if (method == "stm") {
    label_obj <- stm::labelTopics(modelo, n = 10)
    top_words <- label_obj$prob

    if (modo == "detalhado") {
      # Constrói tabela detalhada de termos com pesos
      linhas <- list()
      for (top_i in seq_len(nrow(top_words))) {
        palavras <- top_words[top_i, ]
        linhas[[top_i]] <- tibble::tibble(
          topico = top_i,
          term = palavras,
          rank = seq_along(palavras)
        )
      }
      topicos <- do.call(rbind, linhas)
    } else {
      topicos_texto <- purrr::map_chr(seq_len(nrow(top_words)), function(i) paste(top_words[i, ], collapse = " "))
      topicos <- tibble::tibble(topico = seq_along(topicos_texto), texto = topicos_texto)
    }
  } else if (method == "nmf") {
    stop("NMF está temporariamente suspenso até a validação de um backend mantido.", call. = FALSE)
  }

  associacao_documento_topico <- if (method == "lda") {
    topicmodels::posterior(modelo)$topics
  } else if (method == "stm") {
    modelo$theta
  } else NULL
  if (!is.null(associacao_documento_topico)) {
    rownames(associacao_documento_topico) <- dtm$dimnames$Docs %||% paste0("doc_", seq_len(nrow(associacao_documento_topico)))
    colnames(associacao_documento_topico) <- paste0("topico_", seq_len(ncol(associacao_documento_topico)))
  }

  list(
    topicos = topicos,
    modelo = modelo,
    dtm = dtm,
    exclusoes = exclusoes,
    metadados = metadados,
    associacao_documento_topico = associacao_documento_topico
  )
}

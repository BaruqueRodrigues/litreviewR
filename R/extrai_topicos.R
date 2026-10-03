#' Extrai tópicos representativos via LDA
#'
#' Extrai texto, unifica o pré-processamento via `cria_dtm`, executa modelagem LDA
#' com semente configurável e retorna os principais termos por tópico.
#'
#' @param pasta_pdfs Caminho da pasta contendo arquivos PDF (opcional se `dtm` ou `textos` informado).
#' @param dtm Objeto `DocumentTermMatrix` pré-computado (opcional).
#' @param textos Vetor de textos alternativo (opcional).
#' @param k Número de tópicos a ser extraído (default = 10).
#' @param max_termos Número de termos por tópico (default = 10).
#' @param idioma Idioma para remoção de stopwords. Default = "portuguese".
#' @param min_freq Frequência mínima dos termos na DTM. Default = 1.
#' @param seed Semente para reprodutibilidade (default = 1234).
#' @param modo Modo de retorno: `"agrupado"` (padrão) ou `"detalhado"` (com probabilidade beta por termo).
#'
#' @return Um tibble com tópicos e termos, agrupados ou detalhados.
#' @export
extrai_topicos <- function(pasta_pdfs = NULL,
                           dtm = NULL,
                           textos = NULL,
                           k = 10,
                           max_termos = 10,
                           idioma = "portuguese",
                           min_freq = 1,
                           seed = 1234,
                           modo = c("agrupado", "detalhado")) {
  modo <- match.arg(modo)

  if (is.null(dtm)) {
    if (!is.null(pasta_pdfs)) {
      dtm <- cria_dtm(pasta_pdfs = pasta_pdfs, idioma = idioma, min_freq = min_freq)
    } else if (!is.null(textos)) {
      dtm <- cria_dtm(textos = textos, idioma = idioma, min_freq = min_freq)
    } else {
      stop("Forneça `pasta_pdfs`, `dtm` ou `textos`.", call. = FALSE)
    }
  }

  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("`dtm` deve ser uma DocumentTermMatrix.", call. = FALSE)
  }

  if (nrow(dtm) == 0L || ncol(dtm) == 0L) {
    stop("A DTM está vazia. Não é possível extrair tópicos.", call. = FALSE)
  }

  if (!is.numeric(max_termos) || length(max_termos) != 1L || is.na(max_termos) ||
      max_termos < 1 || max_termos %% 1 != 0) {
    stop("`max_termos` deve ser um inteiro positivo.", call. = FALSE)
  }

  modelo <- modela_topicos(dtm, k = k, method = "lda", seed = seed)

  # Tópicos x termos via tidytext
  termos <- tidytext::tidy(modelo, matrix = "beta") %>%
    dplyr::group_by(topic) %>%
    dplyr::slice_max(beta, n = max_termos) %>%
    dplyr::ungroup()

  if (modo == "detalhado") {
    return(dplyr::rename(termos, topico = topic))
  }

  # Modo agrupado
  agrupado <- termos %>%
    dplyr::group_by(topic) %>%
    dplyr::summarise(texto = paste(term, collapse = " "), .groups = "drop") %>%
    dplyr::rename(topico = topic)

  agrupado
}

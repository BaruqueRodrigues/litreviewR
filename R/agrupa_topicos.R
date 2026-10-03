#' Agrupa tópicos por área temática ou agrupamento léxico
#'
#' Este módulo permite dois métodos de agrupamento dos tópicos extraídos de uma modelagem:
#' (1) Manual, a partir de anotação externa em arquivo tabular (CSV); ou
#' (2) Automático, via representação TF-IDF normalizada e k-means clustering.
#'
#' Atenção metodológica: o agrupamento estatístico via TF-IDF e k-means baseia-se em
#' coocorrência léxica de termos e não deve ser confundido com representações semânticas
#' densas (embeddings neurais pré-treinados).
#'
#' @param topicos Data frame com colunas `topico` e `texto` (concatenação de termos representativos).
#' @param metodo Modo de agrupamento: `"manual"` ou `"cluster"`.
#' @param arquivo_csv Caminho do CSV anotado manualmente com coluna `topico` (obrigatório se metodo = "manual").
#' @param n_clusters Número de grupos para clustering automático. Default = 4.
#' @param seed Semente para inicialização do k-means para reprodutibilidade. Default = 1234.
#' @param return_embeddings Se TRUE, retorna também a matriz de atributos TF-IDF
#'   (não são embeddings semânticos).
#'
#' @return Um data.frame com colunas `topico`, `texto`, `grupo` e, opcionalmente, a matriz de atributos.
#' @export
agrupa_topicos <- function(topicos,
                           metodo = c("manual", "cluster"),
                           arquivo_csv = NULL,
                           n_clusters = 4,
                           seed = 1234,
                           return_embeddings = FALSE) {
  metodo <- match.arg(metodo)

  if (!is.data.frame(topicos) || !all(c("topico", "texto") %in% names(topicos))) {
    stop("O objeto `topicos` deve ser um data frame contendo as colunas `topico` e `texto`.", call. = FALSE)
  }
  if (anyNA(topicos$topico) || anyDuplicated(topicos$topico) ||
      anyNA(topicos$texto) || any(!nzchar(trimws(as.character(topicos$texto))))) {
    stop("`topicos` deve ter IDs únicos e textos não vazios.", call. = FALSE)
  }

  if (metodo == "manual") {
    if (is.null(arquivo_csv) || !is.character(arquivo_csv) || !file.exists(arquivo_csv)) {
      stop("Forneça um caminho válido para o arquivo CSV de anotações em `arquivo_csv`.", call. = FALSE)
    }

    anotacoes <- readr::read_csv(arquivo_csv, show_col_types = FALSE)
    if (!"topico" %in% names(anotacoes)) {
      stop("O arquivo CSV de anotações deve conter uma coluna `topico`.", call. = FALSE)
    }
    coluna_grupo <- intersect(c("grupo", "area", "categoria", "corrente_teorica"), names(anotacoes))
    if (!length(coluna_grupo)) {
      stop("O CSV de tópicos precisa de uma coluna de anotação: `grupo`, `area` ou `categoria`.", call. = FALSE)
    }

    if (anyNA(anotacoes$topico) || anyDuplicated(anotacoes$topico)) {
      stop("O CSV de anotação contém IDs de tópico vazios ou duplicados.", call. = FALSE)
    }
    if (anyNA(anotacoes[[coluna_grupo[[1]]]]) ||
        any(!nzchar(trimws(as.character(anotacoes[[coluna_grupo[[1]]]]))))) {
      stop("Há tópicos sem grupo anotado no CSV.", call. = FALSE)
    }
    faltantes <- setdiff(topicos$topico, anotacoes$topico)
    sem_correspondencia <- setdiff(anotacoes$topico, topicos$topico)
    if (length(faltantes) || length(sem_correspondencia)) {
      stop("As anotações devem corresponder exatamente aos tópicos do resultado. Sem anotação: ",
           paste(faltantes, collapse = ", "), "; sem tópico correspondente: ",
           paste(sem_correspondencia, collapse = ", "), call. = FALSE)
    }

    resultado <- dplyr::left_join(topicos, anotacoes, by = "topico")
    return(resultado)
  }

  if (metodo == "cluster") {
    if (!requireNamespace("text2vec", quietly = TRUE)) {
      stop("O pacote 'text2vec' é necessário para agrupamento por k-means. Instale com install.packages('text2vec').", call. = FALSE)
    }

    if (!is.numeric(n_clusters) || length(n_clusters) != 1L || is.na(n_clusters) ||
        n_clusters < 1 || n_clusters %% 1 != 0) {
      stop("`n_clusters` deve ser um inteiro positivo.", call. = FALSE)
    }
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
      stop("`seed` deve ser um número finito.", call. = FALSE)
    }
    k_efetivo <- min(as.integer(n_clusters), nrow(topicos))
    if (k_efetivo < 2L) {
      topicos$grupo <- "Grupo_1"
      return(topicos)
    }

    it <- text2vec::itoken(topicos$texto, progressbar = FALSE)
    vocab <- text2vec::create_vocabulary(it)
    vectorizer <- text2vec::vocab_vectorizer(vocab)
    if (!length(vocab)) stop("Não há termos utilizáveis nos textos dos tópicos.", call. = FALSE)
    dtm_topicos <- text2vec::create_dtm(it, vectorizer)

    tfidf_model <- text2vec::TfIdf$new()
    matriz_tfidf <- text2vec::normalize(tfidf_model$fit_transform(dtm_topicos), "l2")

    set.seed(seed)
    kmeans_res <- stats::kmeans(as.matrix(matriz_tfidf), centers = k_efetivo)
    topicos$grupo <- paste0("Grupo_", kmeans_res$cluster)

    if (return_embeddings) {
      return(list(topicos = topicos, embeddings = matriz_tfidf))
    } else {
      return(topicos)
    }
  }
}

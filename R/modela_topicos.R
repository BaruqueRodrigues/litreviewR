#' Modelar tópicos a partir de uma DTM usando LDA ou STM
#'
#' Ajusta modelos de tópicos a partir de uma `DocumentTermMatrix`, com controle
#' de semente aleatória e alinhamento seguro de metadados para STM. O caminho
#' NMF fica temporariamente suspenso até haver um backend mantido e verificável.
#'
#' @param dtm Uma `DocumentTermMatrix` gerada via `cria_dtm` ou `tm`.
#' @param k Número de tópicos desejado. Default = 10.
#' @param method Algoritmo de modelagem: `"lda"` ou `"stm"`. `"nmf"` é
#'   reconhecido para retornar uma mensagem explícita de indisponibilidade.
#' @param metadados Data frame de covariáveis (obrigatório para STM com fórmulas ou análise de metadados).
#' @param formula_prevalence Fórmula para covariáveis de prevalência no STM (ex: `~ ano + autor`).
#' @param formula_content Fórmula para covariáveis de conteúdo no STM.
#' @param seed Semente para reprodutibilidade. Default = 1234.
#' @param verbose Indica se o modelo deve exibir mensagens durante o ajuste. Default = FALSE.
#'
#' @return Objeto de modelo ajustado com classe do respectivo método.
#' @export
modela_topicos <- function(dtm,
                           k = 10,
                           method = c("lda", "stm", "nmf"),
                           metadados = NULL,
                           formula_prevalence = NULL,
                           formula_content = NULL,
                           seed = 1234,
                           verbose = FALSE) {
  method <- match.arg(method, c("lda", "stm", "nmf"))

  if (identical(method, "nmf")) {
    stop("NMF está temporariamente suspenso: o backend previamente referenciado não oferece uma API verificável. Use LDA/STM até a seleção e validação de outro backend.", call. = FALSE)
  }

  if (!inherits(dtm, "DocumentTermMatrix")) {
    stop("`dtm` deve ser uma DocumentTermMatrix.", call. = FALSE)
  }

  if (nrow(dtm) < 2L || ncol(dtm) == 0L) {
    stop("A DTM fornecida está vazia.", call. = FALSE)
  }

  if (!is.numeric(k) || length(k) != 1L || is.na(k) || !is.finite(k) || k < 2 || k %% 1 != 0) {
    stop("`k` deve ser um inteiro maior ou igual a 2.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || !is.finite(seed)) {
    stop("`seed` deve ser um número finito.", call. = FALSE)
  }
  if (!is.null(formula_prevalence) && !inherits(formula_prevalence, "formula")) {
    stop("`formula_prevalence` deve ser uma fórmula R.", call. = FALSE)
  }
  if (!is.null(formula_content) && !inherits(formula_content, "formula")) {
    stop("`formula_content` deve ser uma fórmula R.", call. = FALSE)
  }

  k_efetivo <- min(as.integer(k), nrow(dtm))
  if (k_efetivo < 2L) k_efetivo <- 2L

  if (method == "lda") {
    if (!requireNamespace("topicmodels", quietly = TRUE)) {
      stop("O pacote 'topicmodels' é necessário para LDA. Instale com install.packages('topicmodels').", call. = FALSE)
    }
    modelo <- topicmodels::LDA(dtm, k = k_efetivo, control = list(seed = seed))
    return(modelo)
  }

  if (method == "stm") {
    if (!requireNamespace("stm", quietly = TRUE)) {
      stop("O pacote 'stm' é necessário para STM. Instale com install.packages('stm').", call. = FALSE)
    }

    # Conversão robusta de DTM tm para formato nativo stm
    dtm_stm <- stm::readCorpus(dtm, type = "slam")

    # Alinhamento estrito de metadados com as linhas remanescentes da DTM
    dados_alinhados <- NULL
    if (!is.null(metadados)) {
      if (!is.data.frame(metadados)) {
        stop("`metadados` deve ser um data frame.", call. = FALSE)
      }

      doc_ids_dtm <- dtm$dimnames$Docs
      if ("doc_id" %in% names(metadados) && !is.null(doc_ids_dtm)) {
        if (anyNA(metadados$doc_id) || anyDuplicated(metadados$doc_id)) {
          stop("`metadados$doc_id` deve ser completo e único.", call. = FALSE)
        }
        idx <- match(doc_ids_dtm, metadados$doc_id)
        if (anyNA(idx)) {
          stop("Faltam metadados para documentos presentes na DTM: ",
               paste(doc_ids_dtm[is.na(idx)], collapse = ", "), call. = FALSE)
        }
        dados_alinhados <- metadados[idx, , drop = FALSE]
      } else {
        stop("Para alinhar metadados com segurança, inclua uma coluna única `doc_id` correspondente aos IDs da DTM.", call. = FALSE)
      }
    }

    args_stm <- list(
      documents = dtm_stm$documents,
      vocab = dtm_stm$vocab,
      K = k_efetivo,
      seed = seed,
      verbose = verbose
    )

    if (!is.null(formula_prevalence)) {
      if (is.null(dados_alinhados)) stop("Para usar `formula_prevalence`, forneça `metadados`.", call. = FALSE)
      faltantes <- setdiff(all.vars(formula_prevalence), names(dados_alinhados))
      if (length(faltantes)) stop("Variáveis ausentes em `metadados` para prevalence: ", paste(faltantes, collapse = ", "), call. = FALSE)
      args_stm$prevalence <- formula_prevalence
      args_stm$data <- dados_alinhados
    } else if (!is.null(dados_alinhados)) {
      args_stm$data <- dados_alinhados
    }

    if (!is.null(formula_content)) {
      if (is.null(dados_alinhados)) stop("Para usar `formula_content`, forneça `metadados`.", call. = FALSE)
      faltantes <- setdiff(all.vars(formula_content), names(dados_alinhados))
      if (length(faltantes)) stop("Variáveis ausentes em `metadados` para content: ", paste(faltantes, collapse = ", "), call. = FALSE)
      args_stm$content <- formula_content
    }

    modelo <- do.call(stm::stm, args_stm)
    return(modelo)
  }

  stop("Método não suportado: ", method, call. = FALSE)
}

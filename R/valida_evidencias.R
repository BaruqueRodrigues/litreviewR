#' Validação de trecho citado contra texto original de uma passagem
#'
#' Verifica deterministicamente se `quote` ocorre como substring contígua e exata no texto.
#' Retorna as posições 1-indexadas `start` e `end`.
#'
#' @param quote Trecho literal citado.
#' @param passage_text Texto original da passagem.
#'
#' @return Lista com `valid` (lógico), `start` (inteiro), `end` (inteiro) e `error` (mensagem ou NULL).
#' @export
valida_evidencia_trecho <- function(quote, passage_text) {
  if (!is.character(quote) || length(quote) != 1L || is.na(quote) || !nzchar(quote)) {
    return(list(valid = FALSE, start = NULL, end = NULL, error = "`quote` deve ser uma string não vazia."))
  }
  if (!is.character(passage_text) || length(passage_text) != 1L || is.na(passage_text)) {
    return(list(valid = FALSE, start = NULL, end = NULL, error = "`passage_text` deve ser uma string."))
  }

  pos <- regexpr(quote, passage_text, fixed = TRUE)
  if (pos[1] == -1L) {
    return(list(valid = FALSE, start = NULL, end = NULL, error = "`quote` não ocorre literalmente na passagem referenciada."))
  }

  start_idx <- as.integer(pos[1])
  match_len <- as.integer(attr(pos, "match.length"))
  end_idx <- start_idx + match_len - 1L

  list(valid = TRUE, start = start_idx, end = end_idx, error = NULL)
}

#' Criar e validar objeto de evidência documental
#'
#' Cria um objeto em conformidade com o contrato canônico de evidência F0.
#'
#' @param evidence_id Identificador único da evidência.
#' @param passage_id Identificador da passagem citada.
#' @param quote Trecho literal e contíguo citado.
#' @param corpus Opcionalmente, lista de passagens para validação determinística.
#' @param start Posição inicial (opcional se corpus for fornecido para inferência).
#' @param end Posição final (opcional se corpus for fornecido para inferência).
#' @param error Se `TRUE`, interrompe quando o contrato for inválido.
#'
#' @return Lista no formato do contrato `evidence`.
#' @export
criar_evidencia <- function(evidence_id, passage_id, quote, corpus = NULL,
                            start = NULL, end = NULL, error = TRUE) {
  if (!is.null(corpus) && (is.null(start) || is.null(end))) {
    passages <- if (is.list(corpus$passages)) corpus$passages else corpus
    pids <- vapply(passages, function(p) if (is.list(p) && is.character(p$passage_id)) p$passage_id else "", character(1))
    at <- match(passage_id, pids)
    if (!is.na(at)) {
      matched <- valida_evidencia_trecho(quote, passages[[at]]$text)
      if (matched$valid) {
        start <- matched$start
        end <- matched$end
      }
    }
  }

  obj <- list(
    evidence_id = evidence_id,
    passage_id = passage_id,
    quote = quote,
    start = start,
    end = end
  )

  validacao <- contrato_validar(obj, tipo = "evidence", corpus = corpus, error = error)
  if (!validacao$valid && !error) {
    attr(obj, "errors") <- validacao$errors
  }
  obj
}

#' Validar conjunto de evidências vinculadas a uma resposta
#'
#' @param resposta Lista de resposta compatível com contrato F0.
#' @param evidencias Lista de objetos de evidência.
#' @param corpus Opcionalmente, lista de passagens.
#' @param error Se `TRUE`, interrompe se houver incoerência entre resposta e evidências.
#'
#' @return Lista com `valid` e `errors`.
#' @export
valida_evidencias_resposta <- function(resposta, evidencias, corpus = NULL, error = FALSE) {
  errors <- character()

  if (!is.list(resposta) || is.null(resposta$evidence_ids) || !is.list(resposta$evidence_ids)) {
    errors <- c(errors, "A resposta deve conter `evidence_ids` como lista.")
    if (error) stop(paste(errors, collapse = "\n"), call. = FALSE)
    return(list(valid = FALSE, errors = errors))
  }

  ev_ids_declarados <- unlist(resposta$evidence_ids, use.names = FALSE)
  if (length(ev_ids_declarados) > 0) {
    ev_disponiveis <- vapply(evidencias, function(e) if (is.list(e) && is.character(e$evidence_id)) e$evidence_id else "", character(1))

    for (eid in ev_ids_declarados) {
      if (!eid %in% ev_disponiveis) {
        errors <- c(errors, sprintf("A evidência referenciada `%s` não foi encontrada no conjunto de evidências.", eid))
      }
    }
  }

  for (e in evidencias) {
    check_e <- contrato_validar(e, tipo = "evidence", corpus = corpus, error = FALSE)
    if (!check_e$valid) {
      errors <- c(errors, paste0("Evidência inválida: ", paste(check_e$errors, collapse = "; ")))
    }
  }

  result <- list(valid = length(errors) == 0L, errors = unique(errors))
  if (error && !result$valid) stop(paste(result$errors, collapse = "\n"), call. = FALSE)
  result
}

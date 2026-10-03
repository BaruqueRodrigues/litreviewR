#' Normalizar uma cadeia de DOI
#'
#' @param doi DOI, URL DOI ou identificador prefixado por `doi:`.
#' @return DOI sem prefixo/URL, ou string vazia quando não informado.
#' @export
normaliza_doi <- function(doi) {
  if (is.null(doi) || !length(doi) || is.na(doi[[1]]) || !nzchar(trimws(doi[[1]]))) return("")
  doi <- trimws(as.character(doi[[1]]))
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- sub("^doi\\s*:\\s*", "", doi, ignore.case = TRUE)
  utils::URLdecode(trimws(doi))
}

.normalizar_texto_match <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) return("")
  x <- iconv(as.character(x[[1]]), from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^[:alnum:]]+", " ", x))
  trimws(gsub("\\s+", " ", x))
}

.similaridade_titulo <- function(a, b) {
  a <- .normalizar_texto_match(a); b <- .normalizar_texto_match(b)
  if (!nzchar(a) || !nzchar(b)) return(0)
  dist <- as.numeric(utils::adist(a, b)[1])
  edit <- max(0, 1 - dist / max(nchar(a), nchar(b), 1))
  wa <- unique(strsplit(a, " ", fixed = TRUE)[[1]])
  wb <- unique(strsplit(b, " ", fixed = TRUE)[[1]])
  jaccard <- length(intersect(wa, wb)) / max(1, length(union(wa, wb)))
  0.65 * edit + 0.35 * jaccard
}

.flatten_authors <- function(x) {
  if (is.null(x)) return(character())
  if (is.character(x)) return(x)
  if (inherits(x, "person")) return(vapply(x, function(p) paste(p$given %||% "", p$family %||% ""), character(1)))
  if (is.list(x)) return(unlist(lapply(x, .flatten_authors), use.names = FALSE))
  character()
}

.crossref_author_names <- function(x) {
  if (!is.list(x)) return(character())
  vapply(x, function(a) paste(a$given %||% "", a$family %||% ""), character(1))
}

.crossref_year <- function(item) {
  for (field in c("published-print", "published-online", "published", "issued")) {
    parts <- item[[field]]$`date-parts`
    if (is.list(parts) && length(parts) && length(parts[[1]]) && is.numeric(parts[[1]][1])) return(as.character(parts[[1]][1]))
  }
  ""
}

.crossref_cache <- new.env(parent = emptyenv())

#' Avaliar candidatos Crossref para uma referência
#'
#' A decisão considera similaridade do título e, quando disponíveis, autores e
#' ano. Resultados incertos permanecem ambíguos; a função não escolhe o primeiro
#' resultado por padrão.
#'
#' @param titulo Título da referência.
#' @param autores Autores, opcionalmente como vetor, lista ou objeto `person`.
#' @param ano Ano de publicação opcional.
#' @param mailto E-mail para identificação Crossref; por padrão lê
#'   `CROSSREF_MAILTO` do ambiente.
#' @param timeout Tempo limite HTTP em segundos.
#' @param max_attempts Número de tentativas para HTTP 429 ou 5xx.
#' @param cache Reutilizar decisões de busca na sessão R.
#' @param .get Transporte HTTP substituível em testes.
#' @param .sleep Função de espera substituível em testes.
#' @return Lista com status (`confirmada`, `ambigua`, `ausente` ou `erro`), DOI,
#'   pontuação e candidatos avaliados.
#' @export
identifica_doi_referencia <- function(titulo, autores = NULL, ano = NULL,
                                      mailto = Sys.getenv("CROSSREF_MAILTO", unset = ""),
                                      timeout = 20, max_attempts = 3,
                                      cache = TRUE,
                                      .get = httr::GET, .sleep = Sys.sleep) {
  cache_key <- .hash_text(paste(.normalizar_texto_match(titulo),
                                .normalizar_texto_match(paste(.flatten_authors(autores), collapse = " ")),
                                ano %||% "", sep = "|"))
  if (isTRUE(cache) && exists(cache_key, envir = .crossref_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .crossref_cache, inherits = FALSE))
  }
  cache_result <- function(x) {
    if (isTRUE(cache) && !identical(x$status, "erro")) assign(cache_key, x, envir = .crossref_cache)
    x
  }
  fail <- function(status, reason, candidates = list()) {
    list(status = status, doi = NULL, score = NA_real_, source = "crossref", reason = reason, candidates = candidates)
  }
  if (!is.character(titulo) || length(titulo) != 1L || is.na(titulo) || !nzchar(trimws(titulo))) {
    return(cache_result(fail("erro", "Título ausente ou inválido.")))
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0 ||
      !is.numeric(max_attempts) || length(max_attempts) != 1L || is.na(max_attempts) || max_attempts < 1) {
    stop("`timeout` e `max_attempts` devem ser positivos.", call. = FALSE)
  }
  endpoint <- "https://api.crossref.org/works"
  query <- list(`query.bibliographic` = titulo, rows = 8)
  if (is.character(mailto) && length(mailto) == 1L && nzchar(mailto)) query$mailto <- mailto
  response <- NULL
  last_error <- NULL
  for (attempt in seq_len(as.integer(max_attempts))) {
    response <- tryCatch(
      .get(endpoint, query = query, httr::user_agent("litreviewR/0.2.0"), httr::timeout(timeout)),
      error = function(e) e
    )
    if (inherits(response, "error")) {
      last_error <- conditionMessage(response)
      if (attempt < max_attempts) .sleep(0.25 * 2^(attempt - 1L))
      next
    }
    code <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
    if (code >= 200L && code < 300L) break
    if (code != 429L && (is.na(code) || code < 500L) || attempt == max_attempts) {
      return(cache_result(fail("erro", paste("Crossref respondeu HTTP", code))))
    }
    retry_after <- tryCatch(httr::headers(response)[["retry-after"]], error = function(e) NULL)
    retry_seconds <- suppressWarnings(as.numeric(retry_after))
    pause <- if (length(retry_seconds) && is.finite(retry_seconds) && retry_seconds >= 0) {
      min(retry_seconds, 30)
    } else {
      min(0.25 * 2^(attempt - 1L), 30)
    }
    .sleep(pause)
  }
  if (inherits(response, "error") || is.null(response)) return(cache_result(fail("erro", last_error %||% "Falha de comunicação com Crossref.")))
  code <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
  if (is.na(code) || code < 200L || code >= 300L) return(cache_result(fail("erro", paste("Crossref respondeu HTTP", code))))
  payload <- tryCatch(httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8"),
                      error = function(e) NULL)
  items <- payload$message$items
  if (!is.list(items) || !length(items)) return(cache_result(fail("ausente", "Crossref não retornou candidatos.")))

  authors_ref <- .flatten_authors(autores)
  authors_ref_norm <- unique(vapply(authors_ref, .normalizar_texto_match, character(1)))
  year_ref <- if (is.null(ano) || !length(ano) || is.na(ano[[1]])) "" else as.character(ano[[1]])
  candidates <- lapply(items, function(item) {
    candidate_title <- item$title[[1]] %||% ""
    title_score <- .similaridade_titulo(titulo, candidate_title)
    candidate_authors <- .crossref_author_names(item$author)
    authors_score <- NA_real_
    if (length(authors_ref_norm) && length(candidate_authors)) {
      candidate_norm <- unique(vapply(candidate_authors, .normalizar_texto_match, character(1)))
      authors_score <- length(intersect(authors_ref_norm, candidate_norm)) / max(1, length(union(authors_ref_norm, candidate_norm)))
    }
    candidate_year <- .crossref_year(item)
    year_score <- if (!nzchar(year_ref)) NA_real_ else as.numeric(nzchar(candidate_year) && identical(candidate_year, year_ref))
    vals <- c(title = title_score)
    weights <- c(title = 1)
    if (!is.na(authors_score)) { vals <- c(vals, authors = authors_score); weights <- c(weights, authors = .25) }
    if (!is.na(year_score)) { vals <- c(vals, year = year_score); weights <- c(weights, year = .1) }
    weights[["title"]] <- .65
    if (!"authors" %in% names(vals)) weights[["title"]] <- weights[["title"]] + .25
    if (!"year" %in% names(vals)) weights[["title"]] <- weights[["title"]] + .1
    score <- sum(vals * weights[names(vals)])
    list(doi = normaliza_doi(item$DOI %||% ""), title = candidate_title,
         authors = candidate_authors, year = candidate_year, score = score)
  })
  scores <- vapply(candidates, `[[`, numeric(1), "score")
  ord <- order(scores, decreasing = TRUE)
  candidates <- candidates[ord]
  top <- candidates[[1]]
  second <- if (length(candidates) > 1L) candidates[[2]]$score else 0
  if (!nzchar(top$doi) || top$score < .65) return(cache_result(fail("ausente", "Nenhum candidato atingiu correspondência suficiente.", candidates)))
  if (top$score >= .85 && top$score - second >= .08) {
    return(cache_result(list(status = "confirmada", doi = top$doi, score = top$score,
                source = "crossref", reason = NULL, candidates = candidates)))
  }
  cache_result(list(status = "ambigua", doi = NULL, score = top$score, source = "crossref",
       reason = "Há candidato plausível, mas a correspondência não é suficientemente distinta.",
       candidates = candidates))
}

#' Encontrar DOI por título com avaliação de correspondência
#'
#' A forma padrão mantém compatibilidade retornando apenas DOI confirmado ou
#' `NULL`. Use `detalhes = TRUE` para receber pontuação, candidatos e status.
#'
#' @param titulo Título da referência.
#' @param autores Autores opcionais.
#' @param ano Ano opcional.
#' @param detalhes Retornar diagnóstico completo.
#' @param ... Argumentos encaminhados a `identifica_doi_referencia()`.
#' @return DOI confirmado, `NULL` ou resultado detalhado.
#' @export
descobre_doi_por_titulo <- function(titulo, autores = NULL, ano = NULL,
                                    detalhes = FALSE, ...) {
  resultado <- identifica_doi_referencia(titulo, autores, ano, ...)
  if (isTRUE(detalhes)) return(resultado)
  if (identical(resultado$status, "confirmada")) resultado$doi else NULL
}

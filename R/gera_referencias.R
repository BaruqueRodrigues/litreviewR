#' Ler referências de um arquivo BibTeX preservando proveniência
#'
#' Retorna registros com metadados bibliográficos e ID estável. A chave
#' BibTeX é a identidade preferida; DOI normalizado e um hash de título,
#' autores e ano são alternativas quando a chave não está disponível.
#'
#' @param caminho_bib Caminho para arquivo BibTeX.
#' @return Lista de registros de artigo com `article_id` (também mantido como
#'   `id` para compatibilidade), `id_source`, `bib_key` e campos
#'   bibliográficos originais que estiverem disponíveis.
#' @export
#' @examples
#' \dontrun{
#' refs <- gera_referencias("referencias.bib")
#' baixa_pdf_auto(refs, diretorio = "pdfs")
#' }
gera_referencias <- function(caminho_bib) {
  if (!is.character(caminho_bib) || length(caminho_bib) != 1L ||
      is.na(caminho_bib) || !file.exists(caminho_bib)) {
    stop("`caminho_bib` deve apontar para um arquivo existente.", call. = FALSE)
  }
  bib <- RefManageR::ReadBib(caminho_bib, .Encoding = "UTF-8")
  if (!length(bib)) return(list())
  refs <- purrr::map(bib, function(entry) {
    fields <- as.list(entry)
    bib_key <- attr(entry, "key") %||% ""
    title <- fields$title %||% ""
    author <- fields$author %||% list()
    year <- fields$year %||% ""
    doi <- normaliza_doi(fields$doi %||% "")
    if (nzchar(bib_key)) {
      id <- paste0("bib_", gsub("[^A-Za-z0-9._-]+", "_", bib_key))
      id_source <- "bib_key"
    } else if (nzchar(doi)) {
      id <- paste0("doi_", .hash_text(tolower(doi)))
      id_source <- "doi"
    } else {
      author_key <- paste(.flatten_authors(author), collapse = " ")
      id <- paste0("ref_", .hash_text(paste(title, author_key, year)))
      id_source <- "title_author_year_hash"
    }
    out <- list(
      id = id, article_id = id, id_source = id_source, bib_key = bib_key,
      title = title, author = author, year = year, doi = if (nzchar(doi)) doi else NULL,
      journal = fields$journal %||% NULL, booktitle = fields$booktitle %||% NULL,
      volume = fields$volume %||% NULL, number = fields$number %||% NULL,
      pages = fields$pages %||% NULL, publisher = fields$publisher %||% NULL,
      abstract = fields$abstract %||% NULL, url = fields$url %||% NULL,
      keywords = fields$keywords %||% NULL, note = fields$note %||% NULL,
      entry_type = attr(entry, "bibtype") %||% NULL,
      duplicate_candidates = character()
    )
    out
  })
  ids <- vapply(refs, `[[`, character(1), "id")
  if (anyDuplicated(ids)) {
    duplicated_ids <- unique(ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)])
    for (id in duplicated_ids) {
      positions <- which(ids == id)
      ids[positions] <- paste0(id, "_", seq_along(positions))
      for (position in positions) {
        refs[[position]]$id <- ids[[position]]
        refs[[position]]$article_id <- ids[[position]]
      }
    }
  }
  title_keys <- vapply(refs, function(x) .normalizar_texto_match(x$title %||% ""), character(1))
  doi_keys <- vapply(refs, function(x) tolower(normaliza_doi(x$doi %||% "")), character(1))
  for (i in seq_along(refs)) {
    same_title <- if (nzchar(title_keys[[i]])) which(title_keys == title_keys[[i]]) else integer()
    same_doi <- if (nzchar(doi_keys[[i]])) which(doi_keys == doi_keys[[i]]) else integer()
    refs[[i]]$duplicate_candidates <- unique(ids[setdiff(union(same_title, same_doi), i)])
  }
  refs
}

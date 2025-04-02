#' Lê um arquivo BibTeX e extrai referências em formato compatível com download automático
#'
#' Esta função lê um arquivo `.bib` usando `RefManageR::ReadBib()` e extrai os campos `title`, `doi` e gera um `id` baseado no título.
#'
#' @param caminho_bib Caminho para o arquivo BibTeX (.bib) contendo as referências.
#'
#' @return Uma lista de listas, onde cada elemento possui os campos `title`, `doi` (opcional) e `id`, prontos para uso na função `baixa_pdf_com_busca()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' refs <- gera_referencias("referencias.bib")
#' purrr::walk(refs, ~baixa_pdf_scihub(.x, diretorio = "pdfs"))
#' }
gera_referencias <- function(caminho_bib) {
  RefManageR::ReadBib(caminho_bib) |>
    purrr::map(~{
      list(
        title = .x$title,
        doi   = .x$doi %||% NULL
      )
    }) |>
    purrr::imap(~{
      .x$id <- gsub("[^a-zA-Z0-9]", "_", .x$title)
      .x
    })
}

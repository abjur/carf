
#' Download lawsuits from the CARF website
#'
#' @param id A character vector with one or more lawsuit numbers
#' @param path The directory where to save downloaded files
#' @param download_pdfs Whether or not to download the attachments
#' @return A character vector with the paths to the downloaded files
#'
#' @export
download_lawsuit <- function(id, path = ".", download_pdfs = TRUE) {

  # NOTE: apperently intervals aren't necessary, so this isn't a param anymore
  interval <- c("02/2016", "02/2017")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)

  # Iterate over lawsuits to download them
  files <- purrr::map(id, download_lawsuit_, path, interval, download_pdfs)

  return(purrr::flatten_chr(files))
}

#' Download CARF decisions
#'
#' @param id A character vector with one or more decision numbers
#' @param path Directory where to save downloaded files
#' @param download_pdfs Whether or not to download the attachments
#' @return A character vetor with the paths to the downloaded files
#'
#' @export
download_decision <- function(id, path = ".", download_pdfs = TRUE) {

  if (download_pdfs) { stop("Can't download PDFs yet") }

  # NOTE: apperently intervals aren't necessary, so this isn't a param anymore
  interval <- c("02/2016", "02/2017")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)

  # Iterate over decisions to download them
  files <- purrr::map(id, download_decision_, path, interval, download_pdfs)

  return(purrr::flatten_chr(files))
}

#' Download all CARF decisions from a year
#'
#' @param year Year to get decisions from
#' @param path Directory where to save downloaded files
#' @param min_page Page from which to start downloading
#' @param max_page Page at which to stop downloading
#' @return A character vetor with the paths to the downloaded files
#'
#' @export
download_decision_year <- function(year, path = ".", min_page = 1, max_page = Inf) {

  # URLs
  base <- "https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/"
  u_search <- glue::glue(base, "consultarJurisprudenciaCarf.jsf")

  # Create directory if necessary
  dir.create(glue::glue(path, "/", year), FALSE, TRUE)
  path <- normalizePath(glue::glue(path, "/", year))

  # Ping the website to get basic info
  r_ping <- httr::GET(u_search, httr::config("ssl_verifypeer" = FALSE))

  # Get j_id
  j_id <- r_ping %>%
    xml2::read_html() %>%
    rvest::html_node("#botaoPesquisarCarf") %>%
    rvest::html_attr("onclick") %>%
    stringr::str_extract("j_id[0-9]{1,2}")

  # Build search query
  search_query <- list(
    "AJAXREQUEST" = "_viewRoot",
    "consultaJurisprudenciaForm" = "consultaJurisprudenciaForm",
    "dataInicialInputDate" = glue::glue("01/", year),
    "dataFinalInputDate" = glue::glue("12/", year),
    "campo_pesquisa1" = "1",
    "campo_pesquisa2" = "1",
    "campo_pesquisa3" = "1",
    "javax.faces.ViewState" = get_viewstate(r_ping))
  search_query[[j_id]] <- j_id

  # Run search
  r_search <- year_post(u_search, r_ping, search_query)

  # Extract number of pages
  pages <- stringr::str_extract(r_search, "(?<=[0-9]{1,10} de )([0-9]+)")
  max_page <- min(as.numeric(pages), max_page)

  # Create progress bar
  pb <- progress::progress_bar$new(total = length(min_page:max_page)*11)

  # Iterate over pages
  range <- min_page:max_page
  files <- purrr::map(range, download_page_year, r_search, path, pb)

  return(purrr::flatten_chr(files))
}

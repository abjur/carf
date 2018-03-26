
#' Definition operator
#'
#' See \code{\link[rlang]{:=}} for more details.
#'
#' @name :=
#' @rdname definition
#' @keywords internal
#' @importFrom rlang :=
NULL

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
NULL

#' Helper funcion for making download_year_*()-related POST requests
#'
#' @param url Character vector with the request URL
#' @param response Last request's response
#' @param body Body of the request
#' @param file Either `NULL` or the path where to save the response
#' @return The response returned by [httr::POST()]
year_post <- function(url, response, body, file = NULL) {

  # Make request
  r <- httr::POST(
    url, body = body, encode = "form",
    httr::config("ssl_verifypeer" = FALSE),
    httr::set_cookies("JSESSIONID" = response$cookies$value))

  # Write file to disk if necessary
  if (!is.null(file)) { xml2::write_html(xml2::read_html(r), file) }

  return(r)
}

#' Auxiliary function to get viewstate from response
#' @param response Response from an HTTP request
get_viewstate <- function(response) {
  response %>%
    purrr::pluck("content") %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='javax.faces.ViewState']") %>%
    xml2::xml_attr("value")
}

# Auxiliary function to find first XML and get text
get_text <- function(html, xml) {
  text <- html %>% xml2::xml_find_first(xml) %>% xml2::xml_text()
  if (!is.na(text) && text == "") { return(NA) }
  else { return(text) }
}

# Auxiliary function for cleaning text
clean_text <- function(string) {
  string %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:]]|\n\r|\r\n", " ") %>%
    stringr::str_replace_all("\n|\r", " ") %>%
    stringr::str_replace_all(" +", " ") %>%
    abjutils::rm_accent()
}

#' Auxiliary function for downloading attachments
#'
#' @param info Vector with PDF name and _idcl
#' @param path Directory where to save PDF
#' @param u_pdf URL where to make the POST
#' @param viewstate View state from the last page
download_pdf <- function(info, path, u_pdf, viewstate) {

  # Create file name
  file <- glue::glue(path, "/", stringr::str_to_lower(info[1]))

  # Build PDF query depending on type
  if (stringr::str_detect(info[2], "Acordaos")) {
    pdf_query <- list(
      "formAcordaos" = "formAcordaos",
      "uniqueToken" = "",
      "javax.faces.ViewState" = viewstate,
      "formAcordaos:_idcl" = info[2])
  } else {
    pdf_query <- list(
      "formProcesso2" = "formProcesso2",
      "uniqueToken" = "",
      "javax.faces.ViewState" = viewstate,
      "formProcesso2:_idcl" = info[2])
  }

  # Get PDF
  r_pdf <- httr::POST(
    u_pdf, body = pdf_query, encode = "form",
    httr::config(ssl_verifypeer = FALSE))

  # Write PDF to disk
  writeBin(r_pdf$content, file)

  return(file)
}

# Get rid of spurious NOTEs
globalVariables(c(
  ".", "X1", "X2", "X3", "acordao", "all_texts", "chamber", "codigoTipoMovimento", "cpf_cnpj",
  "dataMovimentoEditada", "dataProtocolo", "date_publication", "date_session",
  "decision", "decision2", "deny", "diligence", "five_digs", "give", "id", "id2", "id_decision",
  "id_lawsuit", "indicadorCpfCnpj", "jurisprudence_origins", "key", "majority", "n",
  "nomeOrgaoDestino", "nomeOrgaoOrigem", "nullify", "numeroCpfCnpj",
  "numeroProcessoEditado", "numeroRelacao", "numeroSequencia", "origin", "part",
  "quality", "rapporteur", "rapporteur_data", "rapporteur_goliva", "resolucao", "result",
  "section", "siglaUfMovimento", "state", "subject", "subject2", "summary2", "tax", "tax2",
  "taxes", "taxpayer", "tipo", "title", "type", "type_appeal", "type_decision", "type_party",
  "unanimity", "unknown", "val", "vote"))

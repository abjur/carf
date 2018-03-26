
#' Parse jurisprudence decision
#'
#' @param file A character vector with the path to a decision
#' @param verbose Whether or not to print the progress bar
#' @param pb Progress bar created by parse_decision()
#' @return A tibble with one row containing the data parsed
parse_decision_ <- function(file, verbose, pb) {

  # Read html
  html <- xml2::read_html(file)

  # Create table with information
  table <- dplyr::tibble(
    id_lawsuit = get_text(html, "//*[@id='formAcordaos:numProcesso']"),
    taxpayer = get_text(html, "//*[@id='formAcordaos:contribuinte']"),
    type_appeal = get_text(html, "//*[@id='formAcordaos:tdivTipoRecurso']/span[2]"),
    rapporteur = get_text(html, "//*[@id='formAcordaos:relator']"),
    date_session = get_text(html, "//*[@id='formAcordaos:dataSessao']"),
    id_decision = get_text(html, "//*[@id='formAcordaos:numDecisao']"),
    tax = get_text(html, "//*[@id='formAcordaos:tdivMateria']/span[2]"),
    summary = get_text(html, "//*[@id='formAcordaos:ementa']"),
    decision = get_text(html, "//*[@id='formAcordaos:textDecisao']"),
    num_attachments = length(xml2::xml_find_all(html, "//img[contains(@id, 'imageAnexos')]")))

  if (verbose) { pb$tick() }
  return(table)
}

#' Auxiliary function to call [parse_div()] for each div
#'
#' @param file A character vector of one or more paths to page files
#' @param verbose Whether or not to create a progress bar
#' @param pb Progress bar created by [parse_decision()]
#' @return A tibble with the parsed data
parse_decision_page <- function(file, verbose, pb) {

  # Extract divs from file
  xpath <- "//div[contains(@id, 'tblJurisprudencia') and contains(@id, 'body')]"
  divs <- xml2::xml_find_all(xml2::read_html(file), xpath)

  # Iterate over divs
  purrr::map_dfr(divs, parse_div, verbose, pb, .id = "id")
}

#' Parse each div from a page
#'
#' @param div Div to parse for relevant decision information
#' @param verbose Whether to tick the progress bar
#' @param pb Progress bar created by [parse_decision()]
#' @return A tibble with one row of parsed data
parse_div <- function(div, verbose, pb) {

  # Get keys
  keys <- div %>%
    rvest::html_nodes("span") %>%
    rvest::html_text() %>%
    tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(" +", "_") %>%
    stringr::str_replace_all("[^a-z_]", "")

  # Get values
  values <- div %>%
    rvest::html_nodes(xpath = "./span") %>%
    purrr::map(xml2::xml_find_first, "following-sibling::text()") %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na("")

  # Get first value
  values[1] <- div %>%
    rvest::html_node("a") %>%
    rvest::html_text()

  # Get texts
  texts <- div %>%
    rvest::html_nodes(xpath = "./div/span") %>%
    purrr::map(xml2::xml_find_first, "following-sibling::text()") %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na("")

  # Tick progress bar if necessaryg
  if (verbose) { pb$tick() }

  # Build tibble
  dplyr::tibble(key = keys, val = c(values, texts))
}

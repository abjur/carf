
#' Parse jurisprudence decisions and pages from [download_decision_year()] or
#' [download_decision()]
#'
#' @param file A character vector with the paths to one of more jurisprudence
#' files (as returned by [download_decision_year()] or [download_decision()])
#' @param type Whether to parse for `"page"`, `"decision"` or `"both"`
#' @param verbose Whether or not to print a progress bar
#' @return A tibble with the data parsed
#'
#' @export
parse_decision <- function(file, type = "both", verbose = TRUE) {

  # Separate pages and decisions
  pages <- stringr::str_subset(file, "page\\.html")
  decisions <- file[!(file %in% pages)]

  # Create progress bar
  pb <- progress::progress_bar$new(total = length(file))

  # Depending on pages or decisions, map_dfr()
  if (type %in% c("both", "page") && length(pages) > 0) {

    # Iterate over pages and spread()
    pages_df <- pages %>%
      purrr::map_dfr(parse_decision_page, verbose, pb, .id = "id2") %>%
      dplyr::mutate(id = (as.integer(id2)-1L)*10L + as.integer(id)) %>%
      dplyr::select(-id2) %>%
      tidyr::spread(key, val) %>%
      dplyr::mutate(
        tipo = ifelse(is.na(resolucao), "A", "R"),
        acordao = ifelse(is.na(acordao), resolucao, acordao)) %>%
      dplyr::select(id, acordao, tipo, dplyr::everything()) %>%
      dplyr::select(-resolucao) %>%
      purrr::set_names(c(
        "id", "id_decision", "type_decision", "taxpayer", "date_publication",
        "decision", "summary", "id_lawsuit", "rapporteur"))
  }
  if (type %in% c("both", "decision") && length(decisions) > 0) {

    # Iterate over every decision
    decisions_df <- purrr::map_dfr(decisions, parse_decision_, verbose, pb) %>%
      filter(!is.na(id_lawsuit))
  }

  # Return only the necessary tables
  if (type == "both") {
    return(list(pages = pages_df, decisions = decisions_df))
  } else if (type == "page") {
    return(pages_df)
  } else {
    return(decisions_df)
  }
}

#' Parser for CARF lawsuit files
#'
#' @param file The path to a file returned by [download_lawsuit()]
#' @return A tibble with information about the lawsuit
#'
#' @export
parse_lawsuit <- function(file) {

  # Read file
  html <- xml2::read_html(file)

  # Get columns of final table
  id <- get_text(html, "//*[@id='formProcesso2:numProcessoPrincipal']")
  date <- get_text(html, "//*[@id='formProcesso2:dataEntrada']")
  payer <- get_text(html, "//*[@id='formProcesso2:niContribuinte']")
  tax <- get_text(html, "//*[@id='formProcesso2:tributoMateria']")

  # Create table with events and metadata
  html %>%
    xml2::xml_find_first("//*[@id='formProcesso2:tbandamentos']") %>%
    rvest::html_table(header = FALSE) %>%
    utils::tail(-2) %>%
    dplyr::mutate(X3 = ifelse(X3 == "", FALSE, TRUE)) %>%
    dplyr::select(event_date = X1, event = X2, attachment = X3) %>%
    cbind(id_lawsuit = id, date_lawsuit = date, taxpayer = payer, tax = tax) %>%
    dplyr::as_tibble()
}

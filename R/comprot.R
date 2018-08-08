
#' Get lawsuits' comunication and protocol data (comprot)
#'
#' @param id Character vector with one or more lawsuit IDs
#' @return A tibble with the following columns:
#' \itemize{
#'   \item `id` Number of the lawsuit
#'   \item `date` Date of protocol
#'   \item `document` ID of the document of origin
#'   \item `origin` Name of place of origin
#'   \item `subject` Subject of the lawsuit
#'   \item `party` Name of the interessed party
#'   \item `cpf_cnpf` Party's CPF or CNPJ
#'   \item `status` Lawsuit's status
#'   \item `state` State where the lawsuit was filed
#'   \item `events` A tibble with the events the lawsuit's been through
#' }
#'
#' @export
get_comprot <- function(id) {
  
  # Clean IDs
  id <- stringr::str_replace_all(id, "[^0-9]", "")
  
  # Iterate over IDs
  purrr::map(id, get_comprot_)
}

#' Get the comprot data for one lawsuit
#'
#' @param id Character vector with lawsuit ID
#' @return One row containing lawsuit's comprot data
get_comprot_ <- purrr::possibly(function(id) {
  
  # Fabricate a timestamp
  ts <- stats::runif(1, 0, 999) %>%
    round() %>%
    stringr::str_pad(3, "left", "0") %>%
    stringr::str_c(as.integer(lubridate::now()), .)
  
  # Create URL for query
  base <- "https://comprot.fazenda.gov.br/comprotegov"
  u_query <- glue::glue(base, "/api/processo/{id}?_={ts}")
  
  # Get comprot and convert from JSON
  comprot <- u_query %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    xml2::read_html() %>%
    rvest::html_text() %>%
    jsonlite::fromJSON()
  
  # Separate lawsuit and events
  lwst <- comprot$processo
  movs <- comprot$movimentos
  
  # Clean lawsuit row
  lwst <- lwst %>%
    dplyr::as_tibble() %>%
    dplyr::select(numeroProcessoEditado:siglaUfMovimento) %>%
    dplyr::select(-c(nomeOrgaoOrigem:numeroRelacao, indicadorCpfCnpj)) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::mutate(
      numeroCpfCnpj = as.character(numeroCpfCnpj),
      dataProtocolo = lubridate::ymd(dataProtocolo)) %>%
    purrr::set_names(c(
      "id", "date", "document", "origin", "subject",
      "party", "cpf_cnpj", "status", "state"))
  
  # Clean events table
  movs <- movs %>%
    dplyr::as_tibble() %>%
    dplyr::select(dataMovimentoEditada:nomeOrgaoDestino) %>%
    dplyr::select(-codigoTipoMovimento) %>%
    dplyr::mutate(
      dataMovimentoEditada = lubridate::dmy(dataMovimentoEditada),
      numeroSequencia = stringr::str_replace(numeroSequencia, "^0*", ""),
      numeroSequencia = as.integer(numeroSequencia)) %>%
    purrr::set_names(c(
      "date_event", "type", "number", "id_event", "id_origin",
      "name_origin", "id_destination", "name_destination"))
  
  return(dplyr::mutate(lwst, events = list(movs)))
},'deu ruim')

#' HTML de resultado de Jurisprudência em tibble
#'
#' Recebe um vetor de caminhos de arquivos HTML e transforma os conteúdos dos
#' arquivos em tibbles.
#'
#' @param arqs vetor de arquivos
#' @param verbose imprimir mensagens dos arquivos processados?
#'
#' @return \code{tibble}.
#'
#' @export
parse_jurisprudencia <- function(arqs, verbose = FALSE) {
  abjutils::dvec(parse_jurisprudencia_arq, arqs, verbose = verbose)
}

#' HTML de resultado de página de Jurisprudência em tibble
#'
#' Recebe um vetor de caminhos de arquivos HTML e transforma os conteúdos dos
#' arquivos em tibbles.
#'
#' @param arqs vetor de arquivos.
#' @param spread espalhar keys nas colunas? Default \code{TRUE}.
#' @param verbose imprimir mensagens dos arquivos processados?
#'
#' @return \code{tibble}.
#'
#' @export
parse_jurisprudencia_pags <- function(arqs, spread = TRUE, verbose = FALSE) {
  d <- abjutils::dvec(parse_jurisprudencia_pag, arqs, verbose = verbose)
  if(spread) d <- tidyr::spread(d, key, val)
  d
}

parse_jurisprudencia_div <- function(div) {
  keys <- div %>%
    rvest::html_nodes('span') %>%
    rvest::html_text() %>%
    tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(' +', '_') %>%
    stringr::str_replace_all('[^a-z_]', '')
  values <- div %>%
    rvest::html_nodes(xpath = './span') %>%
    purrr::map(rvest::html_node, xpath = 'following-sibling::text()') %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na('')
  values[1] <- div %>%
    rvest::html_node('a') %>%
    rvest::html_text()
  txts <- div %>%
    rvest::html_nodes(xpath = './div/span') %>%
    purrr::map(rvest::html_node, xpath = 'following-sibling::text()') %>%
    purrr::map_chr(rvest::html_text) %>%
    stringr::str_trim() %>%
    stringr::str_replace_na('')
  tibble::tibble(key = keys, val = c(values, txts), result = 'OK')
}

parse_jurisprudencia_pag <- function(arq) {
  h <- xml2::read_html(arq)
  xp <- '//div[contains(@id, "tblJurisprudencia") and contains(@id, "body")]'
  divs <- h %>% rvest::html_nodes(xpath = xp)
  d <- purrr::map_df(divs, parse_jurisprudencia_div, .id = 'id')
  d
}

pega_tag <- function(h, xpath) {
  tag <- tryCatch({
    rvest::html_text(rvest::html_node(h, xpath = xpath))
  }, error = function(e) {
    ''
  })
  tag
}

parse_jurisprudencia_arq <- function(arq, verbose = FALSE) {
  try({
    if(verbose) cat(arq, '\n')
    h <- xml2::read_html(arq)
    n_processo <- pega_tag(h, '//*[@id="formAcordaos:numProcesso"]')
    contribuinte <- pega_tag(h, '//*[@id="formAcordaos:contribuinte"]')
    tipo_recurso <- pega_tag(h, '//*[@id="formAcordaos:tdivTipoRecurso"]/span[2]')
    relator <- pega_tag(h, '//*[@id="formAcordaos:relator"]')
    data_sessao <- pega_tag(h, '//*[@id="formAcordaos:dataSessao"]')
    n_acordao <- pega_tag(h, '//*[@id="formAcordaos:numDecisao"]')
    tributo <- pega_tag(h, '//*[@id="formAcordaos:tdivMateria"]/span[2]')
    txt_ementa <- pega_tag(h, '//*[@id="formAcordaos:ementa"]')
    txt_decisao <- pega_tag(h, '//*[@id="formAcordaos:textDecisao"]')
    n_anexos <- length(rvest::html_nodes(h, xpath = '//img[contains(@id, "imageAnexos")]'))

    d <- data.frame(n_acordao = n_acordao,
                    n_processo = n_processo,
                    tipo_recurso = tipo_recurso,
                    tributo = tributo,
                    data_sessao = data_sessao,
                    contibuinte = contribuinte,
                    relator = relator,
                    n_anexos = n_anexos,
                    txt_ementa = txt_ementa,
                    txt_decisao = txt_decisao,
                    stringsAsFactors = FALSE)
    return(d)
  })
  return(data.frame(erro = 'erro', stringsAsFactors = FALSE))
}


#-------------------------------------------------------------------
# deprecated functions


#' transforma arquivo html em data_frame com metadados e andamentos
#'
#' @export
parser_processos <- function(processos, path, salvar = FALSE) {
  dfp <- dplyr::data_frame(n_processo = processos,
                           arq = sprintf('%s/%s.html', path, processos))
  d <- dplyr::do(dplyr::group_by(dfp, n_processo, arq),
                 parser_processo, salvar = salvar, path = path)
  d <- dplyr::select(dplyr::ungroup(d), -arq)
  d
}

parser_processo <- function(p, salvar) {

}

pega_anda <- function (a) {
  d <- sapply(getNodeSet(a, 'td'),
         function(x) paste(sapply(getNodeSet(x, '*//text()'), xmlValue), collapse='\n'))
  d <- str_trim(d)
  res <- data.frame(data=d[1], andamento=d[2], anexo=F, stringsAsFactors=F)
  if(length(getNodeSet(a, 'td//a')) > 0) res$anexo <- T
  return(res)
}

parser_carf <- function(html) {
  hp <- htmlParse(html)
  n_processo <- xmlValue(getNodeSet(hp, "//*[@id='formProcesso2:numProcessoPrincipal']")[[1]])
  n_processo_num <- gsub('[^0-9]', '', n_processo)
  data_entrada <- xmlValue(getNodeSet(hp, "//*[@id='formProcesso2:dataEntrada']")[[1]])
  nome_contrib <- xmlValue(getNodeSet(hp, "//*[@id='formProcesso2:niContribuinte']")[[1]])
  tributo <- xmlValue(getNodeSet(hp, "//*[@id='formProcesso2:tributoMateria']")[[1]])
  anda <- getNodeSet(hp, "//*[@id='formProcesso2:tbandamentos:tbody_element']//tr")
  d <- rbind_all(lapply(anda, pega_anda))
  d <- mutate(d, n_processo_num=n_processo_num,
              data_entrada=data_entrada,
              nome_contrib=nome_contrib,
              tributo=tributo)
  return(d)
}

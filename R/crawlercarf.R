#' @export
pega_state <- function(r) {
  rvest::html_attr(rvest::html_node(xml2::read_html(r), '#javax\\.faces\\.ViewState'), 'value')
}

ops <- function() {
  d_jurisprudencia_pag_spread <- d_jurisprudencia_pag %>%
    mutate(aux = ifelse(str_detect(key, 'csrf'), key, 'NA_NA')) %>%
    separate(aux, c('val2', 'key2'), sep = '_') %>%
    mutate(key2 = ifelse(key2=='acordao', 'n_acordao', 'key2')) %>%
    mutate(val = ifelse(str_detect(key, 'csrf') & !is.na(val2), val2, val),
           key = ifelse(str_detect(key, 'csrf') & !is.na(key2), key2, key)) %>%
    select(-key2, -val2) %>%
    spread(key, val) %>%
    select(-nao_informado_acordao, -nao_informado_resolucao)
  save(d_jurisprudencia_pag_spread,
       file = 'data/d_jurisprudencia_pag_spread.rda')
  View(d_jurisprudencia_pag_spread)
}

#' Funcao que baixa lista de processos do carf
#'
#' @export
lista_processos_ano <- function(ano, path, min_pag = 1, max_pag = Inf) {

  url1 <- 'https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/consultarJurisprudenciaCarf.jsf'
  r <- httr::GET(url1, httr::config(ssl_verifypeer = FALSE))
  state <- pega_state(r)
  j_id <- r %>%
    xml2::read_html() %>%
    rvest::html_node('#botaoPesquisarCarf') %>%
    rvest::html_attr('onclick')
  j_id <- regmatches(j_id, regexpr('j_id[0-9]{1,2}', j_id))
  dados <- list(
    'AJAXREQUEST' = '_viewRoot',
    'consultaJurisprudenciaForm' = 'consultaJurisprudenciaForm',
    'dataInicialInputDate' = paste0('01/', ano),
    'dataFinalInputDate' = paste0('12/', ano),
    'campo_pesquisa1' = '1',
    'campo_pesquisa2' = '2',
    'campo_pesquisa3' = '1',
    'javax.faces.ViewState' = state
  )
  dados[[j_id]] <- j_id
  r2 <- httr::POST(url1, body = dados, encode = 'form',
                   config = httr::config(ssl_verifypeer = FALSE))

  if (is.infinite(max_pag - min_pag)) {
    h <- httr::content(r2, 'text')
    max_pag <- regexec('[0-9]+ de ([0-9]+)', h)
    max_pag <- as.numeric(regmatches(h, max_pag)[[1]][2])
  }

  cat(sprintf('baixando de %d a %d...\n', min_pag, max_pag))
  aux <- sapply(min_pag:max_pag, lista_processos_pag, r = r2, path = path)
}

lista_processos_pag <- function(pag, r, path) {
  cat(sprintf('pag_%05d', pag), '\n')
  url_pag <- 'https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/listaJurisprudenciaCarf.jsf'
  state <- pega_state(r)
  dados <- list('AJAXREQUEST' = '_viewRoot',
                'formAcordaos' = 'formAcordaos',
                'javax.faces.ViewState' = state,
                'dataScroller_1' = as.character(pag),
                'ajaxSingle' = 'dataScroller_1',
                'AJAX:EVENTS_COUNT' = '1')

  r_pag <- httr::POST(url_pag, body = dados, encode = 'form',
                      config = httr::config(ssl_verifypeer = FALSE))

  arq_pagina <- sprintf('%s/pag_%05d.html', path, pag)
  if(!file.exists(arq_pagina)) {
    cat(httr::content(r_pag, 'text'), file = arq_pagina)
  }
  state_pag <- pega_state(r_pag)

  aux <- sapply(((pag - 1) * 10):((pag - 1) * 10 + 9),
                baixa_processo_jurisprudencia,
                r = r_pag, pag = pag, path = path)
}

baixa_processo_jurisprudencia <- function(id, r, pag, path) {
  tryCatch({
    url_pag <- 'https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/listaJurisprudenciaCarf.jsf'
    state <- pega_state(r)
    dados <- list('formAcordaos' = 'formAcordaos', 'javax.faces.ViewState' = state)
    form <- sprintf('tblJurisprudencia:%d:numDecisao', id)
    dados[[form]] <- form
    r_processo <- httr::POST(url_pag, body = dados, encode = 'form',
                             config = httr::config(ssl_verifypeer = FALSE))
    n_processo <- xml2::read_html(r_processo)
    n_processo <- rvest::html_node(n_processo, '#formAcordaos\\:numProcesso')
    n_processo <- gsub('[^0-9]', '', rvest::html_text(n_processo))
    cat(sprintf('pag %05d | id %05d | processo %s', pag, id, n_processo), '\n')
    # cat(httr::content(r_processo, 'text'), file='teste.html')
    arq_processo <- sprintf('%s/%07d_pag_%05d_p_%s.html', path, id, pag, n_processo)
    if(!file.exists(arq_processo)) {
      cat(httr::content(r_processo, 'text'), file = arq_processo)
    }
  }, error = function(e) {
    n_processo <- 'xxxxxxxxxxxxxxxxx'
    arq_processo <- sprintf('%s/%07d_pag_%05d_p_%s.html', path, id, pag, n_processo)
    if(!file.exists(arq_processo)) {
      cat(e$message, file = arq_processo)
    }
    cat(sprintf('pag %05d | id %05d | processo %s', pag, id, n_processo), '\n')
  })
}

#---------------------------------------------------------------------
# Daqui pra baixo, Deprecated!
#---------------------------------------------------------------------

baixa_pdf <- function(infos, path = '', curl, viewstate, p) {
  url1 <- 'https://carf.fazenda.gov.br/sincon/public/pages/ConsultarInformacoesProcessuais/exibirProcesso.jsf'
  dados2 <- list('formProcesso2' = 'formProcesso2',
                 'javax.faces.ViewState' = viewstate,
                 'formProcesso2:_idcl' = infos[1])
  r <- postForm(url1, .params = dados2, curl = curl)
  dir.create(paste0(path, '/', p), showWarnings=F)
  f <- file(paste0(path, '/', p, '/', infos[2]), 'wb')
  writeBin(as.vector(r), f)
  close(f)
}

#' Baixa um processo do CARF a partir do numero do processo
#'
#' @export
crawler_carf <- function(p, write_html = FALSE, write_pdf = FALSE, path = getwd()) {
  p <- gsub('[^0-9]', '', p)
  url0 <- 'https://carf.fazenda.gov.br/sincon/public/pages/index.jsf'
  url1 <- 'https://carf.fazenda.gov.br/sincon/public/pages/ConsultarInformacoesProcessuais/exibirProcesso.jsf'
  curl <- getCurlHandle()
  curlSetOpt(cookiejar = tempfile(),
             useragent = 'R',
             followlocation = TRUE,
             ssl.verifypeer = FALSE,
             curl = curl)

  html0 <- getURL(url0, curl=curl)
  viewstate <- xmlGetAttr(getNodeSet(htmlParse(html0), "//*[@id='javax.faces.ViewState']")[[1]], 'value')
  dados <- list('formProcesso' = 'formProcesso',
                'buscaRapidaProcesso' = p,
                'formProcesso:j_id72' = 'Pesquisar',
                'javax.faces.ViewState' = viewstate)
  html1 <- postForm(url0, .params = dados, curl = curl)

  viewstate <- xmlGetAttr(getNodeSet(htmlParse(html1), "//*[@id='javax.faces.ViewState']")[[1]], 'value')
  dados1 <- list('formProcesso2' = 'formProcesso2',
                 'javax.faces.ViewState' = viewstate,
                 'formProcesso2:todasLinhas'='formProcesso2:todasLinhas')
  html2 <- tryCatch(postForm(url1, .params = dados1, curl = curl), error = function(x) 'erro')

  if(write_pdf & html2 != 'erro') {
    a <- getNodeSet(htmlParse(html2), "//*[@id='formProcesso2:tbandamentos:tbody_element']//a")
    arqs <- sapply(a, xmlGetAttr, 'id')
    nms <- sapply(a, xmlGetAttr, 'title')
    nms <- paste0(1:length(nms), '_', nms)
    infos <- cbind(arqs, nms)
    viewstate <- xmlGetAttr(getNodeSet(htmlParse(html2), "//*[@id='javax.faces.ViewState']")[[1]], 'value')
    aux <- apply(infos, 1, baixa_pdf, path, curl, viewstate, p)
  }

  if(write_html) {
    dir.create(sprintf('%s/%s', path, p), showWarnings = FALSE)
    f <- file(sprintf('%s/%s/%s.html', path, p, p), 'w')
    write(html2, f)
    close(f)
  }

  rm(curl)
  gc(verbose = FALSE)
  return(html2)
}

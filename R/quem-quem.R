#' @export
download_quem_quem <- function(path = NULL) {
  if(is.null(path)) {
    path <- system.file('extdata/', package = 'crawlercarf')
  }
  link <- 'http://carf.fazenda.gov.br/sincon/public/pages/ConsultarInstitucional/Historico/HistoricoPopup.jsf#'
  r0 <- httr::GET(link)
  state <- pega_state(r0)
  dados <- list('j_id19' = 'j_id19',
                'javax.faces.ViewState' = state,
                'j_id19:_idcl' = 'j_id19:j_id21')
  r <- httr::POST(link, body = dados)
  arq <- sprintf('%s/quem_quem.pdf', path)
  writeBin(httr::content(r, 'raw'), arq)
  return(arq)
}

#' @export
pega_linhas <- function(l, re) {
  matches <- stringr::str_match_all(l, re)
  matches <- lapply(matches, function(x) stringr::str_trim(as.character(x)))
  ind <- which(sapply(matches, function(x) length(x) > 0))
  d <- data.frame(do.call(rbind, matches), stringsAsFactors = FALSE)
  d$id_linha <- as.character(ind)
  d$linha <- stringr::str_trim(l[ind])
  d
}

#' @export
completa <- function(v1, v2) {
  v1[!is.na(v2)] <- NA
  for(i in 1:length(v1)) {
    if(is.na(v1[i])) {
      v1[i] <- v1[i-1]
    }
  }
  v1[is.na(v2)] <- NA
  v1 <- stringr::str_trim(v1)
  v1
}

#' @export
load_quem_quem <- function(arq) {
  #uri <- 'file:///home/jtrecenti/Projects/crawlercarf/inst/extdata/quem_quem.pdf'
  uri <- sprintf('file://%s', arq)
  if(all(file.exists(Sys.which(c("pdfinfo", "pdftotext"))))) {
    f <- tm::readPDF(control = list(text = "-layout"))
    pdf <- f(elem = list(uri = uri), language = "en", id = "id1")
  }
  linhas <- NLP::content(pdf)
  linhas <- linhas[grep('CSRF – CÂMARA SUPERIOR DE RECURSOS FISCAIS', linhas):length(linhas)]
  d_relator <- pega_linhas(
    toupper(abjutils::rm_accent(linhas)),
    '([A-Z ]+) +(TITULAR|SUBSTITUTO?A?|SUPLENTE|PRO TEMPORE) +(FAZENDA|CONTRIBUINTES)'
  )
  names(d_relator)[2:4] <- c('relator', 'cargo', 'tipo')
  d_turma <- pega_linhas(
    toupper(abjutils::rm_accent(linhas)),
    '(SUPLENTES|TURMA)'
  )
  d_secao <- pega_linhas(
    toupper(abjutils::rm_accent(linhas)),
    '(\\? SECAO)'
  )
  ds <- data.frame(linha = linhas, stringsAsFactors = FALSE)
  ds <- dplyr::add_rownames(ds)
  ds <- dplyr::left_join(ds, d_relator, c('rowname' = 'id_linha'))
  ds <- dplyr::left_join(ds, d_turma, c('rowname' = 'id_linha'))
  ds <- dplyr::select(ds, -linha.y, -linha)
  ds <- dplyr::filter(ds, !is.na(relator) | !is.na(X2))
  ds <- dplyr::mutate(ds, camara_turma = completa(linha.x, relator))
  ds <- dplyr::filter(ds, !is.na(camara_turma))
  ds$secao <- '3ª Seção'
  ds$secao[as.numeric(ds$rowname) < as.numeric(d_secao$id_linha[3])] <- '2ª Seção'
  ds$secao[as.numeric(ds$rowname) < as.numeric(d_secao$id_linha[2])] <- '1ª Seção'
  ds$secao[as.numeric(ds$rowname) < as.numeric(d_secao$id_linha[1])] <- 'CSRF'
  ds <- dplyr::select(ds, relator, cargo, tipo, camara_turma, secao)
  ds
}


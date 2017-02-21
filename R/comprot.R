#' @export
mov_comprot <- function(n_processo) {
  p <- gsub('[[:punct:]]', '', n_processo)
  url <- sprintf('http://comprot.fazenda.gov.br/E-Gov/PvC_Mov_Consulta_Movimentos.asp?processoQ=%s&DDMovimentoQ=01042014&SQOrdemQ=0', p)
  r <- httr::GET(url)
  cont <- httr::content(r, 'text')
  html <- XML::htmlParse(cont, encoding='UTF-8')
  tabela_node <- XML::getNodeSet(html, "//table[@cellpadding='4']")
  if(length(tabela_node) == 0) {
    erro_node <- XML::getNodeSet(html, "//font[@color='red']")
    if(length(erro_node) == 0) {
      return(data.frame(n_processo=n_processo,
                        erro='erro desconhecido',
                        stringsAsFactors=F))
    }
    erro <- stringr::str_trim(XML::xmlValue(erro_node[[1]]))
    return(data.frame(n_processo=n_processo, erro=erro, stringsAsFactors=F))
  }
  tabela_parsed <- XML::readHTMLTable(tabela_node[[1]], header=T, stringsAsFactors=F)
  names(tabela_parsed) <- abjutils::rm_accent(tolower(gsub(' ', '_', stringr::str_trim(gsub(' +', ' ', names(tabela_parsed))))))
  tabela_parsed$n_processo <- n_processo
  return(tabela_parsed)
}

#' @export
parse_comprot_um <- function(r) {
  try({
    html <- XML::htmlParse(r, encoding = 'UTF-8')
    tabela <- rvest::html_node(html, xpath = '//table[1]//table')
    tab <- rvest::html_table(tabela, fill = TRUE)
    names(tab) <- c('key', 'val')
    tab <- dplyr::filter(tab, stringr::str_detect(key, '\\:'))
    tab <- dplyr::mutate(tab,
                         key = abjutils::rm_accent(key),
                         key = gsub('[^a-z ]', '', tolower(key)),
                         key = stringr::str_trim(key),
                         key = gsub(' +', '_', key))
    other_keys <- tab[tab$key == 'sistemas_profisc', 'val']
    other_keys <- stringr::str_split_fixed(other_keys, ' {2,}', 3)[1, -1]
    other_keys <- stringr::str_split_fixed(other_keys, '\\:', 2)
    d_other_keys <- data.frame(other_keys, stringsAsFactors = FALSE)
    names(d_other_keys) <- c('key', 'val')
    d_other_keys$key <- gsub('[^a-z ]', '_', stringr::str_trim(tolower(d_other_keys$key)))
    tab <- dplyr::mutate(tab,
                         val = ifelse(key == 'sistemas_profisc',
                                      stringr::str_split_fixed(val, ' {2,}', 2)[, 1],
                                      val))
    d <- dplyr::bind_rows(tab, d_other_keys)
    return(d)
  })
  return(data.frame(erro = 'erro', stringsAsFactors = FALSE))
}

#' @export
load_comprot <- function(arqs, spread = TRUE) {
  d <- data.frame(arq = arqs, stringsAsFactors = FALSE)
  d <- dplyr::group_by(d, arq)
  d <- dplyr::do(d, parse_comprot_um(.$arq))
  d <- dplyr::ungroup(d)
  if(spread) {
    if(!is.null(d$erro)) d <- dplyr::select(d, -erro)
    d <- dplyr::filter(d, !is.na(key), key != '')
    d <- dplyr::group_by(d, arq, key)
    d <- dplyr::summarise(d, val = paste(unique(val), collapse = '\n'))
    d <- dplyr::ungroup(d)
    d <- tidyr::spread(d, key, val)
  }
  d
}

dados_comprot_um <- function(p, path) {
  try({
    arq <- ''
    if(!is.null(path)) {
      arq <- sprintf('%s/%s.rds', path, p)
    }
    if(is.null(path) | !file.exists(arq)) {
      link <- 'http://comprot.fazenda.gov.br/E-gov/cons_dados_processo.asp?proc='
      p <- gsub('[^0-9]', '', p)
      link <- sprintf('%s%s', link, p)
      r <- httr::GET(link, config = list(followlocation = FALSE))
      if(r$status_code == 200) {
        d <- parse_comprot_um(r)
        if(!is.null(path)) {
          saveRDS(d, arq)
        }
        return(d)
      } else if(r$status_code == 302) {
        return(data.frame(erro = 'nao existe', stringsAsFactors = FALSE))
      }
    }
    if(file.exists(arq)) {
      d <- readRDS(arq)
      return(d)
    }
  })
  cat(p, '\n')
  return(data.frame(erro = 'erro', stringsAsFactors = FALSE))
}

#' @export
dados_comprot <- function(p, spread = TRUE, path = NULL) {
  p <- gsub('[^0-9]', '', p)
  d <- data.frame(n_processo = p, stringsAsFactors = FALSE)
  d <- dplyr::group_by(d, n_processo)
  d <- dplyr::do(d, dados_comprot_um(.$n_processo, path = path))
  d <- dplyr::ungroup(d)
  if(spread) {
    if(length(d$erro) > 0) d <- dplyr::select(d, -erro)
    d <- dplyr::filter(d, !is.na(key))
    d <- tidyr::spread(d, key, val)
  }
  d
}


#' Faz o mesmo que dados comprot mas muito mais rapido
#'
#' @export
dados_comprot2 <- function(p, path, nmax = 10, usr = 'jtrecenti') {
  link <- 'http://comprot.fazenda.gov.br/E-gov/cons_dados_processo.asp?proc='
  p <- gsub('[^0-9]', '', p)
  d <- data.frame(n_processo = p, stringsAsFactors = FALSE)
  d <- dplyr::mutate(d, arq = sprintf('%s/%s.html', path, p),
                     link = sprintf('%s%s', link, p),
                     script = system.file('Rscript/dados-comprot-um.R', package = 'crawlercarf'))
  d <- dplyr::filter(d, !file.exists(arq))
  d <- dplyr::group_by(d, n_processo, arq, link)
  d <- dplyr::do(d, r = dados_comprot_um_async(.$script, .$n_processo, .$arq, .$link, nmax, usr))
  d <- dplyr::ungroup(d)
  d
}

dados_comprot_um_async <- function(script, p, arq, link, nmax, usr) {
  if(runif(1) > .99) {
    inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
    inst <- inst[grep('R$', inst)]
    cat(length(inst), '\n')
    while(length(inst) > nmax) {
      cat('esperou')
      Sys.sleep(1)
      inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
      inst <- inst[grep('R$', inst)]
    }
  }
  system(sprintf('Rscript %s %s %s %s', script, p, arq, link), wait = FALSE)
  return('ok')
}

#' @export
dados_comprot_um2 <- function(p, arq, link) {
  r <- httr::GET(link, config = list(followlocation = FALSE))
  while(r$status_code == 500) {
    Sys.sleep(1)
    r <- httr::GET(link, config = list(followlocation = FALSE))
  }
  if(r$status_code == 200) {
    h <- httr::content(r, 'text')
    #     saveRDS(parse_comprot_um(h), arq)
    #     return('ok')
    result <- 'ok'
  } else if(r$status_code == 302) {
    h <- 'nao existe'
    result <- h
  }
  cat(h, file = arq)
  return(result)
}

download_comprot_async_um <- function(w, nmax, usr) {
  if(runif(1) > .99) {
    inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
    inst <- inst[grep('wget', inst)]
    #cat(length(inst), '\n')
    while(length(inst) > nmax) {
      cat('esperou')
      Sys.sleep(1)
      inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
      inst <- inst[grep('wget', inst)]
    }
  }
  system(w, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
}

#' @export
download_comprot_async <- function(p, path, nmax = 100, usr = 'jtrecenti') {
  p <- unique(gsub('[^0-9]', '', p))
  link <- paste0('http://comprot.fazenda.gov.br/E-gov/cons_dados_processo.asp?proc=', p)
  arq <- sprintf('%s/%s.html', path, p)
  wget <- sprintf('wget %s -O %s', link, arq)
  wget <- wget[!file.exists(arq)]
  aux <- sapply(wget, download_comprot_async_um, nmax, usr)
  invisible(aux)
}

# #' @export
# dados_comprot_um_async2 <- function(p) {
#   script <- '/home/julio/Projects/crawlercarf/inst/Rscript/dados-comprot-um.R'
#   if(runif(1) > .99) {
#     usr <- 'julio'
#     inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
#     inst <- inst[grep('R$', inst)]
#     cat(length(inst), ' requisicoes em paralelo', '\n')
#     while(length(inst) > 100) {
#       cat('esperou')
#       Sys.sleep(1)
#       inst <- system2('ps', sprintf('-u %s', usr), stdout = TRUE)
#       inst <- inst[grep('R$', inst)]
#     }
#   }
#   system(sprintf('Rscript %s %s', script, p), wait = FALSE)
# }

calcula_dig_um <- function(p) {
  num <- as.numeric(strsplit(p, '')[[1]])
  dig1 <- (11 - sum(num * (16:2)) %% 11) %% 10
  num_dig1 <- c(num, dig1)
  dig2 <- (11 - sum(num_dig1 * (17:2)) %% 11) %% 10
  dig <- paste0(dig1, dig2)
  dig
}

#' @export
calcula_dig <- function(p) {
  sapply(p, calcula_dig_um)
}

#' @export
gera_numeros_validos <- function(anos, agencias) {
  numeros <- sprintf('%06d', 0:(1e6-1))
  tab_num <- data.frame(expand.grid(agencias, numeros, anos),
                        stringsAsFactors = FALSE)
  names(tab_num) <- c('agencia', 'numero', 'ano')
  tab_num <- dplyr::arrange(tab_num, ano, agencia, numero)
  tab_num$dig <- calcula_dig(with(tab_num, paste0(agencia, numero, ano)))
  tab_num$n_processo <- with(tab_num, paste0(agencia, numero, ano, dig))
  tab_num
}











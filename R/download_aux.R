#' remasterized auxiliary function to download one lawsuit
#'
#' @param id Lawsuit number
#' @param path Folder where to save files
#' @param interval A vector with two dates for beginning and end of query
#' @param download_pdfs Whether or not to download the attachments
#' 
download_lawsuit_v2 <- function(id, path,download_pdfs = TRUE,pb){
    
    interval <- c("02/2016", "07/2018")
    
    dir.create(path, FALSE, TRUE)
    path <- normalizePath(path)
    
    download_lawsuit_(id, path, interval, download_pdfs)
    
    arq <- str_c(path,'/',id)
    
    i=1
    while(i <= 5){
      if(!dir.exists(arq)){
        download_lawsuit_(id, path, interval, download_pdfs)
      }
      i=i+1
    }
    pb$tick()
  }


#' Auxiliary function to download one lawsuit
#'
#' @param id Lawsuit number
#' @param path Folder where to save files
#' @param interval A vector with two dates for beginning and end of query
#' @param download_pdfs Whether or not to download the attachments
#' @return Path to downloaded file
download_lawsuit_ <- function(id, path, interval, download_pdfs) {

  # Create lawsuit directory
  id <- stringr::str_replace_all(id, "[^0-9]", "")
  path <- glue::glue(path, "/", id)
  dir.create(path, FALSE, TRUE)
  file <- glue::glue(path, "/", id, ".html")

  # URLs
  base <- "http://carf.fazenda.gov.br/sincon/public/pages/ConsultarInformacoesProcessuais/"
  u_search <- glue::glue(base, "consultarInformacoesProcessuais.jsf")
  u_show <- glue::glue(base, "exibirProcesso.jsf")

  # Ping the website to get basic info
  r_ping <- httr::GET(u_search)

  # Build search query
  search_query <- list(
    "AJAXREQUEST" = "_viewRoot",
    "formConsultaProcesso" = "formConsultaProcesso",
    "dataInicialInputDate" = interval[1],
    "dataInicialInputCurrentDate" = interval[1],
    "dataFinalInputDate" = interval[2],
    "dataFinalInputCurrentDate" = interval[2],
    "campo_pesquisa" = "1",
    "valor_pesquisa" = id,
    "javax.faces.ViewState" = get_viewstate(r_ping),
    "j_id30" = "j_id30")

  # Run search
  r_search <- httr::POST(u_search, body = search_query)

  # Build show query
  show_query <- list(
    "formProcesso2" = "formProcesso2",
    "javax.faces.ViewState" = get_viewstate(r_search),
    "formProcesso2:todasLinhas" = "formProcesso2:todasLinhas")

  # Get lawsuit
  r_show <- httr::POST(u_show, body = show_query)
  xml2::write_html(xml2::read_html(r_show), file)

  # Download PDFs if necessary
  pdfs <- c()
  if (download_pdfs) {

    # Get information about attachments
    pdfs <- r_show %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//*[@id='formProcesso2:tbandamentos:tbody_element']//a") %>%
      purrr::map(~c(xml2::xml_attr(.x, "title"), xml2::xml_attr(.x, "id")))

    # Iterate over PDFs
    pdfs <- purrr::map_chr(pdfs, download_pdf, path, u_show, get_viewstate(r_show))
  }

  return(c(file, pdfs))
}

#' Download CARF decision
#'
#' @param id Character vector with a decision numbers
#' @param path Directory where to save file
#' @param interval A vector with two dates for beginning and end of query
#' @param download_pdfs Whether or not to download the attachments
#' @return A character vetor with the path to the downloaded file
download_decision_ <- function(id, path, interval, download_pdfs) {

  # URLs
  base <- "http://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/"
  u_search <- glue::glue(base, "consultarJurisprudenciaCarf.jsf")
  u_page <- glue::glue(base, "listaJurisprudenciaCarf.jsf")

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
    "dataInicialInputDate" = interval[1],
    "dataFinalInputDate" = interval[2],
    "campo_pesquisa1" = "3",
    "valor_pesquisa1" = id,
    "campo_pesquisa2" = "1",
    "campo_pesquisa3" = "1",
    "javax.faces.ViewState" = get_viewstate(r_ping))
  search_query[[j_id]] <- j_id

  # Run search
  r_search <- year_post(u_search, r_ping, search_query)

  # Create lawsuit query
  dcis_query <- list(
    "formAcordaos" = "formAcordaos",
    "javax.faces.ViewState" = get_viewstate(r_search))
  form <- glue::glue("tblJurisprudencia:0:numDecisao")
  dcis_query[[form]] <- form

  # Get lawsuit
  r_dcis <- year_post(u_page, r_search, dcis_query)

  # Get lawsuit ID
  lwst_id <- r_dcis %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='formAcordaos:numProcesso']") %>%
    stringr::str_replace_all("[^0-9]", "")

  # Get decision
  dcis_id <- r_dcis %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='formAcordaos:numDecisao']") %>%
    stringr::str_replace_all("[^0-9]", "")

  # Create lawsuit directory
  path <- glue::glue(path, "/", lwst_id, "_", dcis_id)
  dir.create(path, FALSE, TRUE)
  file <- glue::glue(path, "/", lwst_id, "_", dcis_id, ".html")

  # Save decision to disk
  xml2::write_html(xml2::read_html(r_dcis), file)

  # Download PDFs if necessary
  pdfs <- c()
  if (download_pdfs) {

    # Get information about attachments
    pdfs <- r_dcis %>%
      xml2::read_html() %>%
      xml2::xml_find_all("//*[@id='formAcordaos:tdivAnexos']//a") %>%
      purrr::map(~c(xml2::xml_attr(.x, "title"), xml2::xml_attr(.x, "id")))

    # Iterate over PDFs
    pdfs <- purrr::map_chr(pdfs, download_pdf, path, u_page, get_viewstate(r_dcis))
  }

  return(c(file, pdfs))
}

#' Download all lawsuits form a page
#'
#' @param page_num Number of the page to download
#' @param r_search Response from the search query run in [download_year_carf()]
#' @param path Path to the page directory where to save decisions
#' @param pb Progress bar generated by [download_year_carf()]
#' @return A character vector with the paths to the downloaded files
download_page_year <- function(page_num, r_search, path, pb) {

  # URLs
  base <- "https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/"
  u_page <- glue::glue(base, "listaJurisprudenciaCarf.jsf")

  # Build page query
  page_query <- list(
    "AJAXREQUEST" = "_viewRoot",
    "formAcordaos" = "formAcordaos",
    "javax.faces.ViewState" = get_viewstate(r_search),
    'dataScroller_1' = as.character(page_num),
    'ajaxSingle' = 'dataScroller_1',
    "AJAX:EVENTS_COUNT" = "1")

  # Create folder and file name
  path <- glue::glue(path, "/page_", stringr::str_pad(page_num, 4, "left", "0"))
  dir.create(path, FALSE, TRUE)
  file <- glue::glue(path, "/page.html")

  # Get page
  r_page <- year_post(u_page, r_search, page_query, file); pb$tick()

  # Iterate over lawsuits on the page
  range <- ((page_num-1)*10):((page_num-1)*10+9)
  files <- purrr::map_chr(range, download_decision_year_, r_search, path, pb)

  return(c(file, files))
}

#' Download a decision form a page
#'
#' @param dcis_num Number of the decision to download
#' @param r_page Response from the page query run in [download_year_page()]
#' @param path Path to the page directory where to save decision
#' @param pb Progress bar generated by [download_year_carf()]
#' @return A character vector with the path to the downloaded file
download_decision_year_ <- function(dcis_num, r_page, path, pb) {

  # URLs
  base <- "https://carf.fazenda.gov.br/sincon/public/pages/ConsultarJurisprudencia/"
  u_page <- glue::glue(base, "listaJurisprudenciaCarf.jsf")

  # Create lawsuit query
  dcis_query <- list(
    "formAcordaos" = "formAcordaos",
    "javax.faces.ViewState" = get_viewstate(r_page))
  form <- glue::glue("tblJurisprudencia:{dcis_num}:numDecisao")
  dcis_query[[form]] <- form

  # Get lawsuit
  r_dcis <- year_post(u_page, r_page, dcis_query)

  # Get lawsuit ID
  lwst_id <- r_dcis %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='formAcordaos:numProcesso']") %>%
    stringr::str_replace_all("[^0-9]", "")

  # Get decision
  dcis_id <- r_dcis %>%
    xml2::read_html() %>%
    xml2::xml_find_first("//*[@id='formAcordaos:numDecisao']") %>%
    stringr::str_replace_all("[^0-9]", "")

  # Save lawsuit to disk
  file <- glue::glue(path, "/", lwst_id, "_", dcis_id, ".html")
  xml2::write_html(xml2::read_html(r_dcis), file); pb$tick()

  return(file)
}

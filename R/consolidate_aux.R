#' Get information about pdf lawsuit
#'
#' @param path directory where exists the directory of pdf decisions
#' @return A list

puxa_pdf_ <- possibly(function(path){
  
  pdf <- pdftools::pdf_text(path)
  
  return(pdf[[1]])
}, 'pdf nao encontrado')


#' Get informations about section of a lawsuit
#'
#' @param id a lawsuit id
#' @param path directory where exists the directory of pdf decisions
#' @return A tibble
classificador_turma_ <- function(id, path){
  #mudar aqui
  pasta <- stringr::str_c(path,'/',id,"/decisao_",id,'.pdf')
  
  pdf<-puxa_pdf_(pasta) %>%
    str_trim
  
  CSRF <- str_extract(pdf,'(CSRF)')
  
  secao <- if_else(is.na(CSRF),str_extract(pdf, '(S)[[0-9]?I?]'), 'CSRF')
  camara <- if_else(is.na(CSRF),str_extract(pdf, '(C)[[0-9]?I?]'), 'CSRF')
  turma <- str_extract(pdf, '(T)[[0-9]?I?]')
  
  secao <- case_when(secao == 'CSRF'~'CSRF',
                     str_sub(secao,start = 2) == 'I'~ '1a SECAO',
                     T~str_c(str_sub(secao,start = 2), 'a SECAO'))
  
  camara <- case_when(camara == 'CSRF'~'CSRF',
                      str_sub(camara,start = 2) == 'I'~ '1a CAMARA',
                      T~str_c( str_sub(camara,start = 2),'a CAMARA'))
  
  ord <- case_when(!is.na(CSRF) ~'',
                   str_detect(pdf, regex('ordinária', ignore_case = T)) | str_detect(pdf, regex('ordinaria', ignore_case = T)) ~'ORDINARIA',
                   T~'EXTRAORDINARIA')
  
  turma <- if_else(str_sub(turma,start = 2) == 'I', str_c('1a TURMA ', ord), str_c(str_sub(turma,start = 2), 'a TURMA ', ord))
  
  
  
  resp <- data_frame(n_processo = id,
                     SECAO = secao,
                     CAMARA = camara,
                     TURMA = turma)
  
  
  return(resp)
}

#' Consolidate decisions
#'
#' @param pages `pages` element returned by [parse_decision()]
#' @param decisions `decisions` element returned by [parse_decision()]
#' @return A tibble
consolidate_decisions <- function(pages, decisions) {
  
  # Get number of resolutions
  num_resolutions <- sum(pages$type_decision == "R")
  
  # Create auxiliar table
  pages_aux <- pages %>%
    dplyr::filter(type_decision == "A") %>%
    dplyr::distinct(id_lawsuit, id_decision, .keep_all = TRUE) %>%
    dplyr::select(id_lawsuit, id_decision, date_publication)
  
  # Get residue
  residue <- decisions %>%
    filter(!is.na(id_lawsuit)) %>%
    dplyr::anti_join(pages_aux, c("id_lawsuit", "id_decision")) %>%
    dplyr::select(id_lawsuit, id_decision)
  
  # Generate final result
  if (nrow(residue) == num_resolutions) {
    out <- decisions %>%
      dplyr::distinct(id_lawsuit, id_decision, .keep_all = TRUE) %>%
      dplyr::inner_join(pages_aux, c("id_lawsuit", "id_decision"))
  }
  
  return(out)
}

#' Consolidate origins
#'
#' @param data Data returned by [consolidate_decisions()]
#' @param comprot Table returned by [get_comprot()]
#' @return A tibble
consolidate_origins <- function(data, comprot) {
  
  # Create auxiliary table with origin data
  comprot_aux <- comprot %>%
    dplyr::mutate(type_party = ifelse(
      stringr::str_length(cpf_cnpj) > 11, "Juridica", "Fisica")) %>%
    dplyr::select(id, subject, date, type_party, cpf_cnpj) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::mutate(origin = ifelse(
      stringr::str_detect(subject, "AUTO DE INFRACAO|IMPUGNACAO|MULTA"),
      "auto_infracao_impugnacao_multa", "outros"))
  
  # Create final table
  dplyr::inner_join(data, comprot_aux, c("id_lawsuit" = "id"))
}

#' Consolidate states (UF)
#'
#' @param data Data returned by [consolidate_origins()]
#' @return A tibble
consolidate_states <- function(data) {
  
  # State abbreviations
  states <- c(
    "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
    "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO",
    "RR", "RS", "SC", "SE", "SP", "TO")
  
  # Auxiliary funcion for cleaning the state data
  detect_uf <- function(state, origin, pattern_state, pattern_origin, replacement) {
    ifelse(
      state == pattern_state & stringr::str_detect(origin, pattern_origin),
      replacement, state)
  }
  
  # Create a table with data on lawsuit ID and their origin states
  origins <- jurisprudence_origins %>%
    dplyr::mutate(state = stringr::str_sub(origin, -2)) %>%
    dplyr::mutate(
      state = ifelse(state %in% states, state, "VAZIO") %>%
        detect_uf(origin, "VAZIO", "\\-RJ", "RJ") %>%
        detect_uf(origin, "VAZIO", "\\-SP", "SP") %>%
        detect_uf(origin, "VAZIO", "\\-JOINV", "SC") %>%
        detect_uf(origin, "VAZIO", "\\-AERO VIRAC", "SP") %>%
        detect_uf(origin, "VAZIO", "\\-FLORIANOP", "SC") %>%
        detect_uf(origin, "VAZIO", "\\-BLUMEN", "RS") %>%
        detect_uf(origin, "VAZIO", "\\-PAULIST", "SP") %>%
        detect_uf(origin, "VAZIO", "\\-VIT", "RJ")) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::select(id, state)
  
  # Add state data to final dataset
  data %>%
    dplyr::mutate(five_digs = stringr::str_sub(id_lawsuit, 1, 5)) %>%
    dplyr::left_join(origins, c("five_digs" = "id")) %>%
    dplyr::select(-five_digs) %>%
    dplyr::mutate(state = ifelse(is.na(state), "VAZIO", state))
}

#' Consolidate appeals
#'
#' @param data Data returned by [consolidate_states()]
#' @return A tibble
consolidate_appeals <- function(data) {
  
  # Regex for getting appeal types
  type_regex <- stringr::regex("embargo|extrao|agrav|dilig|div", ignore_case = TRUE)
  
  # Detect kinds of appeals and replace them
  detect_appeal <- function(type_appeal, summary, pattern_summary, replacement) {
    clean_txt <- . %>% abjutils::rm_accent() %>% stringr::str_to_lower()
    ifelse(
      stringr::str_detect(type_appeal, "INFORM") & stringr::str_detect(clean_txt(summary),
                                                                       pattern_summary), replacement, type_appeal)
  }
  
  # Create final table
  data %>%
    dplyr::mutate(type_appeal = ifelse(
      stringr::str_detect(type_appeal, type_regex), NA, type_appeal)) %>%
    dplyr::filter(!is.na(type_appeal)) %>%
    dplyr::mutate(
      type_appeal = type_appeal %>%
        detect_appeal(summary, "recurso volunt", "RECURSO VOLUNTARIO") %>%
        detect_appeal(summary, "recurso de oficio", "RECURSO DE OF\u00cdCIO") %>%
        detect_appeal(summary, "recurso volunt", "RECURSO VOLUNTARIO") %>%
        detect_appeal(summary, "recurso de oficio", "RECURSO DE OF\u00cdCIO")) %>%
    dplyr::mutate(
      date_publication = lubridate::dmy(date_publication),
      date_session = lubridate::dmy(date_session)) %>%
    dplyr::arrange(id_lawsuit, dplyr::desc(date_publication)) %>%
    dplyr::group_by(id_lawsuit) %>%
    dplyr::distinct(type_appeal, .keep_all = TRUE) %>%
    dplyr::ungroup()
}

#' Consolidate votes
#'
#' @param data Data returned by [consolidate_appeals()]
#' @return A tibble
consolidate_votes <- function(data) {
  
  # Regex
  prtc_regex <- stringr::regex("Participaram.+(lheir)[^ ]+ ?\\:?")
  unmt_regex <- stringr::regex("por unanimidade", ignore_case = TRUE)
  mjrt_regex <- stringr::regex("por maioria|maioria de votos", ignore_case = TRUE)
  qalt_regex <- stringr::regex("(por|pelo)( voto de)? qualidade", ignore_case = TRUE)
  
  # Create vote column
  data %>%
    dplyr::mutate(
      decision = stringr::str_replace_all(decision, "\r\n", " "),
      decision = stringr::str_replace_all(decision, " +", " "),
      participated = stringr::str_split_fixed(decision, prtc_regex, 2)[,2],
      unanimity = stringr::str_detect(decision, unmt_regex),
      majority = stringr::str_detect(decision, mjrt_regex),
      quality = stringr::str_detect(decision, qalt_regex),
      unanimity = ifelse(unanimity, "Unanimidade", NA),
      majority = ifelse(majority, "Maioria", NA),
      quality = ifelse(quality, "Voto de qualidade", NA),
      vote = paste(unanimity, majority, quality, sep = ", "),
      vote = stringr::str_replace_all(vote, ", NA|NA,", ""),
      vote = stringr::str_trim(vote)) %>%
    dplyr::select(-unanimity, -majority, -quality)
}

#' Consolidate results
#'
#' @param data Data returned by [consolidate_votes()]
#' @return A tibble
consolidate_results <- function(data) {
  
  # Regex
  dn <- "negar?(do)?(am)? (o )?provimento|negou se (o )?provimento|recurso nao provido"
  gv <- "dar?(do)? (o )?provimento|deu se (o )?provimento|recurso provido"
  pt <- "em parte|parcial"
  dl <- "diligencia"
  uk <- "[^a-z0-9]conhec"
  nl <- "nul(a|o|i)"
  
  data %>%
    dplyr::mutate(
      # Clean decision
      decision2 = clean_text(decision),
      # Detect different results
      deny = stringr::str_detect(decision2, dn),
      give = stringr::str_detect(decision2, gv),
      part = stringr::str_detect(decision2, pt),
      diligence = stringr::str_detect(decision2, dl),
      unknown = stringr::str_detect(decision2, uk),
      nullify = stringr::str_detect(decision2, nl),
      sum = deny + (give|part) + diligence + unknown + nullify,
      # Transform results into text
      deny = ifelse(deny, "negar_provimento", ""),
      give = ifelse(give, "dar_provimento", ""),
      part = ifelse(part, "em_parte", ""),
      diligence = ifelse(diligence, "diligencia", ""),
      unknown = ifelse(unknown, "nao_conhecer", ""),
      nullify = ifelse(nullify, "nao_conhecer", ""),
      # Join results
      result = stringr::str_c(
        deny, give, part, diligence, nullify, unknown, sep = " ") %>%
        stringr::str_replace_all(" +", " ") %>%
        stringr::str_trim(),
      # Create a single, intelligible result
      result = ifelse(
        stringr::str_detect(result, "em_parte"),
        "em_parte", result),
      result = ifelse(
        stringr::str_detect(result, "negar") & stringr::str_detect(result, "dar_"),
        "em_parte", result),
      result = ifelse(
        stringr::str_detect(result, "negar"),
        "negar_provimento", result),
      result = ifelse(
        stringr::str_detect(result, "anular") & stringr::str_detect(result, "dar_"),
        "anular", result),
      result = ifelse(
        stringr::str_detect(result, "anular|nao_conhecer|dilig"),
        "anulado, nao conhecido ou diligencia", result),
      result = ifelse(result == "", "vazio", result)) %>%
    dplyr::select(-(deny:sum), -decision2)
}

#' Consolidate taxes
#'
#' @param data Data returned by [consolidate_results()]
#' @return A tibble
consolidate_taxes <- function(data) {
  
  # Regex for taxes
  rgx <- purrr::partial(stringr::regex, ignore_case = TRUE)
  taxes <- list(
    pis = rgx("pis|programas? de integracao social|pasep"),
    cofins = rgx("cofins|contribuicao para financiamento"),
    contprev = rgx("contribuicao previdenciaria|contrib[a-z]+ prev|assuntos previdenciarios"),
    csl = rgx("csl|cssl|csll|contribuicao social"),
    finsocial = rgx("finsocial"),
    ii = rgx("imposto (de )?importacao"),
    iof = rgx("iof|imposto sobre operac(oes)?(ao)? financeiras?"),
    ipi = rgx("ipi|imposto sobre produtos? industrializados?"),
    irf = rgx("irf|ir fonte|imposto.+retido na fonte|irrf"),
    irpf = rgx("irpf|imposto de renda de pessoa fisica"),
    irpj = rgx("irpj|imposto de renda de pessoa juridica"),
    itr = rgx("itr|imposto territorial rural"),
    simples = rgx("simples"),
    cpmf = rgx("cpmf"),
    outros = rgx("outros|diversos"))
  
  # Join all texts
  data <- data %>%
    dplyr::mutate(
      summary2 = clean_text(summary),
      subject2 = clean_text(subject),
      tax2 = clean_text(tax),
      all_texts = paste(summary2, subject2, tax2))
  
  # Create one column for each tax
  for (i in seq_along(taxes)) {
    name <- stringr::str_c("tax_", names(taxes)[i])
    title <- stringr::str_to_upper(names(taxes)[i])
    data <- data %>%
      dplyr::mutate(
        rlang::UQ(name) := all_texts %>%
          stringr::str_detect(taxes[[i]]) %>%
          dplyr::if_else(title, ""))
  }
  
  # Create final table
  data %>%
    tidyr::unite(taxes, dplyr::starts_with("tax_"), sep = " ") %>%
    dplyr::mutate(
      taxes = stringr::str_replace_all(taxes, " +", " "),
      taxes = stringr::str_trim(taxes),
      taxes = ifelse(taxes == "", "VAZIO", taxes)) %>%
    # dplyr::group_by(taxes) %>%
    # dplyr::mutate(num_taxes = length(taxes)) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(taxes = ifelse(num_taxes < 1000, "OTHER COMBINATIONS", taxes)) %>%
    # dplyr::count(taxes, sort = TRUE) %>%
    dplyr::select(-all_texts, -dplyr::ends_with("2"), -tax)
}

#' Consolidate rapporteurs
#'
#' @param data Data returned by [consolidate_taxes()]
#' @return A tibble
consolidate_rapporteurs <- function(data) {
  
  # Aux function for cleaning rapporteurs
  clean_rapporteur <- . %>%
    abjutils::rm_accent() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("RELATORA? ", "") %>%
    stringr::str_replace_all("- ?(REDATORA? )?(DESIGNADO?A? )?AD?S? HOC", "") %>%
    stringr::str_replace_all(" REDATOR DESIGNADO|DESIGNADA?O?$", "") %>%
    stringr::str_replace_all(" ?-? ?RELATORA?F?(KJH)?$", "") %>%
    stringr::str_replace_all(", RELATOR\\.$", "") %>%
    stringr::str_replace_all("^CONSELHEIRO?A? ", "") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("NAO SE APLICA|RELATOR|^$|^MAR$", "VAZIO/NAO SE APLICA")
  
  # Count rapporteurs
  rapporteur_count <- data %>%
    dplyr::mutate(rapporteur = clean_rapporteur(rapporteur)) %>%
    dplyr::count(rapporteur, sort = TRUE)
  
  # Clean rapporteurs
  rapporteurs <- rapporteur_data %>%
    dplyr::filter(rapporteur != "VAGO") %>%
    dplyr::mutate(
      rapporteur = ifelse(
        rapporteur == "MERCIA HELENA TRAJANO D AMORIM",
        "MERCIA HELENA TRAJANO DAMORIM", rapporteur),
      rapporteur = ifelse(
        rapporteur == "KLEBER FERREIRA ARAUJO",
        "KLEBER FERREIRA DE ARAUJO", rapporteur),
      rapporteur = ifelse(
        rapporteur == "ANDRE LUIS MARISCO LOMBARDI",
        "ANDRE LUIS MARSICO LOMBARDI", rapporteur))
  
  # Create auxiliary table
  rapporteur_aux <- rapporteur_count %>%
    dplyr::select(-n) %>%
    dplyr::filter(!stringr::str_detect(rapporteur, "VAZIO")) %>%
    dplyr::inner_join(rapporteur_goliva, "rapporteur") %>%
    dplyr::select(-n)
  
  # Finalize rapporteurs
  rapporteur_final <- rapporteur_count %>%
    dplyr::select(-n) %>%
    dplyr::filter(!stringr::str_detect(rapporteur, "VAZIO")) %>%
    dplyr::inner_join(rapporteur_data, "rapporteur") %>%
    dplyr::bind_rows(rapporteur_aux) %>%
    dplyr::mutate(type = ifelse(type == "TRABALHADORES", "CONTRIBUINTES", type)) %>%
    dplyr::filter(!section %in% c("CSRF", "CSFR")) %>%
    dplyr::group_by(rapporteur) %>%
    dplyr::mutate(n = length(rapporteur)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      chamber = ifelse(n > 1, NA, chamber),
      section = ifelse(n > 1, NA, section)) %>%
    dplyr::select(-n) %>%
    dplyr::distinct(rapporteur, .keep_all = TRUE)
  
  # Create final table
  data %>%
    dplyr::mutate(rapporteur = clean_rapporteur(rapporteur)) %>%
    dplyr::left_join(rapporteur_final, "rapporteur") %>%
    dplyr::mutate(
      title = ifelse(is.na(title), "N\u00c3O IDENTIFICADO", title),
      type = ifelse(is.na(type), "N\u00c3O IDENTIFICADO", type),
      chamber = ifelse(is.na(chamber), "VAZIO", chamber),
      section = ifelse(is.na(section), "VAZIO", section),
      section = ifelse(
        stringr::str_detect(section, "1"), "PRIMEIRA SECAO",
        ifelse(stringr::str_detect(section, "2"), "SEGUNDA SECAO",
               ifelse(stringr::str_detect(section, "3"), "TERCEIRA SECAO",
                      section))))
}

#' Consolidate chambers
#'
#' @param data Data returned by [consolidate_rapporteurs()]
#' @return A tibble
consolidate_chambers <- function(data) {
  
  # Shortcut for empty keywords
  empty <- c("VAZIO", "N\u00c3O IDENTIFICADO")
  
  # Create final table
  data %>%
    dplyr::mutate(
      chamber = ifelse(stringr::str_detect(type_appeal, "ESPECIAL"), "CSRF", chamber),
      section = ifelse(stringr::str_detect(type_appeal, "ESPECIAL"), "CSRF", section),
      section = ifelse(type_party == "F\u00edsica" & section %in% empty, "SEGUNDA SECAO", section),
      section = ifelse(
        stringr::str_detect(taxes, "IRPJ|CSL|SIMPLES") &
          section %in% empty, "PRIMEIRA SECAO", section),
      section = ifelse(
        stringr::str_detect(taxes, "FINSOCIAL|PIS|COFINS|CPMF|II|IPI") &
          section %in% empty, "TERCEIRA SECAO", section),
      section = ifelse(section %in% empty, "N\u00c3O IDENTIFICADO", section),
      chamber = ifelse(chamber %in% empty, "N\u00c3O IDENTIFICADO", chamber))
}

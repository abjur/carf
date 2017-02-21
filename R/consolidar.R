#' Consolida todas as informacoes
#'
#' Parte da base de jurisprudencia (indexador) e consolida todas as informacoes
#' baseando-se nos processos contidos nessa lista e nas bases de dados do
#' pacote.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import lubridate
#' @importFrom magrittr %>%
#'
#' @export
consolidar <- function(d_pags, d_proc) {
  d_final <- consolidar_datas(d_pags, d_proc) %>%
    consolidar_origens %>%
    consolidar_ufs_origem %>%
    consolidar_tipos_recurso %>%
    consolidar_votacoes %>%
    consolidar_resultados %>%
    consolidar_tributos %>%
    consolidar_conselheiros %>%
    consolidar_turmas %>%
    consolidar_tempos %>%
    tibble::as_tibble()
  d_final <- d_final %>%
    select(arq = item, n_processo, n_acordao, tipo_recurso, contribuinte = contibuinte,
           relator, txt_ementa, txt_decisao, dt_publicacao,
           tipo_pessoa, cn, origem, uf, votacao, resultado, tributos,
           tipo, camara_turma, secao)

  d_final <- d_final %>%
    mutate(tipo_recurso = ifelse(str_detect(tipo_recurso, 'RECURSO DE OF'),
                                 'RECURSO DE OFICIO', tipo_recurso)) %>%
    mutate(origem = ifelse(origem == 'auto_infracao_impugnacao_multa',
                           'AUTO INFRACAO/IMPUGNACAO/MULTA',
                           'OUTROS')) %>%
    mutate(votacao = ifelse(votacao == 'NA', 'VAZIO', toupper(votacao))) %>%
    mutate(decisao = resultado,
           resultado = ifelse(decisao == 'negar_provimento' &
                                tipo_recurso %in% c('RECURSO DE OFICIO',
                                                    'RECURSO ESPECIAL DO PROCURADOR'),
                              'FAVORAVEL', resultado),
           resultado = ifelse(decisao == 'dar_provimento' &
                                tipo_recurso %in% c('RECURSO DE OFICIO',
                                                    'RECURSO ESPECIAL DO PROCURADOR'),
                              'DESFAVORAVEL', resultado),
           resultado = ifelse(decisao == 'dar_provimento' &
                                tipo_recurso %in% c('RECURSO VOLUNTARIO',
                                                    'RECURSO ESPECIAL DO CONTRIBUINTE'),
                              'FAVORAVEL', resultado),
           resultado = ifelse(decisao == 'negar_provimento' &
                                tipo_recurso %in% c('RECURSO VOLUNTARIO',
                                                    'RECURSO ESPECIAL DO CONTRIBUINTE'),
                              'DESFAVORAVEL', resultado),
           resultado = ifelse(decisao == 'em_parte',
                              'PARCIALMENTE FAVORAVEL', resultado),
           resultado = toupper(resultado),
           resultado = ifelse(str_detect(resultado, '_'), 'VAZIO', resultado)) %>%
    mutate(tipo = ifelse(str_detect(tipo, 'IDENTIF'),
                         'NAO IDENTIFICADO', tipo))

  # tabela <- d_final %>%
  #   mutate(camara_turma = toupper(abjutils::rm_accent(camara_turma))) %>%
  #   count(camara_turma, sort = T) %>%
  #   data.frame %>%
  #   mutate(camara = c('4a CAMARA', '1a CAMARA', '3a CAMARA', '2a CAMARA',
  #                     '3a CAMARA', 'CSRF', '4a CAMARA', 'NAO IDENTIFICADO',
  #                     '3a CAMARA', '1a CAMARA', '4a CAMARA', '2a CAMARA',
  #                     '2a CAMARA', '1a CAMARA', '4a CAMARA', '3a CAMARA',
  #                     '2a CAMARA', '3a CAMARA', '1a CAMARA', '1a CAMARA',
  #                     '4a CAMARA', '4a CAMARA', '4a CAMARA', '4a CAMARA',
  #                     '1a CAMARA', '3a CAMARA', '7a CAMARA', '6a CAMARA')) %>%
  #   mutate(turma  = c('2a TURMA', 'TURMA ESPECIAL', 'TURMA ESPECIAL', 'TURMA ESPECIAL',
  #                     '2a TURMA', 'CSRF',    '1a TURMA', 'NAO IDENTIFICADO',
  #                     '1a TURMA', '2a TURMA', '3a TURMA', '1a TURMA',
  #                     '2a TURMA', '1a TURMA', 'TURMA ESPECIAL', 'SUPLENTES',
  #                     'SUPLENTES', 'TURMA ESPECIAL', 'SUPLENTES', 'TERCEIRA',
  #                     'SUPLENTES', 'TURMA ESPECIAL', 'TURMA ESPECIAL', 'TURMA ESPECIAL',
  #                     'NAO IDENTIFICADO', 'NAO IDENTIFICADO', 'NAO IDENTIFICADO', 'NAO IDENTIFICADO'))
  # d_final <- d_final %>%
  #   mutate(camara_turma = toupper(abjutils::rm_accent(camara_turma))) %>%
  #   inner_join(tabela, 'camara_turma')
  cat('OK!')
  return(d_final)
}

# Informacoes contidas na jurisprudencia ---------------------------------------

#' @export
consolidar_tributos <- function(d) {
  pis <- 'pis|programas? de integracao social|pasep'
  cofins <- 'cofins|contribuicao para financiamento'
  contprev <- 'contribuicao previdenciaria|contrib[a-z]+ prev|assuntos previdenciarios'
  csl <- 'csl|cssl|csll|contribuicao social'
  finsocial <- 'finsocial'
  ii <- 'imposto (de )?importacao'
  iof <- 'iof|imposto sobre operac(oes)?(ao)? financeiras?'
  ipi <- 'ipi|imposto sobre produtos? industrializados?'
  irf <- 'irf|ir fonte|imposto.+retido na fonte|irrf'
  irpf <- 'irpf|imposto de renda de pessoa fisica'
  irpj <- 'irpj|imposto de renda de pessoa juridica'
  itr <- 'itr|imposto territorial rural'
  simples <- 'simples'
  cpmf <- 'cpmf'

  d_aux <- d %>%
    mutate(txt_ementa2=gsub(' +', ' ',
                            gsub('\n|\r', ' ',
                                 gsub('[[:punct:]]|\n\r|\r\n', ' ',
                                      tolower(txt_ementa)))),
           assunto2=gsub(' +', ' ',
                         gsub('\n|\r', ' ',
                              gsub('[[:punct:]]|\n\r|\r\n', ' ',
                                   tolower(assunto)))),
           tributo2=gsub(' +', ' ',
                         gsub('\n|\r', ' ',
                              gsub('[[:punct:]]|\n\r|\r\n', ' ',
                                   tolower(tributo)))),
           txt_aux = paste(txt_ementa2, assunto2, tributo2)) %>%
    mutate(trib_pis=str_detect(txt_aux, pis),
           trib_cofins=str_detect(txt_aux, cofins),
           trib_contprev=str_detect(txt_aux, contprev),
           trib_csl=str_detect(txt_aux, csl),
           trib_finsocial=str_detect(txt_aux, finsocial),
           trib_ii=str_detect(txt_aux, ii),
           trib_iof=str_detect(txt_aux, iof),
           trib_ipi=str_detect(txt_aux, ipi),
           trib_irf=str_detect(txt_aux, irf),
           trib_irpf=str_detect(txt_aux, irpf),
           trib_irpj=str_detect(txt_aux, irpj),
           trib_itr=str_detect(txt_aux, itr),
           trib_simples=str_detect(txt_aux, simples),
           trib_cpmf=str_detect(txt_aux, cpmf),
           trib_outros=str_detect(paste(assunto2, tributo2),
                                  'outros|diversos')) %>%
    mutate(trib_pis = ifelse(trib_pis, 'PIS', ''),
           trib_cofins = ifelse(trib_cofins, 'COFINS', ''),
           trib_contprev = ifelse(trib_contprev, 'CONTPREV', ''),
           trib_csl = ifelse(trib_csl, 'CSL', ''),
           trib_finsocial = ifelse(trib_finsocial, 'FINSOCIAL', ''),
           trib_ii = ifelse(trib_ii, 'II', ''),
           trib_iof = ifelse(trib_iof, 'IOF', ''),
           trib_ipi = ifelse(trib_ipi, 'IPI', ''),
           trib_irf = ifelse(trib_irf, 'IRF', ''),
           trib_irpf = ifelse(trib_irpf, 'IRPF', ''),
           trib_irpj = ifelse(trib_irpj, 'IRPJ', ''),
           trib_itr = ifelse(trib_itr, 'ITR', ''),
           trib_simples = ifelse(trib_simples, 'SIMPLES', ''),
           trib_cpmf = ifelse(trib_cpmf, 'CPMF', ''),
           trib_outros = ifelse(trib_outros, 'OUTROS TRIBUTOS', '')) %>%
    mutate(comb = paste(trib_pis, trib_cofins, trib_contprev, trib_csl,
                        trib_finsocial, trib_ii, trib_iof, trib_ipi,
                        trib_irf, trib_irpf, trib_irpj, trib_itr,
                        trib_simples, trib_cpmf),
           comb = str_trim(gsub(' +', ' ', comb)),
           comb = ifelse(comb == '' & trib_outros == 'OUTROS TRIBUTOS',
                         'OUTROS TRIBUTOS', comb),
           comb = ifelse(comb == '', 'VAZIO', comb)) %>%
    group_by(comb) %>%
    mutate(n_trib = n()) %>%
    ungroup %>%
    mutate(comb = ifelse(n_trib < 1000, 'OUTRAS COMBINACOES', comb))

  d_aux %>% count(comb, sort = T)

  result <- d_aux %>%
    select(-(trib_pis:trib_outros),
           -txt_aux, -txt_ementa2, -assunto2, -tributo2, -assunto, -tributo,
           -n_trib) %>%
    rename(tributos = comb)
  result
}

#' @export
consolidar_datas <- function(d_pags, d_proc) {
  cat('datas...')
  d_aux <- d_pags %>%
    mutate(n_acordao = if_else(is.na(acordao), resolucao, acordao),
           tipo_acordao = if_else(is.na(acordao), 'resolucao', 'acordao')) %>%
    select(-acordao, -resolucao)
  n_resolucao <- sum(d_aux$tipo_acordao == 'resolucao')
  d_aux <- d_aux %>%
    filter(tipo_acordao == 'acordao') %>%
    rename(n_processo = numero_do_processo,
           dt_publicacao = data_de_publicacao) %>%
    distinct(n_processo, n_acordao, .keep_all = TRUE) %>%
    select(n_processo, n_acordao, dt_publicacao) %>%
    mutate_at(vars(starts_with('n_')), str_trim)

  residuo <- d_proc %>%
    filter(!str_detect(item, 'xxxxx')) %>%
    mutate(n_processo = str_trim(n_processo),
           n_acordao = tolower(str_trim(n_acordao))) %>%
    filter(!is.na(n_processo), !is.na(n_acordao)) %>%
    anti_join(d_aux, c('n_processo', 'n_acordao')) %>%
    select(n_processo, n_acordao)
  if (nrow(residuo) == n_resolucao) {
    result <- d_proc %>%
      filter(!str_detect(item, 'xxxxx')) %>%
      mutate(n_processo = str_trim(n_processo),
             n_acordao = tolower(str_trim(n_acordao))) %>%
      distinct(n_processo, n_acordao, .keep_all = TRUE) %>%
      inner_join(d_aux, c('n_processo', 'n_acordao'))
  }
  return(result)
}

#' @export
consolidar_resultados <- function(d) {
  cat('resultados...')
  negar_provimento <- 'negar?(do)?(am)? (o )?provimento|negou se (o )?provimento|recurso nao provido'
  dar_provimento <- 'dar?(do)? (o )?provimento|deu se (o )?provimento|recurso provido'
  em_parte <- 'em parte|parcial'
  diligencia <- 'diligencia'
  nao_conhecer <- '[^a-z0-9]conhec'
  anular <- 'nul(a|o|i)'

  d_aux <- d %>%
    mutate(txt_decisao2 = gsub(' +', ' ', gsub(
      '\n|\r', ' ', gsub('[[:punct:]]|\n\r|\r\n', ' ', tolower(txt_decisao)))
    ),
    txt_decisao2 = abjutils::rm_accent(txt_decisao2),
    negar_provimento = str_detect(txt_decisao2, negar_provimento),
    dar_provimento = str_detect(txt_decisao2, dar_provimento),
    em_parte = str_detect(txt_decisao2, em_parte),
    diligencia = str_detect(txt_decisao2, diligencia),
    nao_conhecer = str_detect(txt_decisao2, nao_conhecer),
    anular = str_detect(txt_decisao2, anular)) %>%
    mutate(soma = negar_provimento + (dar_provimento|em_parte) +
             diligencia + nao_conhecer + anular) %>%
    mutate(negar_provimento = ifelse(negar_provimento, 'negar_provimento', ''),
           dar_provimento = ifelse(dar_provimento, 'dar_provimento', ''),
           em_parte = ifelse(em_parte, 'em_parte', ''),
           diligencia = ifelse(diligencia, 'diligencia', ''),
           anular = ifelse(anular, 'anular', ''),
           nao_conhecer = ifelse(nao_conhecer, 'nao_conhecer', ''),
           comb = str_trim(gsub(' +', ' ', paste(negar_provimento,
                                                 dar_provimento,
                                                 em_parte,
                                                 diligencia,
                                                 anular,
                                                 nao_conhecer)))) %>%
    mutate(comb = ifelse(str_detect(comb, 'em_parte'), 'em_parte', comb),
           comb = ifelse(str_detect(comb, 'negar') & str_detect(comb, 'dar_'),
                         'em_parte', comb),
           comb = ifelse(str_detect(comb, 'negar'), 'negar_provimento', comb),
           comb = ifelse(str_detect(comb, 'anular') & str_detect(comb, 'dar_'),
                         'anular', comb),
           comb = ifelse(str_detect(comb, 'anular|nao_conhecer|dilig'),
                         'anulado, nao conhecido ou diligencia', comb),
           comb = ifelse(comb == '', 'vazio', comb))

  #   d_aux %>% filter(comb == 'em_parte') %>% View
  #   d_aux %>% filter(comb == 'dar_provimento em_parte nao_conhecer') %>% View
  #   d_ngram <- d_aux %>%
  #     group_by(n_processo, n_acordao) %>%
  #     do(ngram = (function(x) {
  #       RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3))
  #       })(.$txt_decisao2)) %>%
  #     unnest(ngram)
  #
  #   aff <- d_ngram %>%
  #     group_by(ngram) %>%
  #     mutate(ns = n()) %>%
  #     ungroup %>%
  #     filter(ns > 20000, ns < 60000) %>%
  #     mutate(ns = 1) %>%
  #     spread(ngram, ns, fill = 0)
  #   %>%
  #     mutate(resultado = ifelse(
  #       em_parte,
  #       'Parcialmente favorável ao contribuinte',
  #       ifelse(
  #         dar_provimento & tipo_recurso == 'RECURSO VOLUNTARIO',
  #         'Favorável ao contribuinte',
  #         ifelse(
  #           dar_provimento & tipo_recurso == 'RECURSO DE OFÍCIO',
  #           'Desfavorável ao contribuinte',
  #           ifelse(
  #             negar_provimento & tipo_recurso == 'RECURSO VOLUNTARIO',
  #             'Desfavorável ao contribuinte',
  #             ifelse(
  #               negar_provimento & tipo_recurso=='RECURSO DE OFÍCIO',
  #               'Favorável ao contribuinte',
  #               ifelse(
  #                 diligencia|nao_conhecer|anular,
  #                 'Não conhecido, anulado ou convertido em diligência',
  #                 'Outro / vazio'))))))) %>%
  #     mutate(resultado = ifelse(
  #       dar_provimento & negar_provimento,
  #       'Parcialmente favorável ao contribuinte',
  #       resultado))

  result <- d_aux %>%
    select(-(negar_provimento:soma), -txt_decisao2) %>%
    rename(resultado = comb)
  return(result)
}

#' @export
add_key <- function(d, re, lab) {
  mutate(d, XXXXX = ifelse(str_detect(txt_decisao, re, lab, '')))
  names(d[, 'XXXXX']) <- lab
  d
}

#' @export
consolidar_votacoes <- function(d) {
  cat('votacoes...')
  d_aux <- d %>%
    mutate(txt_decisao = gsub('\r\n', ' ', txt_decisao),
           txt_decisao = gsub(' +', ' ', txt_decisao)) %>%
    mutate(participaram = str_split_fixed(
      txt_decisao, 'Participaram.+(lheir)[^ ]+ ?\\:?', 2)[, 2]
    ) %>%
    mutate(unanimidade = str_detect(txt_decisao,
                                    regex('por unanimidade', ignore_case = TRUE)),
           maioria = str_detect(txt_decisao,
                                regex('por maioria|maioria de votos', ignore_case = TRUE)),
           voto_qualidade = str_detect(txt_decisao,
                                       regex('(por|pelo)( voto de)? qualidade', ignore_case = TRUE)),
           unanimidade = ifelse(unanimidade, 'Unanimidade', NA),
           maioria = ifelse(maioria, 'Maioria', NA),
           voto_qualidade = ifelse(voto_qualidade, 'Voto de qualidade', NA)) %>%
    mutate(tipo_decisao = paste(unanimidade, maioria, voto_qualidade, sep = ', '),
           tipo_decisao = str_trim(gsub(', NA|NA,', '', tipo_decisao)))
  result <- d_aux %>%
    select(-unanimidade, -maioria, -voto_qualidade) %>%
    rename(votacao = tipo_decisao)

  return(result)
}

#' Consolida os conselheiros
#'
#' Utiliza classificacoes externas
#'
#' @export
consolidar_conselheiros <- function(d) {
  cat('conselheiros...')

  # relatores_raw %>% filter(str_detect(relator, 'RIBAS'))

  relatores <- d %>%
    mutate(relator = abjutils::rm_accent(relator),
           relator = toupper(relator),
           relator = gsub('RELATORA? ', '', relator),
           relator = gsub('- ?(REDATORA? )?(DESIGNADO?A? )?AD?S? HOC',
                          '', relator),
           relator = gsub(' REDATOR DESIGNADO|DESIGNADA?O?$', '', relator),
           relator = gsub(' ?-? ?RELATORA?F?(KJH)?$', '', relator),
           relator = gsub(', RELATOR\\.$', '', relator),
           relator = gsub('^CONSELHEIRO?A? ', '', relator),
           relator = str_trim(relator),
           relator = gsub('NAO SE APLICA|RELATOR|^$|^MAR$',
                          'VAZIO/NAO SE APLICA', relator)) %>%
    count(relator, sort = TRUE)

  data(conselheiros_carf, package = 'crawlercarf')
  conselheiros_carf <- conselheiros_carf %>%
    filter(relator != 'VAGO') %>%
    mutate(relator = ifelse(relator == 'MERCIA HELENA TRAJANO D AMORIM',
                            'MERCIA HELENA TRAJANO DAMORIM', relator),
           relator = ifelse(relator == 'KLEBER FERREIRA ARAUJO',
                            'KLEBER FERREIRA DE ARAUJO', relator),
           relator = ifelse(relator == 'ANDRE LUIS MARISCO LOMBARDI',
                            'ANDRE LUIS MARSICO LOMBARDI', relator))

  a <- system.file('extdata/nomes_faltantes_goliva.xlsx', package = 'crawlercarf')
  nomes_arrumados <- tbl_df(openxlsx::read.xlsx(a, sheet = 3))
  aux <- relatores %>%
    select(-n) %>%
    filter(!str_detect(relator, 'VAZIO')) %>%
    inner_join(nomes_arrumados, 'relator') %>%
    select(-n)

  relatores_final <- relatores %>%
    select(-n) %>%
    filter(!str_detect(relator, 'VAZIO')) %>%
    inner_join(conselheiros_carf, 'relator') %>%
    bind_rows(aux) %>%
    mutate(tipo = ifelse(tipo == 'TRABALHADORES', 'CONTRIBUINTES', tipo)) %>%
    filter(!secao %in% c('CSRF', 'CSFR')) %>%
    group_by(relator) %>%
    mutate(n = n()) %>%
    ungroup %>%
    mutate(camara_turma = ifelse(n>1, NA, camara_turma),
           secao = ifelse(n>1, NA, secao)) %>%
    select(-n) %>%
    distinct(relator, .keep_all = TRUE)

  d_aux <- d %>%
    mutate(relator = abjutils::rm_accent(relator),
           relator = toupper(relator),
           relator = gsub('RELATORA? ', '', relator),
           relator = gsub('- ?(REDATORA? )?(DESIGNADO?A? )?AD?S? HOC', '', relator),
           relator = gsub(' REDATOR DESIGNADO|DESIGNADA?O?$', '', relator),
           relator = gsub(' ?-? ?RELATORA?F?(KJH)?$', '', relator),
           relator = gsub(', RELATOR\\.$', '', relator),
           relator = gsub('^CONSELHEIRO?A? ', '', relator),
           relator = str_trim(relator),
           relator = gsub('NAO SE APLICA|RELATOR|^$|^MAR$', 'VAZIO/NAO SE APLICA', relator)) %>%
    left_join(relatores_final, 'relator') %>%
    mutate(cargo = ifelse(is.na(cargo), 'NÃO IDENTIFICADO', cargo),
           tipo = ifelse(is.na(tipo), 'NÃO IDENTIFICADO', tipo),
           camara_turma = ifelse(is.na(camara_turma), 'VAZIO', camara_turma),
           secao = ifelse(is.na(secao), 'VAZIO', secao),
           secao = ifelse(str_detect(secao, '1'), 'PRIMEIRA SECAO',
                          ifelse(str_detect(secao, '2'), 'SEGUNDA SECAO',
                                 ifelse(str_detect(secao, '3'),
                                        'TERCEIRA SECAO',
                                        secao))))
  result <- d_aux
  return(result)
}

#' Consolida turmas e secoes de julgamento
#'
#' Utiliza classificacoes externas
#'
#' @export
consolidar_turmas <- function(d) {
  cat('turmas...')
  d_aux <- d %>%
    mutate(
      camara_turma = ifelse(str_detect(tipo_recurso, 'ESPECIAL'),
                            'CSRF', camara_turma),
      secao = ifelse(str_detect(tipo_recurso, 'ESPECIAL'),
                     'CSRF', secao),
      secao = ifelse(tipo_pessoa == 'Física' &
                       secao %in% c('VAZIO', 'NÃO IDENTIFICADO'),
                     'SEGUNDA SECAO', secao),
      secao = ifelse(str_detect(tributos, 'IRPJ|CSL|SIMPLES') &
                       secao %in% c('VAZIO', 'NÃO IDENTIFICADO'),
                     'PRIMEIRA SECAO', secao),
      secao = ifelse(str_detect(tributos, 'FINSOCIAL|PIS|COFINS|CPMF|II|IPI') &
                       secao %in% c('VAZIO', 'NÃO IDENTIFICADO'),
                     'TERCEIRA SECAO', secao)) %>%
    mutate(secao = ifelse(secao %in% c('VAZIO', 'NÃO IDENTIFICADO'),
                          'NÃO IDENTIFICADO', secao),
           camara_turma = ifelse(camara_turma %in% c('VAZIO', 'NÃO IDENTIFICADO'),
                                 'NÃO IDENTIFICADO', camara_turma))
  result <- d_aux
  result
}

#' tipos de recurso
#'
#' primeiro eu tiro recursos extraordinarios, recursos de divergencia,
#' embargos de declaracao, agravos e diligencias.
#' depois eu estudo os tipos de recursos que aparecem quando o numero do
#' processo se repete. A ideia eh que, se tivermos recursos do mesmo tipo,
#' vamos pegar sempre o mais recente (se as datas forem iguais, sorteamos um
#' deles). Se tivermos recursos normais e recursos especiais, consideramos
#' os recursos especiais. Se tivermos recursos diferentes, mantemos esses
#' recursos. as combinacoes que sobram sao as combinacoes 2 a 2 de
#' nao informado, recurso voluntario e recurso de oficio, mais recursos
#' especiais diferentes.
#'
#' Na verdade eu mudei. Agora considero como unidade amostral decisoes,
#' tirando embargos, recursos extraordinarios, diligencias, recurso de
#' divergencia e agravos. Vamos considera-los todos diferentes,
#' desde que sejam de tipos diferentes
#'
#' @export
consolidar_tipos_recurso <- function(d) {
  cat('tipo recurso...')
  d_aux <- d %>%
    mutate(tipo_recurso = ifelse(
      str_detect(tipo_recurso, regex('embargo|extrao|agrav|dilig|div',
                                     ignore_case = TRUE)),
      'tirar', tipo_recurso
    )) %>%
    filter(tipo_recurso != 'tirar') %>%
    # aqui eu tento recuperar alguns dos valores nao informados
    mutate(tipo_recurso = ifelse(str_detect(tipo_recurso, 'INFORM') &
                                   str_detect(tolower(abjutils::rm_accent(txt_ementa)),
                                              'recurso volunt'),
                                 'RECURSO VOLUNTARIO',
                                 ifelse(str_detect(tipo_recurso, 'INFORM') &
                                          str_detect(tolower(abjutils::rm_accent(txt_ementa)),
                                                     'recurso de oficio'),
                                        'RECURSO DE OFÍCIO',
                                        ifelse(str_detect(tipo_recurso, 'INFORM') &
                                                 str_detect(tolower(abjutils::rm_accent(txt_decisao)),
                                                            'recurso volunt'),
                                               'RECURSO VOLUNTARIO',
                                               ifelse(str_detect(tipo_recurso, 'INFORM') &
                                                        str_detect(tolower(abjutils::rm_accent(txt_decisao)),
                                                                   'recurso de oficio'),
                                                      'RECURSO DE OFÍCIO',
                                                      tipo_recurso
                                               ))))) %>%
    mutate(dt_publicacao = as.Date(dmy(dt_publicacao))) %>%
    arrange(n_processo, desc(dt_publicacao)) %>%
    group_by(n_processo) %>%
    distinct(tipo_recurso, .keep_all = TRUE) %>%
    ungroup()
  result <- d_aux
  return(result)
}

# Informacoes contidas nos andamentos ------------------------------------------

#' @export
consolidar_tempos <- function(d) {
  cat('tempos...')
  d
}

# Informacoes contidas no comprot ----------------------------------------------

#' @export
consolidar_origens <- function(d) {
  cat('assuntos...')
  data('d_comprot', package = 'crawlercarf')
  d_aux <- d_comprot %>%
    mutate(p = gsub('.+//|\\.html', '', arq)) %>%
    filter(key %in% c('assunto', 'data_de_protocolo',
                      'cnpj', 'cpf', 'cpfcnpj')) %>%
    filter(!is.na(key)) %>%
    distinct(arq, key, .keep_all = TRUE) %>%
    spread(key, val) %>%
    mutate(cnpj = ifelse(is.na(cnpj), cpfcnpj, cnpj)) %>%
    select(-cpfcnpj) %>%
    mutate(tipo_pessoa = ifelse(is.na(cnpj), 'Física', 'Jurídica')) %>%
    mutate(cn = ifelse(is.na(cnpj), cpf, cnpj)) %>%
    select(-cnpj, -cpf) %>%
    mutate(cn = ifelse(cn == 'Ausente', NA, cn)) %>%
    select(n_processo = p, assunto, data_protocolo = data_de_protocolo,
           tipo_pessoa, cn) %>%
    distinct(n_processo, .keep_all = TRUE) %>%
    mutate(origem = ifelse(
      str_detect(assunto, 'AUTO DE INFRACAO|IMPUGNACAO|MULTA'),
      'auto_infracao_impugnacao_multa',
      'outros'
    ))

  resultado <- d %>%
    mutate(aux = gsub('[^0-9]', '', n_processo)) %>%
    inner_join(d_aux, c('aux' = 'n_processo')) %>%
    select(-aux)
  return(resultado)
}

#' @export
pega_uf_origem <- function(p) {
  for(i in 1:length(p)) {
    try({
      mov <- mov_comprot(p[i])
      mov <- mov[, c('tipo', 'origem')]
      primeira <- filter(mov, tipo == 'Primeira Distribuição')
      if(nrow(primeira) == 1) {
        return(primeira$origem)
      }
    })
  }
  return('nao deu')
}

#' @export
consolidar_ufs_origem <- function(d) {
  cat('uf...')

#   d_aux <- d %>%
#     mutate(n_processo_num = gsub('[^0-9]', '', n_processo),
#            len = str_length(n_processo_num)) %>%
#     mutate(cinco_digitos = str_sub(n_processo, 1L, 5L),
#            ano = as.numeric(ifelse(len == 17,
#                         str_sub(n_processo_num, 12L, 15L),
#                         str_sub(n_processo_num, 12L, 13L)))) %>%
#     arrange(cinco_digitos, desc(ano)) %>%
#     select(cinco_digitos, n_processo, ano)
#
#   movs_origem <- d_aux %>%
#     group_by(cinco_digitos) %>%
#     do(mov = pega_uf_origem(.$n_processo)) %>%
#     ungroup %>%
#     unnest(mov)
#
#   save(movs_origem, file = 'data/movs_origem.rda')

  ufs <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA",
           "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO",
           "RR", "RS", "SC", "SE", "SP", "TO")
  data("movs_origem", package = 'crawlercarf')
  origens <- movs_origem %>%
    tbl_df %>%
    mutate(uf = str_sub(mov, -2L),
           uf = ifelse(uf %in% ufs, uf, 'VAZIO'),
           uf = ifelse(str_detect(mov, 'CONSELHO') & uf == 'DF', 'VAZIO', uf),
           uf = ifelse(str_detect(mov, '\\-RJ') & uf == 'VAZIO', 'RJ', uf),
           uf = ifelse(str_detect(mov, '\\-SP') & uf == 'VAZIO', 'SP', uf),
           uf = ifelse(str_detect(mov, '\\-JOINV') & uf == 'VAZIO', 'SP', uf),
           uf = ifelse(str_detect(mov, '\\-AER VIRAC') & uf == 'VAZIO', 'SP', uf),
           uf = ifelse(str_detect(mov, 'FLORIANOP') & uf == 'VAZIO', 'SC', uf),
           uf = ifelse(str_detect(mov, 'BLUMEN') & uf == 'VAZIO', 'RS', uf),
           uf = ifelse(str_detect(mov, 'PAULIST') & uf == 'VAZIO', 'SP', uf),
           uf = ifelse(str_detect(mov, '\\-VIT') & uf == 'VAZIO', 'RJ', uf)) %>%
    distinct(cinco_digitos, .keep_all = TRUE) %>%
    select(cinco_digitos, uf)
  result <- d %>%
    mutate(cinco_digitos = str_sub(n_processo, 1L, 5L)) %>%
    left_join(origens, 'cinco_digitos') %>%
    select(-cinco_digitos) %>%
    mutate(uf = ifelse(is.na(uf), 'VAZIO', uf))
  return(result)
}

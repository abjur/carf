
#' Consolidate pages, decisions, and comprot
#'
#' @param pages `pages` element returned by [parse_decision()]
#' @param decisions `decisions` element returned by [parse_decision()]
#' @param comprot Table returned by [get_comprot()]
#' @return A tibble with aggregated data about CARF
#'
#' @export
consolidate <- function(pages, decisions, comprot) {

  detect_result <- function(result, decision, pattern_decision, type_appeal, replacement) {
    pattern_appeal <- c("RECURSO DE OFICIO", "RECURSO ESPECIAL DO PROCURADOR")
    ifelse(decision == pattern_decision & type_appeal %in% pattern_appeal, replacement, result)
  }

  consolidate_decisions(pages, decisions) %>%
    consolidate_origins(comprot) %>%
    consolidate_states() %>%
    consolidate_appeals() %>%
    consolidate_votes() %>%
    consolidate_results() %>%
    consolidate_taxes() %>%
    consolidate_rapporteurs() %>%
    consolidate_chambers() %>%
    dplyr::select(
      id_lawsuit, id_decision, type_appeal, taxpayer, rapporteur,
      summary, decision, date_publication, type_party, cpf_cnpj,
      origin, state, vote, result, taxes, type, chamber, section) %>%
    dplyr::mutate(
      type_appeal = ifelse(stringr::str_detect(type_appeal, "RECURSO DE OF"), "RECURSO DE OFICIO", type_appeal),
      origin = ifelse(origin == "auto_infracao_impugnacao_multa", "AUTO INFRACAO/IMPUGNACAO/MULTA", "OUTROS"),
      vote = ifelse(vote == "NA", "VAZIO", stringr::str_to_upper(vote)),
      decision = result,
      result = result %>%
        detect_result(decision, "negar_provimento", type_appeal, "FAVORAVEL") %>%
        detect_result(decision, "dar_provimento", type_appeal, "DESFAVORAVEL") %>%
        detect_result(decision, "dar_provimento", type_appeal, "FAVORAVEL") %>%
        detect_result(decision, "negar_provimento", type_appeal, "DESFAVORAVEL") %>%
        detect_result(decision, "em_parte", type_appeal, "PARCIALMENTE FAVORAVEL"),
      result = ifelse(decision == "", "", result),
      result = stringr::str_to_upper(result),
      result = ifelse(stringr::str_detect(result, "_"), "VAZIO", result),
      type = ifelse(stringr::str_detect(type, "IDENTIF"), "NAO IDENTIFICADO", type))
}

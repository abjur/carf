#' Information about possible origins for CARF decicions
#'
#' @format A tibble with columns:
#' \describe{
#'  \item{id}{First five digits of a lawsuit ID}
#'  \item{origin}{Name of origin}
#' }
"jurisprudence_origins"

#' Information about rapporteurs from the Goliva dataset
#'
#' @format A tibble with columns:
#' \describe{
#'  \item{rapporteur}{Name of the rapporteur}
#'  \item{n}{Number of times the rapporteur came up}
#'  \item{title}{Rapporteur's title (position)}
#'  \item{type}{Rapporteur type}
#'  \item{chamber}{Chamber where the rapporteur works}
#'  \item{section}{Section of the chamber}
#' }
"rapporteur_goliva"

#' Information about rapporteurs
#'
#' @format A tibble with columns:
#' \describe{
#'  \item{rapporteur}{Name of the rapporteur}
#'  \item{title}{Rapporteur's title (position)}
#'  \item{type}{Rapporteur type}
#'  \item{chamber}{Chamber where the rapporteur works}
#'  \item{section}{Section of the chamber}
#' }
"rapporteur_data"

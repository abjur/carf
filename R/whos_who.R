#' Download CARF's "Who's Who" ("Quem e Quem")
#'
#' @param path Directory where to save PDF
#' @return Character vector with the path to the downloaded file
#'
#' @export
download_whos_who <- function(path = ".") {

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  file <- glue::glue(path, "/whos_who.pdf")

  # Request PDF
  u_pdf <- "http://idg.carf.fazenda.gov.br/acesso-a-informacao/servidores/ex-conselheiros-28052015.pdf"
  r_pdf <- httr::GET(u_pdf)

  # Save PDF to disk
  writeBin(httr::content(r_pdf, "raw"), file)

  return(file)
}

#' Read CARF's "Who's Who" ("Quem e Quem") file
#'
#' @param file Path to PDF downloaded with [download_whos_who()]
#' @return A tibble with who's who information
#'
#' @export
read_whos_who <- function(file) {

  # Read PDF
  text <- pdftools::pdf_text(file)

  # Clean text
  lines <- text %>%
    stringr::str_c(collapse = " ") %>%
    stringr::str_split("\\n") %>%
    purrr::pluck(1) %>%
    magrittr::extract(2:length(.)) %>%
    stringr::str_subset("^((?!2010).)*$") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("( )( )+", " | ")

  # Separate each party
  groups <- list()
  i <- 1
  while (i <= length(lines)) {

    j <- i; flag <- 0
    while (flag < 2) {

      if (j > length(lines)) {
        break
      }

      if (flag == 0) {
        if (stringr::str_detect(lines[j], "[0-9+][\\u00aa\\u00ba]")) {
          flag = flag + 1
        }
      }
      if (flag == 1) {
        if (stringr::str_detect(lines[j], "(FAZENDA|CONTRIBUINTE)")) {
          flag = flag + 1
        }
      }

      j = j + 1
    }

    groups <- append(groups, list(lines[i:(j-1)]))
    i <- j
  }

  # Remove last group
  groups <- groups[1:(length(groups)-1)]

  # Extract representation
  for (i in seq_along(groups)) {
    groups[[i]][length(groups[[i]])+1] <- groups[[i]] %>%
      stringr::str_extract("(CONTRIBUINTE|FAZENDA)") %>%
      purrr::discard(is.na)

    groups[[i]] <- stringr::str_replace(groups[[i]], "(CONTRIBUINTE|FAZENDA) \\| ", "")
  }

  # Manual adjustments
  groups[[16]] <- c("4\u00aa CAM - 2\u00ba CC | 4\u00aa CAM - 2\u00ba CC | 4\u00aa CAM - 2\u00ba CC", "Alexandre Venzon Zanetti", "Tit. | Tit. | Tit.", "CONTRIBUINTE")
  groups[[39]] <- c("2\u00aa CAM - 1\u00aa CC | 2\u00aa CAM - 1\u00aa CC 2\u00aa CAM - 1\u00aa CC", "Antonio de Freitas Dutra", "Tit. | Tit. | Tit.", "FAZENDA")

  # Try to clean title and chamber
  groups <- purrr::map(groups, function(x) {

    # Transform into list
    x <- as.list(x)

    # Resplit chambers if necessary
    n <- stringr::str_count(x[[3]], "\\|")
    if (stringr::str_count(x[[1]], "\\|") != n) {
      x[[1]] <- split_len(x[[1]], n + 1)
    } else {
      x[[1]] <- purrr::flatten_chr(stringr::str_split(x[[1]], " \\| "))
    }

    # Split title
    x[[3]] <- purrr::flatten_chr(stringr::str_split(x[[3]], " \\| "))

    # Repeat other variables
    x[[2]] <- rep(x[[2]], n + 1)
    x[[4]] <- rep(x[[4]], n + 1)

    return(x)
  })

  # Create a table
  names <- c("chamber", "rapporteur", "title", "type")
  groups %>%
    purrr::map(purrr::set_names, names) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::distinct(chamber, rapporteur, title, type) %>%
    dplyr::select(rapporteur, dplyr::everything())
}

#' Auxiliary function for spliting a string at certain points
#'
#' @param string String to be split
#' @param splits Number of splits to be returned
#' @return A character vector with a split string
split_len <- function(string, splits) {

  # Aux function for spliting one element of string
  split_len_ <- function(string, splits) {

    # At each iteration, split one part
    size <- floor(stringr::str_length(string)/splits)
    result <- list()
    for (i in seq_len(splits - 1)) {

      part <- stringr::str_sub(string, 1, size)
      string <- stringr::str_sub(string, size + 2)
      result <- append(result, part)
    }

    # Append last part and flatten
    result <- purrr::flatten_chr(append(result, string))
  }

  # Remove bars
  string <- stringr::str_split(string, " \\| ")[[1]]
  string <- as.list(string)

  # Information
  len <- stringr::str_length(string)
  i <- which(len == max(len))[1]

  string[[i]] <- split_len_(string[[i]], splits - length(string) + 1)
  purrr::flatten(string) %>% as.character()
}

#' Transcode un vecteur vers un type numerique
#'
#' Transcode un vecteur vers un type numérique.
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type numérique.
#'
#' @export
to_numeric <- function(vecteur) {

  if (all(is.na(vecteur))) {
    to_numeric <- as.numeric(vecteur)

  } else if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {
    to_numeric <- stringr::str_match(vecteur, "[\\d\\.,]+") %>%
      stringr::str_replace_all(",", ".") %>%
      as.numeric()

  } else if (any(class(vecteur) %in% c("Date", "POSIXct")) == TRUE) {
    to_numeric <- lubridate::year(vecteur)

  } else if (any(class(vecteur) == "logical") == TRUE) {
    to_numeric <- rep(NA, length(vecteur)) %>%
      as.numeric()

  } else if (class(vecteur) == "integer") {
    to_numeric <- as.numeric(vecteur)
  }

  return(to_numeric)
}

#' Transcode un vecteur vers un type integer
#'
#' Transcode un vecteur vers un type integer
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type integer
#'
#' @export
to_integer <- function(vecteur) {

  if (all(is.na(vecteur))) {
    to_integer <- as.numeric(vecteur)

  } else if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {
    to_integer <- stringr::str_match(vecteur, "^(\\d+)") %>%
      .[, 2] %>%
      as.integer()

  } else if (any(class(vecteur) %in% c("Date", "POSIXct")) == TRUE) {
    to_integer <- lubridate::year(vecteur) %>%
      as.integer()

  } else if (any(class(vecteur) == "logical") == TRUE) {
    to_integer <- rep(NA, length(vecteur)) %>%
      as.integer()

  } else if (any(class(vecteur) == "numeric") == TRUE) {
    to_integer <- as.integer(vecteur)

  }

  return(to_integer)
}

#' Transcode un vecteur vers un type date
#'
#' Transcode un vecteur vers un type date
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type date
#'
#' @export
to_date <- function(vecteur, origin = "1899-12-30") {

  to_date <- as.Date(rep(NA, length(vecteur)))

  if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {

    num_format <- stringr::str_detect(vecteur, "^\\d{5}(\\.0+)?$") %>% which()
    to_date[num_format] <- as.Date.numeric(as.integer(vecteur[num_format]), origin)

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{1,2}/\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d/%m/%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(paste0("01/", vecteur[num_format]), "%d/%m/%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{1,2}/\\d{2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d/%m/%y") %>%
      ifelse(. > lubridate::today(), . - 100 * 365.25, .) %>%
      lubridate::as_date()

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}-\\d{1,2}-\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d-%m-%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}-\\d{1,2}-\\d{2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d-%m-%y") %>%
      ifelse(. > lubridate::today(), . - 100 * 365.25, .) %>%
      lubridate::as_date()

    num_format <- stringr::str_detect(vecteur, "^\\d{4}-\\d{1,2}-\\d{1,2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format])

    num_format <- stringr::str_detect(vecteur, "^\\d{4}/\\d{1,2}/\\d{1,2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format])

  } else if (any(class(vecteur) %in% c("numeric", "integer")) == TRUE) {
    num_format <- stringr::str_detect(as.character(vecteur), "^\\d{5}$") %>% which()
    to_date[num_format] <- as.Date.numeric(vecteur[num_format], origin)

  } else if (any(class(vecteur) == "POSIXct") == TRUE) {
    to_date <- as.Date.POSIXct(vecteur)

  }

  return(to_date)
}

#' as_factor
#'
#' @param champ_factor \dots
#' @param table_niveaux \dots
#'
#' @export
#' @keywords internal
as_factor <- function(champ_factor, table_niveaux = NULL) {

  if (is.null(table_niveaux)) {
    champ_factor <- as.factor(champ_factor)
    return(champ_factor)
  }

  nom_champ_factor <- dplyr::enquo(champ_factor) %>%
    dplyr::quo_name()

  niveaux <- table_niveaux %>%
    dplyr::filter(champ == nom_champ_factor) %>%
    dplyr::arrange(ordre) %>%
    dplyr::pull(niveau)

  if (length(niveaux) != 0) {
    champ_factor <- factor(champ_factor, levels = niveaux)
  } else {
    champ_factor <- as.factor(champ_factor)
  }

  return(champ_factor)
}

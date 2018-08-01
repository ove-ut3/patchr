#' Transcode a vector into numeric.
#'
#' @param x A vector.
#'
#' @return A numeric vector.
#'
#' @export
#' @keywords internal
as_numeric <- function(x) {

  if (all(is.na(x))) {
    return(as.numeric(x))
  }

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    as_numeric <- stringr::str_match(x, "[\\d\\.,]+") %>%
      stringr::str_replace_all(",", ".") %>%
      as.numeric()

    return(as_numeric)
  }

  if (any(class(x) %in% c("Date", "POSIXct")) == TRUE) {
    return(lubridate::year(x))
  }

  if (any(class(x) == "logical") == TRUE) {

    as_numeric <- rep(NA, length(x)) %>%
      as.numeric()

    return(lubridate::year(x))
  }

  if (class(x) == "integer") {
    return(as.numeric(x))
  }

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

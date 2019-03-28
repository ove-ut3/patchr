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

as_integer <- function(x) {

  if (all(is.na(x))) {
    return(as.numeric(x))
  }

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    as_integer <- stringr::str_match(x, "^(\\d+)") %>%
      .[, 2] %>%
      as.integer()

    return(as_integer)
  }

  if (any(class(x) %in% c("Date", "POSIXct")) == TRUE) {

    as_integer <- lubridate::year(x) %>%
      as.integer()

    return(as_integer)
  }

  if (any(class(x) == "logical") == TRUE) {

    as_integer <- rep(NA, length(x)) %>%
      as.integer()

    return(as_integer)
  }

  if (any(class(x) == "numeric") == TRUE) {
    return(as.integer(x))
  }
}

as_date <- function(x, origin = "1899-12-30") {

  as_date <- as.Date(rep(NA, length(x)))

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    position <- stringr::str_detect(x, "^\\d{5}(\\.0+)?$") %>% which()
    as_date[position] <- as.Date.numeric(as.integer(x[position]), origin)

    position <- stringr::str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$") %>% which()
    as_date[position] <- as.Date(x[position], "%d/%m/%Y")

    position <- stringr::str_detect(x, "^\\d{1,2}/\\d{4}$") %>% which()
    as_date[position] <- as.Date(paste0("01/", x[position]), "%d/%m/%Y")

    position <- stringr::str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$") %>% which()
    as_date[position] <- as.Date(x[position], "%d/%m/%y") %>%
      ifelse(. > lubridate::today(), . - 100 * 365.25, .) %>%
      lubridate::as_date()

    position <- stringr::str_detect(x, "^\\d{1,2}-\\d{1,2}-\\d{4}$") %>% which()
    as_date[position] <- as.Date(x[position], "%d-%m-%Y")

    position <- stringr::str_detect(x, "^\\d{1,2}-\\d{1,2}-\\d{2}$") %>% which()
    as_date[position] <- as.Date(x[position], "%d-%m-%y") %>%
      ifelse(. > lubridate::today(), . - 100 * 365.25, .) %>%
      lubridate::as_date()

    position <- stringr::str_detect(x, "^\\d{4}-\\d{1,2}-\\d{1,2}$") %>% which()
    as_date[position] <- as.Date(x[position])

    position <- stringr::str_detect(x, "^\\d{4}/\\d{1,2}/\\d{1,2}$") %>% which()
    as_date[position] <- as.Date(x[position])

    return(as_date)
  }

  if (any(class(x) %in% c("numeric", "integer")) == TRUE) {
    position <- stringr::str_detect(as.character(x), "^\\d{5}$") %>% which()
    as_date[position] <- as.Date.numeric(x[position], origin)

    return(as_date)
  }

  if (any(class(x) == "POSIXct") == TRUE) {
    return(as.Date.POSIXct(x))
  }

  if (any(class(x) == "logical") == TRUE) {
    return(as_date)
  }
}

str_normalise_colnames <- function(string){

  if (class(string) != "character") {
    stop("Input vector must be a character vector", call. = FALSE)
  }

  # Lower case and conv from ISO-8859-1 if it does not work
  normalised_string <- tryCatch(
    {
      tolower(string)
    },
    error = function(cond) {
      normalised_string <- stringr::str_conv(string, "ISO-8859-1") %>%
        tolower()
      return(normalised_string)
    }
  )

  normalised_string <- normalised_string %>%
    # Replacement of punctuation and spaces by an underscore
    stringr::str_replace_all("[[:punct:]\\s]+", "_") %>%
    # A trailing undersore is removed
    stringr::str_remove_all("_$") %>%
    # All non alphanumeric strings are removed
    stringr::str_remove_all("[^\\w]") %>%
    # All accents are removed
    caractr::str_remove_accent()

  # If duplicate, make unique
  if(length(normalised_string) != length(unique(normalised_string))) {
    normalised_string <- make.unique(normalised_string, sep = "_")
  }

  return(normalised_string)
}

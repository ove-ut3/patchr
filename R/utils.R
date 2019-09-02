as_numeric <- function(x) {

  if (all(is.na(x))) {
    return(as.numeric(x))
  }

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    as_numeric <- stringr::str_remove_all(x, "\\s") %>%
      stringr::str_extract("[\\d\\.,]+") %>%
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
    return(as.integer(x))
  }

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    as_integer <- stringr::str_remove_all(x, "\\s") %>%
      stringr::str_match("^(\\d+)") %>%
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

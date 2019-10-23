#' Change column classes in a data frame.
#'
#' The data frame columns are transcoded according to a correspondance table.\cr
#' The correspondance table data_transcode must at least contains tow fields : \code{column} for the column name and \code{class} for the new R class.
#'
#' @param data A data frame.
#' @param data_transcode A correspondance between colmanes and new R classes.
#'
#' @return A transcoded data frame.
#'
#' @details
#' R classes are :\cr
#' - character\cr
#' - date\cr
#' - numeric\cr
#' - integer\cr
#' - factor
#'
#' @examples
#' data <- dplyr::tibble(var1 = "a", var2 = 1)
#' patchr::transcode(data, dplyr::tibble(column = "var2", class = "character"))
#'
#' @export
transcode <- function(data, data_transcode) {

  if (any(class(data) == "data.frame") == FALSE) {

    return(data)

  } else {

    data_transcodage <- dplyr::tibble(column = colnames(data),
                                      class = lapply(data, class) %>%
                                        purrr::map_chr(1) %>%
                                        tolower()
    ) %>%
      dplyr::mutate(num_column = dplyr::row_number()) %>%
      dplyr::inner_join(data_transcode %>%
                          dplyr::rename(new_class = class),
                        by = "column") %>%
      dplyr::filter(class != new_class)

    if (nrow(data_transcodage) != 0) {

      for(num_transcodage in 1:nrow(data_transcodage)) {

        column <- data_transcodage$column[num_transcodage]
        new_class <- data_transcodage$new_class[num_transcodage]

        if (new_class == "character") data[[column]] <- as.character(data[[column]])
        else if (new_class == "date") data[[column]] <- as_date(data[[column]])
        else if (new_class == "numeric") data[[column]] <- as_numeric(data[[column]])
        else if (new_class == "integer") data[[column]] <- as_integer(data[[column]])
        else if (new_class == "factor") data[[column]] <- as.factor(data[[column]])
        else if (new_class == "logical") data[[column]] <- as_logical(data[[column]])

      }

    }

    return(data)
  }

}
#' Convert a character date to POSIXct
#'
#' @param x Character date
#' @param origin Origin date
#'
#' @return A POSIXct date
#'
#'@export
as_date <- function(x, origin = "1899-12-30") {

  as_date <- as.Date(rep(NA, length(x)))

  x <- stringr::str_remove(x, "\\d{2}:\\d{2}:\\d{2}$") %>%
    stringr::str_remove("\\d{2}:\\d{2}$") %>%
    trimws()

  if (any(class(x) %in% c("character", "factor")) == TRUE) {

    # Date as numeric
    position <- stringr::str_detect(x, "^\\d{5}(\\.0+)?$") %>% which()
    if (length(position) >= 1) {
      as_date[position] <- as.Date.numeric(as.integer(x[position]), origin)
    }

    position <- stringr::str_detect(x, "^\\d{1,2}[-/]\\d{1,2}[-/]\\d{4}$") %>% which()
    if (length(position) >= 1) {
      as_date[position] <- lubridate::dmy(x[position])
    }

    position <- stringr::str_detect(x, "^\\d{1,2}[-/]\\d{4}$") %>% which()
    if (length(position) >= 1) {
      as_date[position] <- lubridate::dmy(paste0("01/", x[position]))
    }

    position <- stringr::str_detect(x, "^\\d{1,2}[-/]\\d{1,2}[-/]\\d{1,2}$") %>% which()
    if (length(position) >= 1) {

      first_part <- stringr::str_match(x, "^(\\d{1,2})[-/]")[, 2] %>%
        as.integer()

      if (all(na.omit(first_part) %in% 1:31)) {
        as_date[position] <- lubridate::dmy(x[position])
      } else {
        as_date[position] <- lubridate::ymd(x[position])
      }

    }

    position <- stringr::str_detect(x, "^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}$") %>% which()
    if (length(position) >= 1) {
      as_date[position] <- lubridate::ymd(x[position])
    }

    #libell\u00E9 de mois (excel)
    position <- stringr::str_detect(x, "^[[:alpha:]]{3,4}-") %>% which()
    if (length(position) >= 1) {

      date_char <- x[position] %>%
        stringr::str_subset("^[[:alpha:]]{3,4}-") %>%
        stringr::str_match("^([[:alpha:]]{3,4})-(\\d{2})$") %>%
        as.data.frame() %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(
          date_char = paste0(
            dplyr::recode(V2, "janv" = "01", "f\u00E9vr" = "02", "mars" = "03", "avr" = "04", "mai" = "05", "juin" = "06", "juil" = "07", "ao\u00FBt" = "08", "sept" = "09", "oct" = "10", "nov" = "11", "d\u00E9c" = "12"),
            "/",
            ifelse(as.numeric(V3) >= 76, paste0("19", V3), paste0("20", V3))
          ) %>%
          paste0("01/", .)
        )

      as_date[position] <- lubridate::dmy(date_char$date_char)

    }

    return(as_date)
  }

  if (any(class(x) %in% c("numeric", "integer")) == TRUE) {

    position <- stringr::str_detect(as.character(x), "^\\d{5}$") %>% which()
    if (length(position) >= 1) {
      as_date[position] <- as.Date.numeric(x[position], origin)
    }

    return(as_date)
  }

  if (any(class(x) == "POSIXct") == TRUE) {
    return(as.Date.POSIXct(x))
  }

  if (any(class(x) == "logical") == TRUE) {
    return(as_date)
  }
}

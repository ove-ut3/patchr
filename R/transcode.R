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
#' dplyr::tibble(var1 = "a", var2 = 1) %>%
#'   patchr::transcode(dplyr::tibble(column = "var2", class = "character"))
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

      }

    }

    return(data)
  }

}

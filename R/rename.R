#' Rename data frame column names according to a correspondance table.
#'
#' The correspondance table data_remane must at least contains tow columns : \code{column} for the old colnames and \code{rename} for the new colnames.
#'
#' @param data A data frame.
#' @param data_rename A correspondance table between old and new column names.
#' @param drop If \code{TRUE} then all the columns not present in data_rename are removed.
#'
#' @return A renamed data frame.
#'
#' @examples
#' dplyr::tibble(var1 = "a", old = "b") %>%
#'   patchr::rename(dplyr::tibble(column = "old", rename = "new"), drop = FALSE)
#'
#' @export
rename <- function(data, data_rename, drop = TRUE) {

  if (any(class(data) == "data.frame") == FALSE) {
    return(data)
  }

  new_colnames <- dplyr::tibble(column = colnames(data)) %>%
    dplyr::left_join(dplyr::mutate(data_rename, rename = ifelse(is.na(rename), column, rename)),
                     by = "column")

  if (drop == TRUE) {
    column_drop <- which(is.na(new_colnames$rename))
    if (length(column_drop) != 0) {
      data <- dplyr::select(data, -which(is.na(new_colnames$rename)))
      new_colnames <- dplyr::filter(new_colnames, !is.na(rename))
    }
  } else if (drop == FALSE){
    new_colnames <- dplyr::mutate(new_colnames, rename = ifelse(!is.na(rename), rename, caractr::str_camel_to_snake_case(column)))
  }

  new_colnames <- new_colnames[["rename"]]

  if (length(unique(new_colnames)) != length(new_colnames)) {
    duplicate <- which(data(new_colnames) >= 2) %>% names()
    message("Duplicate in data_rename on column \"rename\" : ", paste0(duplicate, collapse = ", "))
    new_colnames <- make.unique(new_colnames)
  }

  colnames(data) <- new_colnames

  if (ncol(data) == 0) return(NULL)
  else return(data)

}

#' Normalise column names of a data frame.
#'
#' @param data A data frame.
#'
#' @return A data frame with normalised colmun names.\cr
#'
#' Function \code{caractr::str_normalise_field()} is applied for each data column.
#'
#' @examples
#' data <- dplyr::data_frame(
#'   "Type d'unité Sirus : entreprise profilée ou unité légale" = NA_character_,
#'   "Nic du siège"= NA_character_
#' )
#' patchr::normalise_colnames(data)
#'
#' @export
normalise_colnames <- function(data){

  colnames(data) <- colnames(data) %>%
    caractr::str_normalise_colnames()

  return(data)
}

#' Rename data frame columns.
#'
#' The data frame columns are renamed according to a correspondance table.\cr
#' The correspondance table data_remane must at least contains tow columns : \code{column} for the old column names and \code{rename} for the new column names.
#'
#' @param data A data frame.
#' @param data_rename A correspondance table between old and new column names.
#' @param normalise_data_colnames If \code{TRUE} then input data column names are normalised before renamed according to data_rename.
#' @param drop If \code{TRUE} then all the columns not present in data_rename are removed.
#'
#' @return A renamed data frame.
#'
#' @examples
#' dplyr::tibble(var1 = "a", old = "b") %>%
#'   patchr::rename(dplyr::tibble(column = "old", rename = "new"), drop = FALSE)
#'
#' @export
rename <- function(data, data_rename, normalise_data_colnames = TRUE, drop = TRUE) {

  if (any(class(data) == "data.frame") & any(class(data_rename) == "data.frame") == FALSE) {
    return(data)
  }

  if (normalise_data_colnames == TRUE & any(colnames(data) != caractr::str_normalise_colnames(colnames(data)))) {
    data <- patchr::normalise_colnames(data)
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
  } else if (drop == FALSE) {
    new_colnames <- dplyr::mutate(new_colnames, rename = ifelse(!is.na(rename), rename, column))
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

#' Normalise a string for a use as table column names.
#'
#' @param string Input character vector.
#'
#' @return A character vector.
#'
#' @examples
#' patchr::str_normalise_colnames(
#'   c("Type d'unité Sirus : entreprise profilée ou unité légale",
#'    "Nic du siège")
#' )
#'
#' @export
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
    stringi::stri_trans_general("latin-ascii")

  # If duplicate, make unique
  if(length(normalised_string) != length(unique(normalised_string))) {
    normalised_string <- make.unique(normalised_string, sep = "_")
  }

  return(normalised_string)
}

#' Normalise column names of a data frame.
#'
#' @param data A data frame.
#'
#' @return A data frame with normalised colmun names.\cr
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

  colnames(data) <- str_normalise_colnames(colnames(data))

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
#'
#' data <- dplyr::tibble(var1 = "a", old = "b")
#' patchr::rename(data, dplyr::tibble(column = "old", rename = "new"), drop = FALSE)
#'
#' @export
rename <- function(data, data_rename, normalise_data_colnames = TRUE, drop = TRUE) {

  if (any(class(data) == "data.frame") & any(class(data_rename) == "data.frame") == FALSE) {
    return(data)
  }

  if (normalise_data_colnames == TRUE & any(colnames(data) != str_normalise_colnames(colnames(data)))) {
    data <- patchr::normalise_colnames(data)
  }

  new_colnames <- dplyr::tibble(column = colnames(data)) %>%
    dplyr::left_join(dplyr::mutate(data_rename, rename = ifelse(is.na(rename), .data$column, rename)),
                     by = "column")

  if (drop == TRUE) {
    column_drop <- which(is.na(new_colnames$rename))
    if (length(column_drop) != 0) {
      data <- dplyr::select(data, -which(is.na(new_colnames$rename)))
      new_colnames <- dplyr::filter(new_colnames, !is.na(rename))
    }
  } else if (drop == FALSE) {
    new_colnames <- dplyr::mutate(new_colnames, rename = ifelse(!is.na(rename), rename, .data$column))
  }

  new_colnames <- new_colnames[["rename"]]

  if (length(unique(new_colnames)) != length(new_colnames)) {
    duplicate <- which(table(new_colnames) >= 2) %>% names()
    message("Duplicate in data_rename on column \"rename\" : ", paste0(duplicate, collapse = ", "))
    new_colnames <- make.unique(new_colnames)
  }

  colnames(data) <- new_colnames

  if (ncol(data) == 0) return(NULL)
  else return(data)

}

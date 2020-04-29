#' Rename data frame columns.
#'
#' The data frame columns are renamed according to a correspondance table.\cr
#' The correspondance table data_remane must at least contains tow columns : \code{column} for the old column names and \code{rename} for the new column names.
#'
#' @param data A data frame.
#' @param data_rename A correspondance table between old and new column names.
#' @param clean_colnames If \code{TRUE} then input data column names are snake_case normalised before renamed according to data_rename.
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
rename <- function(data, data_rename, clean_colnames = TRUE, drop = TRUE) {

  if (any(class(data) == "data.frame") & any(class(data_rename) == "data.frame") == FALSE) {
    return(data)
  }

  if (clean_colnames == TRUE & any(colnames(data) != janitor::make_clean_names(colnames(data)))) {

    data <- janitor::clean_names(data)

  }

  new_colnames <- dplyr::tibble(column = colnames(data)) %>%
    dplyr::left_join(
      data_rename %>%
        dplyr::mutate(rename = dplyr::if_else(is.na(rename), .data$column, rename)),
      by = "column"
    )

  if (drop == TRUE) {

    column_drop <- which(is.na(new_colnames$rename))

    if (length(column_drop) != 0) {

      data <- dplyr::select(data, -which(is.na(new_colnames$rename)))
      new_colnames <- dplyr::filter(new_colnames, !is.na(rename))

    }

  } else if (drop == FALSE) {

    new_colnames <- dplyr::mutate(new_colnames, rename = dplyr::if_else(!is.na(rename), rename, .data$column))

  }

  new_colnames <- new_colnames[["rename"]]

  if (length(unique(new_colnames)) != length(new_colnames)) {

    duplicate <- which(table(new_colnames) >= 2) %>% names()
    message("Duplicate in data_rename on column \"rename\" : ", paste0(duplicate, collapse = ", "))
    new_colnames <- make.unique(new_colnames)

  }

  colnames(data) <- new_colnames

  if (ncol(data) == 0) {

    return(NULL)

  } else {

    return(data)

  }
}

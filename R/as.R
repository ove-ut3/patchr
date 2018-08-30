#' Transcode a character vector into an factor.
#'
#' An optional table data_levels contains ordered levels associated with column names. It contains at least three colmuns : column, order, level.
#'
#' @param string A character vector.
#' @param data_levels An optional correpondance table between column names and ordered levels.
#'
#' @return A ordered leveled factor.
#'
#' @export
as_factor <- function(string, data_levels = NULL) {

  if (is.null(data_levels)) {
    string <- as.factor(string)
    return(string)
  }

  column <- dplyr::enquo(string) %>%
    dplyr::quo_name()

  levels <- data_levels %>%
    dplyr::filter(column == !!column) %>%
    dplyr::arrange(order) %>%
    dplyr::pull(level)

  if (length(levels) != 0) {
    string <- factor(string, levels = levels)
  } else {
    string <- as.factor(string)
  }

  return(string)
}

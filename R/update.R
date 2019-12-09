#' Update a tibble values according to a second update tibble
#'
#' @param data Original tibble
#' @param update Tibble used to patch the original tibble
#' @param by Columns used to join both tibbles
#'
#' https://github.com/tidyverse/dplyr/issues/4595#issuecomment-547420916
#'
#' @export
df_update <- function(data, update, by) {
  stopifnot(all(names(update) %in% names(data)))

  update <- df_partition(update, {{ by }})
  cols_by <- names(update$keys)
  cols_values <- names(update$values)

  matches <- na.omit(vctrs::vec_match(update$keys, data[cols_by]))
  data[matches, cols_values] <- update$values

  data
}

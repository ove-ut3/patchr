#' Update a tibble values according to a second update tibble
#'
#' @param data Original tibble
#' @param update Tibble used to patch the original tibble
#' @param by Columns used to join both tibbles
#'
#' https://github.com/tidyverse/dplyr/issues/4595#issuecomment-548123242
#'
#' @export
df_update <- function(data, update, by) {

  stopifnot(all(names(update) %in% names(data)))

  target_cols <- setdiff(names(update), by)

  # creating a reference vector to map col to update col
  to_update <- purrr::map_chr(purrr::set_names(target_cols), paste0, ".update")

  tab_joined <- data %>%
    dplyr::left_join(
      update,
      by = by,
      suffix = c("", ".update")
    )

  # use purrr to create a list of your new columns values
  updated_cols <- purrr::imap(
    to_update,
    ~ dplyr::coalesce(purrr::pluck(tab_joined, .x), purrr::pluck(tab_joined, .y))
  )

  data <- data %>%
    dplyr::mutate(
      # use splicing to use the list to update column
      # !!! list(x = y) => x = y
      !!! updated_cols
    )

  data
}

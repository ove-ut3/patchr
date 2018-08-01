#' Supprime les doublons d'une table selon un champ
#'
#' Supprime les doublons d'une table selon un champ.
#'
#' @param table Un data frame.
#' @param champ Un champ pour lequel les doublons sont supprimés.
#'
#' @return un data frame.
#'
#' @export
supprimer_doublons_champ <- function(table, champ) {

  quo_champ <- dplyr::enquo(champ)

  supprimer_doublons_champ <- tidyr::nest(table, !!quo_champ) %>%
    dplyr::mutate(!!dplyr::quo_name(quo_champ) := purrr::map_chr(data, ~ ifelse(length(.[[1]]) == 1, .[[1]], NA_character_))) %>%
    dplyr::select(-data)

  return(supprimer_doublons_champ)

}

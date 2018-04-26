#' Renomme les champs d'un data frame selon une table de correspondance
#'
#' Renomme les champs d'un data frame selon une table de correspondance.
#'
#' @param table Un data frame.
#' @param table_rename Une table de correspondance entre anciens et nouveaux noms de champ.
#' @param source Nom de la source à filtrer dans la table \code{table_rename}.
#' @param filtre La valeur de filtre.
#' @param drop \code{TRUE}: les champs non-rensignés dans la table de correspondance sont supprimés; \code{FALSE}: tous les champs sont conservés.
#'
#' @return Un data frame dont les champs sont renommés.
#'
#' @export
renommer_champs <- function(table, table_rename, source = NULL, filtre = NULL, drop = TRUE) {

  if (is.null(table_rename)) {
    return(table)
  }

  if (any(class(table) == "data.frame") == FALSE) {
    return(table)
  }

  if (!is.null(source)) {
    table_rename <- dplyr::filter(table_rename, source %in% !!source)
  }

  if (!is.null(filtre)) {
    table_rename <- tidyr::separate_rows(table_rename, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))
  }

  colnames_maj <- dplyr::data_frame(champ = colnames(table)) %>%
    dplyr::left_join(dplyr::mutate(table_rename, rename = ifelse(is.na(rename), champ, rename)),
                     by = "champ")

  if (drop == TRUE) {
    num_champ_supression <- which(is.na(colnames_maj$rename))
    if (length(num_champ_supression) != 0) {
      table <- dplyr::select(table, -which(is.na(colnames_maj$rename)))
      colnames_maj <- dplyr::filter(colnames_maj, !is.na(rename))
    }
  } else if (drop == FALSE){
    colnames_maj <- dplyr::mutate(colnames_maj, rename = ifelse(!is.na(rename), rename, caractr::camel_to_snake_case(champ)))
  }

  colnames_maj <- colnames_maj[["rename"]]

  if (length(unique(colnames_maj)) != length(colnames_maj)) {
    doublons <- which(table(colnames_maj) >= 2) %>% names()
    message("Doublon dans la table_rename sur le champ \"rename\" : ", paste0(doublons, collapse = ", "))
    colnames_maj <- make.unique(colnames_maj)
  }

  colnames(table) <- colnames_maj

  if (ncol(table) == 0) return(NULL)
  else return(table)

}

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

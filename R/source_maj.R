#' Met a jour/Harmonise une liste de data frame et les compile dans un data frame unique
#'
#' Met à jour/Harmonise une liste de data frame et les compile dans un data frame unique.
#'
#' @param import Une liste de data frame.
#' @param champs_filtre Un nom de champ pour lequel les valeurs vides sont supprimées.
#' @param filtre_expression Une expression de type character supprimant des lignes des data frame.
#'
#' @return Un data frame unique
#'
#' @export
maj_source <- function(import, champs_filtre = NULL, ligne_fin = NULL) {

  clusters <- divr::initialiser_parallel()

  #Renommer les champs
  table_rename <- importr::importer_table_access(table = "_Mapping_champs") %>%
    dplyr::select(champ, rename)

  message("Renommage des champs (à partir de la table Access \"_Mapping_champs\"):")
  rename <- pbapply::pblapply(import$import, source.maj::renommer_champs, table_rename, cl = clusters)

  #Transcoder les champs
  content_maj <- importr::importer_table_access(table = "_Contents") %>%
    tidyr::drop_na(classe) %>%
    dplyr::select(champ, classe_maj = classe)

  message("Transcodage des champs (à partir de la table Access \"_Contents\"):")
  transcode <- pbapply::pblapply(rename, source.maj::transcoder_champs, content_maj, cl = clusters)

  if (!is.null(champs_filtre) | !is.null(ligne_fin)) {
    message("Suppression des lignes vides:")
    suppression_lignes_na <- pbapply::pblapply(transcode, source.maj::suppression_lignes_fin, champs_filtre = champs_filtre, ligne_fin = ligne_fin
                                               , cl = clusters
                                               )
  } else {
    suppression_lignes_na <- transcode
  }

  divr::stopper_cluster(clusters)

  maj_source <- dplyr::mutate(import, donnees = suppression_lignes_na) %>%
    dplyr::select(-import) %>%
    dplyr::filter(purrr::map_lgl(donnees, ~ !is.null(.))) %>%
    tidyr::unnest() %>%
    importr::caracteres_vides_na()

  return(maj_source)
}

#' Retourne la liste complete et dedoublonnee des champs de data frame
#'
#' Retourne la liste complète et dédoublonnée des champs de data frame
#'
#' @param liste_tbl Une liste de data frame.
#' @param table_rename Une table de correspondance entre anciens et nouveaux noms de champ.
#' @param fichier_csv Un chemin de fichier csv en sortie.
#'
#' @return Un vecteur de noms de champ.
#'
#' @export
liste_champs_unique <- function(liste_tbl, table_rename, fichier_csv = "champs_unique.csv") {

  lapply(liste_tbl, colnames) %>%
    purrr::map_df( ~ tibble::tibble(champ = .)) %>%
    dplyr::anti_join(table_rename, by = "champ") %>%
    dplyr::group_by(champ) %>%
    dplyr::summarise(freq = n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-freq) %>%
    write.csv2(fichier_csv, row.names = FALSE)
}

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
    colnames_maj <- dplyr::mutate(colnames_maj, rename = ifelse(is.na(rename), champ, rename))
  }

  colnames_maj <- colnames_maj[["rename"]]

  if (length(unique(colnames_maj)) != length(colnames_maj)) {
    doublons <- which(table(colnames_maj) >= 2) %>% names()
    message("Doublon dans la table_rename sur le champ \"rename\" : ", doublons)
    colnames_maj <- make.unique(colnames_maj)
  }

  colnames(table) <- colnames_maj

  if (ncol(table) == 0) return(NULL)
  else return(table)

}

#' Supprime les lignes d'une table selon les valeurs vides d'un champ ou une expression
#'
#' Supprime les lignes d'une table selon les valeurs vides d'un champ ou une expression.
#'
#' @param table Un data frame.
#' @param champs_filtre Un vecteur de nom de champ pour lequel les valeurs vides sont supprimées.
#' @param filtre_expression Une expression de type character supprimant des lignes du data frame.
#'
#' @return un data frame.
#'
#' @export
suppression_lignes_fin <- function(table, champs_filtre, ligne_fin = NULL){

  if (any(class(table) != "data.frame") == FALSE) {
    return(NULL)
  }

  if (length(table) == 0) {
    return(NULL)
  }

  table <- dplyr::select(table, which(!is.na(names(table))))

  if (!is.null(ligne_fin)){

    ligne_fin$num_champ <- intersect(ligne_fin$num_champ, 1:ncol(table))

    if (length(ligne_fin$num_champ) == 0) {
      return(table)
    }

    ligne_na <- dplyr::tibble(test = apply(table[, ligne_fin$num_champ], 1, paste0, collapse = "")) %>%
      dplyr::mutate(ligne = row_number()) %>%
      dplyr::filter(stringr::str_detect(test, stringr::regex(ligne_fin$valeur, ignore_case = TRUE))) %>%
      .$ligne %>%
      head(1)

    if (length(ligne_na) == 1) {
      table <- dplyr::filter(table, row_number() <= (ligne_na - 1))
      return(table)
    }
  }

  # Effectuer les test sur les champs passés en paramètres
  champs_filtre <- intersect(colnames(table), champs_filtre)

  if (length(champs_filtre) == 0) {
    return(table)
  }

  # Première ligne à vide
  ligne_na <- table[, champs_filtre] %>%
    map(is.na) %>%
    map(which) %>%
    map_dbl( ~ ifelse(length(.) == 0, nrow(table) + 1, .)) %>%
    min(na.rm = TRUE)

  if (ligne_na == nrow(table) + 1) {
    return(table)
  }

  table <- dplyr::filter(table, row_number() <= (ligne_na - 1))

  return(table)

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

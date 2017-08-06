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
    dplyr::filter(!is.na(classe)) %>%
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

  parallel::stopCluster(clusters)

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
#' @param import Une liste de data frame.
#' @param table_rename Une table de correspondance entre anciens et nouveaux noms de champ.
#' @param fichier_csv Un chemin de fichier csv en sortie.
#'
#' @return Un vecteur de noms de champ.
#'
#' @export
liste_champs_unique <- function(import, table_rename, fichier_csv = "champs_unique.csv"){

  lapply(import$import, colnames) %>%
    purrr::map_df(~ data.frame(champ = .)) %>%
    dplyr::as_data_frame() %>%
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
#' @param drop \code{TRUE}: les champs non-rensignés dans la table de correspondance sont supprimés; \code{FALSE}: tous les champs sont conservés.
#'
#' @return Un data frame dont les champs sont renommés.
#'
#' @export
renommer_champs <- function(table, table_rename, drop = TRUE) {

  if (is.null(table_rename)) {
    return(table)
  }

  if (any(class(table) == "data.frame") == FALSE) {

    return(table)

  } else {

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

}

#' Transcode les champs d'un data frame selon une table de correspondance
#'
#' Transcode les champs d'un data frame selon une table de correspondance.
#'
#' @param table Un data frame.
#' @param content_maj Une table de correspondance entre les noms de champ et les classes attendus.
#'
#' @return Un data frame dont les champs sont transcodés.
#'
#' @export
transcoder_champs <- function(table, content_maj) {

  if (any(class(table) == "data.frame") == FALSE) {

    return(table)

  } else {

    table_transcodage <- dplyr::data_frame(champ = colnames(table),
                                            classe = lapply(table, class) %>%
                                              purrr::map_chr(1) %>%
                                              tolower()
    ) %>%
      dplyr::mutate(num_champ = row_number()) %>%
      dplyr::inner_join(content_maj %>%
                          dplyr::rename(classe_maj = classe),
                        by = "champ") %>%
      dplyr::filter(classe != classe_maj)

    if (nrow(table_transcodage) != 0) {

      for(num_transcodage in 1:nrow(table_transcodage)) {

        champ <- table_transcodage$champ[num_transcodage]
        classe_maj <- table_transcodage$classe_maj[num_transcodage]

        if (classe_maj == "character") table[[champ]] <- as.character(table[[champ]])
        else if (classe_maj == "date") table[[champ]] <- source.maj::to_date(table[[champ]])
        else if (classe_maj == "numeric") table[[champ]] <- source.maj::to_numeric(table[[champ]])
        else if (classe_maj == "integer") table[[champ]] <- source.maj::to_integer(table[[champ]])

      }

    }

    return(table)
  }

}

#' Transcode un vecteur vers un type numerique
#'
#' Transcode un vecteur vers un type numérique.
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type numérique.
#'
#' @export
to_numeric <- function(vecteur) {

  if (which(!is.na(vecteur)) %>% length() == 0) {
    return(as.numeric(vecteur))
  }

  if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {
    to_numeric <- stringr::str_match(vecteur, "^(\\d+)") %>%
      .[, 2] %>%
      as.numeric()

    return(to_numeric)
  }
  if (any(class(vecteur) %in% c("Date", "POSIXct")) == TRUE) {
    to_numeric <- lubridate::year(vecteur)

    return(to_numeric)
  }
  if (any(class(vecteur) == "logical") == TRUE) {
    to_numeric <- rep(NA, length(vecteur)) %>%
      as.numeric()

    return(to_numeric)
  }

}

#' Transcode un vecteur vers un type integer
#'
#' Transcode un vecteur vers un type integer
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type integer
#'
#' @export
to_integer <- function(vecteur) {

  if (which(!is.na(vecteur)) %>% length() == 0) {
    return(as.integer(vecteur))
  }

  if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {
    to_integer <- stringr::str_match(vecteur, "^(\\d+)") %>%
      .[, 2] %>%
      as.integer()

    return(to_integer)
  }
  if (any(class(vecteur) %in% c("Date", "POSIXct")) == TRUE) {
    to_integer <- lubridate::year(vecteur) %>%
      as.integer()

    return(to_integer)
  }
  if (any(class(vecteur) == "logical") == TRUE) {
    to_integer <- rep(NA, length(vecteur)) %>%
      as.integer()

    return(to_integer)
  }
  if (any(class(vecteur) == "numeric") == TRUE) {
    to_integer <- as.integer(vecteur)

    return(to_integer)
  }

}

#' Transcode un vecteur vers un type date
#'
#' Transcode un vecteur vers un type date
#'
#' @param vecteur Un vecteur.
#'
#' @return Un vecteur de type date
#'
#' @export
to_date <- function(vecteur, origin = "1899-12-30") {

  to_date <- as.Date(rep(NA, length(vecteur)))

  if (any(class(vecteur) %in% c("character", "factor")) == TRUE) {

    num_format <- stringr::str_detect(vecteur, "^\\d{5}(\\.0+)?$") %>% which()
    to_date[num_format] <- as.Date.numeric(as.integer(vecteur[num_format]), origin)

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{1,2}/\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d/%m/%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(paste0("01/", vecteur[num_format]), "%d/%m/%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}/\\d{1,2}/\\d{2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d/%m/%y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}-\\d{1,2}-\\d{4}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d-%m-%Y")

    num_format <- stringr::str_detect(vecteur, "^\\d{1,2}-\\d{1,2}-\\d{2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format], "%d-%m-%y")

    num_format <- stringr::str_detect(vecteur, "^\\d{4}-\\d{1,2}-\\d{1,2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format])

    num_format <- stringr::str_detect(vecteur, "^\\d{4}/\\d{1,2}/\\d{1,2}$") %>% which()
    #to_date[num_format] <- dmy(vecteur[num_format])
    to_date[num_format] <- as.Date(vecteur[num_format])

    return(to_date)
  }

  if (any(class(vecteur) %in% c("numeric", "integer")) == TRUE) {

    num_format <- stringr::str_detect(as.character(vecteur), "^\\d{5}$") %>% which()
    to_date[num_format] <- as.Date.numeric(vecteur[num_format], origin)

    return(to_date)
  }

  if (any(class(vecteur) == "POSIXct") == TRUE) {

    to_date <- as.Date.POSIXct(vecteur)

    return(to_date)
  }

  #Pour les autres classes
  return(to_date)
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

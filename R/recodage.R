#' Recoder les champs d'une table a partir d'un identifiant
#'
#' Recoder les champs d'une table à partir d'un identifiant.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param .champ_id Le nom du champ "identifiant".
#'
#' @return La table recodée
#'
#' @export
recoder_individu <- function(table, table_recodage, .champ_id = "identifiant") {

  table_recodage <- dplyr::filter(table_recodage, champ %in% names(table)) %>%
    dplyr::rename(.valeur = valeur)

  if (intersect(table[[.champ_id]], table_recodage[[.champ_id]]) %>% length == 0) {
    return(table)
  }

  table <- dplyr::rename(table, .champ_id = !!.champ_id)
  table_id_na <- dplyr::filter(table, is.na(.champ_id))
  table <- dplyr::filter(table, !is.na(.champ_id))

  if (any(lapply(table, class) == "list")) {
    champs_list <- dplyr::select(table, .champ_id, which(purrr::map_lgl(table, is.list)))
    table <- dplyr::select(table, -which(purrr::map_lgl(table, is.list)))
  }

  content_maj <- dplyr::tibble(champ = names(table),
                               classe = purrr::map_chr(table, class) %>% tolower())

  if (any(lapply(table, class) == "Date")) {
    table <- table %>%
      dplyr::mutate_at(.vars = names(.)[which(purrr::map_lgl(., lubridate::is.Date))], as.character)
  }

  recoder <- tidyr::gather(table, "champ", "valeur", -.champ_id) %>%
    dplyr::left_join(table_recodage, by = c(".champ_id" = .champ_id, "champ")) %>%
    dplyr::mutate(valeur = ifelse(!is.na(.valeur), .valeur, valeur),
                  valeur = dplyr::na_if(valeur, "[null]")) %>%
    dplyr::select(.champ_id, champ, valeur) %>%
    unique() %>% #En cas de question à choix multiple
    tidyr::spread(champ, valeur)

  if (exists("champs_list")) {
    recoder <- dplyr::full_join(recoder, champs_list, by = ".champ_id")
  }

  recoder <- transcoder_champs(recoder, content_maj)

  recoder <- dplyr::select(recoder, purrr::map_int(names(table), ~ which(. == names(recoder))))

  recoder <- dplyr::bind_rows(recoder, table_id_na)

  names(recoder)[which(names(recoder) == ".champ_id")] <- .champ_id

  return(recoder)
}

#' Recoder les champs d'une table a partir d'une expression VRAI/FAUX
#'
#' Recoder les champs d'une table à partir d'une expression VRAI/FAUX.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param filtre La valeur de filtre.
#' @param champs_table Recodage réalisé uniquement sur le schamps déjà déjà présents dans la table.
#'
#' @return La table recodée
#'
#' @export
recoder_champs <- function(table, table_recodage, filtre = NULL, champs_table = TRUE) {

  if (nrow(table) == 0) return(table)

  if (!is.null(filtre)) {
    table_recodage <- tidyr::separate_rows(table_recodage, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))
  }

  if (champs_table == TRUE) {
    table_recodage <- dplyr::filter(table_recodage, champ %in% names(table))
  }

  if (nrow(table_recodage) == 0) return(table)

  if (champs_table == FALSE) {
    champs_na <- table_recodage$champ[which(!table_recodage$champ %in% names(table))] %>%
      unique()
    if (length(champs_na) >= 1) {

      classe_champs_a_creer <- table_recodage %>%
        dplyr::filter(champ %in% champs_na) %>%
        dplyr::mutate(classe = "character",
                      classe = ifelse(stringr::str_detect(valeur, "^\\d+L$"), "integer", classe),
                      classe = ifelse(stringr::str_detect(valeur, "^[\\d\\.]+$"), "real", classe)) %>%
        dplyr::select(champ, classe) %>%
        unique() %>%
        dplyr::pull(classe) %>%
        paste0("NA_", ., "_")

      list_mutate <- stats::setNames(as.list(classe_champs_a_creer), as.list(champs_na)) %>%
        lapply(rlang::parse_quosure)

      table <- dplyr::mutate(table, !!!list_mutate)
    }
  }

  list_instructions <- ifelse(is.na(table_recodage$expression),
                              table_recodage$valeur,
                              paste0("dplyr::if_else(", table_recodage$expression, ", ", table_recodage$valeur,", ", table_recodage$champ, ", ", table_recodage$champ, ")")) %>%
    as.list()


  list_champs <- table_recodage$champ %>%
    as.list()

  list_mutate <- stats::setNames(list_instructions, list_champs) %>%
    lapply(rlang::parse_quosure)

  table <- dplyr::mutate(table, !!!list_mutate)

  return(table)
}

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

  # table_recodage <- recodage_individu
  table <- dplyr::rename_(table, .dots = c(".champ_id" = .champ_id))
  table_recodage <- dplyr::rename(table_recodage, .valeur = valeur)

  if (any(purrr::map(table, class) == "list")) {
    champs_list <- dplyr::select(table, .champ_id, which(purrr::map_lgl(table, is.list)))
    table <- dplyr::select(table, -which(purrr::map_lgl(table, is.list)))
  }

  if (any(purrr::map(table, class) == "Date")) {
    champs_date <- names(table)[which(purrr::map_lgl(table, lubridate::is.Date))]
    table <- dplyr::mutate_at(table, .cols = champs_date, as.character)
  }

  recoder <- tidyr::gather(table, "champ", "valeur", -.champ_id) %>%
    dplyr::left_join(table_recodage, by = c(".champ_id" = "identifiant", "champ")) %>%
    dplyr::mutate(valeur = ifelse(!is.na(.valeur), .valeur, valeur),
                  valeur = na_if(valeur, "[null]")) %>%
    dplyr::select(.champ_id, champ, valeur) %>%
    unique() %>% #En cas de question à choix multiple
    tidyr::spread(champ, valeur)

  if (exists("champs_list")) {
    recoder <- dplyr::full_join(recoder, champs_list, by = ".champ_id")
  }

  if (exists("champs_date")) {
    recoder <- dplyr::mutate_at(recoder, .cols = champs_date, ymd)
  }
  recoder <- source.maj::transcoder_champs(recoder, dplyr::tibble(champ = names(table),
                                                                  classe = purrr::map_chr(table, class)))

  recoder <- dplyr::select(recoder, purrr::map_int(names(table), ~ which(. == names(recoder))))
  names(recoder)[which(names(recoder) == ".champ_id")] <- .champ_id

  return(recoder)
}

#' Recoder les champs d'une table a partir d'une expression VRAI/FAUX
#'
#' Recoder les champs d'une table à partir d'une expression VRAI/FAUX.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param .champ_id Le nom du champ "identifiant".
#'
#' @return La table recodée
#'
#' @export
recoder_champs <- function(table, table_recodage, .champ_id = "identifiant") {

  # table <- maj_perimetre_mesr
  # table_recodage <- importer_table_access("pepip_recodage")
  # .champ_id = "id"

  table_recodage <- dplyr::filter(table_recodage, champ %in% names(table)) %>%
    dplyr::mutate(.id = row_number())

  recodage_individu <- purrr::map_df(table_recodage$expression,
                                      ~ dplyr::filter_(table, .dots = .) %>%
                                       dplyr::select_(.dots = c("identifiant" = .champ_id)),
                                     .id = ".id") %>%
    dplyr::mutate(.id = as.integer(.id)) %>%
    dplyr::left_join(table_recodage, by = ".id") %>%
    dplyr::select(identifiant, champ, valeur) %>%
    unique() %>% # Un individu qui remplit plusieurs recodages
    # Question choix multiple (concaténation)
    dplyr::group_by(identifiant, champ) %>%
    dplyr::summarise(valeur = paste(valeur, collapse = ";")) %>%
    dplyr::ungroup()

  recoder <- recoder_individu(table, recodage_individu, .champ_id = .champ_id)

  return(recoder)
}

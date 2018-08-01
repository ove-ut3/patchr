#' Recode individual rows in a data frame.
#'
#' The data frame is recoded according to a correspondance table containing a row id.\cr
#' The correpondance table contains a least three columns : colname, value and an identification colname named with colname_id.
#'
#' @param data A data frame.
#' @param data_recode The correspondance table.
#' @param colname_id The identification colname in both data and data_recode tables.
#'
#' @return A recoded data frame.
#'
#' @export
recode_id <- function(data, data_recode, colname_id) {

  if (!is.null(source)) {
    data_recode <- dplyr::filter(data_recode, source %in% !!source)
  }

  data_recode <- dplyr::filter(data_recode, champ %in% names(data)) %>%
    dplyr::select(!!colname_id, champ, .valeur = valeur)

  if (dplyr::inner_join(data, data_recode, by = colname_id) %>% nrow() == 0) {
    return(data)
  }

  data <- dplyr::mutate(data, .id = dplyr::row_number())

  options(warn = -1)
  id_na <- dplyr::select(data, !!colname_id, .id) %>%
    tidyr::gather("champ", "valeur", -.id) %>%
    dplyr::filter(is.na(valeur)) %>%
    dplyr::select(.id) %>%
    unique()
  options(warn = 0)

  data_id_na <- dplyr::semi_join(data, id_na, by = ".id") %>%
    dplyr::select(-.id)

  data <- dplyr::anti_join(data, id_na, by = ".id") %>%
    dplyr::select(-.id)

  col_order <- names(data)

  if (any(lapply(data, class) == "list")) {
    col_list <- dplyr::select(data, !!colname_id, which(purrr::map_lgl(data, is.list)))
    data <- dplyr::select(data, -which(purrr::map_lgl(data, is.list)))
  }

  if (any(purrr::map_lgl(data, ~ any(class(.) == "POSIXct")))) {
    col_posix <- dplyr::select(data, !!colname_id, which(purrr::map_lgl(data, ~ any(class(.) == "POSIXct"))))
    data <- dplyr::select(data, -which(purrr::map_lgl(data, ~ any(class(.) == "POSIXct"))))
  }

  data_transcode <- dplyr::tibble(champ = names(data),
                               class = purrr::map_chr(data, class) %>% tolower())

  if (any(lapply(data, class) == "Date")) {
    data <- data %>%
      dplyr::mutate_at(.vars = names(.)[which(purrr::map_lgl(., lubridate::is.Date))], as.character)
  }

  recode <- tidyr::gather(data, "champ", "valeur", -!!colname_id) %>%
    dplyr::left_join(data_recode, by = c(colname_id, "champ")) %>%
    dplyr::mutate(valeur = ifelse(!is.na(.valeur), .valeur, valeur),
                  valeur = dplyr::na_if(valeur, "[null]")) %>%
    dplyr::select(!!colname_id, champ, valeur) %>%
    unique() %>% # In case of multiple answer field
    tidyr::spread(champ, valeur)

  recode <- patchr::transcode(recode, data_transcode)

  if (exists("col_list")) {
    recode <- dplyr::full_join(recode, col_list, by = colname_id)
  }

  if (exists("col_posix")) {
    recode <- dplyr::full_join(recode, col_posix, by = colname_id)
  }

  recode <- recode %>%
    dplyr::select(purrr::map_int(col_order, ~ which(. == names(recode)))) %>%
    dplyr::bind_rows(data_id_na)

  return(recode)
}

#' Recoder les champs d'une table a partir d'une expression VRAI/FAUX
#'
#' Recoder les champs d'une table à partir d'une expression VRAI/FAUX.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param source Nom de la source à filtrer dans la table \code{table_recodage}.
#' @param filtre La valeur de filtre.
#' @param creer_champs Créer les champs présents dans la table de recodage ou de niveaux mais absents dans la table à recoder.
#'
#' @return La table recodée
#'
#' @examples
#' # Recodage sans expression
#' patchr::recoder_champs(
#' table = dplyr::tibble(test = 1L),
#' table_recodage = dplyr::tibble(expression = NA_character_, champ = "test", valeur = "2L")
#' )
#'
#' # Recodage avec expression
#' patchr::recoder_champs(
#' table = dplyr::tibble(test = c(1L, 2L)),
#' table_recodage = dplyr::tibble(expression = "test == 2", champ = "test", valeur = "3L")
#' )
#'
#' @export
recoder_champs <- function(table, table_recodage, source = NULL, filtre = NULL, creer_champs = TRUE) {

  if (nrow(table) == 0) return(table)

  if (!is.null(source)) {
    table_recodage <- dplyr::filter(table_recodage, source %in% !!source)
  }

  if (!is.null(filtre)) {
    table_recodage <- tidyr::separate_rows(table_recodage, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))
  }

  if (creer_champs == FALSE) {
    table_recodage <- dplyr::filter(table_recodage, champ %in% names(table))
  }

  if (nrow(table_recodage) == 0) return(table)

  if (creer_champs == TRUE) {

    champs_a_creer <- table_recodage$champ[which(!table_recodage$champ %in% names(table))] %>%
        unique()

    if (length(champs_a_creer) >= 1) {

      list_mutate <- table_recodage %>%
        dplyr::filter(champ %in% champs_a_creer) %>%
        dplyr::mutate(classe = "character",
                      classe = ifelse(stringr::str_detect(valeur, "^\\d+L$"), "integer", classe),
                      classe = ifelse(valeur == "NA_integer_", "integer", classe),
                      classe = ifelse(stringr::str_detect(valeur, "^[\\d\\.]+$"), "real", classe),
                      classe = ifelse(valeur == "NA_real_", "real", classe)) %>%
        dplyr::select(champ, classe) %>%
        unique() %>%
        dplyr::pull(classe) %>%
        paste0("NA_", ., "_") %>%
        as.list()

      names(list_mutate) <- champs_a_creer
      list_mutate <- lapply(list_mutate, rlang::parse_quosure)

      table <- dplyr::mutate(table, !!!list_mutate)
    }
  }

  if (!is.null(table_recodage[["ordre"]])) {
    table_recodage <- dplyr::arrange(table_recodage, ordre)
  }

  table_recodage <- table_recodage %>%
    dplyr::mutate(classe = purrr::map(champ, ~ class(table[[.]])),
                  valeur = ifelse(purrr::map_lgl(classe, ~ "factor" %in% .),
                                  paste0("factor(", valeur, ", levels = levels(", champ,"))"),
                                  valeur))

  list_mutate <- ifelse(is.na(table_recodage$expression),
                              table_recodage$valeur,
                              paste0("dplyr::if_else(", table_recodage$expression, ", ", table_recodage$valeur,", ", table_recodage$champ, ", ", table_recodage$champ, ")")) %>%
    as.list()

  names(list_mutate) <- table_recodage$champ
  list_mutate <- lapply(list_mutate, rlang::parse_quosure)

  table <- dplyr::mutate(table, !!!list_mutate)

  return(table)
}

#' recoder_factor
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param table_niveaux La table des niveaux.
#' @param source Nom de la source à filtrer dans la table \code{table_recodage}.
#' @param filtre La valeur de filtre.
#' @param creer_champs Créer les champs présents dans la table de recodage ou de niveaux mais absents dans la table à recoder.
#'
#' @return La table recodée
#'
#' @export
recoder_factor <- function(table, table_recodage, table_niveaux = NULL, source = NULL, filtre = NULL, creer_champs = FALSE) {

  if (nrow(table) == 0) return(table)

  if (!is.null(source)) {
    table_recodage <- dplyr::filter(table_recodage, source %in% !!source)

    if (!is.null(table_niveaux)) {
      table_niveaux <- dplyr::filter(table_niveaux, source %in% !!source)
    }
  }

  if (!is.null(filtre)) {
    table_recodage <- tidyr::separate_rows(table_recodage, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))

    if (!is.null(table_niveaux)) {
      table_niveaux <- tidyr::separate_rows(table_niveaux, filtre, sep = ";") %>%
        dplyr::filter(filtre == !!filtre | is.na(filtre))
    }
  }

  if (nrow(table_recodage) == 0) return(table)

  if (creer_champs == TRUE) {

    champs_a_creer <- c(table_recodage$champ, table_niveaux$champ) %>%
      unique() %>%
      .[which(!. %in% names(table))]

    if (length(champs_a_creer) >= 1) {

      table <- table %>%
        patchr::recoder_champs(table_recodage = dplyr::tibble(champ = champs_a_creer,
                                                                  valeur = "NA_character_",
                                                                  expression = NA_character_))
    }

  }

  if (intersect(names(table), table_recodage$champ) %>% length() == 0) {
    return(table)
  }

  champs_factor <- names(table) %>%
    intersect(c(table_recodage$champ, table_niveaux$champ))

  ordre_champs <- names(table)

  recoder <- table %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::select(.id, champs_factor) %>%
    dplyr::mutate_at(dplyr::vars(champs_factor), as.character) %>%
    tidyr::gather("champ", "valeur", -.id) %>%
    dplyr::left_join(table_recodage %>%
                       dplyr::select(champ, valeur, recodage),
                     by = c("champ", "valeur")) %>%
    dplyr::mutate(valeur = ifelse(!is.na(recodage), recodage, valeur) %>%
                    dplyr::na_if("[null]")) %>%
    dplyr::select(-recodage) %>%
    tidyr::spread(champ, valeur) %>%
    dplyr::select(-.id) %>%
    dplyr::mutate_at(dplyr::vars(champs_factor), patchr::as_factor, table_niveaux) %>%
    dplyr::bind_cols(table %>%
                       dplyr::select(which(!names(.) %in% champs_factor)))

  recoder <- recoder %>%
    dplyr::select(purrr::map_int(ordre_champs, ~ which(. == names(recoder))))

  return(recoder)
}

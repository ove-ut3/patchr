#' Recoder les champs d'une table a partir d'un identifiant d'individu
#'
#' Recoder les champs d'une table à partir d'un identifiant d'individu.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param source Nom de la source à filtrer dans la table \code{table_recodage}.
#' @param .champ_id Le nom du champ contenant l'identifiant de l'individu.
#'
#' @return La table recodée
#'
#' @export
recoder_individu <- function(table, table_recodage, source = NULL, .champ_id = "code_etudiant") {

  if (!is.null(source)) {
    table_recodage <- dplyr::filter(table_recodage, source %in% !!source)
  }

  table_recodage <- dplyr::filter(table_recodage, champ %in% names(table)) %>%
    dplyr::select(!!.champ_id, champ, .valeur = valeur)

  if (dplyr::inner_join(table, table_recodage, by = .champ_id) %>% nrow() == 0) {
    return(table)
  }

  table <- dplyr::mutate(table, .id = row_number())

  options(warn = -1)
  id_na <- dplyr::select(table, !!.champ_id, .id) %>%
    tidyr::gather("champ", "valeur", -.id) %>%
    dplyr::filter(is.na(valeur)) %>%
    dplyr::select(.id) %>%
    unique()
  options(warn = 0)

  table_id_na <- dplyr::semi_join(table, id_na, by = ".id") %>%
    dplyr::select(-.id)

  table <- dplyr::anti_join(table, id_na, by = ".id") %>%
    dplyr::select(-.id)

  ordre_champs <- names(table)

  if (any(lapply(table, class) == "list")) {
    champs_list <- dplyr::select(table, !!.champ_id, which(purrr::map_lgl(table, is.list)))
    table <- dplyr::select(table, -which(purrr::map_lgl(table, is.list)))
  }

  if (any(purrr::map_lgl(table, ~ any(class(.) == "POSIXct")))) {
    champs_posix <- dplyr::select(table, !!.champ_id, which(purrr::map_lgl(table, ~ any(class(.) == "POSIXct"))))
    table <- dplyr::select(table, -which(purrr::map_lgl(table, ~ any(class(.) == "POSIXct"))))
  }

  content_maj <- dplyr::tibble(champ = names(table),
                               classe = purrr::map_chr(table, class) %>% tolower())

  if (any(lapply(table, class) == "Date")) {
    table <- table %>%
      dplyr::mutate_at(.vars = names(.)[which(purrr::map_lgl(., lubridate::is.Date))], as.character)
  }

  recoder <- tidyr::gather(table, "champ", "valeur", -!!.champ_id) %>%
    dplyr::left_join(table_recodage, by = c(.champ_id, "champ")) %>%
    dplyr::mutate(valeur = ifelse(!is.na(.valeur), .valeur, valeur),
                  valeur = dplyr::na_if(valeur, "[null]")) %>%
    dplyr::select(!!.champ_id, champ, valeur) %>%
    unique() %>% #En cas de question à choix multiple
    tidyr::spread(champ, valeur)

  recoder <- source.maj::transcoder_champs(recoder, content_maj)

  if (exists("champs_list")) {
    recoder <- dplyr::full_join(recoder, champs_list, by = .champ_id)
  }

  if (exists("champs_posix")) {
    recoder <- dplyr::full_join(recoder, champs_posix, by = .champ_id)
  }

  recoder <- recoder %>%
    dplyr::select(purrr::map_int(ordre_champs, ~ which(. == names(recoder)))) %>%
    dplyr::bind_rows(table_id_na)

  return(recoder)
}

#' Recoder les champs d'une table a partir d'une expression VRAI/FAUX
#'
#' Recoder les champs d'une table à partir d'une expression VRAI/FAUX.
#'
#' @param table La table à recoder.
#' @param table_recodage La table de recodage.
#' @param source Nom de la source à filtrer dans la table \code{table_recodage}.
#' @param filtre La valeur de filtre.
#'
#' @return La table recodée
#'
#' @export
recoder_champs <- function(table, table_recodage, source = NULL, filtre = NULL) {

  if (nrow(table) == 0) return(table)

  if (!is.null(source)) {
    table_recodage <- dplyr::filter(table_recodage, source %in% !!source)
  }

  if (!is.null(filtre)) {
    table_recodage <- tidyr::separate_rows(table_recodage, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))
  }

  if (nrow(table_recodage) == 0) return(table)

  champs_a_creer <- table_recodage$champ[which(!table_recodage$champ %in% names(table))] %>%
      unique()

  if (length(champs_a_creer) >= 1) {

    classe_champs_a_creer <- table_recodage %>%
      dplyr::filter(champ %in% champs_a_creer) %>%
      dplyr::mutate(classe = "character",
                    classe = ifelse(stringr::str_detect(valeur, "^\\d+L$"), "integer", classe),
                    classe = ifelse(valeur == "NA_integer_", "integer", classe),
                    classe = ifelse(stringr::str_detect(valeur, "^[\\d\\.]+$"), "real", classe),
                    classe = ifelse(valeur == "NA_real_", "real", classe)) %>%
      dplyr::select(champ, classe) %>%
      unique() %>%
      dplyr::pull(classe) %>%
      paste0("NA_", ., "_")

    list_mutate <- stats::setNames(as.list(classe_champs_a_creer), as.list(champs_a_creer)) %>%
      lapply(rlang::parse_quosure)

    table <- dplyr::mutate(table, !!!list_mutate)
  }

  if (!is.null(table_recodage[["ordre"]])) {
    table_recodage <- dplyr::arrange(table_recodage, ordre)
  }

  table_recodage <- table_recodage %>%
    dplyr::mutate(classe = purrr::map(champ, ~ class(table[[.]])),
                  valeur = ifelse(purrr::map_lgl(classe, ~ "factor" %in% .),
                                  paste0("factor(", valeur, ", levels = levels(", champ,"))"),
                                  valeur))

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

#' as_factor
#'
#' @param champ_factor \dots
#' @param table_niveaux \dots
#'
#' @export
#' @keywords internal
as_factor <- function(champ_factor, table_niveaux = NULL) {

  if (is.null(table_niveaux)) {
    champ_factor <- as.factor(champ_factor)
    return(champ_factor)
  }

  nom_champ_factor <- dplyr::enquo(champ_factor) %>%
    dplyr::quo_name()

  niveaux <- table_niveaux %>%
    dplyr::filter(champ == nom_champ_factor) %>%
    dplyr::arrange(ordre) %>%
    dplyr::pull(niveau)

  if (length(niveaux) != 0) {
    champ_factor <- factor(champ_factor, levels = niveaux)
  } else {
    champ_factor <- as.factor(champ_factor)
  }

  return(champ_factor)
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
        source.maj::recoder_champs(table_recodage = dplyr::tibble(champ = champs_a_creer,
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
    dplyr::mutate(.id = row_number()) %>%
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
    dplyr::mutate_at(dplyr::vars(champs_factor), source.maj::as_factor, table_niveaux) %>%
    dplyr::bind_cols(table %>%
                       dplyr::select(which(!names(.) %in% champs_factor)))

  recoder <- recoder %>%
    dplyr::select(purrr::map_int(ordre_champs, ~ which(. == names(recoder))))

  return(recoder)
}

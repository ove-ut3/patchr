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

  data_recode <- dplyr::filter(data_recode, colname %in% names(data)) %>%
    dplyr::select(!!colname_id, colname, .value = value)

  if (dplyr::inner_join(data, data_recode, by = colname_id) %>% nrow() == 0) {
    return(data)
  }

  data <- dplyr::mutate(data, .id = dplyr::row_number())

  options(warn = -1)
  id_na <- dplyr::select(data, !!colname_id, .id) %>%
    tidyr::gather("colname", "value", -.id) %>%
    dplyr::filter(is.na(value)) %>%
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

  data_transcode <- dplyr::tibble(colname = names(data),
                               class = purrr::map_chr(data, class) %>% tolower())

  if (any(lapply(data, class) == "Date")) {
    data <- data %>%
      dplyr::mutate_at(.vars = names(.)[which(purrr::map_lgl(., lubridate::is.Date))], as.character)
  }

  recode <- tidyr::gather(data, "colname", "value", -!!colname_id) %>%
    dplyr::left_join(data_recode, by = c(colname_id, "colname")) %>%
    dplyr::mutate(value = ifelse(!is.na(.value), .value, value),
                  value = dplyr::na_if(value, "[null]")) %>%
    dplyr::select(!!colname_id, colname, value) %>%
    unique() %>% # In case of multiple answer field
    tidyr::spread(colname, value)

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

#' Recode a data frame with formulas.
#'
#' The data frame is recoded according to a correspondance table containing formulas.\cr
#' The correpondance table contains a least three columns : expression, colname and value.
#'
#' @param data A data frame.
#' @param data_recode The correspondance table containing formulas.
#' @param new_cols If \code{TRUE} then it computes all colnames referenced in data_recode, not only colnames in data at the first place.
#'
#' @return A recoded data frame.
#'
#' @examples
#' # Recode without expression
#' patchr::recode_formula(
#' data = dplyr::tibble(test = 1L),
#' data_recode = dplyr::tibble(expression = NA_character_, colname = "test", value = "2L")
#' )
#'
#' # Recode with expression
#' patchr::recode_formula(
#' data = dplyr::tibble(test = c(1L, 2L)),
#' data_recode = dplyr::tibble(expression = "test == 2", colname = "test", value = "3L")
#' )
#'
#' @export
recode_formula <- function(data, data_recode, new_cols = TRUE) {

  if (nrow(data) == 0) return(data)

  if (new_cols == FALSE) {
    data_recode <- dplyr::filter(data_recode, colname %in% names(data))
  }

  if (nrow(data_recode) == 0) return(data)

  if (new_cols == TRUE) {

    new_columns <- data_recode$colname[which(!data_recode$colname %in% names(data))] %>%
        unique()

    if (length(new_columns) >= 1) {

      list_mutate <- data_recode %>%
        dplyr::filter(colname %in% new_columns) %>%
        dplyr::mutate(class = "character",
                      class = ifelse(stringr::str_detect(value, "^\\d+L$"), "integer", class),
                      class = ifelse(value == "NA_integer_", "integer", class),
                      class = ifelse(stringr::str_detect(value, "^[\\d\\.]+$"), "real", class),
                      class = ifelse(value == "NA_real_", "real", class)) %>%
        dplyr::select(colname, class) %>%
        unique() %>%
        dplyr::pull(class) %>%
        paste0("NA_", ., "_") %>%
        as.list()

      names(list_mutate) <- new_columns
      list_mutate <- lapply(list_mutate, rlang::parse_quosure)

      data <- dplyr::mutate(data, !!!list_mutate)
    }
  }

  if (!is.null(data_recode[["order"]])) {
    data_recode <- dplyr::arrange(data_recode, order)
  }

  data_recode <- data_recode %>%
    dplyr::mutate(classe = purrr::map(colname, ~ class(data[[.]])),
                  value = ifelse(purrr::map_lgl(classe, ~ "factor" %in% .),
                                  paste0("factor(", value, ", levels = levels(", colname,"))"),
                                  value))

  list_mutate <- ifelse(is.na(data_recode$expression),
                              data_recode$value,
                              paste0("dplyr::if_else(", data_recode$expression, ", ", data_recode$value,", ", data_recode$colname, ", ", data_recode$colname, ")")) %>%
    as.list()

  names(list_mutate) <- data_recode$colname
  list_mutate <- lapply(list_mutate, rlang::parse_quosure)

  data <- dplyr::mutate(data, !!!list_mutate)

  return(data)
}

#' Recode columns to factor in a data frame.
#'
#' The data frame is recoded according to a correspondance table.\cr
#' The correpondance table contains a least three columns : colname, value and factor.\cr
#' An optional table data_levels contains ordered levels for each colname. It contains at least three colmuns : colname, order, level.
#'
#' @param data A data frame.
#' @param data_recode A correspondance table containing between character values and unordered level factors.
#' @param data_levels An optional correpondance table between colnames and ordered levels.
#' @param new_cols If \code{TRUE} then it computes all colnames referenced in data_recode, not only colnames in data at the first place.
#'
#' @return A recoded data frame.
#'
#' @export
recode_factor <- function(data, data_recode, data_levels = NULL, new_cols = FALSE) {

  if (nrow(data) == 0) return(data)

  if (nrow(data_recode) == 0) return(table)

  if (new_cols == TRUE) {

    new_columns <- c(data_recode$colname, data_levels$colname) %>%
      unique() %>%
      .[which(!. %in% names(table))]

    if (length(new_columns) >= 1) {

      table <- table %>%
        patchr::recode_formula(data_recode = dplyr::tibble(colname = new_columns,
                                                           value = "NA_character_",
                                                           expression = NA_character_))
    }

  }

  if (intersect(names(table), data_recode$colname) %>% length() == 0) {
    return(table)
  }

  cols_factor <- names(table) %>%
    intersect(c(data_recode$colname, data_levels$colname))

  cols_order <- names(table)

  recode <- table %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::select(.id, cols_factor) %>%
    dplyr::mutate_at(dplyr::vars(cols_factor), as.character) %>%
    tidyr::gather("colname", "value", -.id) %>%
    dplyr::left_join(data_recode %>%
                       dplyr::select(colname, value, recodage),
                     by = c("colname", "value")) %>%
    dplyr::mutate(value = ifelse(!is.na(recodage), recodage, value) %>%
                    dplyr::na_if("[null]")) %>%
    dplyr::select(-recodage) %>%
    tidyr::spread(colname, value) %>%
    dplyr::select(-.id) %>%
    dplyr::mutate_at(dplyr::vars(cols_factor), patchr::as_factor, data_levels) %>%
    dplyr::bind_cols(table %>%
                       dplyr::select(which(!names(.) %in% cols_factor)))

  recode <- recode %>%
    dplyr::select(purrr::map_int(cols_order, ~ which(. == names(recode))))

  return(recode)
}

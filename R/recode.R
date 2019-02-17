#' Recode individual rows in a data frame.
#'
#' The data frame is recoded according to a correspondance table containing a row id.\cr
#' The correpondance table contains a least three fields : \code{column}, \code{value} and at least one id variable named with vars_id.
#'
#' @param data A data frame.
#' @param data_recode The correspondance table.
#' @param vars_id The identification variables in both data and data_recode tables.
#'
#' @return A recoded data frame.
#'
#' @export
recode_id <- function(data, data_recode, vars_id) {

  duplicate <- data %>%
    dplyr::group_by_at(dplyr::vars(vars_id)) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup()

  if (nrow(duplicate) >= 1) {
    stop("Duplicates are in data according to vars_id arguments", call. = FALSE)
  }

  data_recode <- dplyr::filter(data_recode, column %in% names(data)) %>%
    dplyr::select(!!vars_id, column, .value = value)

  if (dplyr::inner_join(data, data_recode, by = vars_id) %>% nrow() == 0) {
    return(data)
  }

  data <- dplyr::mutate(data, .id = dplyr::row_number())

  options(warn = -1)
  id_na <- dplyr::select(data, !!vars_id, .id) %>%
    tidyr::gather("column", "value", -.id) %>%
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
    col_list <- dplyr::select(data, !!vars_id, which(purrr::map_lgl(data, is.list)))
    data <- dplyr::select(data, -which(purrr::map_lgl(data, is.list)))
  }

  if (any(purrr::map_lgl(data, ~ any(class(.) == "POSIXct")))) {
    col_posix <- dplyr::select(data, !!vars_id, which(purrr::map_lgl(data, ~ any(class(.) == "POSIXct"))))
    data <- dplyr::select(data, -which(purrr::map_lgl(data, ~ any(class(.) == "POSIXct"))))
  }

  data_transcode <- dplyr::tibble(column = names(data),
                               class = purrr::map_chr(data, class) %>% tolower())

  if (any(lapply(data, class) == "Date")) {
    data <- data %>%
      dplyr::mutate_at(.vars = names(.)[which(purrr::map_lgl(., lubridate::is.Date))], as.character)
  }

  recode <- tidyr::gather(data, "column", "value", -!!vars_id) %>%
    dplyr::left_join(data_recode, by = c(vars_id, "column")) %>%
    dplyr::mutate(value = ifelse(!is.na(.value), .value, value),
                  value = dplyr::na_if(value, "[null]")) %>%
    dplyr::select(!!vars_id, column, value) %>%
    unique() %>% # In case of multiple answer field
    tidyr::spread(column, value)

  recode <- patchr::transcode(recode, data_transcode)

  if (exists("col_list")) {
    recode <- dplyr::full_join(recode, col_list, by = vars_id)
  }

  if (exists("col_posix")) {
    recode <- dplyr::full_join(recode, col_posix, by = vars_id)
  }

  recode <- recode %>%
    dplyr::select(purrr::map_int(col_order, ~ which(. == names(recode)))) %>%
    dplyr::bind_rows(data_id_na)

  return(recode)
}

#' Recode a data frame with formulas.
#'
#' The data frame is recoded according to a correspondance table containing formulas.\cr
#' The correpondance table contains a least three fields : \code{condition}, \code{column} and \code{value}.
#'
#' @param data A data frame.
#' @param data_recode The correspondance table containing formulas.
#' @param new_vars If \code{TRUE} then it computes all variables referenced in data_recode, not only variables in data at the first place.
#'
#' @return A recoded data frame.
#'
#' @examples
#' # Recode without condition
#' patchr::recode_formula(
#' data = dplyr::tibble(test = 1L),
#' data_recode = dplyr::tibble(condition = NA_character_, column = "test", value = "2L")
#' )
#'
#' # Recode with condition
#' patchr::recode_formula(
#' data = dplyr::tibble(test = c(1L, 2L)),
#' data_recode = dplyr::tibble(condition = "test == 2", column = "test", value = "3L")
#' )
#'
#' @export
recode_formula <- function(data, data_recode, new_vars = TRUE) {

  if (nrow(data) == 0) return(data)

  if (new_vars == FALSE) {
    data_recode <- dplyr::filter(data_recode, column %in% names(data))
  }

  if (nrow(data_recode) == 0) return(data)

  if (new_vars == TRUE) {

    new_columns <- data_recode$column[which(!data_recode$column %in% names(data))] %>%
        unique()

    if (length(new_columns) >= 1) {

      list_mutate <- data_recode %>%
        dplyr::filter(column %in% new_columns) %>%
        dplyr::mutate(class = dplyr::case_when(
          stringr::str_detect(value, "^\\d+L$") ~ "integer",
          stringr::str_detect(value, "^as\\.integer\\(") ~ "integer",
          value == "NA_integer_"  ~ "integer",
          stringr::str_detect(value, "^[\\d\\.]+$") ~ "real",
          stringr::str_detect(value, "^as\\.numeric\\(") ~ "real",
          value == "NA_real_"  ~ "real",
          TRUE ~ "character")
        ) %>%
        dplyr::select(column, class) %>%
        unique() %>%
        dplyr::pull(class) %>%
        paste0("NA_", ., "_") %>%
        as.list()

      names(list_mutate) <- new_columns
      list_mutate <- lapply(list_mutate, rlang::parse_quo, env = rlang::caller_env())

      data <- dplyr::mutate(data, !!!list_mutate)
    }
  }

  if (!is.null(data_recode[["order"]])) {
    data_recode <- dplyr::arrange(data_recode, order)
  }

  data_recode <- data_recode %>%
    dplyr::mutate(classe = purrr::map(column, ~ class(data[[.]])),
                  value = ifelse(purrr::map_lgl(classe, ~ "factor" %in% .),
                                  glue::glue("factor({value}, levels = levels({column}))"),
                                  value))

  list_mutate <- ifelse(is.na(data_recode$condition),
                              data_recode$value,
                              paste0("dplyr::if_else(", data_recode$condition, ", ", data_recode$value,", `", data_recode$column, "`, `", data_recode$column, "`)")) %>%
    as.list()

  names(list_mutate) <- data_recode$column
  list_mutate <- lapply(list_mutate, rlang::parse_quo, env = rlang::caller_env())

  data <- dplyr::mutate(data, !!!list_mutate)

  return(data)
}

#' Recode columns to factor in a data frame.
#'
#' The data frame is recoded according to a correspondance table.\cr
#' The correpondance table contains a least three columns : \code{column}, \code{value} and \code{factor}.\cr
#' An optional table data_levels contains ordered levels for each column. It contains at least three fields : \code{column}, \code{order}, \code{level}.
#'
#' @param data A data frame.
#' @param data_recode A correspondance table containing between character values and unordered level factors.
#' @param data_levels An optional correpondance table between column names and ordered levels.
#' @param new_vars If \code{TRUE} then it computes all variables referenced in data_recode, not only variables in data at the first place.
#'
#' @return A recoded data frame.
#'
#' @export
recode_factor <- function(data, data_recode, data_levels = NULL, new_vars = FALSE) {

  if (nrow(data) == 0) return(data)

  if (nrow(data_recode) == 0) return(data)

  if (new_vars == TRUE) {

    new_columns <- c(data_recode$column, data_levels$column) %>%
      unique() %>%
      .[which(!. %in% names(data))]

    if (length(new_columns) >= 1) {

      data <- data %>%
        patchr::recode_formula(data_recode = dplyr::tibble(column = new_columns,
                                                           value = "NA_character_",
                                                           condition = NA_character_))
    }

  }

  if (intersect(names(data), data_recode$column) %>% length() == 0) {
    return(data)
  }

  cols_factor <- names(data) %>%
    .[!purrr::map_lgl(data, ~ "list" %in% class(.))] %>%
    intersect(c(data_recode$column, data_levels$column))

  cols_order <- names(data)

  recode <- data %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::select(.id, cols_factor) %>%
    dplyr::mutate_at(dplyr::vars(cols_factor), as.character) %>%
    tidyr::gather("column", "value", -.id) %>%
    dplyr::left_join(data_recode %>%
                       dplyr::select(column, value, factor),
                     by = c("column", "value")) %>%
    dplyr::mutate(value = ifelse(!is.na(factor), factor, value) %>%
                    dplyr::na_if("[null]")) %>%
    dplyr::select(-factor) %>%
    tidyr::spread(column, value) %>%
    dplyr::select(-.id) %>%
    dplyr::mutate_at(dplyr::vars(cols_factor), patchr::as_factor, data_levels) %>%
    dplyr::bind_cols(data %>%
                       dplyr::select(which(!names(.) %in% cols_factor)))

  recode <- recode %>%
    dplyr::select(purrr::map_int(cols_order, ~ which(. == names(recode))))

  return(recode)
}

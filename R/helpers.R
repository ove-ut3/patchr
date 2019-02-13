#' Replace empty character with NA in a data frame.
#'
#' @param data A data frame.
#'
#' @return A data frame without empty character.
#'
#' @examples
#' data <- dplyr::tibble(
#'   champ_1 = "",
#'   champ_2 = ""
#' )
#' patchr::replace_empty_to_na(data)
#'
#' @export
replace_empty_to_na <- function(data){

  if (nrow(data) == 0) {
    return(data)
  }

  data <- dplyr::mutate_if(data, is.character, caractr::str_empty_to_na)

  return(data)
}

#' Filter a correspondance table according to additional colmuns within the table.
#'
#' @param data A rename or recode data frame.
#' @param source A first filter with the source column name.
#' @param filtre A second filter with the filtre column name.
#'
#' @return A filtered data frame.
#'
#' @export
filter_data_patch <- function(data, source = NULL, filtre = NULL) {

  if (!is.null(source)) {
    data <- dplyr::filter(data, source %in% !!source)
  }

  if (!is.null(filtre)) {
    data <- tidyr::separate_rows(data, filtre, sep = ";") %>%
      dplyr::filter(filtre == !!filtre | is.na(filtre))
  }

  return(data)
}

#' Remove duplicate rows from a table according a column name.
#'
#' @param data A data frame.
#' @param var A variable for which duplicate rows are removed.
#'
#' @return A data frame with no duplicate for column name.
#'
#' @export
remove_duplicate <- function(data, var) {

  quo_var <- dplyr::enquo(var)

  remove_duplicate <- tidyr::nest(data, !!quo_var) %>%
    dplyr::mutate(!!dplyr::quo_name(quo_var) := purrr::map_chr(data, ~ ifelse(length(.[[1]]) == 1, .[[1]], NA_character_))) %>%
    dplyr::select(-data)

  return(remove_duplicate)
}

#' Extract duplicate rows from a data frame.
#'
#' @param A data frame.
#'
#' @return A data frame containing only the duplicate rows.
#'
#' @examples
#' data <- dplyr::tibble(cle1 = c("A", "A", "B", "B"), cle2 = c("1", "1", "2", "3"), champ = 1:4)
#'
#' # With duplicate
#' patchr::duplicate(data, cle1, cle2)
#'
#' # Without duplicate
#' patchr::duplicate(data, cle1, cle2, champ))
#'
#' @export
duplicate <- function(data, ...){

  if (any(class(data) == "data.frame") == FALSE) {
    stop("The first paramater must be a data frame", call. = FALSE)
  }

  group_by <- dplyr::quos(...)

  duplicate <- dplyr::group_by(data, !!!group_by) %>%
    dplyr::filter(dplyr::row_number() >= 2) %>%
    dplyr::ungroup() %>%
    dplyr::select(purrr::map_chr(group_by, dplyr::quo_name)) %>%
    unique() %>%
    dplyr::right_join(data, ., by = purrr::map_chr(group_by, dplyr::quo_name))

  return(duplicate)
}

#' Patch a current vector from a target vector.
#'
#' @param current A vector to be patched.
#' @param target A patch vector. If length 1 then recycled to the length of current and only NA are patched.
#' @param only_na If \code{TRUE} then only missing values in current are patched.
#'
#' @return A patched vector.
#'
#' @examples
#' patchr::patch_vector(c(1, NA_real_, 3), c(4, 5, 6))
#' patchr::patch_vector(c(1, NA_real_, 3), c(4, 5, 6), only_na = TRUE)
#' patchr::patch_vector(c(1, NA_real_, 3), 4)
#'
#' @export
patch_vector <- function(current, target, only_na = FALSE){

  if (length(target) == 1) {
    target <- rep(target, length(current))
    only_na <- TRUE
  }

  if (length(current) != length(target)) {
    stop("current and target vector must have the same length", call. = FALSE)
  } else if (class(current) != class(target)) {
    stop("current and target vector must have the same class", call. = FALSE)
  }

  if (only_na == FALSE) {
    current <- ifelse(!is.na(target), target, current)
  } else if (only_na == TRUE) {
    current[which(is.na(current))] <- target[which(is.na(current))]
  }

  return(current)
}

#' Functions anti_join and bind_rows executed successively.
#'
#' The rows from x + the rows from y that are not in x.
#'
#' @param x,y data frames to join.
#' @param by a character vector of variables to join by.
#' @param arrange if \code{TRUE}, arrange rows with the \code{by} variables.
#'
#' @return A data frame.
#'
#' @export
anti_join_bind <- function(x, y, by, arrange = TRUE) {

  anti_join_bind <- y %>%
    dplyr::anti_join(x, by) %>%
    dplyr::bind_rows(x, .)

  if (arrange == TRUE) {
    anti_join_bind <- dplyr::arrange(anti_join_bind, !!!purrr::map(by, rlang::parse_quo, env = rlang::caller_env()))
  }

  return(anti_join_bind)
}

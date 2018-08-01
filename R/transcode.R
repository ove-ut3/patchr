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
      dplyr::mutate(num_champ = dplyr::row_number()) %>%
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
        else if (classe_maj == "factor") table[[champ]] <- as.factor(table[[champ]])

      }

    }

    return(table)
  }

}

#' Title
#'
#' @param df
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
clean_history <- function(df, lang) {
  df_cleaned <- df %>%
    dplyr::mutate(
      n_characters_removed = stringr::str_remove(n_characters_removed, "−"),
      n_characters_added = stringr::str_remove(n_characters_added, "\\+"),
      n_characters_removed  = stringr::str_remove_all(n_characters_removed, "\\D"),
      n_characters_added = stringr::str_remove_all(n_characters_added, "\\D"),
      n_characters_removed = readr::parse_double(n_characters_removed),
      n_characters_added = readr::parse_double(n_characters_added),
      n_characters_removed = tidyr::replace_na(n_characters_removed, 0),
      n_characters_added = tidyr::replace_na(n_characters_added, 0),
      minor_edit = dplyr::case_when(minor_edit == "m" ~ TRUE,
                                    TRUE ~ NA),
      tag = dplyr::case_when(tag == "NA" ~ NA_character_,
                             TRUE ~ tag)
    ) %>%
    dplyr::mutate(
      original_comment = comment,
      section = stringr::str_extract(comment, pattern = "→.*:"),
      section = stringr::str_remove(section, pattern = ":.*$"),
      comment = stringr::str_remove(comment, pattern = "→.*:"),
      section = dplyr::case_when(stringr::str_detect(comment, "^→") ~ comment,
                                 TRUE ~ section),
      section = stringr::str_remove_all(section, pattern = ":|→"),
      section = stringr::str_trim(section),
      comment = stringr::str_remove(comment, "^→.*"),
      comment = stringr::str_trim(comment),
      comment = dplyr::case_when(comment == "" ~ NA_character_, TRUE ~ comment)
    ) %>%
    dplyr::relocate(original_comment, section, comment, .before = n_characters_removed)

  if (lang == "pt") {
    df_cleaned %>%
      tidyr::separate(
        date,
        sep = " de ",
        into = c("time",  "date"),
        extra = "merge"
      ) %>%
      dplyr::mutate(date = readr::parse_date(date, format = "%d de %B de %Y", locale = readr::locale("pt")),)

  } else {
    df_cleaned
  }

}

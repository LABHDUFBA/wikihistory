#' Title
#'
#' @param template
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
get_template <- function(template, lang) {

  if(lang == "pt"){
    template_search <- template %>% stringr::str_replace_all(" ", "_")

    url_wikipedia <-
      glue::glue("https://{lang}.wikipedia.org/wiki/Template:{template_search}")

    url_encoded <- utils::URLencode(url_wikipedia)

    html <- rvest::read_html(url_encoded)

    html %>% rvest::html_table() %>%
      purrr::pluck(1) %>%
      janitor::clean_names() %>%
      dplyr::rename("themes" = 1,
                    "pages" = 2) %>%
      dplyr::select(-3) %>%
      tidyr::separate_rows(pages, sep = "·") %>%
      dplyr::mutate(pages = stringr::str_squish(pages))
  } else {
    stop("This language is not implemented yet.")
  }


}

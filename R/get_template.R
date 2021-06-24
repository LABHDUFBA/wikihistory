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

    navbox <- html %>% rvest::html_element(xpath = '//*[@id="mw-content-text"]/div[1]/div') %>%
      rvest::html_element(xpath = '//*[@class="navbox"]')

    navbox %>%
      rvest::html_elements("a") %>%
      rvest::html_attr("title") %>%
      tibble::enframe() %>%
      tidyr::drop_na(value) %>%
      dplyr::select(-name, "page_name" = value)



    # navbox %>% rvest::html_elements(xpath = '//*[@class="navbox-group"]') %>%
    #   rvest::html_text()
    #
    # navbox %>% rvest::html_table(convert = FALSE)
    #
    #

    #
    # html %>% rvest::html_table() %>%
    #   purrr::pluck(1) %>%
    #   janitor::clean_names() %>%
    #   dplyr::rename("themes" = 1,
    #                 "page_name" = 2) %>%
    #   dplyr::select(-3) %>%
    #   tidyr::separate_rows(page_name, sep = "·") %>%
    #   tidyr::separate_rows(page_name, sep = ":") %>%
    #   dplyr::mutate(page_name = stringr::str_squish(page_name),
    #                 template = template)
  } else {
    stop("This language is not implemented yet.")
  }


}

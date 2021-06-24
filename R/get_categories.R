#' Title
#'
#' @param page
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
get_categories <- function(page, lang = "en") {
  page_search <- page %>% stringr::str_replace_all(" ", "_")

  url_wikipedia <-
    glue::glue("https://{lang}.wikipedia.org/wiki/{page_search}")
  url_encoded <- utils::URLencode(url_wikipedia)

  html <- rvest::read_html(url_encoded)

  itens <-
    rvest::html_node(html, xpath = '//*[@id="mw-normal-catlinks"]') %>%
    rvest::html_nodes("li")

  category <- itens %>%
    rvest::html_text()

  link <- itens %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")

  tibble::tibble(page,
                 category,
                 link = glue::glue("https://{lang}.wikipedia.org{link}"))


}

#' Title
#'
#' @param category
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
get_pages_in_categories <- function(category, lang){

  category_search <- category %>% stringr::str_replace_all(" ", "_")

  url_wikipedia <-
    glue::glue("https://{lang}.wikipedia.org/wiki/Category:{category_search}")

  url_encoded <- utils::URLencode(url_wikipedia)

  html <- rvest::read_html(url_encoded)

  itens <-  rvest::html_node(html, xpath = '//*[@class="mw-category"]') %>%
    rvest::html_nodes("li") %>% rvest::html_nodes("a")

 page_name <- rvest::html_text(itens)

  page_link <- rvest::html_attr(itens, "href")

  tibble::tibble(category,
                 page_name,
                 page_link = glue::glue("https://{lang}.wikipedia.org{page_link}"))

}

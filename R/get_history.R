#' Title
#'
#' @param page
#' @param n_limit
#'
#' @return
#' @export
#'
#' @examples
get_history <- function(page, n_limit = "10000", lang = "en") {
  page_search <- page %>% stringr::str_replace_all(" ", "_")

  url_wikipedia <-
    glue::glue(
      "https://{lang}.wikipedia.org/w/index.php?title={page_search}&offset=&limit={n_limit}&action=history"
    )
  url_encoded <- utils::URLencode(url_wikipedia)

  html <- rvest::read_html(url_encoded)

  itens <-
    rvest::html_node(html, xpath = '//*[@id="pagehistory"]') %>%
    rvest::html_nodes("li")




  id <- find_id(itens)


  user <-
    itens %>%
    rvest::html_nodes("bdi") %>%
    rvest::html_text()

  date <- find_class(itens, "mw-changeslist-date")
  #secao_alterada <- find_class(itens, "autocomment")
  comment <-
    find_class(itens, "comment comment--without-parentheses")
  n_characters_removed <-
    find_class(itens, "mw-plusminus-neg mw-diff-bytes")
  n_characters_added <-
    find_class(itens, "mw-plusminus-pos mw-diff-bytes")
  tamanho_mudanca_bytes <-
    find_class(itens, "history-size mw-diff-bytes")
  minor_edit <- find_class(itens, "minoredit")


  tag <- itens %>%
    purrr::map( ~ rvest::html_nodes(.x, "[class='mw-tag-markers']")) %>%
    purrr::map(~  rvest::html_elements(.x, "span")) %>%
    purrr::map(function(.x) {
      aux <- rvest::html_text(.x)
      if (length(aux) == 0) {
        return(NA_character_)
      } else {
        return(aux)
      }
    }) %>%
    purrr::map( ~ knitr::combine_words(.x, sep = "; ", and = "; ")) %>%
    purrr::as_vector()


  tabela_suja <- tibble::tibble(
    page,
    id,
    user,
    date,
    comment,
    n_characters_removed,
    n_characters_added,
    minor_edit,
    tag
  )

  tabela_suja
}

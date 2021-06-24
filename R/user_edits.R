#' Title
#'
#' @param id_user
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
user_edits <- function(id_user, lang) {
  url <-
    glue::glue("https://xtools.wmflabs.org/topedits/{lang}.wikipedia.org/{id_user}/0")

  url_encoded <- utils::URLencode(url)

  html <- rvest::read_html(url_encoded)

   is_error <- html %>%
   rvest::html_table() %>%
   length()


  if (is_error == 1) {

    table <- html %>%
      rvest::html_node(xpath = '//*[@class="table table-bordered table-hover table-striped topedits-namespace-table xt-show-hide--target"]') %>%
      rvest::html_table()

   table %>%
      janitor::clean_names() %>%
      dplyr::transmute(id_user = id_user,
                       page_title,
                       "n_edits" = edits) %>%
     dplyr::mutate(n_edits =  as.character(n_edits),
                   n_edits = readr::parse_number(n_edits)) %>%
     tibble::as_tibble()



  } else {

    tibble::tibble(
      id_user, page_title = "This user has not opted in to have this data shown",
      n_edits = NA_real_
     )  %>%
      dplyr::mutate(n_edits = as.numeric(n_edits)) %>%
      tibble::as_tibble()
   }

}


#' Title
#'
#' @param users
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
get_all_edits <- function(users, lang = "pt") {
  purrr::map_dfr(users, user_edits, lang = "pt")
}

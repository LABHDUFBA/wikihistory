#' Title
#'
#' @param id_user
#' @param lang
#' @param sys_sleep
#' @param sleep_seconds
#'
#' @return
#' @export
#'
#' @examples
# user_edits <- function(id_user, lang, sys_sleep = FALSE, sleep_seconds = 5) {
#   url <-
#     glue::glue("https://xtools.wmflabs.org/topedits/{lang}.wikipedia.org/{id_user}/0")
#
#   url_encoded <- utils::URLencode(url)
#
#   html <- rvest::read_html(url_encoded)
#
#    is_error <- html %>%
#    rvest::html_table() %>%
#    length()
#
#
#   if (is_error == 1) {
#
#     table <- html %>%
#       rvest::html_node(xpath = '//*[@class="table table-bordered table-hover table-striped topedits-namespace-table xt-show-hide--target"]') %>%
#       rvest::html_table()
#
#    table %>%
#       janitor::clean_names() %>%
#       dplyr::transmute(id_user = id_user,
#                        page_title,
#                        "n_edits" = edits) %>%
#      dplyr::mutate(n_edits =  as.character(n_edits),
#                    n_edits = readr::parse_number(n_edits)) %>%
#      tibble::as_tibble()
#
#
#
#   } else {
#
#     tibble::tibble(
#       id_user, page_title = "This user has not opted in to have this data shown",
#       n_edits = NA_real_
#      )  %>%
#       dplyr::mutate(n_edits = as.numeric(n_edits)) %>%
#       tibble::as_tibble()
#   }
#
#    if(sys_sleep == TRUE){
#      Sys.sleep(time = sleep_seconds)
#    }
#
#
# }


#' Title
#'
#' @param id_user
#' @param lang
#'
#' @return
#' @export
#'
#' @examples
user_edits_json <- function(id_user, lang) {
  url <-
    glue::glue("https://xtools.wmflabs.org/api/user/top_edits/{lang}.wikipedia.org/{id_user}/0")

  url_encoded <- utils::URLencode(url)

 safely_fromJSON <- purrr::safely(jsonlite::fromJSON)

safely_json <- safely_fromJSON(url_encoded, simplifyDataFrame = TRUE)

safely_json$error
if(is.null(safely_json$error)){


  raw_tibble  <- safely_json %>%
    purrr::pluck(1) %>%
    purrr::pluck("top_edits") %>%
    purrr::pluck(1) %>%
    tibble::as_tibble()

  raw_tibble %>%
    janitor::clean_names() %>%
    dplyr::transmute(id_user = id_user,
                     page_title,
                     "n_edits" = count) %>%
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
  purrr::map_dfr(users, user_edits_json, lang = "pt")
}

#' Title
#'
#' @param lista
#' @param classe
#'
#' @return
#' @export
#'
#' @examples
find_class <- function(lista, classe) {
  lista %>%
    purrr::map( ~ rvest::html_nodes(.x, glue::glue("[class='{classe}']"))) %>%
    purrr::map(function(.x) {
      aux <- rvest::html_text(.x)
      if (length(aux) == 0) {
        return(NA_character_)
      } else {
        return(aux)
      }
    }) %>%
    purrr::as_vector()  # é aqui que ta dando problema
}

#' Title
#'
#' @param lista
#'
#' @return
#' @export
#'
#' @examples
find_id <- function(lista) {
  lista %>% rvest::html_attr("data-mw-revid")
}

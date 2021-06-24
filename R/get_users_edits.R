#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
get_users_edits <- function(df){
  df %>%
    dplyr::distinct(user) %>%
    dplyr::pull(user)
}

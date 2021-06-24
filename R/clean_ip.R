#' Title
#'
#' @param ip_vector
#'
#' @return
#' @export
#'
#' @examples
clean_ip <- function(ip_vector) {

  remove <- ip_vector %>%
    stringr::str_detect(pattern =  "\\d\\d\\d\\.|\\d\\d\\d\\d\\:|\\.\\d\\d\\d|\\.\\d\\d", negate = TRUE)

  ip_vector_filtered <- ip_vector[remove]

  ip_vector_filtered

}

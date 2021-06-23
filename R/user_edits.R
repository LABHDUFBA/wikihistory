user_edits <- function(id_user, lang) {
  url <-
    glue::glue("https://xtools.wmflabs.org/topedits/{lang}.wikipedia.org/{id_user}/0")

  url_encoded <- utils::URLencode(url)

  html <- rvest::read_html(url_encoded)

  table <- html %>%
    rvest::html_node(xpath = '//*[@class="table table-bordered table-hover table-striped topedits-namespace-table xt-show-hide--target"]') %>%
    rvest::html_table()

  table %>%
    janitor::clean_names() %>%
    dplyr::transmute(id_user = id_user,
                     page_title,
                     "n_edits" = edits)


}

# users <-
#   get_history("Jair Bolsonaro", n_limit = 100, lang = "pt") %>%
#   get_users_edits()

get_all_edits <- function(users, lang = "pt") {
  purrr::map_dfr(users , user_edits, lang = "pt")

  # Error in UseMethod("html_table") :
  #   no applicable method for 'html_table' applied to an object of class "xml_missing"


}

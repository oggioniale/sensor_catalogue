#' @title
#' @description
#' A short description...
#' 
#' @param manufacturer_name description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @example
#' check_man_exist(manufacturer_name = "CO.L.MAR.")
#' check_man_exist(manufacturer_name = "a")
#' check_man_exist(manufacturer_name = "Campbell Scientific")
#' @keywords internal
#'
### function check_man_exist
check_man_exist <- function(manufacturer_name = NULL) {
  library(magrittr)
  contacts_query <- paste0("PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l
    WHERE {
      ?c rdf:type foaf:Organization.
      ?c foaf:name ?l.
      FILTER( REGEX( ?l, '",
      manufacturer_name,
      "', 'i' ))
    }
    ORDER BY ASC(?l)")
  manufacturer_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") |>
    httr2::req_url_query(query = contacts_query) |>
    httr2::req_method("POST") |>
    httr2::req_headers(Accept = "application/sparql-results+json") |>
    httr2::req_retry(max_tries = 3, max_seconds = 120) |>
    httr2::req_perform()
  httr2::resp_check_status(manufacturer_qr)
  manufacturer_JSON <- httr2::resp_body_json(manufacturer_qr)
  
  man_list <- dplyr::tibble(
    label = as.character(),
    uri = as.character()
  )
  if (length(manufacturer_JSON$results$bindings) == 0) {
    uri <- NULL
    man_exist <- NULL
  } else {
    for (i in 1:length(manufacturer_JSON$results$bindings)) {
      man_list %<>%
        tibble::add_row(
          tibble::tibble_row(
            uri = manufacturer_JSON$results$bindings[[i]]$c$value,
            label = manufacturer_JSON$results$bindings[[i]]$l$value
          )
        )
    }
  }
  
  if (nrow(man_list) == 0) {
    uri <- NULL
    man_exist <- NULL
  } else {
    uri <- man_list$uri
    man_exist <- man_list$label
  }
  man_exist
}

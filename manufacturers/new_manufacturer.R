#' @title
#' @description
#' A short description...
#' 
#' @param excel_path description
#' @param orcid_creator description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom package function
#' @export
#' @example
#' new_manufacturer(
#'   excel_path = "new_manufacturers.xlsx",
#'   orcid_creator = "http://orcid.org/0000-0002-7997-219X"
#' )
#' 
### function new_manufacturer
new_manufacturer <- function(excel_path = NULL,
                             orcid_creator = "http://orcid.org/0000-0002-7997-219X") {
  create_rdf_ttl_manufacturer(
    excel_path = excel_path,
    orcid_creator = orcid_creator
  )
  create_rdf_XML_manufacturer(
    excel_path = excel_path,
    orcid_creator = orcid_creator
  )
}

#' @title
#' @description
#' A short description...
#' 
#' @param excel_path description
#' @param orcid_creator description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom package function
#' @keywords internal
#' @example
#' create_rdf_XML_manufacturer(
#'   excel_path = "new_manufacturers.xlsx",
#'   orcid_creator = "http://orcid.org/0000-0002-7997-219X"
#' )
#' 
### function create_rdf_XML_manufacturer
create_rdf_XML_manufacturer <- function(excel_path = NULL,
                                 orcid_creator = "http://orcid.org/0000-0002-7997-219X") {
  # the last number of manufacture records in fuseki
  query <- "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT (COUNT(?c) AS ?number)
  WHERE {
    ?c rdf:type foaf:Organization.
  }"
  number_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    httr2::req_url_query(query = query) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(Accept = "application/sparql-results+json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(number_qr)
  number_JSON <- httr2::resp_body_json(number_qr)
  last_number <- number_JSON$results$bindings[[1]]$number$value
  # read excel file ----
  excel_file <- readxl::read_excel(excel_path)
  n_new_records <- nrow(excel_file)
  file_name <- tools::file_path_sans_ext(excel_path)
  baseRDF_XML <- xml2::xml_new_document()
  baseRDF_XML <- baseRDF_XML %>%
    xml2::xml_add_child(
      "rdf:RDF",
      "xmlns:rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xmlns" = "http://rdfdata.lteritalia.it/sensors/",
      "xmlns:rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
      "xmlns:vcard" = "http://www.w3.org/2006/vcard/ns#",
      "xmlns:foaf" = "http://xmlns.com/foaf/0.1/",
      "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema#",
      "xmlns:org" = "http://www.w3.org/ns/org#",
      "xmlns:addr" = "http://wymiwyg.org/ontologies/foaf/postaddress#",
      "xml:base" = "http://rdfdata.lteritalia.it/sensors",
      "xmlns:prov" = "http://www.w3.org/ns/prov#", 
      "xmlns:dc" = "http://purl.org/dc/elements/1.1/"
    )
  for (i in 1:n_new_records) {
    a <- baseRDF_XML %T>%
      {xml2::xml_add_child(
        .,
        "foaf:Organization",
        "rdf:about" = paste0("http://rdfdata.get-it.it/sensors/manufacturers/", as.numeric(last_number) + i)
      ) %>%
      xml2::xml_add_child(
        .,
        "dc:creator",
        .where = "after",
        "rdf:resource" = orcid_creator
      ) %>% 
      xml2::xml_add_sibling(
        .,
        "foaf:name",
        .where = "after",
        excel_file$name[i]
      ) %>%
      xml2::xml_add_sibling(
        .value = "foaf:homepage",
        .where = "after",
        "rdf:resource" = excel_file$homepage[i]
      ) %>%
      xml2::xml_add_sibling(
        .value = "vcard:email",
        .where = "after",
        "rdf:resource" = paste0("mailto:", excel_file$email[i])
      ) %>%
      xml2::xml_add_sibling(
        .value = "foaf:phone",
        .where = "after",
        "rdf:resource" = paste0(
          "tel:",
          stringr::str_replace_all(excel_file$phone[i], " ", "-")
        )
      ) %>%
      xml2::xml_add_sibling(
        .value = "addr:address",
        .where = "after",
        "rdf:parseType" = "Resource"
      ) %>%
      xml2::xml_add_child(
        .value = "addr:deliveryPoint",
        "rdf:parseType" = "Resource"
      ) %>%
      xml2::xml_add_child(
        .value = "addr:location",
        "rdf:parseType" = "Resource"
      ) %>%
      xml2::xml_add_child(
        .value = "addr:thoroughfareName",
        excel_file$thoroughfareName[i]
      ) %>%
      xml2::xml_add_sibling(.value = "addr:streetNr", excel_file$streetNr[i]) %>%
      xml2::xml_add_sibling(.value = "addr:postcode", excel_file$postcode[i]) %>%
      xml2::xml_add_sibling(.value = "addr:building", excel_file$building[i]) %>%
      xml2::xml_add_sibling(.value = "addr:town", excel_file$town[i]) %>%
      xml2::xml_add_sibling(.value = "addr:region", excel_file$region[i]) %>%
      xml2::xml_add_sibling(.value = "addr:country", excel_file$country[i])}
  }
  root_dir <- "manufacturers"
  if (!dir.exists(root_dir)) {root_dir}
  time_4file <- strftime(Sys.time(), format = "%Y%m%dT%H%M%S")
  xml2::write_xml(baseRDF_XML, paste0(root_dir, "/", time_4file, "_", file_name, ".rdf"))
}

#' @title
#' @description
#' A short description...
#' 
#' @param excel_path description
#' @param orcid_creator description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom package function
#' @export
#' @example
#' create_rdf_ttl_manufacturer(
#'   excel_path = "new_manufacturers.xlsx",
#'   orcid_creator = "http://orcid.org/0000-0002-7997-219X"
#' )
#' 
### function create_rdf_ttl_manufacturer
create_rdf_ttl_manufacturer <- function(excel_path = NULL,
                                        orcid_creator = "http://orcid.org/0000-0002-7997-219X") {
  # the last number of manufacture records in fuseki ----
  query <- "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT (COUNT(?c) AS ?number)
  WHERE {
    ?c rdf:type foaf:Organization.
  }"
  number_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    httr2::req_url_query(query = query) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(Accept = "application/sparql-results+json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(number_qr)
  number_JSON <- httr2::resp_body_json(number_qr)
  last_number <- number_JSON$results$bindings[[1]]$number$value
  # read excel file ----
  excel_file <- readxl::read_excel(excel_path)
  n_new_records <- nrow(excel_file)
  root_dir <- "manufacturers"
  if (!dir.exists(root_dir)) {root_dir}
  file_name <- tools::file_path_sans_ext(excel_path)
  time_4file <- strftime(Sys.time(), format = "%Y%m%dT%H%M%S")
  fileTTL <- file(
    paste0(root_dir, "/", time_4file, "_", file_name, ".ttl"),
    "wb",
    encoding = "UTF-8"
  )
  org <- '@prefix org:   <http://www.w3.org/ns/org#> 
  @prefix dcterms: <http://purl.org/dc/terms/> 
  @prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> 
  @prefix vcard: <http://www.w3.org/2006/vcard/ns#> 
  @prefix addr:  <http://wymiwyg.org/ontologies/foaf/postaddress#> 
  @prefix foaf:  <http://xmlns.com/foaf/0.1/> 
  @prefix prov: <http://www.w3.org/ns/prov#> 
  @prefix dc: <http://purl.org/dc/elements/1.1/> 
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#>'
  for (i in 1:n_new_records) {
    org <- c(
      org,
      paste0("<http://rdfdata.get-it.it/sensors/manufacturers/", as.numeric(last_number) + i, ">"),
      "        a              foaf:Organization ;",
      paste0("        vcard:email    <mailto:", excel_file$email[i], "> ;"),
      paste0("        addr:address   [ addr:deliveryPoint  [ addr:location  [ addr:building          '", excel_file$building[i], "' ;"),
      paste0("                                                                addr:country           '", excel_file$country[i], "' ;"),
      paste0("                                                                addr:postcode          '", excel_file$postcode[i], "' ;"),
      paste0("                                                                addr:region            '", excel_file$region[i], "' ;"),
      paste0("                                                                addr:streetNr          '", excel_file$streetNr[i], "' ;"),
      paste0("                                                                addr:thoroughfareName  '", excel_file$thoroughfareName[i], "' ;"),
      paste0("                                                                addr:town              '", excel_file$town[i], "'"),
      "                                                              ]",
      "                                             ]",
      "                       ] ;",
      paste0("        foaf:homepage  <", excel_file$homepage[i], "> ;"),
      paste0("        foaf:name      '", excel_file$name[i], "' ;"),
      paste0("        foaf:phone     <tel:", chartr(" ", "-", excel_file$phone[i]), "> ;"),
      paste0("        dc:creator     <", orcid_creator, "> ;"),
      paste0(
        "        prov:generatedAtTime      '",
        strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = ""),
        "'^^xsd:dateTime ."
      )
    )
  }
  write(org, file = fileTTL)
  # close file
  close(fileTTL)
  # send the ttl to SPARQL endpoint
  new_mans_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    httr2::req_auth_basic(username = username_fuseki1, password = pwd_fuseki1) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_file(
      path = paste0(root_dir, "/", time_4file, "_", file_name, ".ttl"),
      type = "text/turtle"
    ) %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120)
  httr2::req_perform(new_mans_qr, verbosity = 3)
}

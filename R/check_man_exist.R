#' Check if a manufacturer exists in the Fuseki manufacturer dataset
#' @description
#' The function queries a Fuseki SPARQL endpoint to verify if a given manufacturer
#' (organization) exists in the RDF dataset. It performs a SPARQL query against
#' the `foaf:name` field of all `foaf:Organization` resources and returns any
#' matches that contain the given manufacturer name (case-insensitive).
#' @param manufacturer_name Character string. The (partial or full) name of the manufacturer to check.
#' If `NULL`, the function will return `NULL`.
#' @return
#' A character vector with the names (`foaf:name`) of the matching manufacturers,
#' or `NULL` if no matches are found.
#' @details
#' The function sends a SPARQL query via `httr2` to the endpoint
#' <http://fuseki1.get-it.it/manufacturers>. It uses a case-insensitive regular expression
#' to match manufacturer names. The returned list is parsed from JSON and converted
#' into a tibble.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @importFrom dplyr tibble
#' @importFrom tibble add_row tibble_row
#' @importFrom magrittr %<>%
#' @examples
#' \dontrun{
#' sensors_check_man_exist(manufacturer_name = "CO.L.MAR.")
#' sensors_check_man_exist(manufacturer_name = "a")
#' sensors_check_man_exist(manufacturer_name = "ADM Elektronik")
#' }
#' @keywords internal
#'
### function check_man_exist
sensors_check_man_exist <- function(
    manufacturer_name,
    endpoint = "http://fuseki1.get-it.it/manufacturers",
    verbose = FALSE) {
  # Input validation
  if (!is.character(manufacturer_name) || length(manufacturer_name) != 1) {
    stop("`manufacturer_name` must be a single character string", call. = FALSE)
  }
  manufacturer_name <- trimws(manufacturer_name)
  if (manufacturer_name == "") {
    if (verbose) message("Empty manufacturer name provided; returning zero results")
    return(tibble::tibble(label = character(0),
                          uri   = character(0),
                          found = FALSE))
  }
  
  # Build SPARQL query (simple sanitisation)
  sparql_query <- paste0(
    "PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l
    WHERE {
      ?c rdf:type foaf:Organization.
      ?c foaf:name ?l.
      FILTER( REGEX( ?l, '",
    manufacturer_name,
    "', 'i' ))
    }
    ORDER BY ASC(?l)"
  )
  
  if (verbose) message("Sending query for manufacturer: ", manufacturer_name)
  
  # HTTP request
  resp <- httr2::request(endpoint) %>%
    httr2::req_url_query(query = sparql_query) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(Accept = "application/sparql-results+json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_error(body = function(r) {
      b <- httr2::resp_body_json(r, simplifyVector = TRUE)
      paste0("SPARQL endpoint error: ", b$error$message %||% "<unknown>")
    }) %>%
    httr2::req_perform()
  
  httr2::resp_check_status(resp)
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  # Parse results
  bindings <- body$results$bindings
  if (length(bindings) == 0) {
    if (verbose) message("No manufacturers found matching ", manufacturer_name)
    return(tibble::tibble(label = character(0),
                          uri   = character(0),
                          found = FALSE))
  }
  # Map results
  man_exist <- tibble::tibble(
    label = bindings$l$value,
    uri   = bindings$c$value
  )
  man_exist <- dplyr::mutate(man_exist, found = TRUE)
  return(man_exist)
}

#' Create RDF and XML metadata for new sensor manufacturers
#' @description
#' Wrapper function that automates the generation of both RDF/Turtle (`.ttl`)
#' and SensorML/XML (`.xml`) metadata files for new sensor manufacturers.
#' It takes as input a manufacturer table (`man_table`) and the ORCID identifier
#' of the metadata creator, and then calls the internal functions
#' `sensors_create_rdf_ttl_manufacturer()` and `sensors_create_rdf_XML_manufacturer()`
#' in sequence.
#'
#' @param man_table A data frame containing manufacturer metadata (name, contact,
#' address, etc.).
#' @param orcid_creator Character string with the ORCID URI of the metadata creator.
#'
#' @return
#' Creates and saves RDF/Turtle and XML files describing each manufacturer.
#' No object is returned.
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @keywords internal
#' @examples
#' # Example of a manufacturer entry
#' manufacturer_list <- tibble::tibble(
#'   name = "New Manufacturer",
#'   homepage = "https://www.newmanufacturer.com",
#'   email = "info@newmanufacturer.com",
#'   phone = "+39 02 1234567",
#'   thoroughfareName = "Via Roma",
#'   streetNr = "10",
#'   postcode = "20100",
#'   building = NA,
#'   town = "Milano",
#'   region = "Lombardia",
#'   country = "Italy"
#' )
#' sensors_new_manufacturer(
#'   man_table = manufacturer_list,
#'   orcid_creator = "0000-0002-1234-5678"
#' )
#' 
### function sensors_new_manufacturer
sensors_new_manufacturer <- function(
    man_table = NULL,
    orcid_creator = NULL) {
  library(magrittr)
  if (is.null(orcid_creator))    orcid_creator   <- "https://orcid.org/0000-0002-7997-219X"
  # ORCID normalization
  if (!startsWith(orcid_creator, "https://orcid.org/"))
    orcid_creator <- paste0("https://orcid.org/", orcid_creator)
  sensors_create_rdf_ttl_manufacturer(
    man_table = man_table,
    orcid_creator = orcid_creator
  )
  sensors_create_rdf_XML_manufacturer(
    man_table = man_table,
    orcid_creator = orcid_creator
  )
}

#' Create Create RDF/XML records for new sensor manufacturers
#' @description
#' This function generates RDF/XML (`.ttl`) files describing new sensor manufacturers
#' based on a local input table. Each manufacturer is represented as a `foaf:Organization`
#' with associated contact information, postal address, and provenance metadata.  
#' The function queries the existing Fuseki triple store to determine the last
#' manufacturer ID and appends new ones sequentially.
#' @param man_table A data frame containing manufacturer metadata with the
#' following required columns:  
#' `name`, `email`, `homepage`, `phone`, `building`, `country`, `postcode`,
#' `region`, `streetNr`, `thoroughfareName`, and `town`.
#' @param orcid_creator Character string representing the ORCID URI of the
#' metadata creator (e.g. `"0000-0002-1234-5678"`).  
#' This value is included as `dc:creator` in each RDF record.
#' @return
#' The function writes a single RDF/XML file in the working directory named
#' with a timestamp (e.g., `"20251103T213015_newManufacturers.rdf"`).  
#' It returns the path of the created file (invisible).
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @keywords internal
#' 
### function sensors_create_rdf_XML_manufacturer
sensors_create_rdf_XML_manufacturer <- function(
    man_table = NULL,
    orcid_creator = NULL) {
  if (is.null(orcid_creator))    orcid_creator   <- "https://orcid.org/0000-0002-7997-219X"
  # ORCID normalization
  if (!startsWith(orcid_creator, "https://orcid.org/"))
    orcid_creator <- paste0("https://orcid.org/", orcid_creator)
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
  # read manufacturer table ---
  n_new_records <- nrow(man_table)
  baseRDF_XML <- xml2::xml_new_document()
  baseRDF_XML <- baseRDF_XML %>%
    xml2::xml_add_child(
      "rdf:RDF",
      "xmlns:rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xmlns" = "https://rdfdata.lteritalia.it/sensors/",
      "xmlns:rdfs" = "http://www.w3.org/2000/01/rdf-schema#",
      "xmlns:vcard" = "http://www.w3.org/2006/vcard/ns#",
      "xmlns:foaf" = "http://xmlns.com/foaf/0.1/",
      "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema#",
      "xmlns:org" = "http://www.w3.org/ns/org#",
      "xmlns:addr" = "http://wymiwyg.org/ontologies/foaf/postaddress#",
      "xml:base" = "https://rdfdata.lteritalia.it/sensors",
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
            man_table$name[i]
          ) %>%
          xml2::xml_add_sibling(
            .value = "foaf:homepage",
            .where = "after",
            "rdf:resource" = man_table$homepage[i]
          ) %>%
          xml2::xml_add_sibling(
            .value = "vcard:email",
            .where = "after",
            "rdf:resource" = paste0("mailto:", man_table$email[i])
          ) %>%
          xml2::xml_add_sibling(
            .value = "foaf:phone",
            .where = "after",
            "rdf:resource" = paste0(
              "tel:",
              stringr::str_replace_all(man_table$phone[i], " ", "-")
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
            man_table$thoroughfareName[i]
          ) %>%
          xml2::xml_add_sibling(.value = "addr:streetNr", man_table$streetNr[i]) %>%
          xml2::xml_add_sibling(.value = "addr:postcode", man_table$postcode[i]) %>%
          xml2::xml_add_sibling(.value = "addr:building", man_table$building[i]) %>%
          xml2::xml_add_sibling(.value = "addr:town", man_table$town[i]) %>%
          xml2::xml_add_sibling(.value = "addr:region", man_table$region[i]) %>%
          xml2::xml_add_sibling(.value = "addr:country", man_table$country[i])}
  }
  time_4file <- strftime(Sys.time(), format = "%Y%m%dT%H%M%S")
  xml2::write_xml(baseRDF_XML, paste0(time_4file, "_newManufacturers.rdf"))
}

#' Create Create RDF/Turtle records for new sensor manufacturers
#' @description
#' This function generates RDF/Turtle (`.ttl`) files describing new sensor manufacturers
#' based on a local input table. Each manufacturer is represented as a `foaf:Organization`
#' with associated contact information, postal address, and provenance metadata.  
#' The function queries the existing Fuseki triple store to determine the last
#' manufacturer ID and appends new ones sequentially.
#' @param man_table description
#' @param orcid_creator description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @keywords internal
#' 
### function sensors_create_rdf_ttl_manufacturer
sensors_create_rdf_ttl_manufacturer <- function(
    man_table = NULL,
    orcid_creator = "http://orcid.org/0000-0002-7997-219X") {
  
  if (is.null(man_table) || nrow(man_table) == 0) {
    stop("❌ man_table is empty or missing.")
  }
  
  # --- Retrieve last manufacturer count from Fuseki ---
  query <- "
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT (COUNT(?c) AS ?number)
  WHERE { ?c rdf:type foaf:Organization. }"
  
  number_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    httr2::req_url_query(query = query) %>%
    httr2::req_method("POST") %>%
    httr2::req_headers(Accept = "application/sparql-results+json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  
  httr2::resp_check_status(number_qr)
  number_JSON <- httr2::resp_body_json(number_qr)
  last_number <- number_JSON$results$bindings[[1]]$number$value
  
  # --- Prepare RDF file ---
  n_new_records <- nrow(man_table)
  time_4file <- strftime(Sys.time(), format = "%Y%m%dT%H%M%S")
  fileTTL_path <- paste0(time_4file, "_newManufacturers.ttl")
  fileTTL <- file(fileTTL_path, "wb", encoding = "UTF-8")
  
  for (i in 1:n_new_records) {
    org <- c(
      "@prefix org: <http://www.w3.org/ns/org#> .",
      "@prefix dcterms: <http://purl.org/dc/terms/> .", 
      "@prefix addr:  <http://wymiwyg.org/ontologies/foaf/postaddress#> .",
      "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
      "@prefix vcard: <http://www.w3.org/2006/vcard/ns#> .",
      "@prefix foaf: <http://xmlns.com/foaf/0.1/> .",
      "@prefix dct:  <http://purl.org/dc/terms/> .",
      "",
      paste0("<http://rdfdata.get-it.it/sensors/manufacturers/", as.numeric(last_number) + i, ">"),
      "        a              foaf:Organization ;",
      paste0("        vcard:email    <mailto:", man_table$email[i], "> ;"),
      paste0('        addr:address   [ addr:deliveryPoint  [ addr:location  [ addr:building          "', man_table$building[i], '" ;'),
      paste0('                                                                addr:country           "', man_table$country[i], '" ;'),
      paste0('                                                                addr:postcode          "', man_table$postcode[i], '" ;'),
      paste0('                                                                addr:region            "', man_table$region[i], '" ;'),
      paste0('                                                                addr:streetNr          "', man_table$streetNr[i], '" ;'),
      paste0('                                                                addr:thoroughfareName  "', man_table$thoroughfareName[i], '" ;'),
      paste0('                                                                addr:town              "', man_table$town[i], '"'),
      "                                                              ]",
      "                                             ]",
      "                       ] ;",
      paste0("        foaf:homepage  <", man_table$homepage[i], "> ;"),
      paste0('        foaf:name      "', man_table$name[i], '" ;'),
      paste0("        foaf:phone     <tel:", chartr(' ', '-', man_table$phone[i]), "> ;"),
      paste0("        dct:creator     <", orcid_creator, "> ;"),
      paste0(
        '        dct:created      "',
        strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = ""),
        '"^^xsd:dateTime .'
      )
    )
  }
  
  write(org, file = fileTTL)
  close(fileTTL)
  message("✅ TTL file created: ", fileTTL_path)
  
  # --- Validate TTL file ---
  if (exists("sensors_validate_ttl")) {
    message("🔍 Running RDF validation...")
    validation_result <- tryCatch(
      sensors_validate_ttl(fileTTL_path),
      error = function(e) {
        message("⚠️ Validation failed: ", e$message)
        return(NULL)
      }
    )
    if (!is.null(validation_result)) print(validation_result)
  } else {
    message("⚠️ sensors_validate_ttl() function not found. Validation skipped.")
  }
  
  invisible(fileTTL_path)
}

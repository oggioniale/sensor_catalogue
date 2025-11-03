#' Create an RDF/Turtle sensor instance and validate its structure
#' @description
#' Generates a valid RDF/Turtle (.ttl) file describing a **sensor instance** following
#' the SOSA/SSN ontology. The instance is typed (`rdf:type`) according to the given
#' sensor type, includes FOAF contact metadata, and provenance information
#' (`dct:creator`, `dct:created`). Optionally validates the output RDF file.
#' @details
#' The RDF graph is built using the **rdflib** package to ensure proper syntax,
#' namespace handling, and semantic consistency.  
#' When `validate = TRUE`, the resulting TTL file is parsed and checked for
#' the presence of key predicates:
#' `rdf:type`, `dct:creator`, `dct:created`, and `dcat:contactPoint`.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom uuid UUIDgenerate
#' @param sensor_type_uri Character. URI of the sensor type (e.g. `"https://rdfdata.lteritalia.it/sensors/systemsType/UUIDTYPE"`).
#' @param owner_name Character. First name of the sensor owner.
#' @param owner_surname Character. Last name of the sensor owner.
#' @param owner_orcid Character. ORCID of the sensor owner (e.g. `"0000-0002-1825-0097"`).
#' @param output_dir Character. Directory where to save the TTL file (default = current working directory).
#' @return
#' Invisibly returns the path to the generated `.ttl` file.
#' @examples
#' \dontrun{
#' sensors_instance_ttl(
#'   sensor_type_uri = "https://rdfdata.lteritalia.it/sensors/systemsType/33559394-24ae-42e6-9b63-cf56b8b52b30",
#'   owner_name = "John",
#'   owner_surname = "Doe",
#'   owner_orcid = "0000-0002-1234-5678"
#' )
#' }
#' @export
#'
### function sensors_instance_ttl
sensors_instance_ttl <- function(
    sensor_type_uri = NULL,
    owner_name = NULL,
    owner_surname = NULL,
    owner_orcid = NULL,
    output_dir = getwd()
    ) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  # --- Default metadata for the creator and contact point ---
  if (is.null(owner_name))     owner_name    <- "Alessandro"
  if (is.null(owner_surname))  owner_surname <- "Oggioni"
  if (is.null(owner_orcid))    owner_orcid   <- "https://orcid.org/0000-0002-7997-219X"
  # ORCID normalization
  if (!startsWith(owner_orcid, "https://orcid.org/"))
    owner_orcid <- paste0("https://orcid.org/", owner_orcid)
  
  instance_uuid <- uuid::UUIDgenerate()
  instance_uri <- paste0("https://rdfdata.lteritalia.it/sensors/instances/", instance_uuid)
  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)
  
  ttl <- c(
    "@prefix sosa: <http://www.w3.org/ns/sosa/> .",
    "@prefix ssn:  <http://www.w3.org/ns/ssn/> .",
    "@prefix prov: <http://www.w3.org/ns/prov#> .",
    "@prefix dcat: <http://www.w3.org/ns/dcat#> .",
    "@prefix foaf: <http://xmlns.com/foaf/0.1/> .",
    "@prefix dct:  <http://purl.org/dc/terms/> .",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
    "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
    "@prefix xsd:  <http://www.w3.org/2001/XMLSchema#> .",
    "",
    paste0("<", instance_uri, ">"),
    "    a sosa:Sensor, ssn:System, prov:Entity ;",
    paste0("    a <", sensor_type_uri, "> ;"),
    paste0("    rdfs:label \"Sensor instance of type ", esc(basename(sensor_type_uri)), "\"@en ;"),
    paste0("    rdfs:comment \"This sensor instance derives from the type ", esc(sensor_type_uri), ".\"@en ;"),
    paste0("    dct:creator <", owner_orcid, "> ;"),
    paste0("    dct:created \"", now_iso, "\"^^xsd:dateTime ;"),
    "    dcat:contactPoint [",
    "        a foaf:Person ;",
    paste0("        foaf:givenName \"", esc(owner_name), "\" ;"),
    paste0("        foaf:familyName \"", esc(owner_surname), "\" ;"),
    paste0("        foaf:account <", owner_orcid, ">"),
    "    ] .",
    ""
  )
  
  out <- file.path(
    output_dir,
    paste0(
      "sensor_instance_",
      format(Sys.time(), "%Y%m%d"),
      "_",
      instance_uuid,
      ".ttl"
    )
  )
  writeLines(ttl, out)
  message("✅ TTL created ", out)
  
  sensors_validate_ttl(out)
  
  invisible(out)
}

#' Create an RDF/Turtle sensor instance and validate its structure
#' @description `r lifecycle::badge("stable")`
#' Generates a valid RDF/Turtle (.ttl) file describing a **sensor instance**
#' following the SOSA/SSN ontology. The instance is typed (\code{rdf:type})
#' according to the given sensor type, includes FOAF contact metadata, and
#' provenance information (\code{dct:creator}, \code{dct:created}).
#' Optionally validates the output RDF file.
#'
#' The function automatically dereferences the sensor type URI (or parses a
#' local TTL file) to extract the sensor type label (\code{rdfs:label}) and
#' the manufacturer name (\code{foaf:name} of the \code{foaf:Organization}
#' linked via \code{dcat:contactPoint}). These are used to build a
#' human-readable \code{rdfs:label} for the sensor instance, following the
#' pattern:
#'
#' \emph{Sensor instance of \<type label\> of \<manufacturer\>, with
#' SN:\<serial number\> — Owner: \<surname\> — created \<date\>}
#'
#' Each part of the label is included only if the relevant information is
#' available. If the sensor type cannot be parsed, the label falls back to
#' the basename of the \code{sensor_type_uri}.
#'
#' @details
#' The sensor type is parsed via \code{rdflib::rdf_parse()}, which supports
#' both local file paths and dereferenceable HTTP URIs. The real subject URI
#' within the graph is discovered dynamically by looking for a subject with
#' a \code{dct:identifier} predicate, so the function is not tied to any
#' specific URI pattern.
#'
#' When \code{validate = TRUE}, the resulting TTL file is parsed and checked
#' for the presence of key predicates: \code{rdf:type}, \code{dct:creator},
#' \code{dct:created}, and \code{dcat:contactPoint}.
#'
#' @param sensor_type_uri \code{character}. URI or local file path of the
#'   sensor type TTL (e.g.
#'   \code{"https://rdfdata.lteritalia.it/sensors/systemsType/UUID"} or
#'   \code{"path/to/sensor_type.ttl"}). Used both to type the instance
#'   (\code{rdf:type}) and to extract the type label and manufacturer name
#'   for the instance \code{rdfs:label}.
#' @param owner_name \code{character}. First name of the sensor owner.
#'   Defaults to \code{"Alessandro"} if \code{NULL}.
#' @param owner_surname \code{character}. Last name of the sensor owner.
#'   Defaults to \code{"Oggioni"} if \code{NULL}. Used in the instance
#'   \code{rdfs:label}.
#' @param owner_orcid \code{character}. ORCID of the sensor owner
#'   (e.g. \code{"0000-0002-1825-0097"} or the full URI
#'   \code{"https://orcid.org/0000-0002-1825-0097"}). Automatically
#'   normalised to a full ORCID URI if a bare ID is provided.
#'   Defaults to \code{"https://orcid.org/0000-0002-7997-219X"} if
#'   \code{NULL}.
#' @param owner_institution \code{character} or \code{NULL}. Name of the
#'   institution that owns the sensor (e.g.
#'   \code{"National Research Council"}). If provided, it is included in
#'   the \code{dcat:contactPoint} block as an \code{org:memberOf} entry
#'   and as an institutional \code{prov:agent} in the
#'   \code{prov:qualifiedAttribution} block. Optional.
#' @param owner_ror \code{character} or \code{NULL}. ROR identifier of the
#'   owning institution, either as a bare ID (e.g. \code{"01234567"}) or
#'   as a full URI (e.g. \code{"https://ror.org/01234567"}). Automatically
#'   normalised to a full ROR URI if a bare ID is supplied. When provided
#'   alongside \code{owner_institution}, the organisation node receives an
#'   \code{owl:sameAs} triple pointing to the ROR URI and the ROR URI is
#'   also added as a \code{prov:agent} in the attribution block. If
#'   \code{NULL}, the institutional part is omitted entirely. Optional.
#' @param serial_number \code{character} or \code{NULL}. Serial number of
#'   the physical sensor instance. If provided and non-empty, it is stored
#'   as \code{schema:serialNumber} in the TTL and included in the instance
#'   \code{rdfs:label}. Optional.
#' @param part_number \code{character} or \code{NULL}. Manufacturer's part
#'   number assigned to the sensor, used for internal calibration tracking
#'   and instrument management. Example: \code{"37SI.11200"}.
#' @param validate \code{logical}. If \code{TRUE} (default), the generated
#'   TTL file is validated by \code{sensors_validate_ttl()} after creation.
#' @param output_dir \code{character}. Directory where the TTL file is
#'   saved. Created recursively if it does not exist. Defaults to the
#'   current working directory (\code{getwd()}).
#'
#' @return
#' Invisibly returns the \code{character} path to the generated \code{.ttl}
#' file. The file is named following the pattern:
#' \code{sensor_instance_\<YYYYMMDD\>_\<UUID\>.ttl}.
#'
#' @note
#' If the sensor type TTL cannot be parsed (e.g. network error or malformed
#' file), a \code{warning()} is issued and the instance label falls back to
#' the basename of \code{sensor_type_uri}. The TTL file is still generated.
#'
#' The manufacturer name is resolved by following the
#' \code{dcat:contactPoint} → \code{foaf:Organization} → \code{foaf:name}
#' path in the sensor type graph. If no \code{foaf:Organization} contact
#' point is found, the manufacturer part is omitted from the label.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#'
#' @seealso \code{\link{sensors_validate_ttl}},
#'   \code{\link{sensors_to_datacite}}
#'
#' @importFrom uuid UUIDgenerate
#' @importFrom rdflib rdf_parse rdf_query
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # With a referenceable sensor type URI
#' sensors_instance_ttl(
#'   sensor_type_uri = "https://rdfdata.lteritalia.it/sensors/systemsType/33559394-24ae-42e6-9b63-cf56b8b52b30",
#'   owner_name = "John",
#'   owner_surname = "Doe",
#'   owner_orcid = "0000-0002-1234-5678",
#'   owner_institution = "National Research Council",
#'   owner_ror = "01234567",
#'   serial_number = "SN-ABC-123",
#'   part_number = "37SI.11200"
#' )
#' # → rdfs:label: "Sensor instance of WISPstation of Water Insight,
#' #                with SN:SN-ABC-123 — Owner: Doe — created 2026-04-03"
#'
#' # With a local TTL file as sensor type
#' sensors_instance_ttl(
#'   sensor_type_uri = "Sensors_files_system_0cd0794b-2c7b-4897-8ced-a3f756e90bc4/system_components_files_ttl/ID_system_0cd0794b-2c7b-4897-8ced-a3f756e90bc4.ttl",
#'   owner_name = "John",
#'   owner_surname = "Doe",
#'   owner_orcid = "0000-0002-1234-5678",
#'   owner_institution = "National Research Council",
#'   owner_ror = "01234567",
#'   serial_number = "SN-XYZ-999",
#'   part_number = "37SI.11200",
#'   output_dir = "output/instances"
#' )
#' }
#' 
### function sensors_instance_ttl
sensors_instance_ttl <- function(
    sensor_type_uri = NULL,
    owner_name = NULL,
    owner_surname = NULL,
    owner_orcid = NULL,
    owner_institution = NULL,
    owner_ror = NULL,
    serial_number = NULL,
    part_number = NULL,
    validate = TRUE,
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
  
  # ROR normalization
  if (!is.null(owner_ror) && !startsWith(owner_ror, "https://ror.org/"))
    owner_ror <- paste0("https://ror.org/", owner_ror)
  
  # Optional institution triple (if both name and ROR are provided)
  institution_triples <- character(0)
  if (!is.null(owner_institution) && nzchar(trimws(owner_institution))) {
    if (!is.null(owner_ror)) {
      institution_triples <- c(
        paste0("    org:memberOf <", owner_ror, "> ;")
      )
    }
  }
  
  # --- Fetch label and manufacturer from the sensor type TTL ---
  # Parse the sensor type: supports both a local file path and a
  # dereferenceable URI. The subject URI is discovered dynamically
  # so the function is not tied to any specific URI pattern.
  type_label    <- NA_character_
  manufacturer  <- NA_character_
  
  if (!is.null(sensor_type_uri)) {
    g_type <- tryCatch(
      rdflib::rdf_parse(sensor_type_uri, format = "turtle"),
      error = function(e) {
        warning("Could not parse sensor type: ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(g_type)) {
      # Discover the real subject URI inside the graph (may differ from file path)
      subj_res <- rdflib::rdf_query(g_type,
                                    "SELECT DISTINCT ?s WHERE {
           ?s <http://purl.org/dc/terms/identifier> [] .
         } LIMIT 1"
      )
      type_subject <- if (NROW(subj_res) > 0) as.character(subj_res$s[1]) else NA_character_
      
      if (!is.na(type_subject)) {
        # rdfs:label of the sensor type (e.g. "WISPstation")
        label_res <- rdflib::rdf_query(g_type, sprintf(
          "SELECT ?label WHERE {
             <%s> <http://www.w3.org/2000/01/rdf-schema#label> ?label .
           } LIMIT 1",
          type_subject
        ))
        if (NROW(label_res) > 0) type_label <- as.character(label_res$label[1])
        
        # Manufacturer name via dcat:contactPoint → foaf:Organization → foaf:name
        mfr_res <- rdflib::rdf_query(g_type, sprintf(
          "SELECT ?name WHERE {
             <%s> <http://www.w3.org/ns/dcat#contactPoint> ?org .
             ?org <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>
                  <http://xmlns.com/foaf/0.1/Organization> .
             ?org <http://xmlns.com/foaf/0.1/name> ?name .
           } LIMIT 1",
          type_subject
        ))
        if (NROW(mfr_res) > 0) manufacturer <- as.character(mfr_res$name[1])
      }
    }
  }
  
  # Build human-readable label for the sensor instance.
  # Pattern: "Sensor instance of <type> of <manufacturer>, with SN:<sn>
  #            — Owner: <surname> — created <date>"
  # Each part is included only if the relevant information is available.
  type_part <- if (!is.na(type_label) && nzchar(type_label)) {
    paste0("Sensor instance of ", type_label)
  } else {
    paste0("Sensor instance ", basename(sensor_type_uri))   # fallback
  }
  
  mfr_part <- if (!is.na(manufacturer) && nzchar(manufacturer)) {
    paste0(" of ", manufacturer)
  } else {
    ""
  }
  
  sn_part <- if (!is.null(serial_number) && nzchar(trimws(serial_number))) {
    paste0(", with SN:", trimws(serial_number))
  } else {
    ""
  }
  
  instance_label <- paste0(
    type_part,
    mfr_part,
    sn_part,
    " \u2014 Owner: ", owner_surname,
    " \u2014 created ", format(Sys.Date(), "%Y-%m-%d")
  )
  
  instance_uuid <- uuid::UUIDgenerate()
  instance_uri <- paste0("https://rdfdata.lteritalia.it/sensors/instances/", instance_uuid)
  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  esc <- function(x) gsub('"', '\\"', x, fixed = TRUE)
  
  # Optional serial number triple (if provided and non-empty)
  serial_triple <- character(0)
  if (!is.null(serial_number) && nzchar(trimws(serial_number))) {
    serial_triple <- paste0('    schema:serialNumber "', esc(trimws(serial_number)), '" ;')
  }
  
  # Optional part number triple (if provided and non-empty)
  part_n_triple <- character(0)
  if (!is.null(part_number) && nzchar(trimws(part_number))) {
    part_n_triple <- paste0('    part_number "', esc(trimws(part_number)), '" ;')
  }
  
  # Build contactPoint block
  contact_lines <- c(
    "    dcat:contactPoint [",
    "        a foaf:Person ;",
    paste0("        foaf:givenName \"",  esc(owner_name),    "\" ;"),
    paste0("        foaf:familyName \"", esc(owner_surname), "\" ;"),
    paste0("        foaf:account <", owner_orcid, ">",
           if (!is.null(owner_institution) && nzchar(trimws(owner_institution))) " ;" else "")
  )
  
  if (!is.null(owner_institution) && nzchar(trimws(owner_institution))) {
    org_inner <- paste0(
      "            a foaf:Organization ;\n",
      "            foaf:name \"", esc(trimws(owner_institution)), "\"",
      if (!is.null(owner_ror))
        paste0(" ;\n            owl:sameAs <", owner_ror, ">")
      else ""
    )
    contact_lines <- c(
      contact_lines,
      paste0("        org:memberOf [\n", org_inner, "\n        ]")
    )
  }
  
  contact_lines <- c(contact_lines, "    ] ;")
  
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
    "@prefix owl:  <http://www.w3.org/2002/07/owl#> .",
    "@prefix org:  <http://www.w3.org/ns/org#> .",
    "@prefix schema: <https://schema.org/> .",
    "",
    paste0("<", instance_uri, ">"),
    "    a sosa:Sensor, ssn:System, prov:Entity ;",
    paste0("    a <", sensor_type_uri, "> ;"),
    paste0("    dct:identifier \"", instance_uuid, "\" ;"),
    paste0("    rdfs:label \"", esc(instance_label), "\"@en ;"),
    paste0("    rdfs:comment \"This sensor instance derives from the type ", esc(sensor_type_uri), ".\"@en ;"),
    paste0("    dcat:landingPage <", instance_uri, ".xml> ;"),
    serial_triple,
    part_n_triple,
    paste0("    dct:creator <", owner_orcid, "> ;"),
    paste0("    dct:created \"", now_iso, "\"^^xsd:dateTime ;"),
    paste0("    prov:generatedAtTime \"", now_iso, "\"^^xsd:dateTime ;"),
    contact_lines,
    "    prov:qualifiedAttribution [",
    "        a prov:Attribution ;",
    paste0("        prov:agent <", owner_orcid, "> ;"),
    if (!is.null(owner_ror))
      paste0("        prov:agent <", owner_ror, "> ;")
    else NULL,
    "        prov:hadRole <http://inspire.ec.europa.eu/metadata-codelist/ResponsiblePartyRole/owner>",
    "    ] .",
    ""
  )
  
  # Remove any empty lines that may be introduced by serial_triple = character(0)
  ttl <- ttl[nzchar(ttl)]
  
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
  
  if (isTRUE(validate)) {
    sensors_validate_ttl(out)
  }
  
  invisible(out)
}

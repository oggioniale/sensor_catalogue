#' Convert SensorML sensor type XML files into RDF/Turtle representation
#' @description
#' This function reads a local directory containing SensorML XML descriptions
#' of a sensor *type* (and its possible component types), extracts relevant
#' metadata such as identifiers, capabilities, manufacturer, observed
#' properties, and relationships, and generates corresponding RDF/Turtle (`.ttl`)
#' files following the SOSA/SSN ontology and schema.org properties.
#' @details
#' The function processes SensorML files that follows the OGC SensorML 2.0 specification.  
#' It identifies whether the XML describes a sensor *type* (i.e., a reusable
#' system description rather than an instance) by checking the value of
#' `swes:extension/swe:Boolean/swe:value`.  
#' If the file is confirmed as a type, the function:
#' \itemize{
#'   \item Parses the main system SensorML file under `system/`;
#'   \item Extracts identifiers, descriptions, manufacturer info, and capabilities;
#'   \item Detects and parses all component SensorML files under `components/`;
#'   \item Writes one `.ttl` file for the system type and one for each component type;
#'   \item Serializes RDF triples using SOSA, SSN, PROV, DCAT, FOAF, and Schema.org vocabularies.
#' }
#' The resulting TTL files are stored in a folder named `system_components_files_ttl`
#' within the input path.
#' @param files_path Character string. The path to the local directory containing
#' the downloaded SensorML XML files for a sensor system type and its components.
#' The directory must include at least one file under `system/` and optionally several
#' under `components/`.
#' @param creator_name Character. First name of the sensor owner.
#' @param creator_surname Character. Last name of the sensor owner.
#' @param creator_orcid Character. ORCID of the sensor owner (e.g. `"0000-0002-1825-0097"`).
#' @return
#' The function produces one or more RDF/Turtle files in the subdirectory
#' `system_components_files_ttl/`. It does not return an object in R.
#' If the provided XML does not represent a sensor type, the function stops
#' with an informative message.
#' @note
#' The generated RDF files can be uploaded to a Fuseki triple store
#' (e.g. dataset `/systemsType`) using a subsequent script or API call.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text xml_attr
#' @importFrom stringr str_replace word str_replace_all
#' @importFrom tools file_path_sans_ext
#' @importFrom utils write.table
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#' sensors_type_rdf(
#'   files_path = "Sensors_files_system_29347abd-69da-49e5-8a22-94a45674c775/",
#'   creator_name = "John",
#'   creator_surname = "Doe",
#'   creator_orcid = "0000-0002-1234-5678"
#' )
#' }
#'
### function sensors_type_rdf
sensors_type_rdf <- function(
    files_path = NULL,
    creator_name = NULL,
    creator_surname = NULL,
    creator_orcid = NULL
    ) {
  # load the file ---
  file_name <- tools::file_path_sans_ext(
    list.files(
      paste0(files_path, "system/")
    )
  )
  setwd(files_path)
  xml <- xml2::read_xml(
    paste0(
      "system/",
      file_name,
      ".xml"
    )
  )
  dir_name <- paste0(
    getwd(),
    "/system_components_files_ttl"
  )
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  # --- Default metadata for the creator and contact point ---
  if (is.null(creator_name))     creator_name    <- "Alessandro"
  if (is.null(creator_surname))  creator_surname <- "Oggioni"
  if (is.null(creator_orcid))    creator_orcid   <- "https://orcid.org/0000-0002-7997-219X"
  # ORCID normalization
  if (!startsWith(creator_orcid, "https://orcid.org/"))
    creator_orcid <- paste0("https://orcid.org/", creator_orcid)
  # sensors
  sensorTypeTTL <- file(paste0(dir_name, "/", file_name, ".ttl"))
  # check if is type ---
  is_type <- xml2::xml_text(
    xml2::xml_find_all(
      xml,
      ".//swes:extension/swe:Boolean/swe:value/text()"
    )
  )
  if (is_type == "true") {
    # list of the variables for TTL ---
    n_components <- length(xml2::xml_find_all(xml, ".//sml:component"))
    is_system <- xml2::xml_find_first(xml, ".//sml:PhysicalSystem") # remove because now the folders components and system distinguish the type of sensors
    is_component <- xml2::xml_find_first(xml, ".//sml:PhysicalComponent") # remove because now the folders components and system distinguish the type of sensors
    # identifier
    identifier <- xml2::xml_find_all(xml, ".//gml:identifier/text()")
    # capabilities
    has_capability <- xml2::xml_find_first(xml, ".//sml:capabilities/@name='capabilities'")
    has_accuracy <- xml2::xml_find_first(xml, ".//sml:capability/@name='Accuracy'")
    has_measurement_range <- xml2::xml_find_first(xml, ".//sml:capability/@name='MeasurementRange'")
    has_precision <- xml2::xml_find_first(xml, ".//sml:capability/@name='Precision'")
    has_resolution <- xml2::xml_find_first(xml, ".//sml:capability/@name='Resolution'")
    has_sensitivity <- xml2::xml_find_first(xml, ".//sml:capability/@name='Sensitivity'")
    has_frequency <- xml2::xml_find_first(xml, ".//sml:capability/@name='MinimumReportingFrequency'")
    # have system components?
    # System ---
    # other elements
    description <- xml2::xml_find_all(xml, ".//gml:description/text()")
    name <- xml2::xml_find_all(xml, ".//gml:name/text()")
    documentLink <- xml2::xml_find_all(
      xml,
      ".//sml:documentation[@xlink:arcrole='datasheet']/sml:DocumentList/sml:document/gmd:CI_OnlineResource/gmd:linkage/gmd:URL/text()"
    )
    imageLink <- xml2::xml_find_all(
      xml,
      ".//sml:documentation[@xlink:arcrole='image']/sml:DocumentList/sml:document/gmd:CI_OnlineResource/gmd:linkage/gmd:URL/text()"
    )
    manufacturerLink <- xml2::xml_attr(
      xml2::xml_find_all(
        xml, ".//sml:contacts/sml:ContactList/sml:contact"
      ),
      "href"
    )
    manufacturerName <- xml2::xml_find_all(
      xml, ".//sml:identifier/sml:Term[@definition='http://vocab.nerc.ac.uk/collection/W07/current/IDEN0012/']/sml:value/text()"
    )
    if (length(manufacturerName) != 0) {
      manufacturer <- paste0("       dcat:contactPoint      [ rdf:type       prov:Organization , foaf:Organization ;",
                             "                                rdfs:seeAlso   <", manufacturerLink, "> ;",
                             '                                foaf:name      """', manufacturerName, '"""] ;')
    } else {
      manufacturer <- ""
    }
    # following this example: https://www.w3.org/TR/vocab-ssn/#dht22-deployment
    sensor_type <- "       rdf:type               sosa:System , ssn:System , prov:Entity ;"
    # observable properties
    obsPropURI <- as.character(xml2::xml_find_all(
      xml, ".//swes:observableProperty/text()"
    ))
    obsPropList <- paste("<", obsPropURI, ">", collapse = ", ", sep = "")
    # Combine full name for display
    creator_fullname <- paste(creator_name, creator_surname)
    # --- Provenance RDF blocks ---
    creator_block <- paste0('       dct:creator         <', creator_orcid, '> ;')
    contact_block <- paste0(
      '       dcat:contactPoint [ ',
      'a foaf:Person ; ',
      'foaf:givenName """', creator_name, '""" ; ',
      'foaf:familyName """', creator_surname, '""" ; ',
      'foaf:name """', creator_fullname, '""" ; ',
      '] ;'
    )
    # dct:created metadata for the SYSTEM
    now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    created_block <- paste0('       dct:created         "', now_iso, '"^^xsd:dateTime ;')
    observedProperties <- paste0("       sosa:observes             ", obsPropList,  " .")  # observed property
    # derived from
    uuid <- stringr::str_replace(as.character(identifier), "^.+/", "")
    if (length(documentLink) != 0) {
      derivedFrom <- paste0("       prov:wasDerivedFrom    <", documentLink,"> ;") # sml:documentation xlink:arcrole="datasheet"
    } else {
      derivedFrom <- ""
    }
    # image
    if (length(imageLink) != 0) {
      imageFrom <- paste0("       dcmitype:Image    <", imageLink,"> ;") # sml:documentation xlink:arcrole="datasheet"
    } else {
      imageFrom <- ""
    }
    # sameAs
    sameAs <- paste0("       rdfs:seeAlso             <", identifier, "> ;")
    # RDF_identifier
    RDF_identifier <- paste0("<https://rdfdata.lteritalia.it/sensors/systemsType/", uuid, ">") #TODO add model name and manufacturer name (e.g. https://rdfdata.lteritalia.it/sensors/systemsType/Vantage/Davis_Instruments/uuid)
    components <- ""
    attached <- ""
    # Which capabilities are in the system?
    if (has_capability&(has_accuracy|has_measurement_range|has_precision|has_resolution|has_sensitivity|has_frequency)) {
      sensor_capability <- paste0("<https://rdfdata.lteritalia.it/sensors/systemsType/", uuid, "#SensorCapability>")
      if (has_accuracy) {
        accuracy_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='Accuracy']")
        has_quantity_or_text <- xml2::xml_find_first(accuracy_xml, ".//swe:Quantity")
        has_quantity_range <- xml2::xml_find_first(accuracy_xml, ".//swe:QuantityRange")
        accuracy_uri <- paste0("<", identifier, "#SensorAccuracy>")
        if (length(has_quantity_or_text) > 0) {
          accuracy_uom_uri <- xml2::xml_attr(xml2::xml_find_first(accuracy_xml, ".//swe:uom"), "href")
          accuracy_value <- xml2::xml_text(accuracy_xml, ".//swe:value")
          accuracy_value_min <- ""
          accuracy_value_max <- ""
          accuracy_schema <- c(
            paste0("schema:value ", accuracy_value, " ;"),
            paste0("schema:unitCode <", accuracy_uom_uri, "> .")
          )
        } else if (length(has_quantity_range) > 0) {
          accuracy_uom_uri <- xml2::xml_attr(xml2::xml_find_first(accuracy_xml, ".//swe:uom"), "href")
          accuracy_value <- ""
          accuracy_value_min <- stringr::word(xml2::xml_text(accuracy_xml, ".//swe:value"), 1)
          accuracy_value_max <- stringr::word(xml2::xml_text(accuracy_xml, ".//swe:value"), 2)
          accuracy_schema <- c(
            paste0("schema:minValue ", accuracy_value_min, " ;"),
            paste0("schema:maxValue ", accuracy_value_max, " ;"),
            paste0("schema:unitCode <", accuracy_uom_uri, "> .")
          )
        }
        accuracy_description <- c(
          paste0(accuracy_uri, " a ssn:Property , ssn-system:Accuracy , schema:PropertyValue ;"),
          accuracy_schema,
          ""
        )
      } else {
        accuracy_uri <- ""
        accuracy_description <- ""
      }
      if (has_measurement_range) {
        measurement_range_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='MeasurementRange']")
        measurement_range_uom_uri <- xml2::xml_attr(xml2::xml_find_first(measurement_range_xml, ".//swe:uom"), "href")
        measurement_range_value_min <- stringr::word(xml2::xml_text(measurement_range_xml, ".//swe:value"), 1)
        measurement_range_value_max <- stringr::word(xml2::xml_text(measurement_range_xml, ".//swe:value"), 2)
        measurement_range_uri <- paste0("<", identifier, "#SensorMeasurementRange>")
        measurement_range_schema <- c(
          paste0("schema:minValue ", measurement_range_value_min, " ;"),
          paste0("schema:maxValue ", measurement_range_value_max, " ;"),
          paste0("schema:unitCode <", measurement_range_uom_uri, "> .")
        )
        measurement_range_description <- c(
          paste0(measurement_range_uri, " a ssn:Property , ssn-system:MeasurementRange , schema:PropertyValue ;"),
          measurement_range_schema,
          ""
        )
      } else {
        measurement_range_uom_uri <- ""
        measurement_range_value_min <- ""
        measurement_range_value_max <- ""
        measurement_range_uri <- ""
        measurement_range_description <- ""
      }
      if (has_precision) {
        precision_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='Precision']")
        precision_uom_uri <- xml2::xml_attr(xml2::xml_find_first(precision_xml, ".//swe:uom"), "href")
        precision_value <- xml2::xml_text(precision_xml, ".//swe:value")
        precision_uri <- paste0("<", identifier, "#SensorPrecision>")
        precision_schema <- c(
          paste0("schema:value ", precision_value, " ;"),
          paste0("schema:unitCode <", precision_uom_uri, "> .")
        )
        precision_description <- c(
          paste0(precision_uri, " a ssn:Property , ssn-system:Precision , schema:PropertyValue ;"),
          precision_schema,
          ""
        )
      } else {
        precision_uom_uri <- ""
        precision_value <- ""
        precision_uri <- ""
        precision_description <- ""
      }
      if (has_resolution) {
        resolution_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='Resolution']")
        has_quantity_or_text <- xml2::xml_find_first(resolution_xml, ".//swe:Quantity")
        has_quantity_range <- xml2::xml_find_first(resolution_xml, ".//swe:QuantityRange")
        resolution_uri <- paste0("<", identifier, "#SensorResolution>")
        if (length(has_quantity_or_text) > 0) {
          resolution_uom_uri <- xml2::xml_attr(xml2::xml_find_first(resolution_xml, ".//swe:uom"), "href")
          resolution_value <- xml2::xml_text(resolution_xml, ".//swe:value")
          resolution_value_min <- ""
          resolution_value_max <- ""
          resolution_schema <- c(
            paste0("schema:value ", resolution_value, " ;"),
            paste0("schema:unitCode <", resolution_uom_uri, "> .")
          )
        } else if (length(has_quantity_range) > 0) {
          resolution_uom_uri <- xml2::xml_attr(xml2::xml_find_first(resolution_xml, ".//swe:uom"), "href")
          resolution_value <- ""
          resolution_value_min <- stringr::word(xml2::xml_text(resolution_xml, ".//swe:value"), 1)
          resolution_value_max <- stringr::word(xml2::xml_text(resolution_xml, ".//swe:value"), 2)
          resolution_schema <- c(
            paste0("schema:minValue ", resolution_value_min, " ;"),
            paste0("schema:maxValue ", resolution_value_max, " ;"),
            paste0("schema:unitCode <", resolution_uom_uri, "> .")
          )
        }
        resolution_description <- c(
          paste0(resolution_uri, " a ssn:Property , ssn-system:Resolution , schema:PropertyValue ;"),
          resolution_schema,
          ""
        )
      } else {
        resolution_uri <- ""
        resolution_description <- ""
      }
      if (has_sensitivity) {
        sensitivity_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='Sensitivity']")
        sensitivity_uom_uri <- xml2::xml_attr(xml2::xml_find_first(sensitivity_xml, ".//swe:uom"), "href")
        sensitivity_value <- xml2::xml_text(sensitivity_xml, ".//swe:value")
        sensitivity_uri <- paste0("<", identifier, "#SensorSensitivity>")
        sensitivity_schema <- c(
          paste0("schema:value ", sensitivity_value, " ;"),
          paste0("schema:unitCode <", sensitivity_uom_uri, "> .")
        )
        sensitivity_description <- c(
          paste0(sensitivity_uri, " a ssn:Property , ssn-system:Sensitivity , schema:PropertyValue ;"),
          sensitivity_schema,
          ""
        )
      } else {
        sensitivity_uom_uri <- ""
        sensitivity_value <- ""
        sensitivity_uri <- ""
        sensitivity_description <- ""
      }
      if (has_frequency) {
        frequency_xml <- xml2::xml_find_first(xml, ".//sml:capability[@name='MinimumReportingFrequency']")
        has_quantity_or_text <- xml2::xml_find_first(frequency_xml, ".//swe:Quantity")
        has_quantity_range <- xml2::xml_find_first(frequency_xml, ".//swe:QuantityRange")
        frequency_uri <- paste0("<", identifier, "#SensorFrequency>")
        if (length(has_quantity_or_text) > 0) {
          frequency_uom_uri <- xml2::xml_attr(xml2::xml_find_first(frequency_xml, ".//swe:uom"), "href")
          frequency_value <- xml2::xml_text(frequency_xml, ".//swe:value")
          frequency_value_min <- ""
          frequency_value_max <- ""
          frequency_schema <- c(
            paste0("schema:value ", frequency_value, " ;"),
            paste0("schema:unitCode <", frequency_uom_uri, "> .")
          )
        } else if (length(has_quantity_range) > 0) {
          frequency_uom_uri <- xml2::xml_attr(xml2::xml_find_first(frequency_xml, ".//swe:uom"), "href")
          frequency_value <- ""
          frequency_value_min <- stringr::word(xml2::xml_text(frequency_xml, ".//swe:value"), 1)
          frequency_value_max <- stringr::word(xml2::xml_text(frequency_xml, ".//swe:value"), 2)
          frequency_schema <- c(
            paste0("schema:minValue ", frequency_value_min, " ;"),
            paste0("schema:maxValue ", frequency_value_max, " ;"),
            paste0("schema:unitCode <", frequency_uom_uri, "> .")
          )
        }
        frequency_description <- c(
          paste0(frequency_uri, " a ssn:Property , ssn-system:Frequency , schema:PropertyValue ;"),
          frequency_schema,
          ""
        )
      } else {
        frequency_uri <- ""
        frequency_description <- ""
      }
      sensor_capability_01 <- paste0("       ssn-system:hasSystemCapability ", sensor_capability, " ;")
      sensor_capability_02 <- c(
        paste0(sensor_capability, " a ssn:Property , ssn-system:SystemCapability , schema:PropertyValue ;"),
        gsub("([,&\\s])\\1+", "\\1",
             stringr::str_replace_all(string = paste0(
               "ssn-system:hasSystemProperty ",
               paste(accuracy_uri, measurement_range_uri, precision_uri, resolution_uri, sensitivity_uri, frequency_uri),
               " ."
             ), pattern = ">\\s+<", replacement = ">,<"),
             perl = TRUE
        ),
        "",
        accuracy_description,
        measurement_range_description,
        precision_description,
        resolution_description,
        sensitivity_description,
        frequency_description
      )
    } else {
      sensor_capability_01 <- ""
      sensor_capability_02 <- ""
    }
    if (n_components > 0) {
      # write components ttl files ---
      components_files <- tools::file_path_sans_ext(
        list.files(
          paste0(getwd(), "/components/")
        )
      )
      for (i in 1:length(components_files)) {
        comp_ttl_prefixes <- c(
          "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
          "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
          "@prefix sosa: <http://www.w3.org/ns/sosa/> .",
          "@prefix ssn: <http://www.w3.org/ns/ssn/> .",
          "@prefix ssn-system: <http://www.w3.org/ns/ssn/systems/> .",
          "@prefix foaf: <http://xmlns.com/foaf/0.1/> .",
          "@prefix owl: <http://www.w3.org/2002/07/owl#> .",
          "@prefix dcat: <http://www.w3.org/ns/dcat#> .",
          "@prefix prov: <http://www.w3.org/ns/prov#> .",
          "@prefix schema: <http://schema.org/> .",
          "@prefix dcmitype: <http://purl.org/dc/dcmitype/> .",
          "@prefix dct: <http://purl.org/dc/terms/> .",
          "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
          ""
        )
        comp_xml <- xml2::read_xml(
          paste0(
            getwd(),
            "/components/",
            components_files[i],
            ".xml"
          )
        )
        componentTypeTTL <- file(paste0(dir_name, "/", components_files[i], ".ttl"))
        comp_id <- stringr::str_replace(as.character(
          xml2::xml_find_all(comp_xml, ".//gml:identifier/text()")),
          "^.+/", "")
        RDF_comp_uri <- paste0("https://rdfdata.lteritalia.it/sensors/componentsType/", comp_id)
        comp_type <- "       rdf:type               sosa:Sensor , ssn:System , prov:Entity ;"
        comp_sameAs <- paste0("       rdfs:seeAlso             <", xml2::xml_find_all(comp_xml, ".//gml:identifier/text()"), "> ;")
        comp_description <- xml2::xml_find_all(comp_xml, ".//gml:description/text()")
        comp_name <- xml2::xml_find_all(comp_xml, ".//gml:name/text()")
        comp_attached <- paste0(
              "       sosa:isHostedBy             ", RDF_identifier, " ;")
        # --- dct:created metadata for the COMPONENT ---
        comp_now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        comp_created_block <- paste0('       dct:created         "', comp_now_iso, '"^^xsd:dateTime ;')
        comp_obsProps <- as.character(xml2::xml_find_all(
          comp_xml, ".//swes:observableProperty/text()"
        ))
        comp_has_capability <- xml2::xml_find_first(comp_xml, ".//sml:capabilities/@name='capabilities'")
        comp_has_accuracy <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='Accuracy'")
        comp_has_measurement_range <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='MeasurementRange'")
        comp_has_precision <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='Precision'")
        comp_has_resolution <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='Resolution'")
        comp_has_sensitivity <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='Sensitivity'")
        comp_has_frequency <- xml2::xml_find_first(comp_xml, ".//sml:capability/@name='MinimumReportingFrequency'")
        if (comp_has_capability&(comp_has_accuracy|comp_has_measurement_range|comp_has_precision|comp_has_resolution|comp_has_sensitivity|comp_has_frequency)) {
          comp_capability <- paste0("<", RDF_comp_uri, "#SensorCapability>")
          if (comp_has_accuracy) {
            comp_accuracy_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='Accuracy']")
            comp_has_quantity_or_text <- xml2::xml_find_first(comp_accuracy_xml, ".//swe:Quantity")
            comp_has_quantity_range <- xml2::xml_find_first(comp_accuracy_xml, ".//swe:QuantityRange")
            comp_accuracy_uri <- paste0("<", RDF_comp_uri, "#SensorAccuracy>")
            if (length(comp_has_quantity_or_text) > 0) {
              comp_accuracy_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_accuracy_xml, ".//swe:uom"), "href")
              comp_accuracy_value <- xml2::xml_text(comp_accuracy_xml, ".//swe:value")
              comp_accuracy_value_min <- ""
              comp_accuracy_value_max <- ""
              comp_accuracy_schema <- c(
                paste0("schema:value ", comp_accuracy_value, " ;"),
                paste0("schema:unitCode <", comp_accuracy_uom_uri, "> .")
              )
            } else if (length(comp_has_quantity_range) > 0) {
              comp_accuracy_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_accuracy_xml, ".//swe:uom"), "href")
              comp_accuracy_value <- ""
              comp_accuracy_value_min <- stringr::word(xml2::xml_text(comp_accuracy_xml, ".//swe:value"), 1)
              comp_accuracy_value_max <- stringr::word(xml2::xml_text(comp_accuracy_xml, ".//swe:value"), 2)
              comp_accuracy_schema <- c(
                paste0("schema:minValue ", comp_accuracy_value_min, " ;"),
                paste0("schema:maxValue ", comp_accuracy_value_max, " ;"),
                paste0("schema:unitCode <", comp_accuracy_uom_uri, "> .")
              )
            }
            comp_accuracy_description <- c(
              paste0(comp_accuracy_uri, " a ssn:Property , ssn-system:Accuracy , schema:PropertyValue ;"),
              comp_accuracy_schema,
              ""
            )
          } else {
            comp_accuracy_uri <- ""
            comp_accuracy_description <- ""
          }
          if (comp_has_measurement_range) {
            comp_measurement_range_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='MeasurementRange']")
            comp_measurement_range_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_measurement_range_xml, ".//swe:uom"), "href")
            comp_measurement_range_value_min <- stringr::word(xml2::xml_text(comp_measurement_range_xml, ".//swe:value"), 1)
            comp_measurement_range_value_max <- stringr::word(xml2::xml_text(comp_measurement_range_xml, ".//swe:value"), 2)
            comp_measurement_range_uri <- paste0("<", RDF_comp_uri, "#SensorMeasurementRange>")
            comp_measurement_range_schema <- c(
              paste0("schema:minValue ", comp_measurement_range_value_min, " ;"),
              paste0("schema:maxValue ", comp_measurement_range_value_max, " ;"),
              paste0("schema:unitCode <", comp_measurement_range_uom_uri, "> .")
            )
            comp_measurement_range_description <- c(
              paste0(comp_measurement_range_uri, " a ssn:Property , ssn-system:MeasurementRange , schema:PropertyValue ;"),
              comp_measurement_range_schema,
              ""
            )
          } else {
            comp_measurement_range_uom_uri <- ""
            comp_measurement_range_value_min <- ""
            comp_measurement_range_value_max <- ""
            comp_measurement_range_uri <- ""
            comp_measurement_range_description <- ""
          }
          if (comp_has_precision) {
            comp_precision_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='Precision']")
            comp_precision_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_precision_xml, ".//swe:uom"), "href")
            comp_precision_value <- xml2::xml_text(comp_precision_xml, ".//swe:value")
            comp_precision_uri <- paste0("<", RDF_comp_uri, "#SensorPrecision>")
            comp_precision_schema <- c(
              paste0("schema:value ", comp_precision_value, " ;"),
              paste0("schema:unitCode <", comp_precision_uom_uri, "> .")
            )
            comp_precision_description <- c(
              paste0(comp_precision_uri, " a ssn:Property , ssn-system:Precision , schema:PropertyValue ;"),
              comp_precision_schema,
              ""
            )
          } else {
            comp_precision_uom_uri <- ""
            comp_precision_value <- ""
            comp_precision_uri <- ""
            comp_precision_description <- ""
          }
          if (comp_has_resolution) {
            comp_resolution_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='Resolution']")
            comp_has_quantity_or_text <- xml2::xml_find_first(comp_resolution_xml, ".//swe:Quantity")
            comp_has_quantity_range <- xml2::xml_find_first(comp_resolution_xml, ".//swe:QuantityRange")
            comp_resolution_uri <- paste0("<", RDF_comp_uri, "#SensorResolution>")
            if (length(comp_has_quantity_or_text) > 0) {
              comp_resolution_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_resolution_xml, ".//swe:uom"), "href")
              comp_resolution_value <- xml2::xml_text(comp_resolution_xml, ".//swe:value")
              comp_resolution_value_min <- ""
              comp_resolution_value_max <- ""
              comp_resolution_schema <- c(
                paste0("schema:value ", comp_resolution_value, " ;"),
                paste0("schema:unitCode <", comp_resolution_uom_uri, "> .")
              )
            } else if (length(comp_has_quantity_range) > 0) {
              comp_resolution_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_resolution_xml, ".//swe:uom"), "href")
              comp_resolution_value <- ""
              comp_resolution_value_min <- stringr::word(xml2::xml_text(comp_resolution_xml, ".//swe:value"), 1)
              comp_resolution_value_max <- stringr::word(xml2::xml_text(comp_resolution_xml, ".//swe:value"), 2)
              comp_resolution_schema <- c(
                paste0("schema:minValue ", comp_resolution_value_min, " ;"),
                paste0("schema:maxValue ", comp_resolution_value_max, " ;"),
                paste0("schema:unitCode <", comp_resolution_uom_uri, "> .")
              )
            }
            comp_resolution_description <- c(
              paste0(comp_resolution_uri, " a ssn:Property , ssn-system:Resolution , schema:PropertyValue ;"),
              comp_resolution_schema,
              ""
            )
          } else {
            comp_resolution_uri <- ""
            comp_resolution_description <- ""
          }
          if (comp_has_sensitivity) {
            comp_sensitivity_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='Sensitivity']")
            comp_sensitivity_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_sensitivity_xml, ".//swe:uom"), "href")
            comp_sensitivity_value <- xml2::xml_text(comp_sensitivity_xml, ".//swe:value")
            comp_sensitivity_uri <- paste0("<", RDF_comp_uri, "#SensorSensitivity>")
            comp_sensitivity_schema <- c(
              paste0("schema:value ", comp_sensitivity_value, " ;"),
              paste0("schema:unitCode <", comp_sensitivity_uom_uri, "> .")
            )
            comp_sensitivity_description <- c(
              paste0(comp_sensitivity_uri, " a ssn:Property , ssn-system:Sensitivity , schema:PropertyValue ;"),
              comp_sensitivity_schema,
              ""
            )
          } else {
            comp_sensitivity_uom_uri <- ""
            comp_sensitivity_value <- ""
            comp_sensitivity_uri <- ""
            comp_sensitivity_description <- ""
          }
          if (comp_has_frequency) {
            comp_frequency_xml <- xml2::xml_find_first(comp_xml, ".//sml:capability[@name='MinimumReportingFrequency']")
            comp_has_quantity_or_text <- xml2::xml_find_first(comp_frequency_xml, ".//swe:Quantity")
            comp_has_quantity_range <- xml2::xml_find_first(comp_frequency_xml, ".//swe:QuantityRange")
            comp_frequency_uri <- paste0("<", RDF_comp_uri, "#SensorFrequency>")
            if (length(comp_has_quantity_or_text) > 0) {
              comp_frequency_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_frequency_xml, ".//swe:uom"), "href")
              comp_frequency_value <- xml2::xml_text(comp_frequency_xml, ".//swe:value")
              comp_frequency_value_min <- ""
              comp_frequency_value_max <- ""
              comp_frequency_schema <- c(
                paste0("schema:value ", comp_frequency_value, " ;"),
                paste0("schema:unitCode <", comp_frequency_uom_uri, "> .")
              )
            } else if (length(has_quantity_range) > 0) {
              comp_frequency_uom_uri <- xml2::xml_attr(xml2::xml_find_first(comp_frequency_xml, ".//swe:uom"), "href")
              comp_frequency_value <- ""
              comp_frequency_value_min <- stringr::word(xml2::xml_text(comp_frequency_xml, ".//swe:value"), 1)
              comp_frequency_value_max <- stringr::word(xml2::xml_text(comp_frequency_xml, ".//swe:value"), 2)
              comp_frequency_schema <- c(
                paste0("schema:minValue ", comp_frequency_value_min, " ;"),
                paste0("schema:maxValue ", comp_frequency_value_max, " ;"),
                paste0("schema:unitCode <", comp_frequency_uom_uri, "> .")
              )
            }
            comp_frequency_description <- c(
              paste0(comp_frequency_uri, " a ssn:Property , ssn-system:Frequency , schema:PropertyValue ;"),
              comp_frequency_schema,
              ""
            )
          } else {
            comp_frequency_uri <- ""
            comp_frequency_description <- ""
          }
          comp_sensor_capability_01 <- paste0("       ssn-system:hasSystemCapability ", comp_capability, " .")
          comp_sensor_capability_02 <- c(
            paste0(comp_capability, " a ssn:Property , ssn-system:SystemCapability , schema:PropertyValue ;"),
            gsub("([,&\\s])\\1+", "\\1",
                 stringr::str_replace_all(string = paste0(
                   "ssn-system:hasSystemProperty ",
                   paste(comp_accuracy_uri, comp_measurement_range_uri,
                         comp_precision_uri, comp_resolution_uri,
                         comp_sensitivity_uri, comp_frequency_uri),
                   " ."
                 ), pattern = ">\\s+<", replacement = ">,<"),
                 perl = TRUE
            ),
            "",
            comp_accuracy_description,
            comp_measurement_range_description,
            comp_precision_description,
            comp_resolution_description,
            comp_sensitivity_description,
            comp_frequency_description
          )
        } else {
          comp_sensor_capability_01 <- ""
          comp_sensor_capability_02 <- ""
        }
        # close component file ttl ---
        component <- c(
          comp_ttl_prefixes,
          paste0("<", RDF_comp_uri, ">"),
          comp_type,
          comp_sameAs, # gml:identifier
          paste0('       rdfs:comment           """', comp_description, '""" @en ;'), # gml:description
          paste0('       rdfs:label             """', comp_name, '""" @en ;'), # gml:name
          comp_attached,
          comp_created_block,
          creator_block,
          contact_block,
          paste0(
            "       sosa:observes             <", comp_obsProps, "> ;"),
          comp_sensor_capability_01,
          "",
          comp_sensor_capability_02
        )
        write(component, file = componentTypeTTL)
        # close file
        close(componentTypeTTL)
      }
      # are the components in system
      # capabilities are in each components
      comp_sensor_capability_01 <- ""
      comp_sensor_capability_02 <- ""
      components_uri <- xml2::xml_attr(
        xml2::xml_find_all(xml, ".//sml:component"),
        "href"
      )
      components_uuid <- stringr::str_replace(as.character(components_uri), "^.+/", "")
      components <- paste0(
        "       sosa:hasSubSystem             ",
        paste(
          "<",
          paste0(
            "https://rdfdata.lteritalia.it/sensors/componentsType/", #TODO add model name and manufacturer name (e.g. https://rdfdata.lteritalia.it/sensors/componentsType/Vantage/Davis_Instruments/uuid)
            sub('.*\\/', '', components_uuid)
          ),
          ">",
          sep = "",
          collapse = ", "
        ),
        " ;"
      )
      attached <- ""
    }
    # contents ---
    ttl_prefixes <- c(
      "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .",
      "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .",
      "@prefix sosa: <http://www.w3.org/ns/sosa/> .",
      "@prefix ssn: <http://www.w3.org/ns/ssn/> .",
      "@prefix ssn-system: <http://www.w3.org/ns/ssn/systems/> .",
      "@prefix foaf: <http://xmlns.com/foaf/0.1/> .",
      "@prefix owl: <http://www.w3.org/2002/07/owl#> .",
      "@prefix dcat: <http://www.w3.org/ns/dcat#> .",
      "@prefix prov: <http://www.w3.org/ns/prov#> .",
      "@prefix schema: <http://schema.org/> .",
      "@prefix dcmitype: <http://purl.org/dc/dcmitype/> .",
      "@prefix dct: <http://purl.org/dc/terms/> .",
      "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .",
      ""
    )
    sensor <- c(
      ttl_prefixes,
      RDF_identifier,
      sensor_type,
      sameAs, # gml:identifier
      paste0('       rdfs:comment           """', description, '""" @en ;'), # gml:description
      paste0('       rdfs:label             """', name, '""" @en ;'), # gml:name
      manufacturer,
      components,
      attached,
      sensor_capability_01,
      derivedFrom,
      imageFrom,
      created_block,
      creator_block,
      contact_block,
      observedProperties,
      "",
      sensor_capability_02,
      ""
    )
    write(sensor, file = sensorTypeTTL)
    # close file
    close(sensorTypeTTL)
    # --- RDF validation (optional) ---
    if (requireNamespace("rdflib", quietly = TRUE)) {
      tryCatch({
        ttl_files <- list.files(dir_name, pattern = "\\.ttl$", full.names = TRUE)
        message("\n--- Validating generated TTL files ---")
        
        # run validation for each TTL
        validation_results <- lapply(ttl_files, sensors_validate_ttl)
        
        # aggregate results
        validation_summary <- dplyr::bind_rows(validation_results)
        
        message("\n✅ RDF validation summary:")
        print(validation_summary)
        
        # optional: stop if any file missing critical predicates
        if (any(validation_summary$missing_predicates != "None")) {
          message("⚠️ Some TTL files have missing predicates, please check the output above.")
        } else {
          message("✅ All TTL files passed validation.")
        }
      },
      error = function(e) {
        message("⚠️ Validation skipped due to error: ", e$message)
      })
    } else {
      message("⚠️ Package 'rdflib' not installed. TTL validation skipped.")
    }
  } else {
    stop("\n----\nThe XML provided is not a sensor type.\nPlease change the file.\n----\n")
  }
  setwd("..")
}

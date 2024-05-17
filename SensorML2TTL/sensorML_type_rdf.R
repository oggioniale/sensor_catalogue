#' @title
#' @description
#' A short description...
#' 
#' @param files_path description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @export
#' @example
#'  \dontrun{
#' sensorML_type_rdf(files_path = "sensorML_files_system_4ce8484c-b9e1-11ee-98e3-daf69f6cfb8a/")
#' sensorML_type_rdf(files_path = "sensorML_files_system_9db904a0-0ddd-11ef-bbea-daf69f6cfb8b/")
#' sensorML_type_rdf(files_path = "sensorML_files_system_9e817e58-0ddd-11ef-bbea-daf69f6cfb8b/")
#' }
#'
### function sensorML_type_rdf
sensorML_type_rdf <- function(files_path = NULL) {
  # load the file ----
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
  sensorTypeTTL <- file(paste0(dir_name, "/", file_name, ".ttl"))
  # check if is type ----
  is_type <- xml2::xml_text(
    xml2::xml_find_all(
      xml,
      ".//swes:extension/swe:Boolean/swe:value/text()"
    )
  )
  if (is_type == "true") {
    # list of the variables for TTL ----
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
    # System ----
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
                             "                                foaf:name      '", manufacturerName, "'] ;")
    } else {
      manufacturer <- ""
    }
    # following this example: https://www.w3.org/TR/vocab-ssn/#dht22-deployment
    sensor_type <- "       rdf:type               sosa:Platform , ssn:System , prov:Entity ;"
    # observable properties
    obsPropURI <- as.character(xml2::xml_find_all(
      xml, ".//swes:observableProperty/text()"
    ))
    obsPropList <- paste("<", obsPropURI, ">", collapse = ", ", sep = "")
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
    sameAs <- paste0("       owl:sameAs             <", identifier, "> ;")
    # RDF_identifier
    RDF_identifier <- paste0("<http://rdfdata.lteritalia.it/sensors/systemsType/", uuid, ">") #TODO add model name and manufacturer name (e.g. http://rdfdata.lteritalia.it/sensors/systemsType/Vantage/Davis_Instruments/uuid)
    components <- ""
    attached <- ""
    # Which capabilities are in the system?
    if (has_capability&(has_accuracy|has_measurement_range|has_precision|has_resolution|has_sensitivity|has_frequency)) {
      sensor_capability <- paste0("<http://rdfdata.lteritalia.it/sensors/systemsType/", uuid, "#SensorCapability>")
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
      # write components ttl files ----
      components_files <- tools::file_path_sans_ext(
        list.files(
          paste0(getwd(), "/components/")
        )
      )
      for (i in 1:length(components_files)) {
        component <- '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        @prefix sosa: <http://www.w3.org/ns/sosa/>
        @prefix ssn: <http://www.w3.org/ns/ssn/>
        @prefix ssn-system: <http://www.w3.org/ns/ssn/systems/>
        @prefix foaf: <http://xmlns.com/foaf/0.1/>
        @prefix owl: <http://www.w3.org/2002/07/owl#>
        @prefix dcat: <http://www.w3.org/ns/dcat#>
        @prefix prov: <http://www.w3.org/ns/prov#>
        @prefix schema: <http://schema.org/>
        @prefix dcmitype: <http://purl.org/dc/dcmitype/>'
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
        RDF_comp_uri <- paste0("http://rdfdata.lteritalia.it/sensors/componentsType/", comp_id) #TODO add model name and manufacturer name (e.g. http://rdfdata.lteritalia.it/sensors/componentsType/Vantage/Davis_Instruments/uuid)
        comp_type <- "       rdf:type               sosa:Sensor , ssn:System , prov:Entity ;"
        comp_sameAs <- paste0("       owl:sameAs             <", xml2::xml_find_all(comp_xml, ".//gml:identifier/text()"), "> ;")
        comp_description <- xml2::xml_find_all(comp_xml, ".//gml:description/text()")
        comp_name <- xml2::xml_find_all(comp_xml, ".//gml:name/text()")
        comp_attached <- paste0(
              "       sosa:isHostedBy             <",
              RDF_comp_uri,
              "> ;")
        # comp_obsProps <- as.character(xml2::xml_find_all(
        #   comp_xml, ".//swes:observableProperty/text()"
        # )) # TODO check if the Observational properties they must also be considered in the components
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
        # close component file ttl ----
        component <- c(
          component,
          paste0("<", RDF_comp_uri, ">"),
          comp_type,
          comp_sameAs, # gml:identifier
          paste0("       rdfs:comment           '", comp_description, "'@en ;"), # gml:description
          paste0("       rdfs:label             '", comp_name, "'@en ;"), # gml:name
          comp_attached,
          # comp_obsProps, # TODO check if the Observational properties they must also be considered in the components
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
        "       sosa:hosts             ",
        paste(
          "<",
          paste0(
            "http://rdfdata.lteritalia.it/sensors/componentsType/", #TODO add model name and manufacturer name (e.g. http://rdfdata.lteritalia.it/sensors/componentsType/Vantage/Davis_Instruments/uuid)
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
    # contents ----
    sensor <- '@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    @prefix sosa: <http://www.w3.org/ns/sosa/>
    @prefix ssn: <http://www.w3.org/ns/ssn/>
    @prefix ssn-system: <http://www.w3.org/ns/ssn/systems/>
    @prefix foaf: <http://xmlns.com/foaf/0.1/>
    @prefix owl: <http://www.w3.org/2002/07/owl#>
    @prefix dcat: <http://www.w3.org/ns/dcat#>
    @prefix prov: <http://www.w3.org/ns/prov#>
    @prefix schema: <http://schema.org/>
    @prefix dcmitype: <http://purl.org/dc/dcmitype/>'
    sensor <- c(
      sensor,
      RDF_identifier,
      sensor_type,
      sameAs, # gml:identifier
      paste0("       rdfs:comment           '", description, "'@en ;"), # gml:description
      paste0("       rdfs:label             '", name, "'@en ;"), # gml:name
      manufacturer,
      components,
      attached,
      sensor_capability_01,
      derivedFrom,
      imageFrom,
      observedProperties,
      "",
      sensor_capability_02,
      ""
    )
    write(sensor, file = sensorTypeTTL)
    # close file
    close(sensorTypeTTL)
    # send the ttl files to SPARQL endpoint
    ttl_system_files <- list.files(path = dir_name, pattern = "ID_system_*")
    for (j in 1:length(ttl_system_files)) {
      new_sensorType_qr <- httr2::request("http://fuseki1.get-it.it/systemsType") |>
        httr2::req_auth_basic(username = username_fuseki1, password = pwd_fuseki1) |>
        httr2::req_method("POST") |>
        httr2::req_body_file(
          path = paste0(dir_name, "/", ttl_system_files[j]),
          type = "text/turtle"
        ) |>
        httr2::req_retry(max_tries = 3, max_seconds = 120)
      httr2::req_perform(new_sensorType_qr, verbosity = 3)
    }
    ttl_component_files <- list.files(path = dir_name, pattern = "ID_component_*")
    for (j in 1:length(ttl_system_files)) {
      new_sensorType_qr <- httr2::request("http://fuseki1.get-it.it/systemsType") |>
        httr2::req_auth_basic(username = username_fuseki1, password = pwd_fuseki1) |>
        httr2::req_method("POST") |>
        httr2::req_body_file(
          path = paste0(dir_name, "/", ttl_system_files[j]),
          type = "text/turtle"
        ) |>
        httr2::req_retry(max_tries = 3, max_seconds = 120)
      httr2::req_perform(new_sensorType_qr, verbosity = 3)
    }
  } else {
    stop("\n----\nThe XML provided is not a sensor type.\nPlease change the file.\n----\n")
  }
  setwd("..")
}

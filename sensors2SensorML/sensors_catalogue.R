#' @title
#' @description
#' A short description...
#' 
#' @param excel_path description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom package function
#' @export
#' @example
#' \dontrun{
#' sensors_catalogue(excel_path = "./sensors_template.xlsx")
#' }
#' 
### function sensors_catalogue
sensors_catalogue <- function(excel_path = NULL) {
  # read excel file ----
  excel_file <- readxl::read_excel(excel_path, sheet = "SensorInfo")
  # lines concerning examples, units of measurement and data types are removed ----
  excel_file <- excel_file[-c(1:10),]
  # if excel file contains only example sensor ----
  # excel_file <- excel_file[c(-1, -2),]
  # delete row without sensor or components info ----
  excel_file <- dplyr::filter(excel_file, rowSums(is.na(excel_file)) != ncol(excel_file))
  # group by sensor name ----
  excel_file <- excel_file |>
    dplyr::group_by(sensor) |>
    tidyr::nest()
  # cycle for each system + components (if any) ----
  for (i in 1:nrow(excel_file)) {
    # check if manufacturer of this sensor exist in the triplestore
    man_exist <- check_man_exist(
      manufacturer_name = excel_file$data[[i]]$manufacturer[1]
    )
    if (length(man_exist) >= 1) {
      sensor <- excel_file$data[[i]]
      # assign system and component IDs, so that references can be made ----
      uuids <- sapply(1:nrow(sensor), uuid::UUIDgenerate)
      # use the functions sensorML_sysTypeXML() and sensorML_compTypeXML() to create the different XML ----
      sensorML_sysTypeXML(sensorList = sensor, uuidsList = uuids)
      if (nrow(sensor) > 1) {
        sensorML_compTypeXML(sensorList = sensor, uuidsList = uuids)
      }
      # use the function sensorMLType2rdf to create ttl of SensorML ----
      root_dir <- paste0(
        "sensorML_files_system_",
        uuids[1]
      )
      # TODO modify the sensorMLType2rdf function. The function as it is built now starts from the XML files of type systems
      # and type components, both of which do not contain information regarding e.g. observed property, contacts, position, etc. 
      # In addition in SOSA or SSN it is not possible to have the hierarchy and discriminate between sensor type and sensor instance.
      # So, I think we need to produce the transformation for sensor instances only. How can this be done? How is it possible to take
      # all the XML type information (system, component and instance) and transform it into TTL?
      # if (dir.exists(root_dir)) {
      #   if (nrow(sensor) > 1 && 'component' %in% sensor$sensor_level) {
      #     sensorMLType2rdf(
      #       file_folder = paste0(getwd(), "/", root_dir, "/system/")
      #     )
      #     sensorMLType2rdf(
      #       file_folder = paste0(getwd(), "/", root_dir, "/components/")
      #     )
      #   } else { # nrow(sensor) = 1
      #     sensorMLType2rdf(
      #       file_folder = paste0(getwd(), "/", root_dir, "/system/")
      #     )
      #   }
      # }
      # send the XML files to 52N SOS endpoint
      # xml_comp_files <- list.files(path = paste0(getwd(), "/", root_dir, "/components"), pattern = "\\.xml$")
      # xml_sys_files <- list.files(path = paste0(getwd(), "/", root_dir, "/system"), pattern = "\\.xml$")
      # for (z in 1:length(xml_comp_files)) {
      #   new_compType_qr <- httr2::request("https://dev.get-it.it/observations/sos") |>
      #     httr2::req_auth_basic(username = user_sos, password = pwd_sos) |>
      #     httr2::req_auth_bearer_token(token = token_sos) |>
      #     httr2::req_method("POST") |>
      #     httr2::req_body_file(
      #       path = paste0(getwd(), "/", root_dir, "/components/", xml_comp_files[z]),
      #       type = "application/xml"
      #     ) |>
      #     httr2::req_retry(max_tries = 3, max_seconds = 120)
      #   httr2::req_perform(new_compType_qr, verbosity = 3)
      # }
      # new_sysType_qr <- httr2::request("https://dev.get-it.it/observations/sos") |>
      #   httr2::req_auth_basic(username = user_sos, password = pwd_sos) |>
      #   httr2::req_auth_bearer_token(token = token_sos) |>
      #   httr2::req_method("POST") |>
      #   httr2::req_body_file(
      #     path = paste0(getwd(), "/", root_dir, "/system/", xml_sys_files),
      #     type = "application/xml"
      #   ) |>
      #   httr2::req_retry(max_tries = 3, max_seconds = 120)
      # httr2::req_perform(new_sysType_qr, verbosity = 3)
      # TODO send the spreadsheet file to website
      # doc_qr <- httr2::request("xxx") |>
      #   httr2::req_auth_basic(username = xxx, password = xxx) |>
      #   httr2::req_auth_bearer_token(token = xxx) |>
      #   httr2::req_method("POST") |>
      #   httr2::req_body_file(
      #     path = list.files(paste0(getwd(), "/", root_dir, "/datasheet/")),
      #     type = "xxx"
      #   ) |>
      #   httr2::req_retry(max_tries = 3, max_seconds = 120)
      # httr2::req_perform(doc_qr, verbosity = 3)
      # TODO send the image file to website
      # img_qr <- httr2::request("xxx") |>
      #   httr2::req_auth_basic(username = xxx, password = xxx) |>
      #   httr2::req_auth_bearer_token(token = xxx) |>
      #   httr2::req_method("POST") |>
      #   httr2::req_body_file(
      #     path = list.files(paste0(getwd(), "/", root_dir, "/datasheet/")),
      #     type = "xxx"
      #   ) |>
      #   httr2::req_retry(max_tries = 3, max_seconds = 120)
      # httr2::req_perform(img_qr, verbosity = 3)
    } else if (is.null(man_exist)) {
      stop("\n----\nThe manufacturer", sensor$manufacturer[1], " entered for the sensor/system,",  sensor$name[1], 
" in the excel sheet is not listed.
Please consider the use new_manufacturer_ui() function for create single manufacturer record in a triple store by web app form.
Or the use of rdf_ttl_manufacturer() function for bash creation of multiple record in triple store.\n----\n")
    }
  }
}

#' @title
#' @description
#' A short description...
#' 
#' @param sensorList description
#' @param uuidsList description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @keywords internal
#'
### function sensorML_sysTypeXML
sensorML_sysTypeXML <- function(sensorList, uuidsList) {
  # sensor, name, and uuid ----
  sensor_system <- sensorList %>%
    dplyr::filter(sensor_level == 'system')
  system_uuid <- uuidsList[1]
  # folders creation ----
  # root folder
  root_dir <- paste0(
    "sensorML_files_system_",
    system_uuid
  )
  if (!dir.exists(root_dir)) {
    dir.create(root_dir)
    dir_name <- "system"
    dir.create(paste0(
      root_dir, "/", dir_name
    ))
  } else if (dir.exists(root_dir) && !dir.exists(
    paste0(
      root_dir, "/", dir_name
    )
  )) {
    dir.create(paste0(
      root_dir, "/", dir_name
    ))
  }
  # download and save locally datasheet file ----
  if (!is.na(sensor_system$datasheet)) {
    if (!dir.exists(
      paste0(
        root_dir, "/datasheet"
      ))) {
      dir.create(paste0(
        root_dir, "/datasheet"
      ))
    }
    doc_ext <- tools::file_ext(sensor_system$datasheet)
    if (doc_ext == '') {
      doc_ext <- ".html"
    }
    download.file(
      sensor_system$datasheet,
      destfile = paste0(
        root_dir,
        "/datasheet/",
        sensor_system$short_name, ".",
        doc_ext
      ),
      method = "libcurl"
    )
  }
  # download and save locally image file ----
  if (!is.na(sensor_system$image)) {
    if (!dir.exists(
      paste0(
        root_dir, "/image"
      ))) {
      dir.create(paste0(
        root_dir, "/image"
      ))
    }
    download.file(
      sensor_system$image,
      destfile = paste0(
        root_dir,
        "/image/",
        sensor_system$short_name, ".",
        tools::file_ext(sensor_system$image)
      ),
      method = "libcurl"
    )
  }
  # name of system file ----
  file_name <- paste0("ID_system_", system_uuid)
  # read xml ----
  physicalSystemType_XML_base <- xml2::read_xml("base_insertSensor.xml")
  # xml ----
  a <- physicalSystemType_XML_base %>%
    # swes:extension
    xml2::xml_add_child(., "swes:extension") %>%
    xml2::xml_add_child(., "swe:Boolean", "definition" = "isType") %>%
    xml2::xml_add_child(., "swe:value", "true")
  
  # swes:procedureDescriptionFormat
  b <- xml2::xml_add_child(physicalSystemType_XML_base, "swes:procedureDescriptionFormat", "http://www.opengis.net/sensorml/2.0")
  # swes:procedureDescription
  c <- xml2::xml_add_child(physicalSystemType_XML_base, "swes:procedureDescription") %>%
    xml2::xml_add_child(., "sml:PhysicalSystem", "xmlns:gco" = "http://www.isotc211.org/2005/gco",
                        "xmlns:gmd" = "http://www.isotc211.org/2005/gmd", "xmlns:gml" = "http://www.opengis.net/gml/3.2",
                        "xmlns:sml" = "http://www.opengis.net/sensorml/2.0", "xmlns:swe" = "http://www.opengis.net/swe/2.0",
                        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmlns:xlink" = "http://www.w3.org/1999/xlink",
                        "gml:id"= paste0("ID_system_", system_uuid),
                        "xsi:schemaLocation" = "http://www.opengis.net/sensorml/2.0 http://schemas.opengis.net/sensorML/2.0/sensorML.xsd http://www.opengis.net/swe/2.0 http://schemas.opengis.net/sweCommon/2.0/swe.xsd"
    )
  
  # gml:description
  if (!is.na(sensor_system$description)) {
    xml2::xml_add_child(c, "gml:description", sensor_system$description)
  }
  
  # gml:identifier
  if (!is.na(sensor_system$model_name)) {
    model_name = sub(" ", "_", sensor_system$model_name)
  } else {
    model_name = "model_name"
  }
  if (!is.na(sensor_system$manufacturer)) {
    manufacturer = sub(" ", "_", sensor_system$manufacturer)
  } else {
    manufacturer = "manufacturer"
  }
  xml2::xml_add_child(
    c,
    "gml:identifier",
    "codeSpace" = "uniqueID",
    paste0(
      "http://rdfdata.lteritalia.it/sensors/systemsType/",
      model_name, "/",
      manufacturer, "/",
      system_uuid
    )
  )
  
  # gml:name
  if (!is.na(sensor_system$name)) {
    xml2::xml_add_child(c, "gml:name", sensor_system$name)    
  }
  
  # sml:keywords
  if (!is.na(sensor_system$keywords)) {
    c1 <- xml2::xml_add_child(c, "sml:keywords") %>%
      xml2::xml_add_child(., "sml:KeywordList")
    list_keywords <- stringr::str_split(sensor_system$keywords, pattern = ",")
    for (x in 1:length(list_keywords[[1]])) {
      xml2::xml_add_child(c1, "sml:keyword", list_keywords[[1]][x])
    }
  }
  
  # sml:identification
  d <- xml2::xml_add_child(c, "sml:identification") %>%
    xml2::xml_add_child(., "sml:IdentifierList")
  # sml:identification - uniqueID
  xml2::xml_add_child(d, "sml:identifier") %>%
    xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0008/") %>%
    xml2::xml_add_child(., "sml:label", "uniqueID") %>%
    xml2::xml_add_sibling(
      .,
      "sml:value",
      paste0(
        "http://rdfdata.lteritalia.it/sensors/systemsType/",
        model_name, "/",
        manufacturer, "/",
        system_uuid
      )
    )
  # sml:identification - short_name
  if (!is.na(sensor_system$short_name)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0006/") %>%
      xml2::xml_add_child(., "sml:label", "shortName") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$short_name)
  }
  # sml:identification - Manufacturer
  if (!is.na(sensor_system$manufacturer)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0012/") %>%
      xml2::xml_add_child(., "sml:label", "Manufacturer") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$manufacturer)
  }
  # sml:identification - Model Name
  if (!is.na(sensor_system$model_name)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0003/") %>%
      xml2::xml_add_child(., "sml:label", "Model Name") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$model_name)
  }
  # sml:identification - Model Number
  if (!is.na(sensor_system$model_number)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0004/") %>%
      xml2::xml_add_child(., "sml:label", "Model Number") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$model_number)
  }
  # sml:identification - Long Name
  if (!is.na(sensor_system$long_name)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0002/") %>%
      xml2::xml_add_child(., "sml:label", "longName") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$long_name)
  }
  # sml:identification - Version
  if (!is.na(sensor_system$version)) {
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0013/") %>%
      xml2::xml_add_child(., "sml:label", "Version") %>%
      xml2::xml_add_sibling(., "sml:value", sensor_system$version)
  }
  
  # sml:classification
  if (!is.na(sensor_system$sensor_type)) {
    sensorType_query <- paste0("PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?c ?l
  WHERE {
    SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
      ?c rdf:type skos:Concept .
      <http://vocab.nerc.ac.uk/collection/P10/current/> skos:member ?c .
      OPTIONAL {
        ?c skos:prefLabel ?l .
      }
      FILTER( REGEX( STR(?l), '",
                               sensor_system$sensor_type,
                               "', 'i' ))
       }
      }
      ORDER BY ASC(?l)
      LIMIT 1")
    sensorType_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
      httr2::req_url_query(query = sensorType_query) %>%
      httr2::req_method("POST") %>%
      httr2::req_headers(Accept = "application/sparql-results+json") %>%
      httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
      httr2::req_perform()
    httr2::resp_check_status(sensorType_qr)
    sensorType_JSON <- httr2::resp_body_json(sensorType_qr)
    sensorType_rdf <- sensorType_JSON$results$bindings[[1]]$c$value
    e <- xml2::xml_add_child(c, "sml:classification") %>%
      xml2::xml_add_child(., "sml:ClassifierList")
    xml2::xml_add_child(e, "sml:classifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://www.opengis.net/def/property/OGC/0/SensorType") %>%
      xml2::xml_add_child(., "sml:label", sensor_system$sensor_type) %>%
      xml2::xml_add_sibling(., "sml:value", sensorType_rdf)
  }
  
  # sml:characteristics
  # sml:characteristics - physicalProperties
  if (!is.na(sensor_system$material) |
      !is.na(sensor_system$weight) |
      !is.na(sensor_system$height) |
      !is.na(sensor_system$width) |
      !is.na(sensor_system$length)) {
    f <- xml2::xml_add_child(c, "sml:characteristics", "name" = "characteristics") %>%
      xml2::xml_add_child(., "sml:CharacteristicList")
    f1 <- xml2::xml_add_child(f, "sml:characteristic", "name" = "physicalProperties") %>%
      xml2::xml_add_child(., "swe:DataRecord") %>%
      xml2::xml_add_child(., "swe:field", "name" = "PhysicalProperties") %>%
      xml2::xml_add_child(., "swe:DataRecord")
    # sml:characteristics - physicalProperties - Material
    if (!is.na(sensor_system$material)) {
      xml2::xml_add_child(f1, "swe:field", "name" = "Material") %>%
        xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0005/") %>%
        xml2::xml_add_child(., "swe:description", sensor_system$material)
    }
    # sml:characteristics - physicalProperties - Weight
    if (!is.na(sensor_system$weight)) {
      xml2::xml_add_child(f1, "swe:field", "name" = "Weight") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0001/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "kg", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/KGXX/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$weight)
    }
    # sml:characteristics - physicalProperties - Length
    if (!is.na(sensor_system$length)) {
      xml2::xml_add_child(f1, "swe:field", "name" = "Length") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0002/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "mm", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UXMM/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$length)
    }
    # sml:characteristics - physicalProperties - Width
    if (!is.na(sensor_system$width)) {
      xml2::xml_add_child(f1, "swe:field", "name" = "Width") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0004/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "mm", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UXMM/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$width)
    }
    # sml:characteristics - physicalProperties - Height
    if (!is.na(sensor_system$height)) {
      xml2::xml_add_child(f1, "swe:field", "name" = "Height") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0003/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "mm", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UXMM/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$height)
    }
  }
  # sml:characteristics - electricalRequirements
  if (!is.na(sensor_system$electrical_current_type) |
      !is.na(sensor_system$input_power_range) |
      !is.na(sensor_system$battery_charging_current) |
      !is.na(sensor_system$battery_output)) {
    f2 <- xml2::xml_add_child(f, "sml:characteristic", "name" = "electricalRequirements") %>%
      xml2::xml_add_child(., "swe:DataRecord")
    # sml:characteristics - electricalRequirements - ElectricalCurrentType
    if (!is.na(sensor_system$electrical_current_type)) {
      xml2::xml_add_child(f2, "swe:field", "name" = "ElectricalCurrentType") %>%
        xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0009/") %>%
        xml2::xml_add_child(., "swe:description", sensor_system$electrical_current_type)
    }
    # sml:characteristics - electricalRequirements - InputPowerRange
    if (!is.na(sensor_system$input_power_range)) {
      xml2::xml_add_child(f2, "swe:field", "name" = "InputPowerRange") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0008/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "W", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/WATT/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$input_power_range)
    }
    # sml:characteristics - electricalRequirements - BatteryChargingCurrent
    if (!is.na(sensor_system$battery_charging_current)) {
      if (grepl("\\s+", sensor_system$battery_charging_current)) {
        xml2::xml_add_child(f2, "swe:field", "name" = "BatteryChargingCurrent") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/MVB/current/MVB000064/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = "mA", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UMAM/") %>%
          xml2::xml_add_sibling(., "swe:value", sensor_system$battery_charging_current)
      } else {
        if (!is.na(as.numeric(sensor_system$battery_charging_current))) {
          xml2::xml_add_child(f2, "swe:field", "name" = "BatteryChargingCurrent") %>%
            xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/MVB/current/MVB000064/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = "mA", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UMAM/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$battery_charging_current)
        } else {
          xml2::xml_add_child(f2, "swe:field", "name" = "BatteryChargingCurrent") %>%
            xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/MVB/current/MVB000064/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$battery_charging_current)
        }
      }
    }
    # sml:characteristics - electricalRequirements - BatteryOutput
    if (!is.na(sensor_system$battery_output)) {
      if (grepl("\\s+", sensor_system$battery_output)) {
        xml2::xml_add_child(f2, "swe:field", "name" = "BatteryOutput") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/S29/current/PE001260/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = "V", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UVLT/") %>%
          xml2::xml_add_sibling(., "swe:value", sensor_system$battery_output)
      } else {
        if (!is.na(as.numeric(sensor_system$battery_output))) {
          xml2::xml_add_child(f2, "swe:field", "name" = "BatteryOutput") %>%
            xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/S29/current/PE001260/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = "V", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UVLT/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$battery_output)
        } else {
          xml2::xml_add_child(f2, "swe:field", "name" = "BatteryOutput") %>%
            xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/S29/current/PE001260/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$battery_output)
        }
      }
    }
  }
  # sml:characteristics - dataSpecifications
  if (!is.na(sensor_system$data_storage_type) |
      !is.na(sensor_system$data_storage) |
      !is.na(sensor_system$data_format)) {
    f3 <- xml2::xml_add_child(f, "sml:characteristic", "name" = "dataSpecifications") %>%
      xml2::xml_add_child(., "swe:DataRecord")
    # sml:characteristics - dataSpecifications - DataStorageType
    if(!is.na(sensor_system$data_storage_type)) {
      xml2::xml_add_child(f3, "swe:field", "name" = "DataStorageType") %>%
        xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0010/") %>%
        xml2::xml_add_child(., "swe:description", sensor_system$data_storage_type)
    }
    # sml:characteristics - dataSpecifications - DataStorage
    if(!is.na(sensor_system$data_storage)) {
      xml2::xml_add_child(f3, "swe:field", "name" = "DataStorage") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/MVB/current/MVB000064/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "MB", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/MBYT/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$data_storage)
    }
    # sml:characteristics - dataSpecifications - Dataformat
    if (!is.na(sensor_system$data_format)) {
      xml2::xml_add_child(f3, "swe:field", "name" = "Dataformat") %>%
        xml2::xml_add_child(., "swe:Text") %>%
        xml2::xml_add_child(., "swe:description", sensor_system$data_format)
    }
  }
  # sml:characteristics - transmissionMode
  if (!is.na(sensor_system$transmission_mode)) {
    f4 <- xml2::xml_add_child(f, "sml:characteristic", "name" = "transmissionMode") %>%
      xml2::xml_add_child(., "swe:DataRecord")
    # sml:characteristics - TransmissionMode
    if(!is.na(sensor_system$transmission_mode)) {
      xml2::xml_add_child(f4, "swe:field", "name" = "TransmissionMode") %>%
        xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W05/current/CHAR0007/") %>%
        xml2::xml_add_child(., "swe:description", sensor_system$transmission_mode)
    }
  }
  
  # sml:capabilities
  if (!is.na(sensor_system$accuracy) |
      !is.na(sensor_system$measurement_range) |
      !is.na(sensor_system$operating_depth) |
      !is.na(sensor_system$precision) |
      !is.na(sensor_system$resolution) |
      !is.na(sensor_system$sensitivity) |
      !is.na(sensor_system$minimum_reporting_frequency) |
      !is.na(sensor_system$mobile) |
      !is.na(sensor_system$insitu)) {
    g <- xml2::xml_add_child(c, "sml:capabilities", "name" = "capabilities") %>%
      xml2::xml_add_child(., "sml:CapabilityList")
    # for SPARQL query
    ireaEndpoint <- "http://fuseki1.get-it.it/directory/query"
    uom_query_I_part <- "PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l ?s
    WHERE {
      SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
        ?c rdf:type skos:Concept .
        <http://vocab.nerc.ac.uk/collection/P06/current/> skos:member ?c .
        OPTIONAL {
          ?c skos:altLabel ?l .
          ?c owl:sameAs ?s .
        }
        FILTER( STR(?l) = '"
    uom_query_II_part <- "' )
         }
        }
        ORDER BY ASC(?l)
        LIMIT 1"
    # sml:capabilities - Accuracy
    if (!is.na(sensor_system$accuracy)) {
      uom_nerc_query <- paste0(
        uom_query_I_part,
        sensor_system$accuracy_uom,
        uom_query_II_part
      )
      nercUOM <- httr2::request(ireaEndpoint) %>%
        httr2::req_url_query(query = uom_nerc_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(nercUOM)
      nercUOM_JSON <- httr2::resp_body_json(nercUOM)
      accuracy_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
      if (grepl("\\s+", sensor_system$accuracy)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = sensor_system$accuracy_uom, "xlink:href" = accuracy_uom_def) %>%
          xml2::xml_add_sibling(., "swe:value", sensor_system$accuracy)
      } else {
        if (!is.na(as.numeric(sensor_system$accuracy))) {
          xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
            xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = sensor_system$accuracy_uom, "xlink:href" = accuracy_uom_def) %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$accuracy)
        } else {
          xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
            xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$accuracy)
        }
      }
    }
    # sml:capabilities - MeasurementRange
    if (!is.na(sensor_system$measurement_range)) {
      uom_nerc_query <- paste0(
        uom_query_I_part,
        sensor_system$measurement_range_uom,
        uom_query_II_part
      )
      nercUOM <- httr2::request(ireaEndpoint) %>%
        httr2::req_url_query(query = uom_nerc_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(nercUOM)
      nercUOM_JSON <- httr2::resp_body_json(nercUOM)
      measurement_range_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
      xml2::xml_add_child(g, "sml:capability", "name" = "MeasurementRange") %>%
        xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0006/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = sensor_system$measurement_range_uom, "xlink:href" = measurement_range_uom_def) %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$measurement_range)
    }
    # sml:capabilities - OperatingDepth
    if (!is.na(sensor_system$operating_depth)) {
      xml2::xml_add_child(g, "sml:capability", "name" = "OperatingDepth") %>%
        xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0012/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "m", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$operating_depth)
    }
    # sml:capabilities - Precision
    if (!is.na(sensor_system$precision)) {
      uom_nerc_query <- paste0(
        uom_query_I_part,
        sensor_system$precision_uom,
        uom_query_II_part
      )
      nercUOM <- httr2::request(ireaEndpoint) %>%
        httr2::req_url_query(query = uom_nerc_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(nercUOM)
      nercUOM_JSON <- httr2::resp_body_json(nercUOM)
      precision_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
      xml2::xml_add_child(g, "sml:capability", "name" = "Precision") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0005/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = sensor_system$precision_uom, "xlink:href" = precision_uom_def) %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$precision)
    }
    # sml:capabilities - Resolution
    if (!is.na(sensor_system$resolution)) {
      uom_nerc_query <- paste0(
        uom_query_I_part,
        sensor_system$accuracy_uom,
        uom_query_II_part
      )
      nercUOM <- httr2::request(ireaEndpoint) %>%
        httr2::req_url_query(query = uom_nerc_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(nercUOM)
      nercUOM_JSON <- httr2::resp_body_json(nercUOM)
      accuracy_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
      if (grepl("\\s+", sensor_system$resolution)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = sensor_system$resolution_uom, "xlink:href" = sensor_system$resolution_uom_def) %>%
          xml2::xml_add_sibling(., "swe:value", sensor_system$resolution)
      } else {
        if (!is.na(as.numeric(sensor_system$resolution))) {
          xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
            xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = sensor_system$resolution_uom, "xlink:href" = sensor_system$resolution_uom_def) %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$resolution)
        } else {
          xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
            xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
            xml2::xml_add_sibling(., "swe:value", sensor_system$resolution)
        }
      }
    }
    # sml:capabilities - Sensitivity
    if (!is.na(sensor_system$sensitivity)) {
      uom_nerc_query <- paste0(
        uom_query_I_part,
        sensor_system$sensitivity_uom,
        uom_query_II_part
      )
      nercUOM <- httr2::request(ireaEndpoint) %>%
        httr2::req_url_query(query = uom_nerc_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(nercUOM)
      nercUOM_JSON <- httr2::resp_body_json(nercUOM)
      sensitivity_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
      xml2::xml_add_child(g, "sml:capability", "name" = "Sensitivity") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0009/") %>%
        xml2::xml_add_child(., "swe:uom", "code" = sensor_system$sensitivity_uom, "xlink:href" = sensitivity_uom_def) %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$sensitivity)
    }
    # sml:capabilities - MinimumReportingFrequency
    if (!is.na(sensor_system$minimum_reporting_frequency)) {
      xml2::xml_add_child(g, "sml:capability", "name" = "MinimumReportingFrequency") %>%
        xml2::xml_add_child(., "swe:Quantity", "definition" = "https://www.w3.org/TR/vocab-ssn/#SSNSYSTEMFrequency") %>%
        xml2::xml_add_child(., "swe:uom", "code" = "s", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UTBB/") %>%
        xml2::xml_add_sibling(., "swe:value", sensor_system$minimum_reporting_frequency)
    }
    # sml:capabilities - Mobile
    if (!is.na(sensor_system$mobile)) {
      xml2::xml_add_child(g, "sml:capability", "name" = "Mobile") %>%
        xml2::xml_add_child(., "swe:Boolean") %>%
        xml2::xml_add_child(., "swe:value", tolower(sensor_system$mobile))
    }
    # sml:capabilities - Insitu
    if (!is.na(sensor_system$insitu)) {
      xml2::xml_add_child(g, "sml:capability", "name" = "Insitu") %>%
        xml2::xml_add_child(., "swe:Boolean") %>%
        xml2::xml_add_child(., "swe:value", tolower(sensor_system$insitu))
    }
  }
  
  # sml:contacts
  if (!is.na(sensor_system$manufacturer)) {
    contacts_query <- paste0("PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l
    WHERE {
      ?c rdf:type foaf:Organization.
      ?c foaf:name ?l.
      FILTER( STR(?l) = '",
                             sensor_system$manufacturer,
                             "' )
    }
    ORDER BY ASC(?l)
    LIMIT 1")
    manufacturer_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
      httr2::req_url_query(query = contacts_query) %>%
      httr2::req_method("POST") %>%
      httr2::req_headers(Accept = "application/sparql-results+json") %>%
      httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
      httr2::req_perform()
    httr2::resp_check_status(manufacturer_qr)
    manufacturer_JSON <- httr2::resp_body_json(manufacturer_qr)
    manufacturer_rdf <- manufacturer_JSON$results$bindings[[1]]$c$value
    h <- xml2::xml_add_child(c, "sml:contacts") %>%
      xml2::xml_add_child(., "sml:ContactList")
    xml2::xml_add_child(h, "sml:contact", "xlink:title" = "manufacturer", "xlink:href" = manufacturer_rdf) %>%
      xml2::xml_add_child(., "gmd:CI_ResponsibleParty") %>%
      xml2::xml_add_child(., "gmd:role") %>%
      xml2::xml_add_child(., "gmd:CI_RoleCode",
                          "codeList" = "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode/",
                          "codeListValue" = "originator", "manufacturer"
      )
  }
  
  # sml:documentation
  if (!is.na(sensor_system$datasheet)) {
    i <- xml2::xml_add_child(c, "sml:documentation", "xlink:arcrole" = "datasheet") %>%
      xml2::xml_add_child(., "sml:DocumentList")
    # sml:documentation - document
    # TODO when the image is uploaded to a website substitute the sensor_system$datasheet with the path of datasheet
    # using also the uuid of sensor type
    xml2::xml_add_child(i, "sml:document") %>%
      xml2::xml_add_child(., "gmd:CI_OnlineResource") %>%
      xml2::xml_add_child(., "gmd:linkage") %>%
      xml2::xml_add_child(., "gmd:URL", sensor_system$datasheet)
  }
  if (!is.na(sensor_system$image)) {
    l <- xml2::xml_add_child(c, "sml:documentation", "xlink:arcrole" = "image") %>%
      xml2::xml_add_child(., "sml:DocumentList")
    l1 <- xml2::xml_add_child(l, "sml:document") %>%
      xml2::xml_add_child(., "gmd:CI_OnlineResource") %>%
      xml2::xml_add_child(., "gmd:linkage")
    # sml:documentation - image
    # TODO when the image is uploaded to a website substitute the sensor_system$image with the path of image
    # using also the uuid of sensor type
    xml2::xml_add_child(l1, "gmd:URL", sensor_system$image)
    xml2::xml_add_sibling(l1, "gmd:name") %>%
      xml2::xml_add_child(., "gco:CharacterString", "Image")
  }
  
  if (nrow(sensorList) > 1) {
    # sml:components
    components_number <- nrow(sensorList) - 1
    uuids_components <- uuidsList[2:nrow(sensorList)]
    m <- xml2::xml_add_child(c, "sml:components") %>%
      xml2::xml_add_child(., "sml:ComponentList")
    if (!is.na(sensor_system$model_name)) {
      model_name = sub(" ", "_", sensor_system$model_name)
    } else {
      model_name = "model_name"
    }
    if (!is.na(sensor_system$manufacturer)) {
      manufacturer = sub(" ", "_", sensor_system$manufacturer)
    } else {
      manufacturer = "manufacturer"
    }
    components_name <- paste(
      "ID_component_",
      uuids_components,
      sep = ""
    )
    components_xlink_href <- paste(
      paste0(
        "http://rdfdata.lteritalia.it/sensors/componentsType/",
        model_name, "/",
        manufacturer, "/"
      ),
      uuids_components,
      sep = ""
    )
    for (j in 1:length(components_xlink_href)) {
      xml2::xml_add_child(
        m,
        "sml:component",
        "name" = components_name[j],
        "xlink:href" = components_xlink_href[j]
      )
    }
    
    # swes:observableProperty
    xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", "not_defined")
    # In case of obsProps in sensor component(s) is fill remove the comment above
    #   components_info <- sensorList %>%
    #     dplyr::filter(sensor_level == 'component')
    #   list_obsProp <- components_info$observed_property
    #   list_obsProp_uris <- NA
    #   for (n in 1:length(list_obsProp)) {
    #     obsProp_query <- paste0("PREFIX owl: <http://www.w3.org/2002/07/owl#>
    # PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # SELECT ?c ?l ?s
    # WHERE {
    #   SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
    #     ?c rdf:type skos:Concept .
    #     <http://vocab.nerc.ac.uk/collection/P02/current/> skos:member ?c .
    #     OPTIONAL {
    #       ?c skos:prefLabel ?l .
    #       ?c owl:sameAs ?s .
    #     }
    #     FILTER( REGEX (STR(?l), '",
    #                             list_obsProp[1],
    #                             "', 'i' ))
    #      }
    #     }
    #     ORDER BY ASC(?l)
    #     LIMIT 1")
    #     obsProp_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    #       httr2::req_url_query(query = obsProp_query) %>%
    #       httr2::req_method("POST") %>%
    #       httr2::req_headers(Accept = "application/sparql-results+json") %>%
    #       httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    #       httr2::req_perform()
    #     httr2::resp_check_status(obsProp_qr)
    #     obsProp_JSON <- httr2::resp_body_json(obsProp_qr)
    #     obsProp_rdf <- obsProp_JSON$results$bindings[[1]]$c$value
    #     list_obsProp_uris <- paste(obsProp_rdf, list_obsProp_uris, sep = ",")
    #   }
    #   list_obsProp_uris <- stringr::str_sub(list_obsProp_uris, end = -5)
    #   if (!is.na(list_obsProp_uris)) {
    #     xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", list_obsProp_uris)
    #   } else {
    #     xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", "not_defined")
    #   }
  }
  
  # write file ----
  xml2::write_xml(
    physicalSystemType_XML_base,
    paste0(
      root_dir,
      "/",
      dir_name,
      "/",
      file_name,
      ".xml"
    )
  )
}

#' @title
#' @description
#' A short description...
#' 
#' @param sensorList description
#' @param uuidsList description
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom httr2 request req_url_query req_method
#' @importFrom httr2 req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @keywords internal
#'
### function sensorML_compTypeXML
# execute this only after sensorML_sysTypeXML() function!
sensorML_compTypeXML <- function(sensorList, uuidsList) {
  components_info <- sensorList %>%
    dplyr::filter(sensor_level == 'component')
  components_uuid <- uuidsList[c(2:length(uuidsList))]
  # folder creation ----
  root_dir <- paste0(
    "sensorML_files_system_",
    uuidsList[1]
  )
  dir_name <- "components"
  if (!dir.exists(
    paste0(
      root_dir, "/", dir_name
    )
  )) {
    dir.create(paste0(
      root_dir, "/", dir_name
    ))
  }
  for (z in 1:nrow(components_info)) {
    component_info <- components_info[z,]
    component_uuid <- components_uuid[z]
    # name of component file ----
    file_name <- paste0("ID_component_", component_uuid)
    # read xml ----
    physicalComponentType_XML_base <- xml2::read_xml("base_insertSensor.xml")
    # xml ----
    physicalComponentType_XML_base %>%
      # swes:extension
      xml2::xml_add_child(., "swes:extension") %>%
      xml2::xml_add_child(., "swe:Boolean", "definition" = "isType") %>%
      xml2::xml_add_child(., "swe:value", "true")
    
    # swes:procedureDescriptionFormat
    b <- xml2::xml_add_child(physicalComponentType_XML_base, "swes:procedureDescriptionFormat", "http://www.opengis.net/sensorml/2.0")
    # swes:procedureDescription
    c <- xml2::xml_add_child(physicalComponentType_XML_base, "swes:procedureDescription") %>%
      xml2::xml_add_child(., "sml:PhysicalComponent", "xmlns:gco" = "http://www.isotc211.org/2005/gco",
                          "xmlns:gmd" = "http://www.isotc211.org/2005/gmd", "xmlns:gml" = "http://www.opengis.net/gml/3.2",
                          "xmlns:sml" = "http://www.opengis.net/sensorml/2.0", "xmlns:swe" = "http://www.opengis.net/swe/2.0",
                          "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmlns:xlink" = "http://www.w3.org/1999/xlink",
                          "gml:id"= paste0("ID_component_", component_uuid),
                          "xsi:schemaLocation" = "http://www.opengis.net/sensorml/2.0 http://schemas.opengis.net/sensorML/2.0/sensorML.xsd http://www.opengis.net/swe/2.0 http://schemas.opengis.net/sweCommon/2.0/swe.xsd"
      )
    
    # gml:description
    if (!is.na(component_info$description)) {
      xml2::xml_add_child(c, "gml:description", component_info$description)
    }
    
    # gml:identifier
    if (!is.na(sensorList$model_name[1])) {
      model_name = sub(" ", "_", sensorList$model_name[1])
    } else {
      model_name = "model_name"
    }
    if (!is.na(sensorList$manufacturer[1])) {
      manufacturer = sub(" ", "_", sensorList$manufacturer[1])
    } else {
      manufacturer = "manufacturer"
    }
    xml2::xml_add_child(
      c,
      "gml:identifier",
      "codeSpace" = "uniqueID",
      paste0(
        "http://rdfdata.lteritalia.it/sensors/componentsType/",
        model_name, "/",
        manufacturer, "/",
        component_uuid
      )
    )
    
    # gml:name
    if (!is.na(component_info$name)) {
      xml2::xml_add_child(c, "gml:name", component_info$name)
    }
    
    # sml:keywords
    if (!is.na(component_info$keywords)) {
      c1 <- xml2::xml_add_child(c, "sml:keywords") %>%
        xml2::xml_add_child(., "sml:KeywordList")
      list_keywords <- stringr::str_split(component_info$keywords, pattern = ",")
      for (x in 1:length(list_keywords[[1]])) {
        xml2::xml_add_child(c1, "sml:keyword", list_keywords[[1]][x])
      }
    }
    
    # sml:identification
    d <- xml2::xml_add_child(c, "sml:identification") %>%
      xml2::xml_add_child(., "sml:IdentifierList")
    # sml:identification - uniqueID
    xml2::xml_add_child(d, "sml:identifier") %>%
      xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0008/") %>%
      xml2::xml_add_child(., "sml:label", "uniqueID") %>%
      xml2::xml_add_sibling(
        .,
        "sml:value",
        paste0(
          "http://rdfdata.lteritalia.it/sensors/componentsType/",
          model_name, "/",
          manufacturer, "/",
          component_uuid
        )
      )
    # sml:identification - short_name
    if (!is.na(component_info$short_name)) {
      xml2::xml_add_child(d, "sml:identifier") %>%
        xml2::xml_add_child(., "sml:Term", "definition" = "http://vocab.nerc.ac.uk/collection/W07/current/IDEN0006/") %>%
        xml2::xml_add_child(., "sml:label", "shortName") %>%
        xml2::xml_add_sibling(., "sml:value", component_info$short_name)
    }
    
    # sml:classification
    if (!is.na(component_info$sensor_type)) {
      componentType_query <- paste0("PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?c ?l
  WHERE {
    SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
      ?c rdf:type skos:Concept .
      <http://vocab.nerc.ac.uk/collection/P10/current/> skos:member ?c .
      OPTIONAL {
        ?c skos:prefLabel ?l .
      }
      FILTER( REGEX( STR(?l), '",
                                    component_info$sensor_type,
                                    "', 'i' ))
       }
      }
      ORDER BY ASC(?l)
      LIMIT 1")
      componentType_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
        httr2::req_url_query(query = componentType_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(componentType_qr)
      componentType_JSON <- httr2::resp_body_json(componentType_qr)
      component_type_def <- componentType_JSON$results$bindings[[1]]$c$value
      e <- xml2::xml_add_child(c, "sml:classification") %>%
        xml2::xml_add_child(., "sml:ClassifierList")
      xml2::xml_add_child(e, "sml:classifier") %>%
        xml2::xml_add_child(., "sml:Term", "definition" = "http://www.opengis.net/def/property/OGC/0/SensorType") %>%
        xml2::xml_add_child(., "sml:label", component_info$sensor_type) %>%
        xml2::xml_add_sibling(., "sml:value", component_type_def)
    }
    
    # sml:capabilities
    if (!is.na(component_info$accuracy) |
        !is.na(component_info$measurement_range) |
        !is.na(component_info$operating_depth) |
        !is.na(component_info$precision) |
        !is.na(component_info$resolution) |
        !is.na(component_info$sensitivity) |
        !is.na(component_info$minimum_reporting_frequency) |
        !is.na(component_info$mobile) |
        !is.na(component_info$insitu)) {
      g <- xml2::xml_add_child(c, "sml:capabilities", "name" = "capabilities") %>%
        xml2::xml_add_child(., "sml:CapabilityList")
      # for SPARQL query ----
      ireaEndpoint <- "http://fuseki1.get-it.it/directory/query"
      uom_query_I_part <- "PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l ?s
    WHERE {
      SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
        ?c rdf:type skos:Concept .
        <http://vocab.nerc.ac.uk/collection/P06/current/> skos:member ?c .
        OPTIONAL {
          ?c skos:altLabel ?l .
          ?c owl:sameAs ?s .
        }
        FILTER( STR(?l) = '"
      uom_query_II_part <- "' )
         }
        }
        ORDER BY ASC(?l)
        LIMIT 1"
      # sml:capabilities - Accuracy
      if (!is.na(component_info$accuracy)) {
        uom_nerc_query <- paste0(
          uom_query_I_part,
          component_info$accuracy_uom,
          uom_query_II_part
        )
        nercUOM <- httr2::request(ireaEndpoint) %>%
          httr2::req_url_query(query = uom_nerc_query) %>%
          httr2::req_method("POST") %>%
          httr2::req_headers(Accept = "application/sparql-results+json") %>%
          httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
          httr2::req_perform()
        httr2::resp_check_status(nercUOM)
        nercUOM_JSON <- httr2::resp_body_json(nercUOM)
        accuracy_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
        if (grepl("\\s+", component_info$accuracy)) {
          xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
            xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = component_info$accuracy_uom, "xlink:href" = accuracy_uom_def) %>%
            xml2::xml_add_sibling(., "swe:value", component_info$accuracy)
        } else {
          if (!is.na(as.numeric(component_info$accuracy))) {
            xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
              xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
              xml2::xml_add_child(., "swe:uom", "code" = component_info$accuracy_uom, "xlink:href" = accuracy_uom_def) %>%
              xml2::xml_add_sibling(., "swe:value", component_info$accuracy)
          } else {
            xml2::xml_add_child(g, "sml:capability", "name" = "Accuracy") %>%
              xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0001/") %>%
              xml2::xml_add_child(., "swe:value", component_info$accuracy)
          }
        }
      }
      # sml:capabilities - MeasurementRange
      if (!is.na(component_info$measurement_range)) {
        uom_nerc_query <- paste0(
          uom_query_I_part,
          component_info$measurement_range_uom,
          uom_query_II_part
        )
        nercUOM <- httr2::request(ireaEndpoint) %>%
          httr2::req_url_query(query = uom_nerc_query) %>%
          httr2::req_method("POST") %>%
          httr2::req_headers(Accept = "application/sparql-results+json") %>%
          httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
          httr2::req_perform()
        httr2::resp_check_status(nercUOM)
        nercUOM_JSON <- httr2::resp_body_json(nercUOM)
        measurement_range_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
        xml2::xml_add_child(g, "sml:capability", "name" = "MeasurementRange") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0006/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = component_info$measurement_range_uom, "xlink:href" = measurement_range_uom_def) %>%
          xml2::xml_add_sibling(., "swe:value", component_info$measurement_range)
      }
      # sml:capabilities - OperatingDepth
      if (!is.na(component_info$operating_depth)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "OperatingDepth") %>%
          xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0012/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = "m", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/ULAA/") %>%
          xml2::xml_add_sibling(., "swe:value", component_info$operating_depth)
      }
      # sml:capabilities - Precision
      if (!is.na(component_info$precision)) {
        uom_nerc_query <- paste0(
          uom_query_I_part,
          component_info$precision_uom,
          uom_query_II_part
        )
        nercUOM <- httr2::request(ireaEndpoint) %>%
          httr2::req_url_query(query = uom_nerc_query) %>%
          httr2::req_method("POST") %>%
          httr2::req_headers(Accept = "application/sparql-results+json") %>%
          httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
          httr2::req_perform()
        httr2::resp_check_status(nercUOM)
        nercUOM_JSON <- httr2::resp_body_json(nercUOM)
        precision_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
        xml2::xml_add_child(g, "sml:capability", "name" = "Precision") %>%
          xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0005/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = component_info$precision_uom, "xlink:href" = precision_uom_def) %>%
          xml2::xml_add_sibling(., "swe:value", component_info$precision)
      }
      # sml:capabilities - Resolution
      if (!is.na(component_info$resolution)) {
        uom_nerc_query <- paste0(
          uom_query_I_part,
          component_info$resolution_uom,
          uom_query_II_part
        )
        nercUOM <- httr2::request(ireaEndpoint) %>%
          httr2::req_url_query(query = uom_nerc_query) %>%
          httr2::req_method("POST") %>%
          httr2::req_headers(Accept = "application/sparql-results+json") %>%
          httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
          httr2::req_perform()
        httr2::resp_check_status(nercUOM)
        nercUOM_JSON <- httr2::resp_body_json(nercUOM)
        resolution_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
        if (grepl("\\s+", component_info$resolution)) {
          xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
            xml2::xml_add_child(., "swe:QuantityRange", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
            xml2::xml_add_child(., "swe:uom", "code" = component_info$resolution_uom, "xlink:href" = resolution_uom_def) %>%
            xml2::xml_add_sibling(., "swe:value", component_info$resolution)
        } else {
          if (!is.na(as.numeric(component_info$resolution))) {
            xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
              xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
              xml2::xml_add_child(., "swe:uom", "code" = component_info$resolution_uom, "xlink:href" = resolution_uom_def) %>%
              xml2::xml_add_sibling(., "swe:value", component_info$resolution)
          } else {
            xml2::xml_add_child(g, "sml:capability", "name" = "Resolution") %>%
              xml2::xml_add_child(., "swe:Text", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0007/") %>%
              xml2::xml_add_sibling(., "swe:value", component_info$resolution)
          }
        }
      }  
      # sml:capabilities - Sensitivity
      if (!is.na(component_info$sensitivity)) {
        uom_nerc_query <- paste0(
          uom_query_I_part,
          component_info$sensitivity_uom,
          uom_query_II_part
        )
        nercUOM <- httr2::request(ireaEndpoint) %>%
          httr2::req_url_query(query = uom_nerc_query) %>%
          httr2::req_method("POST") %>%
          httr2::req_headers(Accept = "application/sparql-results+json") %>%
          httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
          httr2::req_perform()
        httr2::resp_check_status(nercUOM)
        nercUOM_JSON <- httr2::resp_body_json(nercUOM)
        sensitivity_uom_def <- nercUOM_JSON$results$bindings[[1]]$c$value
        xml2::xml_add_child(g, "sml:capability", "name" = "Sensitivity") %>%
          xml2::xml_add_child(., "swe:Quantity", "definition" = "http://vocab.nerc.ac.uk/collection/W04/current/CAPB0009/") %>%
          xml2::xml_add_child(., "swe:uom", "code" = component_info$sensitivity_uom, "xlink:href" = sensitivity_uom_def) %>%
          xml2::xml_add_sibling(., "swe:value", component_info$sensitivity)
      }
      # sml:capabilities - MinimumReportingFrequency
      if (!is.na(component_info$minimum_reporting_frequency)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "MinimumReportingFrequency") %>%
          xml2::xml_add_child(., "swe:Quantity", "definition" = "https://www.w3.org/TR/vocab-ssn/#SSNSYSTEMFrequency") %>%
          xml2::xml_add_child(., "swe:uom", "code" = "s", "xlink:href" = "http://vocab.nerc.ac.uk/collection/P06/current/UTBB/") %>%
          xml2::xml_add_sibling(., "swe:value", component_info$minimum_reporting_frequency)
      }
      # sml:capabilities - Mobile
      if (!is.na(component_info$mobile)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "Mobile") %>%
          xml2::xml_add_child(., "swe:Boolean") %>%
          xml2::xml_add_child(., "swe:value", tolower(component_info$mobile))
      }
      # sml:capabilities - Insitu
      if (!is.na(component_info$insitu)) {
        xml2::xml_add_child(g, "sml:capability", "name" = "Insitu") %>%
          xml2::xml_add_child(., "swe:Boolean") %>%
          xml2::xml_add_child(., "swe:value", tolower(component_info$insitu))
      }
    }
    
    # sml:contacts
    if (!is.na(sensorList$manufacturer[1])) {
      contacts_query <- paste0("PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?c ?l
    WHERE {
      ?c rdf:type foaf:Organization.
      ?c foaf:name ?l.
      FILTER( STR(?l) = '",
                               sensorList$manufacturer[1],
                               "' )
    }
    ORDER BY ASC(?l)
    LIMIT 1")
      manufacturer_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
        httr2::req_url_query(query = contacts_query) %>%
        httr2::req_method("POST") %>%
        httr2::req_headers(Accept = "application/sparql-results+json") %>%
        httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
        httr2::req_perform()
      httr2::resp_check_status(manufacturer_qr)
      manufacturer_JSON <- httr2::resp_body_json(manufacturer_qr)
      manufacturer_rdf <- manufacturer_JSON$results$bindings[[1]]$c$value
      h <- xml2::xml_add_child(c, "sml:contacts") %>%
        xml2::xml_add_child(., "sml:ContactList")
      xml2::xml_add_child(h, "sml:contact", "xlink:title" = "manufacturer", "xlink:href" = manufacturer_rdf) %>%
        xml2::xml_add_child(., "gmd:CI_ResponsibleParty") %>%
        xml2::xml_add_child(., "gmd:role") %>%
        xml2::xml_add_child(., "gmd:CI_RoleCode",
                            "codeList" = "http://www.isotc211.org/2005/resources/Codelist/gmxCodelists.xml#CI_RoleCode/",
                            "codeListValue" = "originator", "manufacturer")
    }
    
    # sml:attachedTo
    if (!is.na(sensorList$model_name[1])) {
      model_name = sub(" ", "_", sensorList$model_name[1])
    } else {
      model_name = "model_name"
    }
    if (!is.na(sensorList$manufacturer[1])) {
      manufacturer = sub(" ", "_", sensorList$manufacturer[1])
    } else {
      manufacturer = "manufacturer"
    }
    xml2::xml_add_child(
      c,
      "sml:attachedTo",
      "xlink:title" = paste0(
        "http://rdfdata.lteritalia.it/sensors/systemsType/",
        model_name, "/",
        manufacturer, "/",
        uuidsList[1]
      )
    )
    
    # swes:observableProperty
    xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", "not_defined")
    # In case of obsProps in sensor component(s) is fill remove the comment above
    #   if (!is.na(component_info$observed_property)) {
    #     obsProp_query <- paste0("PREFIX owl: <http://www.w3.org/2002/07/owl#>
    # PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # SELECT ?c ?l ?s
    # WHERE {
    #   SERVICE <http://vocab.nerc.ac.uk/sparql/sparql> {
    #     ?c rdf:type skos:Concept .
    #     <http://vocab.nerc.ac.uk/collection/P02/current/> skos:member ?c .
    #     OPTIONAL {
    #       ?c skos:prefLabel ?l .
    #       ?c owl:sameAs ?s .
    #     }
    #     FILTER( REGEX( STR(?l), '",
    #     component_info$observed_property,
    #     "', 'i' ))
    #      }
    #     }
    #     ORDER BY ASC(?l)
    #     LIMIT 1")
    #     obsProp_qr <- httr2::request("http://fuseki1.get-it.it/manufacturers") %>%
    #       httr2::req_url_query(query = obsProp_query) %>%
    #       httr2::req_method("POST") %>%
    #       httr2::req_headers(Accept = "application/sparql-results+json") %>%
    #       httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    #       httr2::req_perform()
    #     httr2::resp_check_status(obsProp_qr)
    #     obsProp_JSON <- httr2::resp_body_json(obsProp_qr)
    #     obsProp_rdf <- obsProp_JSON$results$bindings[[1]]$c$value
    #     xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", obsProp_rdf)
    #   } else {
    #     xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", "not_defined")
    #   }
    
    # write file ----
    xml2::write_xml(
      physicalComponentType_XML_base,
      paste0(
        root_dir,
        "/",
        dir_name,
        "/",
        file_name,
        ".xml"
      )
    )
  }
}
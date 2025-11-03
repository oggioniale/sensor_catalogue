#' Generate SensorML XML descriptions from a sensor catalogue Excel file
#' @description
#' This function reads an Excel file describing sensor systems and components,
#' validates manufacturer information against a reference triple store (Fuseki),
#' and generates corresponding SensorML 2.0 XML files for each system and its components.
#' If new manufacturers are listed in the Excel file, the function checks their presence
#' in the Fuseki *manufacturers* dataset and, if missing, automatically creates
#' RDF/Turtle (`.ttl`) files describing them via the `sensors_new_manufacturer()` function.
#' In this case, the execution stops until the new TTL files are uploaded manually to Fuseki.
#' @details
#' The input Excel file must contain at least one sheet named **"SensorInfo"**,
#' listing metadata for sensors, systems, and components (e.g. manufacturer name,
#' model, identifier, observed properties, capabilities, etc.).  
#' Optionally, a second sheet **"new_manufacturer"** may define metadata for new
#' manufacturers to be added to the catalogue.
#'
#' The workflow executed by this function includes the following steps:
#' \itemize{
#'   \item Reads and cleans valid rows from the `"SensorInfo"` sheet;
#'   \item If the `"new_manufacturer"` sheet exists:
#'     \itemize{
#'       \item Checks each manufacturer against the Fuseki *manufacturers* dataset (`sensors_check_man_exist()`);
#'       \item If missing, generates RDF/TTL records using `sensors_new_manufacturer()` and stops execution;
#'     }
#'   \item If all manufacturers exist in Fuseki:
#'     \itemize{
#'       \item Groups entries by sensor name;
#'       \item Generates unique UUIDs for each sensor and component;
#'       \item Creates SensorML 2.0 XML files for each system and component
#'             via `sensors_sysTypeXML()` and `sensors_compTypeXML()`;
#'     }
#'   \item The XML files can later be converted into RDF/Turtle using
#'         the `sensors_type_rdf()` function or uploaded to a Sensor Observation Service (SOS).
#' }
#'
#' @param excel_path Character string. Path to the Excel file containing the sensor catalogue.
#' Must include the sheet `"SensorInfo"`, and `"new_manufacturer"`.
#'
#' @return
#' The function does not return an object in R.
#' It creates a directory for each system named `Sensors_files_system_<UUID>/`
#' containing:
#' \itemize{
#'   \item `/system/` — XML file for the main system;
#'   \item `/components/` — XML files for all components (if any);
#'   \item `/system_components_files_ttl/` — generated RDF/Turtle files after running `sensors_type_rdf()`.
#' }
#' If missing manufacturers are detected, the function generates RDF/Turtle files
#' via `sensors_new_manufacturer()` and stops, prompting the user to upload these TTL files
#' into the Fuseki *manufacturers* dataset before resuming execution.
#'
#' @seealso
#' \code{\link{sensors_check_man_exist}}, \code{\link{sensors_new_manufacturer}},
#' \code{\link{sensors_sysTypeXML}}, \code{\link{sensors_compTypeXML}},
#' \code{\link{sensors_type_rdf}}
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom readxl read_excel
#' @importFrom dplyr filter group_by
#' @importFrom tidyr nest
#' @importFrom uuid UUIDgenerate
#' @export
#' @examples
#' \dontrun{
#' sensors_catalogue(
#'   excel_path = "sensors_list/CristianoFugazza/sensors_CristianoFugazza.xlsx"
#' )
#' }
#'
### function sensors_catalogue
sensors_catalogue <- function(excel_path = NULL) {
  # --- 1. Load Excel file ---
  if (is.null(excel_path) || !file.exists(excel_path)) {
    stop("Excel file not found. Please provide a valid path.")
  }
  message("\n--- Reading 'SensorInfo' sheet ---")
  excel_file <- readxl::read_excel(excel_path, sheet = "SensorInfo")
  # Remove example/unit/type rows and empty lines
  excel_file <- excel_file[-c(1:10), ]
  excel_file <- dplyr::filter(excel_file, rowSums(is.na(excel_file)) != ncol(excel_file))
  # Group by sensor name
  excel_file <- excel_file |>
    dplyr::group_by(sensor) |>
    tidyr::nest()
  # --- 2. Check new manufacturers ---
  message("\n--- Checking 'new_manufacturer' sheet ---")
  # Read manufacturer sheet if available
  if ("new_manufacturer" %in% readxl::excel_sheets(excel_path)) {
    manufacturer_list <- readxl::read_excel(excel_path, sheet = "new_manufacturer")
    manufacturer_list <- manufacturer_list[-c(1), ]  # remove header example line
  } else {
    manufacturer_list <- data.frame()
  }
  # --- 3. Verify manufacturer existence in Fuseki ---
  if (nrow(manufacturer_list) > 0) {
    message("\n--- Verifying manufacturers in Fuseki ---")
    # track which are missing and which exist
    missing_manufacturers <- c()
    existing_manufacturers <- c()
    for (i in seq_len(nrow(manufacturer_list))) {
      man_name <- manufacturer_list$name[i]
      if (is.na(man_name) || man_name == "") next
      man_exist <- sensors_check_man_exist(manufacturer_name = man_name)
      if (!is.null(man_exist) && nrow(man_exist) > 0) {
        message("✓ Manufacturer already exists in Fuseki: ", man_name)
        existing_manufacturers <- c(existing_manufacturers, man_name)
      } else {
        message("⚠️ Manufacturer not found in Fuseki: ", man_name)
        missing_manufacturers <- c(missing_manufacturers, man_name)
      }
    }
    # if missing manufacturers found, filter list and generate TTL
    if (length(missing_manufacturers) > 0) {
      message("\n--- Generating RDF TTL only for new (missing) manufacturers ---")
      # Filter the Excel "new_manufacturer" sheet to keep only the missing ones
      manu_to_create <- manufacturer_list |>
        dplyr::filter(name %in% missing_manufacturers)
      # Run sensors_new_manufacturer() only for this filtered set
      sensors_new_manufacturer(man_table = manu_to_create)
      stop(
        "\n----\n",
        "The following manufacturers were not found in Fuseki:\n",
        paste0(" - ", missing_manufacturers, collapse = "\n"),
        "\n\nA new RDF TTL file has been generated (only for missing manufacturers) ",
        "via sensors_new_manufacturer().\nPlease upload it to the 'manufacturers' dataset in Fuseki, ",
        "then re-run sensors_catalogue().\n",
        "Execution stopped.\n----\n"
      )
    } else {
      message("\nAll manufacturers from 'new_manufacturer' sheet already exist in Fuseki.")
    }
  } else {
    message("\nNo new manufacturers declared in Excel file.")
  }
  # --- 4. Process sensors and components ---
  message("\n--- Processing sensors and components ---")
  for (i in seq_len(nrow(excel_file))) {
    sensor <- excel_file$data[[i]]
    manufacturer_name <- sensor$manufacturer[1]
    message("\nChecking manufacturer in Fuseki: ", manufacturer_name)
    man_exist <- sensors_check_man_exist(manufacturer_name = manufacturer_name)
    if (!is.null(man_exist) && nrow(man_exist) > 0) {
      message("✓ Manufacturer found. Proceeding with SensorML generation...")
      uuids <- sapply(seq_len(nrow(sensor)), uuid::UUIDgenerate)
      sensors_sysTypeXML(sensorList = sensor, uuidsList = uuids)
      if (nrow(sensor) > 1) {
        sensors_compTypeXML(sensorList = sensor, uuidsList = uuids)
      }
      root_dir <- paste0("sensors_files_system_", uuids[1])
    } else {
      stop(
        "\n----\nThe manufacturer '", manufacturer_name, "' (for sensor '", sensor$name[1],
        "') is not registered in Fuseki.\nPlease upload its TTL file using sensors_new_manufacturer(), ",
        "then re-run sensors_catalogue().\n----\n"
      )
    }
  }
  message("\n✅ sensors_catalogue() completed successfully.\n")
}

#' Generate SensorML XML description for a sensor system type
#' @description
#' This function creates a SensorML 2.0 XML file describing a sensor **system type**,
#' using metadata from a structured sensor data frame.  
#' It populates standard SensorML sections including identification, classification,
#' characteristics, capabilities, contacts, and documentation,
#' and stores the XML under `Sensors_files_system_<UUID>/system/`.
#' @details
#' The function expects a data frame containing system-level metadata (e.g. name, model,
#' manufacturer, accuracy, resolution, physical properties, etc.) and a vector of UUIDs.
#' It reads a base XML template (`baseSystem_insertSensor.xml`), fills the relevant fields,
#' retrieves additional metadata (e.g., units and manufacturer URI) from SPARQL endpoints,
#' and writes the final SensorML PhysicalSystem XML.  
#' The generated XML conforms to OGC SensorML 2.0 and can be registered in a Sensor Observation Service (SOS).
#' @param sensorList A `data.frame` or tibble containing metadata for the system and its components.
#' Must include fields such as `sensor_level`, `name`, `manufacturer`, `model_name`, etc.
#' @param uuidsList A character vector of UUIDs corresponding to each sensor and component.
#' @return
#' The function writes one XML file to
#' `Sensors_files_system_<UUID>/system/ID_system_<UUID>.xml`.  
#' It does not return any R object.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom xml2 read_xml xml_add_child xml_add_sibling xml_add_child xml_find_all write_xml
#' @importFrom httr2 request req_url_query req_method req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @importFrom dplyr filter
#' @importFrom stringr str_split str_replace
#' @importFrom tools file_ext
#' @keywords internal
#' @examples
#' \dontrun{
#' sensors_sysTypeXML(sensorList = df_system, uuidsList = uuids)
#' }
#'
### function sensors_sysTypeXML
sensors_sysTypeXML <- function(sensorList, uuidsList) {
  # sensor, name, and uuid ---
  sensor_system <- sensorList %>%
    dplyr::filter(sensor_level == 'system')
  system_uuid <- uuidsList[1]
  # folders creation ---
  # root folder
  root_dir <- paste0(
    "Sensors_files_system_",
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
  # --- Download and save datasheet file ---
  if (!is.na(sensor_system$datasheet)) {
    datasheet_dir <- file.path(root_dir, "datasheet")
    if (!dir.exists(datasheet_dir)) dir.create(datasheet_dir, recursive = TRUE)
    
    # Sanitize the sensor short name (no spaces or special chars)
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", sensor_system$short_name)
    
    # Try to get file extension from URL
    doc_ext <- tools::file_ext(sensor_system$datasheet)
    
    if (doc_ext == "") {
      # Try to detect MIME type from HTTP headers
      mime_type <- tryCatch({
        con <- curl::curl(sensor_system$datasheet)
        info <- summary(con)
        close(con)
        if (!is.null(info$contenttype) && length(info$contenttype) > 0)
          info$contenttype
        else
          NA_character_
      },
      error = function(e) NA_character_)
      
      # Decide extension based on MIME type
      if (!is.na(mime_type)) {
        if (grepl("pdf", mime_type, ignore.case = TRUE)) {
          doc_ext <- "pdf"
        } else if (grepl("html", mime_type, ignore.case = TRUE)) {
          doc_ext <- "html"
        } else if (grepl("plain|text", mime_type, ignore.case = TRUE)) {
          doc_ext <- "txt"
        } else {
          doc_ext <- "dat"  # fallback generic data
        }
      } else {
        doc_ext <- "pdf"  # default fallback when detection fails
      }
    }
    
    destfile <- file.path(datasheet_dir, paste0(safe_name, ".", doc_ext))
    
    tryCatch({
      download.file(sensor_system$datasheet, destfile = destfile, method = "libcurl", quiet = TRUE)
      message("✅ Datasheet saved as: ", destfile)
    },
    error = function(e) {
      message("⚠️ Failed to download datasheet: ", e$message)
    })
  }
  # --- Download and save image file ---
  if (!is.na(sensor_system$image)) {
    image_dir <- file.path(root_dir, "image")
    if (!dir.exists(image_dir)) dir.create(image_dir, recursive = TRUE)
    
    # Sanitize short name
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", sensor_system$short_name)
    
    # Detect or infer extension
    img_ext <- tools::file_ext(sensor_system$image)
    if (img_ext == "") {
      mime_type <- tryCatch({
        con <- curl::curl(sensor_system$image)
        info <- summary(con)
        close(con)
        if (!is.null(info$contenttype) && length(info$contenttype) > 0)
          info$contenttype
        else
          NA_character_
      },
      error = function(e) NA_character_)
      
      if (!is.na(mime_type)) {
        if (grepl("png", mime_type, ignore.case = TRUE)) {
          img_ext <- "png"
        } else if (grepl("jpeg|jpg", mime_type, ignore.case = TRUE)) {
          img_ext <- "jpg"
        } else if (grepl("gif", mime_type, ignore.case = TRUE)) {
          img_ext <- "gif"
        } else {
          img_ext <- "jpg"
        }
      } else {
        img_ext <- "jpg"
      }
    }
    
    destfile <- file.path(image_dir, paste0(safe_name, ".", img_ext))
    
    tryCatch({
      download.file(sensor_system$image, destfile = destfile, method = "libcurl", quiet = TRUE)
      message("✅ Image saved as: ", destfile)
    },
    error = function(e) {
      message("⚠️ Failed to download image: ", e$message)
    })
  }
  # name of system file ---
  file_name <- paste0("ID_system_", system_uuid)
  # read xml ---
  physicalSystemType_XML_base <- xml2::read_xml("baseSystem_insertSensor.xml")
  # xml ---
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
    model_name = gsub(" ", "_", sensor_system$model_name)
  } else {
    model_name = "model_name"
  }
  if (!is.na(sensor_system$manufacturer)) {
    manufacturer = gsub(" ", "_", sensor_system$manufacturer)
  } else {
    manufacturer = "manufacturer"
  }
  xml2::xml_add_child(
    c,
    "gml:identifier",
    "codeSpace" = "uniqueID",
    paste0(
      "https://rdfdata.lteritalia.it/sensors/systemsType/",
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
        "https://rdfdata.lteritalia.it/sensors/systemsType/",
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
      model_name = gsub(" ", "_", sensor_system$model_name)
    } else {
      model_name = "model_name"
    }
    if (!is.na(sensor_system$manufacturer)) {
      manufacturer = gsub(" ", "_", sensor_system$manufacturer)
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
        "https://rdfdata.lteritalia.it/sensors/componentsType/",
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
    # xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", "not_defined")
    # In case of obsProps in sensor component(s) is fill, remove the comment after this line
    # the line above is alternative to the code block below
    components_info <- sensorList %>%
      dplyr::filter(sensor_level == 'component')
    list_obsProp <- components_info$observed_property
    list_obsProp <- unique(list_obsProp[!is.na(list_obsProp)])
    list_obsProp_uris <- c()
    for (n in seq_along(list_obsProp)) {
      label <- list_obsProp[n]
      if (is.na(label) || label == "") next
      obsProp_query <- sprintf("
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      SELECT ?c ?l
      WHERE {
        ?c a skos:Concept ;
           skos:prefLabel ?l .
        FILTER(STRSTARTS(STR(?c), 'http://vocab.nerc.ac.uk/collection/P02/'))
        FILTER(REGEX(STR(?l), '%s', 'i'))
      }
      ORDER BY ASC(?l)
      LIMIT 1", label)
      obsProp_qr <- httr2::request("http://vocab.nerc.ac.uk/sparql/sparql") |>
        httr2::req_url_query(query = obsProp_query, format = "application/sparql-results+json") |>
        httr2::req_method("GET") |>
        httr2::req_headers(Accept = "application/sparql-results+json") |>
        httr2::req_retry(max_tries = 3, max_seconds = 120) |>
        httr2::req_perform()
      
      httr2::resp_check_status(obsProp_qr)
      obsProp_JSON <- httr2::resp_body_json(obsProp_qr)
      if (length(obsProp_JSON$results$bindings) > 0) {
        uri <- obsProp_JSON$results$bindings[[1]]$c$value
        list_obsProp_uris <- c(list_obsProp_uris, uri)
      } else {
        message("No match found for observed property: ", label)
      }
    }
    if (length(list_obsProp_uris) > 0) {
      for (uri in list_obsProp_uris) {
        xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", uri)
      }
    } else {
      xml2::xml_add_child(physicalSystemType_XML_base, "swes:observableProperty", "not_defined")
    }
  }
  
  # write file ---
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

#' Generate SensorML XML descriptions for sensor component types
#' @description
#' This function creates one SensorML 2.0 XML file for each **sensor component type**
#' associated with a given sensor system.  
#' It extracts metadata such as identifiers, classification, and measurement capabilities
#' from the provided data frame and writes individual XML files under
#' `Sensor_files_system_<UUID>/components/`.
#' @details
#' The function should be executed **after** `sensors_sysTypeXML()`.  
#' It reads a base XML template (`baseComponent_insertSensor.xml`), fills in metadata fields,
#' retrieves NERC vocabulary URIs and manufacturer information from SPARQL endpoints,
#' and generates complete SensorML PhysicalComponent descriptions.
#' @param sensorList A `data.frame` or tibble containing component-level metadata.
#' Must include the column `sensor_level == "component"` and fields such as
#' `name`, `manufacturer`, `sensor_type`, and capabilities (e.g. `accuracy`, `resolution`).
#' @param uuidsList A character vector of UUIDs generated for the system and its components.
#' @return
#' One XML file per component is created under
#' `Sensors_files_system_<UUID>/components/ID_component_<UUID>.xml`.  
#' The function does not return a value.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom xml2 read_xml xml_add_child xml_add_sibling write_xml
#' @importFrom httr2 request req_url_query req_method req_headers req_retry req_perform
#' @importFrom httr2 resp_check_status resp_body_json
#' @importFrom dplyr filter
#' @importFrom stringr str_split str_replace
#' @keywords internal
#' @examples
#' \dontrun{
#' sensors_compTypeXML(sensorList = df_components, uuidsList = uuids)
#' }
#'
### function sensors_compTypeXML
# execute this only after sensors_sysTypeXML() function!
sensors_compTypeXML <- function(sensorList, uuidsList) {
  components_info <- sensorList %>%
    dplyr::filter(sensor_level == 'component')
  components_uuid <- uuidsList[c(2:length(uuidsList))]
  # folder creation ---
  root_dir <- paste0(
    "Sensors_files_system_",
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
    # name of component file ---
    file_name <- paste0("ID_component_", component_uuid)
    # read xml ---
    physicalComponentType_XML_base <- xml2::read_xml("baseComponent_insertSensor.xml")
    # xml ---
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
      model_name = gsub(" ", "_", sensorList$model_name[1])
    } else {
      model_name = "model_name"
    }
    if (!is.na(sensorList$manufacturer[1])) {
      manufacturer = gsub(" ", "_", sensorList$manufacturer[1])
    } else {
      manufacturer = "manufacturer"
    }
    xml2::xml_add_child(
      c,
      "gml:identifier",
      "codeSpace" = "uniqueID",
      paste0(
        "https://rdfdata.lteritalia.it/sensors/componentsType/",
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
          "https://rdfdata.lteritalia.it/sensors/componentsType/",
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
      # for SPARQL query ---
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
        "https://rdfdata.lteritalia.it/sensors/systemsType/",
        model_name, "/",
        manufacturer, "/",
        uuidsList[1]
      )
    )
    
    # swes:observableProperty
    # xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", "not_defined")
    # In case of obsProps in sensor component(s) is fill remove the comment below
    if (!is.na(component_info$observed_property)) {
      label <- component_info$observed_property
      if (is.na(label) || label == "") next
      obsProp_query <- sprintf(
        "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
        SELECT ?c ?l
        WHERE {
          ?c a skos:Concept ;
          skos:prefLabel ?l .
          FILTER(STRSTARTS(STR(?c), 'http://vocab.nerc.ac.uk/collection/P02/'))
          FILTER(REGEX(STR(?l), '%s', 'i'))
        }
        ORDER BY ASC(?l)
        LIMIT 1", label)
      obsProp_qr <- httr2::request("http://vocab.nerc.ac.uk/sparql/sparql") |>
        httr2::req_url_query(query = obsProp_query, format = "application/sparql-results+json") |>
        httr2::req_method("GET") |>
        httr2::req_headers(Accept = "application/sparql-results+json") |>
        httr2::req_retry(max_tries = 3, max_seconds = 120) |>
        httr2::req_perform()
      httr2::resp_check_status(obsProp_qr)
      obsProp_JSON <- httr2::resp_body_json(obsProp_qr)
      if (length(obsProp_JSON$results$bindings) > 0) {
        uri <- obsProp_JSON$results$bindings[[1]]$c$value
      } else {
        message("No match found for observed property: ", label)
      }
    xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", uri)
    } else {
      xml2::xml_add_child(physicalComponentType_XML_base, "swes:observableProperty", "not_defined")
    }
    
    # write file ---
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

#' Validate a generated RDF/Turtle sensor instance file
#' @description
#' Checks that a Turtle (.ttl) file is RDF-valid and contains the expected predicates:
#' `rdf:type`, `dct:creator`, `dct:created`, and `dcat:contactPoint`.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom tibble tibble
#' @param ttl_path Character. Path to the `.ttl` file to validate.
#' @return
#' A tibble summarizing the validation results. Messages are printed in the console.
#' @keywords internal
#'
### function sensors_validate_ttl
sensors_validate_ttl <- function(ttl_path) {
  if (!requireNamespace("rdflib", quietly = TRUE)) {
    stop("Package 'rdflib' is required. Please install it with install.packages('rdflib').")
  }
  if (!file.exists(ttl_path)) {
    stop("File not found: ", ttl_path)
  }
  
  g <- tryCatch(
    rdflib::rdf_parse(ttl_path, format = "turtle"),
    error = function(e) {
      stop("❌ Invalid Turtle syntax: ", e$message)
    }
  )
  
  triples <- rdflib::rdf_query(g, "SELECT ?s ?p ?o WHERE { ?s ?p ?o . }")
  
  required_preds <- c(
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
    "http://purl.org/dc/terms/creator",
    "http://purl.org/dc/terms/created",
    "http://www.w3.org/ns/dcat#contactPoint"
  )
  
  present_preds <- unique(triples$p)
  missing_preds <- setdiff(required_preds, present_preds)
  
  if (length(missing_preds) == 0) {
    message("✅ RDF validation passed: all expected predicates are present.")
  } else {
    message("⚠️ RDF validation warning:")
    message("Missing predicates:\n", paste(" -", missing_preds, collapse = "\n"))
  }
  
  tibble::tibble(
    ttl_file = basename(ttl_path),
    total_triples = nrow(triples),
    predicates_found = length(present_preds),
    missing_predicates = ifelse(
      length(missing_preds) == 0,
      "None",
      paste(missing_preds, collapse = ", ")
    )
  )
}

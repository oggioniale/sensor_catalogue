library(shiny)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function(input, output, session) {
    # data table
    sensorsType_dataset <- "http://fuseki1.get-it.it/systemsType/query"
    systemsType_query <- paste0(
        "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
         PREFIX dcmitype: <http://purl.org/dc/dcmitype/>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX sosa: <http://www.w3.org/ns/sosa/>
         PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX prov: <http://www.w3.org/ns/prov#>
         PREFIX ssn: <http://www.w3.org/ns/ssn/>
         PREFIX dcat: <http://www.w3.org/ns/dcat#>
         PREFIX foaf: <http://xmlns.com/foaf/0.1/>
         SELECT ?c ?label ?doc ?image ?descr ?hosts ?comp_label ?man_name ?man_uri
         WHERE {
           ?c rdf:type sosa:Platform .
           OPTIONAL { ?c rdfs:label ?label . }
           OPTIONAL { ?c prov:wasDerivedFrom ?doc .}
           OPTIONAL { ?c dcat:contactPoint ?contact .}
           OPTIONAL { ?c dcmitype:Image ?image . }
           OPTIONAL { ?c rdfs:comment ?descr . }
           OPTIONAL { ?c sosa:hosts ?hosts . 
               OPTIONAL { ?hosts rdfs:label ?comp_label .}
           }
           OPTIONAL {
             ?contact foaf:name ?man_name .
             ?contact rdfs:seeAlso ?man_uri .
           }
         }
         ORDER BY ASC(?l)"
    )
    list_systemsType_req <- httr2::request(sensorsType_dataset) |>
        httr2::req_url_query(query = systemsType_query) |>
        httr2::req_method("POST") |>
        httr2::req_headers(Accept = "application/sparql-results+json") |>
        httr2::req_user_agent("ReLTER dev") |>
        httr2::req_retry(max_tries = 3, max_seconds = 120) |>
        httr2::req_perform()
    httr2::resp_check_status(list_systemsType_req)
    list_systemType <- httr2::resp_body_json(list_systemsType_req, simplifyVector = TRUE) |>
        purrr::pluck("results") |>
        tibble::as_tibble() |>
        dplyr::mutate(
            system_uri = bindings$c$value,
            system_label = bindings$label$value,
            system_description = bindings$descr$value,
            system_doc = bindings$doc$value,
            system_image = bindings$image$value,
            component_uri = bindings$hosts$value,
            component_label = bindings$comp_label$value,
            manufacturer_name = bindings$man_name$value,
            manufacturer_uri = bindings$man_uri$value,
            .keep = "used"
        ) |>
        dplyr::select(system_uri, system_label, system_description, system_doc, system_image, component_uri, component_label, manufacturer_name, manufacturer_uri)
    # data table output
    output$sensorTbl <- DT::renderDataTable({
        dfSensorsType <- list_systemType |>
            dplyr::mutate(
                rdfURL_html = paste0(
                    "<a href='",
                    system_uri,
                    "' target = '_blank'>", system_label, "</a>"
                ),
                doc = paste0(
                    "<a href='",
                    system_doc,
                    "' target = '_blank'>",
                    as.character(icon("file", lib = "glyphicon")),
                    " click to read the documentation</a>"
                ),
                thumb_html = paste0(
                    "<a href='",
                    system_image,
                    "' target = '_blank'>",
                    "<img src='",
                    system_image,
                    "' height='52'/></a>"
                ),
                compURL_html = paste0(
                    "<a href='",
                    component_uri,
                    "' target = '_blank'>", component_label, "</a>"
                ),
                man_html = paste0(
                    "<a href='",
                    manufacturer_uri,
                    "' target = '_blank'>", manufacturer_name, "</a>"
                ),
                .keep = "used"
            ) |>
            unique() |>
            dplyr::select(rdfURL_html, doc, thumb_html, compURL_html, man_html) |>
            dplyr::group_by(rdfURL_html, doc, thumb_html, man_html) |>
            dplyr::summarize(compURL_html = stringr::str_c(compURL_html, collapse = "<br> ")) |>
            dplyr::mutate(
                your_sensor = paste0(
                    "<a href='http://edidemo.get-it.it/dist/SensorML20_instance.html' target = '_blank'>Provide information about your sensor instance</a>"
                )
            ) |>
            dplyr::select(
                `System name` = rdfURL_html,
                Documentation = doc,
                Image = thumb_html,
                `System components` = compURL_html,
                Manufacturer = man_html,
                Instance = your_sensor
            ) 
        DT::datatable(
            dfSensorsType,
            escape = FALSE,
            caption = htmltools::tags$caption(
                style = 'caption-side: bottom; text-align: center;',
                'Table - ', htmltools::em(paste0(
                    'Contains model sensors'
                ))
            ),
            filter = 'top'
        )
    })
}

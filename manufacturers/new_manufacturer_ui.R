#' @title
#' @description
#' A short description...
#' 
#' @return description
#' @author Alessandro Oggioni, phD (2023) \email{oggioni.a@@cnr.it}
#' @importFrom 
#' @export
#' @example
#' new_manufacturer_ui()
#'
### function new_manufacturer_ui
new_manufacturer_ui <- function() {
  gui = TRUE
  if (gui) {
    #nocov start
    gui_deps <- c("shiny", "shinyjs", "shinyalert")
    gui_deps_missing <- !sapply(gui_deps, requireNamespace, quietly = TRUE)

    if (sum(gui_deps_missing) > 0) {
      stop("You need to install the following Suggested packages to use this function.
           Please install them with:
           install.packages(c(\"leaflet\", \"shiny\",\"leaflet.extras\", \"magrittr\"))")
    } else {
      requireNamespace("leaflet")
      requireNamespace("shiny")
      requireNamespace("leaflet.extras")
      requireNamespace("magrittr")
    }
    #nocov end
  }
  
  man_app <- function() {
    ui <- shiny::shinyUI(shiny::fluidPage(
      shinydashboard::dashboardPage(
        skin = "green",
        shinydashboard::dashboardHeader(
          title = shiny::tagList(
            shiny::tags$span(class = "logo-lg", "Add information of"),
            shiny::tags$img(src = "http://www.get-it.it/assets/img/loghi/lter_leaf.jpg")
          ),
          # shiny::tags$li(
          #   class = "dropdown",
          #   shiny::tags$a(
          #     href = "http://www.lter-europe.net",
          #     shiny::tags$img(src = "http://www.get-it.it/assets/img/loghi/eLTERH2020.png"),
          #     style = "margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
          #     target = "_blank"
          #   )
          # ),
          shiny::tags$li(
            class = "dropdown",
            shiny::tags$a(
              href = "http://www.lteritalia.it",
              shiny::tags$img(src = "http://www.get-it.it/assets/img/loghi/LogoLTERIta.png"),
              style = "margin:0;padding-top:2px;padding-bottom:2px;padding-left:10px;padding-right:10px;",
              target = "_blank"
            )
          )
        ),
        shinydashboard::dashboardSidebar(collapsed = FALSE,
                                         shinydashboard::sidebarMenu(
                                           shinydashboard::menuItem(
                             "Manufacturer",
                             tabName = "manufacturer",
                             icon = shiny::icon("briefcase", lib = "font-awesome")
                           )
                         )),
        shinydashboard::dashboardBody(
          shiny::tags$script(
            shiny::HTML(
              "var openTab = function(tabName){
             $('a', $('.sidebar')).each(function() {
                if(this.getAttribute('data-value') == tabName) {
                   this.click()
                };
             });
           }"
            )
          ),
          shinydashboard::tabItems(
            shinydashboard::tabItem(
              tabName = 'manufacturer',
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shinydashboard::box(
                    width = 12,
                    closable = FALSE,
                    status = "danger",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    sidebar_start_open = FALSE,
                    sidebar_content = shiny::tagList(
                      shiny::tags$p(
                        "Fill all fileds in order to create new new record the company."
                      )
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manName', 'Manufacturer name*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manTel', 'Phone number*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manAdd', 'Address*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manNumber', 'Address number*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manCity', 'City*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manAdm', 'Administrative area*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manPostCode', 'Postal code*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manCountry', 'Country*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manEMail', 'e-mail address*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px",
                      shiny::textInput('manWebSite', 'Web site*')
                    ),
                    shiny::tags$div(
                      style = "padding-right: 5px;padding-left: 5px; padding-bottom: 10px;",
                      shinyjs::disabled(
                        shiny::actionButton(
                          "sendQManufacturer",
                          "Add new manufacturer",
                          style = "color: #fff; background-color: #6D9538; border-color: #435926"
                        )
                      )
                    )
                  )
                ),
                shiny::column(
                  width = 8,
                  shinydashboard::box(
                    width = 12,
                    closable = FALSE,
                    status = "success",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    height = '400px',
                    sidebar_start_open = FALSE,
                    sidebar_content = shiny::tagList(
                      shiny::tags$p(
                        ""
                      )
                    ),
                    shiny::tags$div(
                      class = "CodeMirror cm-s-default",
                      style = "top: 10px;",
                      shiny::uiOutput("selectedVarManufacturer")
                    )
                  )
                )
              )
            )
          )
        )
      )
    ))
      
    server <- function(input, output, session) {
      
      # Set up preliminaries and define query
      # Define the fuseki GET-IT endpoint
      endpointUpdate <- "http://fuseki1.get-it.it/manufacturers/update"
      
      # SPARQL package to submit query and save results
      observeEvent(input$sendQManufacturer, {
        SPARQL(url = endpointUpdate, 
               query = queryContentSPARQLManufacturer(),
               ns = c(
                 '', '<http://rdfdata.get-it.it/sensors/manufacturers/>',
                 'foaf',  '<http://xmlns.com/foaf/0.1/>',
                 'addr',  '<http://wymiwyg.org/ontologies/foaf/postaddress#>',
                 'vcard', '<http://www.w3.org/2006/vcard/ns#>'
               ),
               curl_args=list(
                 'userpwd' = paste('yyy',':','xxxxxxxxxxxx',sep=''),
                 style = "post"
               )
        )
      })
      
      queryContentSPARQLManufacturer <- reactive(paste0(
        "prefix : <http://rdfdata.get-it.it/sensors/manufacturers/> prefix foaf:  <http://xmlns.com/foaf/0.1/> prefix addr:  <http://wymiwyg.org/ontologies/foaf/postaddress#> prefix vcard: <http://www.w3.org/2006/vcard/ns#> INSERT DATA {<http://rdfdata.get-it.it/sensors/manufacturers/", gsub(" ", "", input$manName), "> a foaf:Organization; vcard:email <mailto:", input$manEMail, ">; addr:address [addr:deliveryPoint  [addr:location [addr:building '' ; addr:country '", input$manCountry, "'; addr:postcode '", input$manPostCode, "'; addr:region '' ; addr:streetNr '", input$manNumber, "'; addr:thoroughfareName '", input$manAdd, "'; addr:town '", input$manCity, "'] ] ]; addr:address [addr:deliveryPoint [addr:location [addr:building '' ; addr:country '", input$manCountry, "'; addr:postcode '", input$manPostCode, "'; addr:region ''; addr:streetNr '", input$manNumber, "'; addr:thoroughfareName '", input$manAdd, "'; addr:town '", input$manCity, "'] ] ] ; foaf:homepage  <", input$manWebSite, ">; foaf:name '", input$manName, "'; foaf:phone <tel:", input$manTel, ">}",
        sep = ""
      ))
      
      queryContentUIManufacturer <- reactive(paste0(
        "
    prefix : <http://rdfdata.get-it.it/sensors/manufacturers/>
    prefix foaf:  <http://xmlns.com/foaf/0.1/>
    prefix addr:  <http://wymiwyg.org/ontologies/foaf/postaddress#>
    prefix vcard: <http://www.w3.org/2006/vcard/ns#>

    INSERT DATA
    {
    <http://rdfdata.get-it.it/sensors/manufacturers/",
        gsub(" ", "", input$manName),
        "> a foaf:Organization ;
    vcard:email <mailto:",
        input$manEMail,
        "> ;
    addr:address   [ addr:deliveryPoint  [ addr:location
    [ addr:building          '' ;
    addr:country           '",
        input$manCountry,
        "' ;
    addr:postcode          '",
        input$manPostCode,
        "' ;
    addr:region            '' ;
    addr:streetNr          '",
        input$manNumber,
        "' ;
    addr:thoroughfareName  '",
        input$manAdd,
        "' ;
    addr:town              '",
        input$manCity,
        "'
    ] ] ] ;
    addr:address   [ addr:deliveryPoint  [ addr:location
    [ addr:building          '' ;
    addr:country           '",
        input$manCountry,
        "' ;
    addr:postcode          '",
        input$manPostCode,
        "' ;
    addr:region            '' ;
    addr:streetNr          '",
        input$manNumber,
        "' ;
    addr:thoroughfareName  '",
        input$manAdd,
        "' ;
    addr:town              '",
        input$manCity,
        "'
    ] ] ] ;
    foaf:homepage  <",
        input$manWebSite,
        "> ;
    foaf:name      '",
        input$manName,
        "' ;
    foaf:phone     <tel:",
        input$manTel,
        ">  }",
        sep = ""
      ))
      
      output$selectedVarManufacturer <- shiny::renderUI({
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Making XML request", value = 0)
        
        tags$form(
          tags$textarea(
            id = "code",
            name = "code",
            queryContentUIManufacturer()
          ),
          tags$script(HTML(
            "var editorXML = CodeMirror.fromTextArea(document.getElementById(\"code\"), {
          mode: \"application/sparql-query\",
          matchBrackets: true,
          lineNumbers: true,
          smartindent: true,
          extraKeys: {\"Ctrl-Space\": \"autocomplete\"}
        });
        editorXML.setSize(\"100%\",\"100%\");"
          ))
        )
      })
      
      shiny::observe({
        shinyjs::toggleState("sendQManufacturer",
                    condition = (
                      input$manName != "" &&
                        input$manTel != "" &&
                        input$manAdd != "" &&
                        input$manNumber != "" &&
                        input$manCity != "" &&
                        input$manAdm != "" &&
                        input$manPostCode != "" &&
                        input$manCountry != "" &&
                        input$manWebSite != ""
                    )
        )
      })
      
      shiny::observeEvent(input$sendQManufacturer, {
        # Show a modal when the button sendQPerson is pressed
        shinyalert::shinyalert(
          title = "Well done!",
          type = "info",
          confirmButtonText = "Ok",
          timer = 3000
        )
      })
      
    }
    shiny::runGadget(ui, server)
  }
  man_app()
}
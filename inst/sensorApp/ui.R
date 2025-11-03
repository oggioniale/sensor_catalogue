library(shiny)
library(DT)

ui <- tagList(
    # ---- HEAD (meta, CSS, JS, etc.) ----
    tags$head(
        tags$meta(charset = "UTF-8"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$title("Sensor system type description"),
        tags$link(rel = "icon", href = "http://www.get-it.it/objects/specimen/assets/img/solo_foglia.png"),
        tags$link(rel = "stylesheet", href = "http://www.get-it.it/objects/specimen/assets/css/bootstrap-3.3.7.min.css"),
        tags$link(rel = "stylesheet", href = "http://www.get-it.it/objects/specimen/assets/css/font-awesome.min.css"),
        tags$link(rel = "stylesheet", href = "http://www.get-it.it/objects/specimen/assets/DataTables/datatables.css"),
        tags$script(src = "http://www.get-it.it/objects/specimen/assets/js/jquery-1.12.4.js"),
        tags$script(src = "http://www.get-it.it/objects/specimen/assets/DataTables/datatables.js"),
        tags$script(src = "http://www.get-it.it/objects/specimen/assets/js/bootstrap-3.0.3.min.js"),
        tags$script(HTML("$(function () { $('[data-toggle=\"tooltip\"]').tooltip(); })")),
        
        # ---- CSS layout styling (fixed header and footer, scrollable main content) ----
        tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden; /* prevent the whole page from scrolling */
      }

      /* Fixed top navbar */
      .navbar {
        background-color: #334155;
        margin: 0;
        border-radius: 0;
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 1000;
      }
      
      .navbar-default {
        margin-top: 90px;
      }
      
      /* Logo and title */
      .navbar-brand img {
        height: 50px;
        margin-top: -10px;
      }

      .navbar-text {
        color: #94c5e5;
        font-weight: bold;
      }

      /* Main scrollable content area */
      #content {
        position: absolute;
        top: 90px;       /* space below the header */
        bottom: 140px;   /* space above the footer */
        left: 0;
        right: 0;
        overflow-y: auto;
        padding: 20px;
      }

      /* Fixed footer */
      footer.footer {
        background-color: #f8f8f8;
        position: fixed;
        bottom: 0;
        width: 100%;
        border-top: 1px solid #ccc;
        padding: 15px 30px;
      }

      /* Footer layout */
      footer .col-lg-6 {
        float: left;
        width: 50%;
      }

      footer a, footer strong {
        color: #334155;
      }
    "))
    ),
    
    # ---- BODY ----
    fluidPage(
        # ---- HEADER (fixed top navigation bar) ----
        tags$nav(
            class = "navbar navbar-light",
            tags$div(class = "container-fluid",
                     tags$div(class = "navbar-header",
                              tags$a(
                                  class = "navbar-brand",
                                  href = "http://www.lteritalia.it", target = "_blank",
                                  tags$img(src = "https://www.lteritalia.it/wp-content/uploads/LTER-IT-033-scaled-480x143.png")
                              )
                     ),
                     tags$ul(class = "nav navbar-nav navbar-right",
                             tags$li(class = "navbar-text", "Sensor catalogue")
                     )
            )
        ),
        
        # ---- MAIN SCROLLABLE CONTENT ----
        tags$div(
            id = "content",
            navbarPage(
                "Catalogue",
                id = "nav",
                tabPanel(
                    "Sensors",
                    DTOutput("sensorTbl")
                )
            )
        ),
        
        # ---- FOOTER (fixed at the bottom) ----
        tags$footer(class = "footer",
                    tags$hr(),
                    fluidRow(
                        column(
                            6,
                            HTML('<p><span style="color: #94c5e5"><strong>Contacts</strong></span><br>
                <strong>Secretariat:</strong> Via Roberto Cozzi, 53 – 20156 Milan (Italy)<br>
                <strong>Phone:</strong> +02 66173307<br>
                <strong>E-mail:</strong> <a href="mailto:lteritaly@gmail.com" target="_blank">lteritaly@gmail.com</a></p>')
                        ),
                        column(
                            6,
                            HTML('<span style="color: #94c5e5"><strong>Useful links</strong></span><br>
                <a href="http://sparql.lteritalia.it/" target="_blank">SPARQL Endpoint</a>')
                        )
                    )
        )
    )
)
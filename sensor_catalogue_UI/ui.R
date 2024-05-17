library(shiny)

navbarPage(
    "Catalogue",
    id = "nav",
    
    tabPanel(
        "Sensors",
        DT::dataTableOutput("sensorTbl")
    )
)

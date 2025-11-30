#' Launch the eLTER-IT Catalogue (Sensors view)
#' @description `r lifecycle::badge("stable")`
#' This function launches the unified eLTER-IT Shiny catalogue
#' and automatically opens the **Sensors** tab.
#' It serves as a convenient wrapper around
#' \code{elter_catalogues_app()}, ensuring that users accessing
#' the Sensor Catalogue are directed immediately to the
#' sensor-type metadata section.
#' @details
#' The eLTER-IT catalogue is a multi-view Shiny application
#' combining both:
#' \itemize{
#'   \item the **Samples Catalogue**, and
#'   \item the **Sensor Type Catalogue**.
#' }
#' Calling \code{sensor_runApp()} opens the application with the
#' \strong{"Sensors type"} tab preselected.
#' Internally, this function simply calls
#' \code{Specimen::elter_catalogues_app(default_tab = "sensors")}.
#' @author
#' Alessandro Oggioni, PhD (2023–2025)  
#' \email{oggioni.a@@cnr.it}
#' @return
#' No return value.  
#' The function launches the Shiny application.
#' @examples
#' if (interactive()) {
#'   sensor_runApp()
#' }
#' @export
#' 
### function sensor_runApp
sensor_runApp <- function(...) {
  # lancia l’app unica con tab di default "sensors"
  SpecimenCat::elter_catalogues_app(default_tab = "sensors")
}

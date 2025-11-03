#' Launch the Sensor Catalogue Shiny Application
#'
#' This function starts a local Shiny web application
#' for visualising sensor data in an interactive catalogue format.
#' The app includes a fixed header and footer consistent
#' with the eLTER/LTER-Italy design style.
#'
#' @details
#' The application provides an interactive DataTable interface
#' to browse and explore sensor metadata.  
#' The header and footer remain fixed, while the main content
#' (sensor table) is scrollable.
#' @author
#' Alessandro Oggioni, PhD (2023) \email{oggioni.a@@cnr.it}
#' @examples
#' if (interactive()) {
#'   sensor_runApp()
#' }
#' @return
#' The function launches the Shiny application and returns no value.
#' @export
#' 
### function sensor_runApp
sensor_runApp <- function() {
  app_dir <- system.file("sensorApp", package = "Sensor")
  if (app_dir == "") {
    stop("The Shiny app directory could not be found. Try reinstalling the package.",
         call. = FALSE)
  }
  tryCatch(
    {
      shiny::runApp(app_dir, display.mode = "normal")
    },
    interrupt = function(e) {
      message("\n🟢 Shiny app stopped by user.")
      invisible(NULL)
    },
    error = function(e) {
      message("\n❌ Unexpected error: ", e$message)
      invisible(NULL)
    }
  )
}

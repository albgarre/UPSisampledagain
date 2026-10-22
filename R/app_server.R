#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_program_server("program_1")

  mod_calculadoras_server("calculadoras_1")
  mod_F2_server("F2_1")
  mod_F4_server("F4_1")
  mod_F7_server("F7_1")
  mod_F8_server("F8_1")
  mod_DEFF_server("DEFF_1")

  mod_distribucion_server("distribucion_1")
}

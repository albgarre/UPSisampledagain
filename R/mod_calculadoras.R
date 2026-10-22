#' calculadoras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calculadoras_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_sidebar(
      sidebar = sidebar(
        radioButtons(ns("choose_calculator"),
                     "",
                     choices = c(`Detección` = "det",
                                 `Estimación de prevalencia` = "prev",
                                 `Estimación de concentración (normal)` = "conc_norm",
                                 `Estimación de concentración (log-normal)` = "conc_lognorm",
                                 `Effecto del diseño (DEFF)` = "deff"
                                 )
                     )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_calculator == 'det'",
        mod_F2_ui("F2_1")
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_calculator == 'prev'",
        mod_F4_ui("F4_1")
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_calculator == 'conc_norm'",
        mod_F7_ui("F7_1")
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_calculator == 'conc_lognorm'",
        mod_F8_ui("F8_1")
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_calculator == 'deff'",
        mod_DEFF_ui("DEFF_1")
      )
    )

  )
}

#' calculadoras Server Functions
#'
#' @noRd
mod_calculadoras_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_calculadoras_ui("calculadoras_1")

## To be copied in the server
# mod_calculadoras_server("calculadoras_1")

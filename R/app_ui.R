#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_panel bs_theme nav_menu nav_item nav_spacer
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      page_navbar(
        theme = bs_theme(preset = "minty"),
        # fillable = FALSE,
        title = "OPSmuestreo",

        nav_panel(
          title = "Home",
          "AA"
          # mod_home_ui("home_1")
        ),
        nav_panel(
          title = "Calculadora tamaño muestral",
          mod_calculadoras_ui("calculadoras_1")
        ),
        nav_panel(
          title = "Muestreo basado en riesgo",
          mod_program_ui("program_1")
        ),
        nav_panel(
          title = "Distribución muestral",
          mod_distribucion_ui("distribucion_1")
        )
        # nav_panel(
        #   title = "Efecto del diseño",
        #
        # )
        # nav_spacer(),

        # nav_panel(
        #   title = "Calculadora: prevalencia diseño",
        #   # mod_deteccion_ui("deteccion_1")
        # ),
        #
        # nav_panel(
        #   title = "Calculadora: estimación de prevalencia",
        #   # mod_prevalencia_ui("prevalencia_1")
        # ),
        #
        # nav_panel(
        #   title = "Estimación de concentración",
        #   # mod_concentration_ui("concentration_1")
        # ),
        )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "RBsampling"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

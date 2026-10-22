#' F2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F2_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      "Detecta si el peligro está presente a partir de una prevalencia mínima (p*). El plan se diseña para hallar al menos un positivo con el nivel de confianza elegido: un 95% significa que, si el peligro estuviera presente a esa prevalencia mínima, el plan lo encontraría en 95 de cada 100 casos. No encontrar ninguno indica, con esa misma confianza, que el peligro está por debajo de la prevalencia mínima. Útil en programas de certificación de ausencia, en programas nuevos que buscan detectar la circulación del peligro, o para sustancias prohibidas o de tolerancia cero."
    ),
    hr(),

    layout_columns(
      col_widths = c(6, 6),
      layout_columns(
        col_widths = 12,

        # ## Parameter C
        #
        numericInput(ns("F1_C"),
                     value = 90,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList("Nivel de confianza (%)",
                                     tooltip(
                                       bs_icon("info-circle"),
                                       "Probabilidad de detectar el peligro si está presente a la prevalencia p*. Un 95% significa que el plan lo encontraría en 95 de cada 100 casos. Lo define el programa según la importancia del resultado."
                                       )
                                     )

                     ),

        # ## Parameter p
        #
        numericInput(ns("F1_p"),
                     value = 1,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList("Prevalencia mínima de diseño (%)",
                                     tooltip(
                                       bs_icon("info-circle"),
                                       "Prevalencia más baja que se quiere poder detectar. Se define según la prevalencia real esperada del peligro, a partir de datos previos, literatura o el umbral que el programa necesita vigilar."
                                     )
                                     )
                     ),
        #
        #
        # ## Parameter SeU

        numericInput(ns("SeU"),
                     value = 100,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList("Sensibilidad a nivel de unidad (%)",
                                     tooltip(
                                       bs_icon("info-circle"),
                                       "Probabilidad de que una unidad realmente contaminada dé positivo en todo el proceso: combina la toma de muestra (masa analizada y distribución del contaminante en la unidad) y el análisis. Cuando no se puede estimar completa, se aproxima con la sensibilidad del método validado."
                                     )
                     )
        ),

        ## Population size

        checkboxInput(ns("use_N"),
                      "¿Se conoce el tamaño de la población?"
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.use_N",
          numericInput(ns("N_pob"),
                       value = 1e6,
                       min = 0,
                       step = 100,
                       label = tagList(
                         "Tamaño de la población",
                         tooltip(
                           bs_icon("info-circle"),
                           "Tamaño del universo del que se muestrea (lotes, toneladas o establecimientos por año), tomado de registros de producción. Solo interviene cuando el tamaño de la muestra supera el 10% del universo, activando la corrección por población finita (CPF)."
                         )
                       )

                       )
        )
      ),
      layout_columns(
        col_widths = 12,
        uiOutput(ns("F1_outcome"))
      )


    )


  )
}

#' F2 Server Functions
#'
#' @noRd
mod_F2_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of C

      # C <- switch(
      #   input$F1_C_type,
      #   graves = 0.95,
      #   explor = 0.9,
      #   export = 0.99,
      #   user = input$F1_C/100
      #
      # )

      C <- input$F1_C/100

      ## Get the value of p

      # p <- switch(
      #   input$F1_p_type,
      #   graves = 0.01,
      #   rutina = 0.05,
      #   user = input$F1_p/100
      #
      # )

      p <- input$F1_p/100

      ## Get the SeU

      # if (input$use_SeU) {
      #
      #   SeU <- input$SeU/100
      #
      # } else {
      #
      #   SeU <- 1
      #
      # }

      SeU <- input$SeU/100

      ## Calculate

      n <- log(1-C)/log(1-p*SeU)
      n <- ceiling(n)

      ## Correction by population size

      if (input$use_N) {

        ## Check the proportion

        N <- input$N_pob

        if (n/N < .1) {
          flag <- TRUE
        } else {
          flag <- FALSE
        }

        d <- p*N
        n <- N/SeU * (1 - (1 - C)^(1/d))
        n <- ceiling(n)

      }

      ## Build the value box

      if (input$use_N) {

        if (flag) {

          value_box(title = paste0("Número mínimo de muestras aleatorias con correción por PF (no recomendada: f = ",
                                   round(n/N, 2)
                                   ),
                    value = n)

        } else {

          value_box(title = "Número mínimo de muestras aleatorias con correción por PF:",
                    value = n)

        }

      } else {

        value_box(title = "Número mínimo de muestras aleatorias:",
                  value = n)

      }

    })

  })
}

## To be copied in the UI
# mod_F2_ui("F2_1")

## To be copied in the server
# mod_F2_server("F2_1")

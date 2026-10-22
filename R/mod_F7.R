#' F7 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib popover
mod_F7_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      "Estima el nivel medio de un contaminante (ej.: un metal en mg/kg) cuando las concentraciones son aproximadamente simétricas y de variabilidad moderada (CV por debajo de ~40-50%). Se usa para estimar la concentración media de contaminantes químicos bien controlados, de concentraciones poco dispersas (ej.: un proceso industrial estable), como insumo para la evaluación de la exposición."
    ),
    hr(),

    layout_columns(
      col_widths = c(6, 6),
      layout_columns(
        col_widths = 12,

        ## Parameter alpha

        numericInput(ns("conf"),
                     value = 95,
                     min = 0,
                     max = 0,
                     step = 1,
                     label = tagList(
                       "Nivel de confianza (%)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Probabilidad de que el intervalo estimado contenga la media real. Un 95% significa que, repitiendo el muestreo, el intervalo contendría el valor verdadero en 95 de cada 100 casos. Lo define el programa según el uso del resultado."
                       )
                     )
        ),

        ## Parameter s

        numericInput(ns("s"),
                     value = 2,
                     min = 0,
                     step = .1,
                     label = tagList(
                       "Desvío estándar",
                       tooltip(
                         bs_icon("info-circle"),
                         "Variabilidad de la concentración entre muestras, en la unidad del resultado (ej.: mg/kg). Se calcula a partir de las mediciones del ciclo anterior o de un piloto. Debe ser mayor que 0."
                       )
                     )

        ),

        ## Parameter delta

        numericInput(ns("d"),
                     value = 0.02,
                     min = 0,
                     step = .01,
                     label = tagList(
                       "Precisión absoluta",
                       tooltip(
                         bs_icon("info-circle"),
                         "Margen de error máximo que se acepta para la media estimada, en las mismas unidades que el desvío estándar (ej.: ±0,02 mg/kg). No surge de un cálculo: lo decide el programa, por lo general fijándolo como un porcentaje de la media esperada. Debe ser mayor que 0."
                       )
                     )
                     ),

        ## Correccion por poblacion finita

        checkboxInput(ns("use_PPF"),
                      "¿Usar corrección por población finita?"
                      ),

        ## Parameter N

        conditionalPanel(
          ns = ns,
          condition = "input.use_PPF",
          numericInput(ns("big_N"),
                       value = 1000,
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

#' F7 Server Functions
#'
#' @noRd
mod_F7_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of Z

      alpha <- 1 - input$conf/100
      Z <- qnorm(1 - alpha/2)

      ## Get N

      N <- input$big_N

      ## Get s2

      s2 <- input$s^2

      ## Get d

      d <- input$d

      ## Calculate

      if (input$use_PPF) {

        print("Poblacion finita")

        n <- N*s2*Z^2/( (N-1)*d^2 +s2*Z^2 )

      } else {
        print("Poblacion infinita")

        n <- Z^2 * s2 / d^2

      }

      n <- ceiling(n)


      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n)

    })

  })
}

## To be copied in the UI
# mod_F7_ui("F7_1")

## To be copied in the server
# mod_F7_server("F7_1")

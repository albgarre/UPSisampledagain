#' F8 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F8_ui <- function(id) {
  ns <- NS(id)
  tagList(

    layout_columns(
      "Estima la concentración típica (media geométrica) cuando los valores son muy variables y asimétricos: la mayoría bajos y unos pocos muy altos (CV por encima de ~50-60%). Es el modelo natural de los recuentos microbiológicos (UFC/g) y de los contaminantes químicos que se distribuyen de forma desigual entre las unidades del alimento (ej.: micotoxinas, metales pesados, dioxinas). La elección entre normal y lognormal depende de la distribución observada, no del contaminante. Los resultados sirven de base para la evaluación de la exposición y el QMRA."
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
                     max = 100,
                     step = 1,
                     label = tagList(
                       "Nivel de confianza (%)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Probabilidad de que el intervalo estimado contenga la media geométrica real. Un 95% significa que, repitiendo el muestreo, el intervalo contendría el valor verdadero en 95 de cada 100 casos. Lo define el programa según el uso del resultado."
                       )
                     )

        ),

        ## Parameter s

        numericInput(ns("s"),
                     value = 1.5,
                     min = 0,
                     step = .1,
                     label = tagList(
                       "Desvío geométrico estandard",
                       tooltip(
                         bs_icon("info-circle"),
                         "Variabilidad de la concentración expresada como factor multiplicativo (sin unidades), no en mg/kg. Se obtiene como el exponencial del desvío estándar de los logaritmos de las mediciones del ciclo anterior o de un piloto. Debe ser mayor o igual a 1; GSD = 1 indica que no hay variabilidad."
                       )
                     )

        ),

        ## Parameter delta

        numericInput(ns("d"),
                     value = 2,
                     min = 0,
                     step = 1,
                     label = tagList(
                       "Margen de error relativo",
                       tooltip(
                         bs_icon("info-circle"),
                         "Margen de error máximo que se acepta para la media geométrica, expresado como porcentaje de ella (ej.: ±20%). Se ingresa directamente como porcentaje, sin convertir a unidades del resultado. Lo decide el programa; no surge de un cálculo. Debe ser mayor que 0."
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

#' F8 Server Functions
#'
#' @noRd
mod_F8_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of Z

      alpha <- 1 - input$conf/100
      Z <- qnorm(1 - alpha/2)

      ## Get N

      N <- input$big_N

      ## Get s2

      s2 <- log(input$s)^2

      ## Get d

      d <- log(1 + input$d/100)

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
# mod_F8_ui("F8_1")

## To be copied in the server
# mod_F8_server("F8_1")

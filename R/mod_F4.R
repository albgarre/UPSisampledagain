#' F4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_F4_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # card(
    #   min_height = "0px",
    #   card_title("F4: Estimación sin CPF"),
    #   card_body(

    layout_columns(
      "Estima la prevalencia, es decir, qué proporción de la población está afectada por el peligro, con un margen de error de ±d. Se aplica cuando la presencia del peligro ya es conocida, ya sea por un muestreo de detección previo o por otras fuentes (vigilancia de ETA, estudios, hallazgos de laboratorio), y se busca cuantificar su extensión: establecer una línea de base, seguir tendencias o evaluar la eficacia de las medidas de control."
    ),
    hr(),
    layout_columns(
      col_widths = c(6, 6),
      layout_columns(
        col_widths = 12,

        ## Parameter p

        numericInput(ns("F1_p"),
                     value = 50,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList(
                       "Prevalencia esperada (%)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Proporción de positivos que se anticipa encontrar. Se toma del ciclo anterior o de estudios regionales. Sin ningún dato previo, se usa 50%, que maximiza el número de muestras."
                       )
                     )

                     ),

        ## Parameter alpha

        numericInput(ns("conf"),
                     value = 95,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList("Nivel de confianza (%)",
                                     tooltip(
                                       bs_icon("info-circle"),
                                       "Probabilidad de que el intervalo estimado contenga la prevalencia real. Un 95% significa que, repitiendo el muestreo, el intervalo contendría el valor verdadero en 95 de cada 100 casos. Lo define el programa según el uso del resultado."
                                     )
                                     )
                     ),

        ## Parameter d

        numericInput(ns("d"),
                     value = 0.02,
                     min = 0,
                     step = .01,
                     label = tagList(
                       "Precisión ±d (p.p.)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Margen de error máximo aceptado para la prevalencia estimada, en puntos porcentuales. Con ±5 p.p., una prevalencia estimada de 15% podría estar en realidad entre 10% y 20%. Cuanto más estrecho el margen, más muestras se necesitan. Lo define el programa."
                       )
                     )
                     ),

        ## Sensibility

        numericInput(ns("Sen"),
                     value = 100,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList(
                       "Sensibilidad analítica (%)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Probabilidad de que el método dé positivo en una muestra realmente contaminada. Se obtiene de la validación del método. Junto con la especificidad, corrige la prevalencia observada para estimar la prevalencia verdadera, descontando falsos negativos y positivos."
                       )
                     )
                     ),

        ## Specificity

        numericInput(ns("Sp"),
                     value = 100,
                     min = 0,
                     max = 100,
                     step = 1,
                     label = tagList(
                       "Especificidad analítica (%)",
                       tooltip(
                         bs_icon("info-circle"),
                         "Probabilidad de que el método dé negativo en una muestra realmente limpia. Se obtiene de la validación del método. Junto con la sensibilidad, corrige la prevalencia observada para estimar la prevalencia verdadera."
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
    #   )
    # )

  )
}

#' F4 Server Functions
#'
#' @noRd
mod_F4_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$F1_outcome <- renderUI({

      ## Get the value of Z

      alpha <- 1 - input$conf/100
      Z <- qnorm(1 - alpha/2)

      ## Get the value of p

      # p <- switch(
      #   input$F1_p_type,
      #   unknown = 0.5,
      #   known = input$F1_p/100
      #
      # )

      p <- input$F1_p/100

      ## Get the value of d

      d <- input$d

      ## Get the value of N

      N <- input$big_N

      ## Calculate

      if (input$use_PPF) {

        print("con correccion")

        n <- N * Z^2 * p * (1-p) / ( (N-1)*d^2 + Z^2*p*(1-p) )

        ## Check whether correction is recommended

      } else {
        print("sin correccion")

        n <- Z^2 * p * (1-p)/d^2
      }

      ## Correct by specificity and sensitivity

      n <- n/(input$Sen/100 + input$Sp/100 - 1)^2

      ## Round up

      n <- ceiling(n)

      ## Build the value box

      ## Build the value box

      if (input$use_PPF) {

        if (n/N < .1) {

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


      # if (n/N < .1) {
      #   showNotification(paste("No se recomienda la corrección debido a que la fracción muestreada es",
      #                          round(n/N, 2), "< 0,1"),
      #                    type = "warning",
      #                    duration = NULL)
      # }
      #
      # value_box(title = "Número mínimo de muestras aleatorias:",
      #           value = n)

    })

  })
}

## To be copied in the UI
# mod_F4_ui("F4_1")

## To be copied in the server
# mod_F4_server("F4_1")

#' DEFF UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DEFF_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = 12,
      "Calcula el efecto de diseño (DEFF) de un muestreo, permitiendo evaluar cómo el diseño muestral influye en la precisión de las estimaciones en comparación con un muestreo aleatorio simple. Su resultado ayuda a determinar el impacto del diseño sobre la varianza y a ajustar el tamaño muestral necesario para obtener resultados fiables."
    ),
    hr(),
    layout_columns(
      col_widths = c(6,6),
      layout_columns(
        col_widths = 12,
        p(
          tagList("Resultado de muestreos anteriores",
                  tooltip(
                    bs_icon("info-circle"),
                    "Los resultados de una campaña de muestreo anterior que se utiliza para calcular la correlación.",
                    "La tabla es editable (se pueden añadir y eliminar filas haciendo click derecho)."
                  )
                  )
        ),
        rHandsontableOutput(ns("sampling_results")),
        numericInput(ns("n0"),
                     value = 200,
                     min = 0,
                     step = 1,
                     label = tagList(
                       "Tamaño muestral calculado",
                       tooltip(
                         bs_icon("info-circle"),
                         "Tamaño muestral calculado en base a los objetivos del muestreo. Será calculado en base a la correlación intra-clase estimada en base a los resultados de campañas anteriores."
                       )
                     )

                     ),
        numericInput(ns("perdidas"),
                     value = 0.9,
                     min = 0,
                     max = 1,
                     step = .1,
                     label = tagList(
                       "Coeficiente de pérdidas",
                       tooltip(
                         bs_icon("info-circle"),
                         "Factor (entre 0 y 1) que representa la proporción de aquellas muestras que se estima no puedan ser utilizadas en el monitoreo."
                       )
                     )

                     )
      ),
      uiOutput(ns("outcome"))
    )

  )
}

#' DEFF Server Functions
#'
#' @importFrom dplyr pull
#'
#' @noRd
mod_DEFF_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$sampling_results <- renderRHandsontable({

      if (!is.null(input$sampling_results)) {
        DF <- hot_to_r(input$sampling_results)

      } else {
        DF <- data.frame(Establecimiento = c("E1", "E2", "E3", "E4", "E5"),
                         `N. muestras` = c(10, 10, 10, 10, 10),
                         `N. positivos` = c(1, 2, 3, 4, 5)
        )
      }

      DF |>
        set_names(c("Establecimiento", "N. muestras", "N. positivos")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome <- renderUI({

      ## Get the data

      DF <- hot_to_r(input$sampling_results)

      ## Calculate the DEFF

      AP <- DF |>
        summarize(x = sum(`N. positivos`)/sum(`N. muestras`)) |>
        pull(x)

      DF  <- DF |>
        mutate(p = `N. positivos`/`N. muestras`,
               eps = p - AP,
               eps2 = eps^2)

      s2 <- sum(DF$eps2)/(nrow(DF) - 1)
      var_con <- s2/nrow(DF)

      var_mas <- AP*(1-AP)/sum(DF$`N. muestras`)

      DEFF <- var_con/var_mas

      ## Apply the correction

      n_out <- input$n0 * DEFF / input$perdidas
      n_out <- ceiling(n_out)

      ## Build the value box

      rho <- (DEFF - 1)/(mean(DF$`N. muestras`) - 1)

      tagList(
        layout_columns(
          col_widths = c(6,6),
          value_box(title = "Correlación estimada",
                    value = round(rho, 3)),
          value_box(title = "Corrección DEFF",
                    value = round(DEFF, 3))
        ),
        br(),
        layout_columns(
          col_widths = 12,
          value_box(title = "Número mínimo de muestras aleatorias:",
                    value = n_out)
        )
      )

    })

  })
}

## To be copied in the UI
# mod_DEFF_ui("DEFF_1")

## To be copied in the server
# mod_DEFF_server("DEFF_1")

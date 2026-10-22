#' distribucion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distribucion_ui <- function(id) {
  ns <- NS(id)
  tagList(

    ## Excel upload ------------------------------------------------------------

    fileInput(ns("excel_file"),
              tagList(
                "Configuración de muestreo",
                tooltip(
                  bs_icon("info-circle"),
                  "Archivo Excel generado por el módulo 'Muestreo basado en riesgo' describiendo el número de muestras a tomar para cada combinación peligro/alimento"
                )
              )

              ),
    selectInput(ns("excel_sheet"), "Hoja Excel a importar", choices = c()),
    actionButton(ns("load_data"), "Cargar"),

    tableOutput(ns("test_table")),

    ## Edición de cada caso

    navset_card_pill(
      title = "Diseño muestral",
      sidebar = sidebar(
        selectInput(ns("scenario_for_design"),
                    label = tagList(
                      "Combinación peligro/alimento",
                      tooltip(
                        bs_icon("info-circle"),
                        "para la que se va a realizar el diseño muestral"
                      )
                    ),
                    choices = c()
        ),
        p("Número de muestras asignadas a esta combinación:"),
        textOutput(ns("n_samples_scenario"))
      ),
      nav_panel("Distribución espacial",
                radioButtons(
                  ns("tipo_muestreo"),
                  "Tipo de diseño muestral",
                  c(`Homogénea` = "random",
                    `Estratificado proporcional al tamaño` = "size",
                    `Asignación óptima (Neyman)` = "neisy")
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "input.tipo_muestreo == 'size'",
                  layout_columns(
                    col_widths = c(6, 6),
                    layout_columns(
                      col_widths = 12,
                      tooltip(
                        bs_icon("info-circle"),
                        "",
                        "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                      ),
                      rHandsontableOutput(ns("size_input"))
                    ),
                    tableOutput(ns("outcome_prop"))
                  )
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "input.tipo_muestreo == 'neisy'",
                  layout_columns(
                    col_widths = c(6, 6),
                    layout_columns(
                      col_widths = 12,
                      tooltip(
                        bs_icon("info-circle"),
                        "",
                        "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                      ),
                      rHandsontableOutput(ns("neisy_input"))
                    ),
                    tableOutput(ns("outcome_neisy"))
                  )
                )
      ),
      nav_panel("Distribución temporal",
                radioButtons(
                  ns("tipo_temporal"),
                  "Tipo de distribución temporal",
                  c(`Distribución temporal uniforme` = "random",
                    `Distribución temporal variable (en función de la producción u ocurrencia del peligro a lo largo del año)` = "prop")
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "input.tipo_temporal == 'prop'",
                  layout_columns(
                    col_widths = c(6, 6),
                    layout_columns(
                      col_widths = 12,
                      tooltip(
                        bs_icon("info-circle"),
                        "",
                        "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
                      ),
                      rHandsontableOutput(ns("time_input"))
                    ),
                    tableOutput(ns("outcome_time"))
                  )
                )
      ),
      nav_panel("Calendario",
                tableOutput(ns("calendar_table")),
                card_footer(
                  actionButton(ns("save_calendar"), "Guardar calendario para esta combinación")
                )
                # card_footer(
                #   downloadButton(ns("download_calendar"))
                # )

      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(ns("choose_output"),
                     "",
                     choices = c(`Distribución temporal` = "time",
                                 `Distribución por combinación` = "scenario",
                                 `Total` = "all"
                     )
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_output == 'time'",
        plotOutput(ns("plot_temporal"))
        ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_output == 'scenario'",
        plotOutput(ns("plot_estrato"))
        ),
      conditionalPanel(
        ns = ns,
        condition = "input.choose_output == 'all'",
        tableOutput(ns("to_junto")),
        downloadButton(ns("download_all"))
        )
      )




    # card(
    #   card_title("To junto"),
    #   plotOutput(ns("plot_temporal")),
    #   plotOutput(ns("plot_estrato")),
    #   tableOutput(ns("to_junto"))
    # )

  )
}

#' distribucion Server Functions
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr if_else
#' @importFrom purrr imap_dfr
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs theme facet_wrap
#' @importFrom tidyr pivot_longer
#' @importFrom janitor clean_names
#'
#' @noRd
mod_distribucion_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Reactive for the data

    my_scenarios <- reactiveVal()

    ## Excel file upload -------------------------------------------------------

    excelFile <- reactive({
      input$excel_file
    })

    # Update the excel sheets

    observeEvent(excelFile(), {

      validate(need(excelFile(), message = ""))

      updateSelectInput(session = session,
                        inputId = "excel_sheet",
                        choices = excel_sheets(excelFile()$datapath)
      )

    })

    observeEvent(input$load_data, {

      validate(need(excelFile(), message = ""))

      ## Load the data

      d <- read_excel(excelFile()$datapath,
                      sheet = input$excel_sheet
                      )

      ## Get the n used

      d <- d |>
        mutate(`N utilizado` = if_else(is.na(`Tamaño muestral ajustado`),
                                       `Tamaño muestral calculado`,
                                       `Tamaño muestral ajustado`
                                       ))

      ## Update the reactive

      my_scenarios(d)

      ## Update the selector

      chosen <- d |>
        filter(`N utilizado` > 0)

      updateSelectInput(session = session,
                        inputId = "scenario_for_design",
                        choices = chosen$`Combinación peligro/alimento`
      )


    })

    ## Showing the table

    output$test_table <- renderTable({

      validate(need(my_scenarios(), message = "Sube un archivo de Excel"))

      my_scenarios()

    })


    ## Showing the number of samples in the scenario selected

    output$n_samples_scenario <- renderText({

      validate(need(my_scenarios(), message = "Sube un archivo de Excel"))

      aa <- my_scenarios() |>
        filter(`Combinación peligro/alimento` == input$scenario_for_design)

      ntot <- aa$`N utilizado`

      paste0("Number of samples: ", ntot)

    })

    ## Diseño proporcional al tamaño del estrato -------------------------------

    output$size_input <- renderRHandsontable({

      if (!is.null(input$size_input)) {
        DF <- hot_to_r(input$size_input)

      } else {
        DF <- data.frame(Estrato = c("E1", "E2", "E3"),
                         `Tamaño` = c(1000, 2000, 7000)
        )
      }

      DF |>
        set_names(c("Estrato", "Tamaño")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_prop <- renderTable({

      validate(need(my_scenarios(), message = "Sube un archivo de Excel"))

      ## Get the table

      DF <- hot_to_r(input$size_input)

      ## Get the number of samples

      aa <- my_scenarios() |>
        filter(`Combinación peligro/alimento` == input$scenario_for_design)

      ntot <- aa$`N utilizado`

      ## Calculate the distribution

      DF |>
        mutate(f = `Tamaño`/sum(`Tamaño`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    ## Diseño óptimo -----------------------------------------------------------

    output$neisy_input <- renderRHandsontable({

      if (!is.null(input$neisy_input)) {
        DF <- hot_to_r(input$neisy_input)

      } else {
        DF <- data.frame(Estrato = c("E1", "E2", "E3"),
                         `Tamaño` = c(1000, 2000, 7000),
                         `Desv.std esperada` = c(5, 3, 10)
        )
      }

      DF |>
        set_names(c("Estrato", "Tamaño", "Desv.std esperada")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_neisy <- renderTable({

      validate(need(my_scenarios(), message = "Sube un archivo de Excel"))

      ## Get the table

      DF <- hot_to_r(input$neisy_input)

      ## Get the number of samples

      aa <- my_scenarios() |>
        filter(`Combinación peligro/alimento` == input$scenario_for_design)

      ntot <- aa$`N utilizado`

      ## Calculate the distribution

      DF |>
        mutate(eps = `Tamaño`*`Desv.std esperada`,
               f = eps/sum(eps, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    ## Diseño temporal ---------------------------------------------------------

    output$time_input <- renderRHandsontable({

      if (!is.null(input$time_input)) {
        DF <- hot_to_r(input$time_input)

      } else {
        DF <- data.frame(Temporada = c("T1", "T2", "T3"),
                         `Producción` = c(1000, 2000, 7000)
        )
      }

      DF |>
        set_names(c("Temporada", "Producción")) |>
        rhandsontable() |>
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    output$outcome_time <- renderTable({

      validate(need(my_scenarios(), message = "Sube un archivo de Excel"))

      ## Get the table

      DF <- hot_to_r(input$time_input)

      ## Get the number of samples

      aa <- my_scenarios() |>
        filter(`Combinación peligro/alimento` == input$scenario_for_design)

      ntot <- aa$`N utilizado`

      ## Calculate the proportion

      DF |>
        mutate(f = `Producción`/sum(`Producción`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    ## Reactive to save the calendars ------------------------------------------

    all_calendars <- reactiveVal(list())

    ## Reactive for the calendar of the current scenario -----------------------

    this_calendar <- reactive({

      validate(
        need(my_scenarios(), "Define at least one scenario"),
      )

      ## Get the sample size

      aa <- my_scenarios() |>
        filter(`Combinación peligro/alimento` == input$scenario_for_design)

      ntot <- aa$`N utilizado`

      # browser()

      ## Get the type of estrato sampling

      if (input$tipo_muestreo == "random") {

        dist_estrato <- tibble(Estrato = "Unique", f = 1)

      } else if (input$tipo_muestreo == "size") {

        ## Get the table

        DF <- hot_to_r(input$size_input)

        ## Get the number of samples

        dist_estrato <- DF |>
          mutate(f = `Tamaño`/sum(`Tamaño`, na.rm = TRUE),
                 n = round(ntot*f)
          ) |>
          select(Estrato, f)

      } else if (input$tipo_muestreo == "neisy") {

        ## Get the table

        DF <- hot_to_r(input$neisy_input)

        dist_estrato <- DF |>
          mutate(eps = `Tamaño`*`Desv.std esperada`,
                 f = eps/sum(eps, na.rm = TRUE),
                 n = round(ntot*f)
          ) |>
          select(Estrato, f)


      }

      ## Get the type of temporal sampling

      if (input$tipo_temporal == "random") {

        dist_time <- tibble(Temporada = c("Primavera", "Verano", "Otoño", "Invierno"),
                            f_time = .25)

      } else if (input$tipo_temporal == "prop") {

        DF <- hot_to_r(input$time_input)

        dist_time <- DF |>
          mutate(f = `Producción`/sum(`Producción`, na.rm = TRUE),
                 `Número de muestras` = round(ntot*f)
          ) |>
          select(Temporada, f_time = f)

      }

      ## Scale stuff

      dist_time |>
        pull(Temporada) |>
        map_dfr(
          ~ mutate(dist_estrato, Temporada = .)
        ) |>
        left_join(dist_time) |>
        mutate(f_tot = f*f_time) |>
        mutate(n = ceiling(f_tot*ntot)) |>
        select(Estrato, Temporada, n) |>
        pivot_wider(names_from = Temporada, values_from = n)

    })

    ## Showing the calendar

    output$calendar_table <- renderTable({

      validate(
        need(this_calendar(), "")
      )

      this_calendar()

    })

    ## Saving the current calendar

    observeEvent(input$save_calendar, {

      validate(
        need(this_calendar(), "")
      )

      # browser()

      ## Get the calendars

      out <- all_calendars()

      ## Save the current one

      out[[input$scenario_for_design]] <- this_calendar()

      ## Update the reactive

      all_calendars(out)

    })

    # output$download_calendar <- downloadHandler(
    #
    #   filename = function() {
    #     paste0("sampling_calendar", ".xlsx")
    #   },
    #   content = function(file) {
    #     # Write the dataset to the `file` that will be downloaded
    #     write_xlsx(final_calendar(), file)
    #   }
    #
    # )

    ## Plot of the number of samples per stratum

    output$plot_estrato <- renderPlot({

      validate(
        need(length(all_calendars()) > 0, "")
      )

      # browser()

      all_calendars() |>
        imap_dfr(~ mutate(.x, scenario = .y)) |>
        pivot_longer(-c("Estrato", "scenario")) |>
        # summarize(n = sum(value), .by = c("scenario", "name")) |>
        ggplot() +
        geom_col(aes(x = Estrato, y = value, fill = name)) +
        facet_wrap("scenario")

    })

    ## Plot of the number of samples per time

    output$plot_temporal <- renderPlot({

      validate(
        need(length(all_calendars()) > 0, "")
      )

      # browser()

      all_calendars() |>
        imap_dfr(~ mutate(.x, scenario = .y)) |>
        pivot_longer(-c("Estrato", "scenario")) |>
        summarize(n = sum(value), .by = c("scenario", "name")) |>
        ggplot() +
        geom_col(aes(x = name, y = n, fill = scenario))


    })

    ## Table with everything

    output$to_junto <- renderTable({

      validate(
        need(length(all_calendars()) > 0, "")
      )

      all_calendars() |>
        imap_dfr(~ mutate(.x, scenario = .y)) |>
        pivot_longer(-c("Estrato", "scenario")) |>
        filter(!is.na(value)) |>
        select(Combinacion = scenario, Periodo = name, Estrato, `N muestras` = value)

    })

    ## Download the table

    output$download_all <- downloadHandler(

      filename = function() {
        paste0("overall_samples", ".xlsx")
      },
      content = function(file) {

        all <- all_calendars() |>
          imap_dfr(~ mutate(.x, scenario = .y)) |>
          pivot_longer(-c("Estrato", "scenario")) |>
          filter(!is.na(value)) |>
          select(Combinacion = scenario, Periodo = name, Estrato, `N muestras` = value)

        out <- all_calendars()

        out[["Todas las combinaciones"]] <- all

        out <- clean_names(out)

        write_xlsx(out, file)
      }

    )

  })
}

## To be copied in the UI
# mod_distribucion_ui("distribucion_1")

## To be copied in the server
# mod_distribucion_server("distribucion_1")

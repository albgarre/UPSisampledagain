
## TODO: save the parameters used for calculating the sample size (so it is tractable)

#' program UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_column_wrap navset_card_pill nav_item popover value_box card_footer card_body layout_columns card_title card tooltip layout_sidebar sidebar
#' @importFrom bsicons bs_icon
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable hot_to_r hot_table rhandsontable
#' @importFrom rlang set_names
#' @importFrom shinyWidgets prettySwitch
#'
mod_program_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      "Compara la capacidad operativo de muestreo frente a las necesidades determinadas utilizando la calculadora. Proporciona funciones para ajustar el muestreo basado en riesgo, generando una tabla resumen que se puede utilizar para alimentar al módulo de 'Distribución muestral'."
    ),
    layout_column_wrap(  # DEFINICIÓN CAPACIDAD OPERATIVA ######################
      card(
        card_title("Definición de capacidad operativa"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            layout_columns(
              col_widths = 12,
              numericInput(ns("cap_lab"), "Capacidad laboratorial", 100),
              numericInput(ns("cap_campo"), "Capacidad de campo", 80),

              prettySwitch(ns("use_cap_eur"), "Capacidad presupuestaria", FALSE,
                           fill = TRUE, bigger = TRUE, status = "success"),
              conditionalPanel(
                ns = ns,
                condition = "input.use_cap_eur",
                numericInput(ns("cap_eur"), "", 0)
              ),

              prettySwitch(ns("use_cap_acc"), "Capacidad de acceso", FALSE,
                           fill = TRUE, bigger = TRUE, status = "success"),
              conditionalPanel(
                ns = ns,
                condition = "input.use_cap_acc",
                numericInput(ns("cap_acc"), "", 0)
              )
            ),
            layout_columns(
              col_widths = 12,
              uiOutput(ns("out_capOper"))
            )
          )
        )
      )


    ),
    layout_column_wrap(  # DEFINICION DE ESCENARIOS ############################
      width = 1,
      card(
        card_title("Definición de escenarios"),
        layout_column_wrap(
          min_height = "200px",
          width = 1/4,
          textInput(ns("new_name"),
                    value = "Peligro/alimento #1",
                    label = tagList(
                      "Combinación peligro/alimento",
                      tooltip(
                        bs_icon("info-circle"),
                        "Nombre de la combinación peligro/alimento. Se mostrará para cálculos posteriores. Si ya se ha anteriormente, se sobreescriben los datos introducidos."
                      )
                      )

                    ),
          selectInput(ns("new_riskLevel"),

                      label = tagList(
                        "Clasificación de riesgo",
                        tooltip(
                          bs_icon("info-circle"),
                          "Clasificación cualitativa de riesgo determinada para esta combinación peligro/alimento."
                        )
                        ),
                      choices = c(`Muy alto` = "my_alto",
                                  Alto = "alto",
                                  Medio = "medio",
                                  Bajo = "bajo"
                      )
          ),
          selectInput(ns("new_type"),
                      label = tagList(
                        "Objetivo estadístico",
                        tooltip(
                          bs_icon("info-circle"),
                          "Objetivo del muestreo (de acuerdo a las definiciones de la calculadora)"
                        )
                      ),

                      choices = c(`Detección` = "detect",
                                  `Estimación de prevalencia` = "prev",
                                  `Estimación de concentración` = "conc"
                      )
          ),
          numericInput(ns("n_manual"),
                       value = 0,
                       min = 0,
                       step = 1,
                       label = tagList(
                         "Número de muestras",
                         tooltip(
                           bs_icon("info-circle"),
                           "Tamaño muestral para esta combinación. Se recomienda utilizar el módulo 'Calculadora tamaño muestral'"
                         )
                         )
                       )
        ),
        card_footer(
          actionButton(ns("add_scenario"), "Añadir escenario")
        )

      )

    ),

    layout_column_wrap(  # Comparing needs versus capacity----------------------
      width = 1,
      card(
        card_title("Necesidades vs. capacidad"),
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            # width = 500,
            h5(
              tagList(
                "Ajuste del muestreo",
                tooltip(
                  bs_icon("info-circle"),
                  "Permite adaptar el número de muestras para cada combinación. Se recomienda incluir una justificación que razone la adaptación del tamaño muestral"
                )
              )
            ),
            selectInput(ns("scenario_for_design"),
                        tagList("Peligro/alimento",
                                tooltip(
                                  bs_icon("info-circle"),
                                  "A modificar"
                                  )
                                ),
                        choices = c()
            ),
            numericInput(ns("new_size"),
                         "Nuevo número de muestras",
                         0,
                         min = 0,
                         step = 1
                         ),
            textInput(ns("new_justificacion"),
                      label = tagList(
                        "Priorización y justificación",
                        tooltip(
                          bs_icon("info-circle"),
                          "Razonamiento para justificar la modificación del número de muestras asignadas a esta combinación."
                        )
                      ),
                      ""
                      ),
            actionButton(ns("modify_entry"),
                         "Modificar")

          ),
          tableOutput(ns("summary_cases")),
          uiOutput(ns("boxes_sizes")),
          # layout_columns(
          #   col_widths = c(6,6),
          #   uiOutput(ns("boxes_sizes"))
          # ),
          layout_columns(
            col_widths = 12,
            uiOutput(ns("box_comparison"))
          )

        ),
        card_footer(
          downloadButton(ns("download_selection"))
        )
      )
    ),
    # layout_column_wrap(  # Diseño muestral -------------------------------------
    #
    #   navset_card_pill(
    #     title = "Diseño muestral",
    #     sidebar = sidebar(
    # selectInput(ns("scenario_for_design"),
    #             "Escenario",
    #             choices = c()
    #             ),
    #       textOutput(ns("n_samples_scenario"))
    #     ),
    #     nav_panel("Distribución espacial",
    #               radioButtons(
    #                 ns("tipo_muestreo"),
    #                 "Tipo de diseño muestral",
    #                 c(`Homogénea` = "random",
    #                   `Estratificado proporcional al tamaño` = "size",
    #                   `Asignación óptima (Neyman)` = "neisy")
    #                 ),
    #               conditionalPanel(
    #                 ns = ns,
    #                 condition = "input.tipo_muestreo == 'size'",
    #                 layout_columns(
    #                   col_widths = c(6, 6),
    #                   layout_columns(
    #                     col_widths = 12,
    #                     tooltip(
    #                       bs_icon("info-circle"),
    #                       "",
    #                       "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
    #                     ),
    #                     rHandsontableOutput(ns("size_input"))
    #                   ),
    #                   tableOutput(ns("outcome_prop"))
    #                 )
    #               ),
    #               conditionalPanel(
    #                 ns = ns,
    #                 condition = "input.tipo_muestreo == 'neisy'",
    #                 layout_columns(
    #                   col_widths = c(6, 6),
    #                   layout_columns(
    #                     col_widths = 12,
    #                     tooltip(
    #                       bs_icon("info-circle"),
    #                       "",
    #                       "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
    #                     ),
    #                     rHandsontableOutput(ns("neisy_input"))
    #                   ),
    #                   tableOutput(ns("outcome_neisy"))
    #                 )
    #               )
    #               ),
    #     nav_panel("Distribución temporal",
    #               radioButtons(
    #                 ns("tipo_temporal"),
    #                 "Tipo de distribución temporal",
    #                 c(`Distribución temporal uniforme` = "random",
    #                   `Distribución temporal variable (en función de la producción u ocurrencia del peligro a lo largo del año)` = "prop")
    #                 ),
    #               conditionalPanel(
    #                 ns = ns,
    #                 condition = "input.tipo_temporal == 'prop'",
    #                 layout_columns(
    #                   col_widths = c(6, 6),
    #                   layout_columns(
    #                     col_widths = 12,
    #                     tooltip(
    #                       bs_icon("info-circle"),
    #                       "",
    #                       "La tabla es editable. Se pueden añadir y eliminar filas haciendo click derecho"
    #                     ),
    #                     rHandsontableOutput(ns("time_input"))
    #                   ),
    #                   tableOutput(ns("outcome_time"))
    #                 )
    #               )
    #               ),
    #     nav_panel("Calendario",
    #               tableOutput(ns("calendar_table")),
    #               card_footer(
    #                 downloadButton(ns("download_calendar"))
    #               )
    #
    #               ),
    #     )
    #
    # )
  )
}

#' program Server Functions
#'
#' @importFrom purrr map map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr summarize pull mutate filter select left_join
#' @importFrom tidyr pivot_wider
#' @importFrom writexl write_xlsx
#'
#' @noRd
mod_program_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    ## Capacidad operativa

    cap_oper <- reactive({

      x <- c(input$cap_lab, input$cap_campo)

      if (input$use_cap_eur) {

        x <- c(x, input$cap_eur)

      }

      if (input$use_cap_acc) {
        x <- c(x, input$cap_acc)
      }

      min(x)

    })

    output$out_capOper <- renderUI({

      validate(
        need(cap_oper(), "")
      )


      value_box(title = "Capacidad operativa",
                value = cap_oper())

    })



    ## Reactives for the scenarios

    my_scenarios <- reactiveVal(list())



    ## Calculation of the sampling size

    n_detection <- reactive({ # Sample size by detection prevalence

      ## Get the value of C

      C <- switch(
        input$F1_C_type,
        graves = 0.95,
        explor = 0.9,
        export = 0.99,
        user = input$F1_C/100

      )

      ## Get the value of p

      p <- switch(
        input$F1_p_type,
        graves = 0.01,
        rutina = 0.05,
        user = input$F1_p/100

      )

      ## Get the SeU

      if (input$use_SeU) {

        SeU <- input$SeU/100

      } else {

        SeU <- 1

      }

      ## Calculate

      n <- log(1-C)/log(1-p*SeU)
      ceiling(n)

    })

    output$F1_outcome <- renderUI({

      validate(
        need(n_detection(), ""),
      )

      ## Build the value box

      value_box(title = "Número mínimo de muestras aleatorias:",
                value = n_detection())

    })

    ## Adding scenarios

    observeEvent(input$add_scenario, {

      out_scenarios <- my_scenarios()

      ## Get the metadata

      new_scenario <- list()

      new_scenario[["name"]] <- input$new_name
      new_scenario[["risk_level"]] <- input$new_riskLevel
      new_scenario[["type"]] <- input$new_type
      new_scenario[["n"]] <- input$n_manual
      new_scenario[["comments"]] <- ""

      ## Update the reactive

      out_scenarios[[input$new_name]] <- new_scenario

      my_scenarios(out_scenarios)

    })

    ## Comparison of capacity and requirements

    output$summary_cases <- renderTable({

      validate(
        need(length(my_scenarios()) > 0, "Define at least one scenario"),
      )

      # browser()

      my_scenarios() |>
        map_dfr(
          ~ tibble(`Combinación peligro/alimento` = .$name,
                   `Clasificación de riesgo` = .$risk_level,
                   `Objetivo estadístico` = .$type,
                   `Tamaño muestral calculado` = .$n,
                   `Tamaño muestral ajustado` = .$n_mod,
                   `Priorización y justificación` = .$comments
          ))

    })

    output$boxes_sizes <- renderUI({

      validate(
        need(length(my_scenarios()) > 0, ""),
      )

      ## Calculate the numbers

      n_needed <- my_scenarios() |>
        map_dfr(
          ~ tibble(
                   n = .$n,

          )) |>
        summarize(n = sum(n)) |>
        pull(n)

      n_oper <- cap_oper()

      ## Return the boxes

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          value_box(title = "Capacidad operativa",
                    value = cap_oper()),
          value_box(title = "Número total de muestras",
                    value = n_needed)
        )
      )
    })

    output$box_comparison <- renderUI({

      validate(
        need(length(my_scenarios()) > 0, ""),
      )

      ## Calculate the numbers

      n_needed <- my_scenarios() |>
        map_dfr(
          ~ tibble(
            n = .$n,

          )) |>
        summarize(n = sum(n)) |>
        pull(n)

      n_oper <- cap_oper()

      cap <- n_oper - n_needed

      ## Return the box

      if (cap > 0) {

        value_box(title = "Hay un excendente de capacidad operativa",
                  value = cap,
                  theme = "primary")

      } else if (cap == 0) {

        value_box(title = "Hay una concordancia perfecta",
                  value = cap,
                  theme = "success")

      } else {

        value_box(title = "Hay una falta de capacidad operativa",
                  value = cap,
                  theme = "danger")

      }

    })

    ## Diseño muestral ---------------------------------------------------------

    observeEvent(my_scenarios(), {

      validate(
        need(length(my_scenarios()) > 0, "")
      )

      ## Get the scenarios with at least one

      sampled_scenarios <- my_scenarios() |>
        map_dfr(
          ~ tibble(scenario = .$name,
                   n = .$n
          )) |>
        # filter(n > 0) |>
        pull(scenario)

      updateSelectInput(
        session = session,
        "scenario_for_design",
        choices = sampled_scenarios
      )


    })

    output$n_samples_scenario <- renderText({

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      paste0("Number of samples: ", ntot)

    })

    output$size_input <- renderRHandsontable({  # Proportional estrato

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

      ## Get the table

      DF <- hot_to_r(input$size_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(f = `Tamaño`/sum(`Tamaño`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    output$neisy_input <- renderRHandsontable({  # Neyman

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

      ## Get the table

      DF <- hot_to_r(input$neisy_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(eps = `Tamaño`*`Desv.std esperada`,
               f = eps/sum(eps, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    output$time_input <- renderRHandsontable({  # Proportional to production

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

      ## Get the table

      DF <- hot_to_r(input$time_input)

      ## Get the number of samples

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

      DF |>
        mutate(f = `Producción`/sum(`Producción`, na.rm = TRUE),
               `Número de muestras` = round(ntot*f)
        )

    })

    ## Calendar

    final_calendar <- reactive({

      validate(
        need(length(my_scenarios()) > 0, "Define at least one scenario"),
      )

      ## Get the sample size

      ntot <- my_scenarios()[[input$scenario_for_design]]$n

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

    output$calendar_table <- renderTable({

      validate(
        need(final_calendar(), "")
      )

      final_calendar()

    })

    output$download_calendar <- downloadHandler(

      filename = function() {
        paste0("sampling_calendar", ".xlsx")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write_xlsx(final_calendar(), file)
      }

    )

    ## Modifying the scenarios

    observeEvent(input$modify_entry, {

      validate(
        need(my_scenarios(), "")
      )

      out <- my_scenarios()

      out[[input$scenario_for_design]]$n_mod <- input$new_size
      out[[input$scenario_for_design]]$comments <- input$new_justificacion

      my_scenarios(out)

    })

    ## Downloading the table

    output$download_selection <- downloadHandler(

      filename = function() {
        paste0("escenarios", ".xlsx")
      },
      content = function(file) {

        validate(
          need(length(my_scenarios()) > 0, "")
        )

        out <- my_scenarios() |>
          map_dfr(
            ~ tibble(`Combinación peligro/alimento` = .$name,
                     `Clasificación de riesgo` = .$risk_level,
                     `Objetivo estadístico` = .$type,
                     `Tamaño muestral calculado` = .$n,
                     `Tamaño muestral ajustado` = .$n_mod,
                     `Priorización y justificación` = .$comments
            ))


        write_xlsx(out, file)
      }

    )
  })
}

## To be copied in the UI
# mod_program_ui("program_1")

## To be copied in the server
# mod_program_server("program_1")

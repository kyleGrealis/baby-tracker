# app/view/trends.R

box::use(
  dplyr[filter, group_by, mutate, n, summarize, ],
  lubridate[floor_date, ],
  echarts4r[
    e_charts, echarts4rOutput, e_color, e_line,
    e_tooltip, e_x_axis, e_y_axis, renderEcharts4r,
  ],
  htmlwidgets[JS, ],
  shiny[br, div, moduleServer, NS, reactive, selectInput, tagList, ],
)

box::use(
  app / logic / db,
  app / logic / events,
)


#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    div(
      class = "btn-row",
      selectInput(
        ns("time_range"),
        label = "Time range:",
        choices = c("This week" = "this_week", "Weekly" = "weekly")
      ),

      # The choice value maps to the event_cat in the sqlite table
      selectInput(
        ns("category"),
        label = "Category:",
        choices = c(
          "Diapers" = "diaper",
          "Feedings" = "food",
          "Naps" = "nap",
          "Sleep" = "bed"
        )
      ),
    ),
    div(
      class = "charts-output",
      style = "border: 1px solid black;",
      echarts4rOutput(ns("charts"))
    )
  )
}


#' Trend charts server
#'
#' Renders dropdowns and echarts4r outputs to show weekly or
#'   this week's trends.
#'
#' @param id Character. The module's namespace ID.
#' @param con DBI connection to the SQLite database.
#' @param trigger Reactive value (counter) that fires on data
#'   changes, used to invalidate and refresh the log table.
#' @export
server <- function(id, con, trigger) {
  moduleServer(id, function(input, output, session) {
    all_events <- reactive({
      trigger()
      db$get_all_events(con) |>
        mutate(date = as.Date(timestamp))
    })

    plot_data <- reactive({
      event_data <- all_events() |>
        filter(event_cat == input$category)

      if (input$time_range == "this_week") {
        event_data <- event_data |>
          filter(date > Sys.Date() - 7)
      } else {
        # Round each date down to its week-start (Sunday).
        # The prep functions group by `date`, so collapsing
        # dates to week boundaries makes them aggregate by week.
        event_data <- event_data |>
          mutate(date = floor_date(date, "week"))
      }

      switch(input$category,
        "diaper" = events$prep_diapers(event_data),
        "food" = events$prep_feedings(event_data),
        "nap" = events$prep_naps(event_data),
        "bed" = events$prep_sleep(event_data)
      )
    })

    output$charts <- renderEcharts4r({
      dat <- plot_data()
      y_label <- attr(dat, "y_label")
      colors <- attr(dat, "colors")

      if (input$time_range == "this_week") {
        x_type <- "time"
        x_fmt <- JS("
          function(value) {
            var d = new Date(value);
            var m = ['Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec'];
            return m[d.getMonth()] + ' ' + d.getDate();
          }
        ")
      } else {
        x_type <- "category"
        x_fmt <- JS("
          function(value) {
            var d = new Date(value);
            var m = ['Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec'];
            return 'Wk ' + m[d.getMonth()] + ' ' + d.getDate();
          }
        ")
      }

      dat |>
        e_charts(date) |>
        e_line(n) |>
        e_color(colors) |>
        e_y_axis(name = y_label) |>
        e_x_axis(
          type = x_type,
          axisLabel = list(formatter = x_fmt)
        ) |>
        e_tooltip(
          backgroundColor = "#e0e0e0",
          formatter = JS("
            function(params) {
              return params.seriesName + ': ' + params.value[1]
                + '<br>Date: ' + params.value[0];
            }
          ")
        )
    })
  })
}

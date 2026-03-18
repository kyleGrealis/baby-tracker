# app/view/trends.R

box::use(
  dplyr[filter, group_by, group_vars, mutate, n, summarize, ungroup, ],
  echarts4r[
    e_charts, echarts4rOutput, e_color, e_line,
    e_tooltip, e_x_axis, e_y_axis, renderEcharts4r,
  ],
  htmlwidgets[JS, ],
  rlang[syms, ],
  shiny[
    br, div, moduleServer, NS, reactive,
    selectInput, tagList,
  ],
  tidyr[complete, ],
)

box::use(
  app / logic / db,
  app / logic / events,
)


# Fill missing dates with 0s so the x-axis spans the full range.
# Dynamically reads the grouping column set by the prep function
# (label, event_type, or series) so we don't need a per-category
# switch. complete() drops custom attributes, so save & restore.
pad_dates <- function(data, dates) {
  y_lab <- attr(data, "y_label")
  cols <- attr(data, "colors")
  grp <- syms(group_vars(data))
  data <- ungroup(data)
  data <- complete(
    data,
    date = dates, !!!grp,
    fill = list(n = 0)
  ) |> group_by(!!!grp)
  attr(data, "y_label") <- y_lab
  attr(data, "colors") <- cols
  data
}


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
#' Renders echarts4r line charts showing daily or weekly
#'   trends for diapers, feedings, naps, and sleep.
#'
#' @param id Character. The module's namespace ID.
#' @param con DBI connection to the SQLite database.
#' @param trigger Reactive value (counter) that fires on data
#'   changes, used to invalidate and refresh the chart.
#' @export
server <- function(id, con, trigger) {
  moduleServer(id, function(input, output, session) {
    all_events <- reactive({
      trigger()
      db$get_all_events(con) |>
        mutate(date = as.Date(timestamp))
    })

    plot_data <- reactive({
      is_weekly <- input$time_range == "weekly"

      event_data <- all_events() |>
        filter(event_cat == input$category)

      if (is_weekly) {
        # Bucket into 7-day windows anchored to today.
        # e.g. today Mar 17: bucket 1 = Mar 11-17,
        # bucket 2 = Mar 4-10, etc. Each bucket's date
        # is the last day (end) of that window.
        today <- Sys.Date()
        event_data <- event_data |>
          mutate(
            days_ago = as.integer(today - date),
            date = today - (days_ago %/% 7L) * 7L
          )
      } else {
        event_data <- event_data |>
          filter(date > Sys.Date() - 7)
      }

      result <- switch(input$category,
        "diaper" = events$prep_diapers(event_data),
        "food" = events$prep_feedings(event_data),
        "nap" = events$prep_naps(event_data),
        "bed" = events$prep_sleep(event_data)
      )

      if (is_weekly) {
        # Divide totals by 7 for daily averages
        result <- result |> mutate(n = round(n / 7, 1))
        attr(result, "y_label") <- paste(
          "Avg Daily", attr(result, "y_label")
        )
      } else {
        # Pad missing dates so x-axis spans the full range.
        # Sleep ends at yesterday (tonight hasn't happened);
        # everything else ends at today.
        end_date <- if (input$category == "bed") {
          Sys.Date() - 1L
        } else {
          Sys.Date()
        }
        result <- pad_dates(
          result, seq(Sys.Date() - 6L, end_date, by = "day")
        )
      }

      result
    })

    output$charts <- renderEcharts4r({
      dat <- plot_data()
      y_label <- attr(dat, "y_label")
      colors <- attr(dat, "colors")

      x_fmt <- if (input$time_range == "this_week") {
        JS("
          function(value) {
            var p = String(value).split('-');
            var m = ['Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec'];
            return m[parseInt(p[1],10) - 1] +
              ' ' + parseInt(p[2],10);
          }
        ")
      } else {
        JS("
          function(value) {
            var p = String(value).split('-');
            var y = parseInt(p[0],10);
            var mo = parseInt(p[1],10) - 1;
            var dy = parseInt(p[2],10);
            var end = new Date(y, mo, dy);
            var start = new Date(y, mo, dy - 6);
            var m = ['Jan','Feb','Mar','Apr','May','Jun',
                     'Jul','Aug','Sep','Oct','Nov','Dec'];
            var eL = (start.getMonth() === end.getMonth())
              ? end.getDate()
              : m[end.getMonth()] + ' ' + end.getDate();
            return m[start.getMonth()] + ' ' +
              start.getDate() + '-' + eL;
          }
        ")
      }

      dat |>
        e_charts(date) |>
        e_line(n) |>
        e_color(colors) |>
        e_y_axis(name = y_label) |>
        e_x_axis(
          type = "category",
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

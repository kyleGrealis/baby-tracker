# app/view/summary_cards.R
# Rolling 7-day average cards: diapers, feedings, sleep

box::use(
  shiny[
    div, moduleServer, NS, renderUI, span, tagList, uiOutput,
  ],
)

box::use(
  app / logic / db,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  div(
    class = "summary-cards",
    uiOutput(ns("cards"))
  )
}

#' Summary cards server
#'
#' Reads the reactive trigger to refresh card values whenever
#' events are added, edited, or deleted. Calls
#' db$get_weekly_summary() to compute rolling 7-day averages.
#'
#' @param id Character. The module's namespace ID.
#' @param con DBI connection to the SQLite database.
#' @param trigger Reactive value (counter) that fires on data
#'   changes, used to invalidate and refresh the card output.
#' @export
server <- function(id, con, trigger) {
  moduleServer(id, function(input, output, session) {
    output$cards <- renderUI({
      trigger()
      stats <- db$get_weekly_summary(con)

      tagList(
        card(
          "Diapers",
          span(class = "stat-poopy", stats$poopy),
          " poopy / ",
          span(class = "stat-wet", stats$wet),
          " wet"
        ),
        card(
          "Feedings",
          span(class = "stat-value", stats$feedings),
          " / day"
        ),
        card(
          "Sleep",
          span(class = "stat-value", stats$naps),
          " naps / ",
          span(class = "stat-value", stats$sleep_hrs),
          " hrs"
        )
      )
    })
  })
}

#' Build a single summary card div
#'
#' @param title Character. The card heading (e.g. "Diapers").
#' @param ... Additional tag elements for the card value row.
card <- function(title, ...) {
  div(
    class = "summary-card",
    div(class = "card-title", title),
    div(class = "card-value", ...),
    # div(class = "card-subtitle", "7-day avg")
  )
}

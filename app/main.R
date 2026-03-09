# main.R

box::use(
  shiny[bootstrapPage, div, h1, moduleServer, NS, reactiveVal, tags, ],
)

box::use(
  app / logic / db,
  app / view / event_inputs,
  app / view / event_log,
  app / view / summary_cards,
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    title = "Baby Care Tracker",
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    div(
      class = "app-header",
      h1("Baby Care Tracker")
    ),
    div(
      class = "components-container",
      event_inputs$ui(ns("event_inputs")),
      summary_cards$ui(ns("summary_cards")),
      event_log$ui(ns("event_log"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # --- Database setup ---
    # connect() opens (or creates) the SQLite file.
    # init_db() creates the events table on first run.
    # onSessionEnded() closes the connection when the user leaves.
    con <- db$connect()
    db$init_db(con)
    session$onSessionEnded(function() {
      db$disconnect(con)
    })

    # --- Reactive trigger ---
    # A simple counter. Every time we bump it, any output that
    # reads trigger() knows to re-execute. This is how we tell
    # the log table "hey, new data was inserted, refresh yourself."
    trigger <- reactiveVal(0)
    bump <- function() trigger(trigger() + 1)

    event_inputs$server("event_inputs", con, bump, trigger)
    summary_cards$server("summary_cards", con, trigger)
    event_log$server("event_log", con, trigger, bump)
  })
}

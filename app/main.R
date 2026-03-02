box::use(
  shiny[
    br,
    bootstrapPage, div, h1, h2, h4, hr,
    actionButton, HTML,
    wellPanel,
    moduleServer, NS,
    observeEvent, reactiveVal,
    tags,
    textAreaInput, updateTextAreaInput,
  ],
  reactable[
    reactable, reactableOutput, renderReactable,
    colDef, JS
  ],
  app / logic / db,
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
      class = "header-row",
      h1("Baby Care Tracker"),
      tags$a(
        href = "https://github.com/kyleGrealis/baby-tracker",
        target = "_blank",
        class = "github-link",
        title = "View source on GitHub",
        HTML(
          "<svg xmlns='http://www.w3.org/2000/svg'
           viewBox='0 0 16 16' width='28' height='28'
           fill='currentColor'><path d='M8 0C3.58 0 0
           3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55
           -.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37
           -2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13
           -.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23
           .82.72 1.21 1.87.87 2.33.66.07-.52.28-.9.48
           -1.13-1.69-.19-3.46-.84-3.46-3.74 0-.83.29
           -1.51.77-2.04-.08-.19-.34-.97.07-2.01 0 0
           .63-.2 2.07.77.6-.17.83-.28 1.72-.28s1.12.1
           1.72.28c1.44-.99 2.07-.77 2.07-.77.41 1.04
           .15 1.82.07 2.01.48.53.77 1.21.77 2.04 0
           2.9-1.77 3.55-3.46 3.74.27.24.49.69.49 1.38
           0 1-.01 1.8-.01 2.05 0 .21.15.46.55.38A8.01
           8.01 0 0 0 16 8c0-4.42-3.58-8-8-8z'/></svg>"
        )
      )
    ),
    div(
      class = "components-container",
      wellPanel(
        class = "input-panel",
        h2("Events"),
        div(
          class = "baby-cat diaper",
          h4("Diaper:"),
          div(
            class = "btn-row",
            actionButton(ns("poop"), "Poopy 💩", class = "btn-poop"),
            actionButton(ns("wet"), "Wet 💦", class = "btn-wet"),
          )
        ),
        div(
          class = "baby-cat food",
          h4("Feeding:"),
          div(
            class = "btn-row",
            actionButton(
              ns("food_start"), "Start 🍼",
              class = "btn-start"
            ),
            actionButton(
              ns("food_stop"), "Stop 🛑",
              class = "btn-stop"
            ),
          )
        ),
        div(
          class = "baby-cat sleep",
          h4("Sleeping:"),
          div(
            class = "btn-row",
            actionButton(
              ns("sleep_start"), "Start 💤",
              class = "btn-start"
            ),
            actionButton(
              ns("sleep_stop"), "Stop 🛑",
              class = "btn-stop"
            ),
          )
        ),
        br(),
        hr(),
        div(
          class = "baby-cat bed",
          h2("Nighttime", class = "bed"),
          div(
            class = "btn-row",
            actionButton(
              ns("night_start"), "Goodnight 🌛",
              class = "btn-night"
            ),
            actionButton(
              ns("night_stop"),
              "Good morning 🌞",
              class = "btn-morning"
            ),
          )
        ),
        br(),
        hr(),
        div(
          h4("Notes:", class = "notes"),
          textAreaInput(
            ns("misc_text"), NULL,
            placeholder = "miscellaneous notes...",
            height = "100px",
            resize = "none"
          ),
          actionButton(ns("submit"), "Submit", class = "btn-submit"),
        )
      ),
      div(
        class = "output-log",
        h4("Today's Log"),
        hr(),
        reactableOutput(ns("log"))
      )
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

    # --- Event observers ---
    # Each button: write a row to the DB, then bump the trigger.
    observeEvent(input$poop, {
      db$log_event(con, "diaper", "poop", "Poopy diaper")
      bump()
    })
    observeEvent(input$wet, {
      db$log_event(con, "diaper", "wet", "Wet diaper")
      bump()
    })
    observeEvent(input$food_start, {
      db$log_event(con, "food", "food_start", "Feeding started")
      bump()
    })
    observeEvent(input$food_stop, {
      db$log_event(con, "food", "food_stop", "Feeding ended")
      bump()
    })
    observeEvent(input$sleep_start, {
      db$log_event(con, "sleep", "sleep_start", "Sleeping started")
      bump()
    })
    observeEvent(input$sleep_stop, {
      db$log_event(con, "sleep", "sleep_stop", "Sleeping ended")
      bump()
    })
    observeEvent(input$night_start, {
      db$log_event(con, "bed", "night_start", "Goodnight")
      bump()
    })
    observeEvent(input$night_stop, {
      db$log_event(con, "bed", "night_stop", "Good morning")
      bump()
    })
    observeEvent(input$submit, {
      db$log_event(con, "misc", "note", input$misc_text,
        notes = input$misc_text
      )
      updateTextAreaInput(session, "misc_text", value = "")
      bump()
    })

    # --- Delete observer ---
    # When a delete button is clicked in the table, JS sends
    # the row's database id back to R via Shiny.setInputValue().
    # We catch it here, delete the row, and refresh.
    observeEvent(input$delete_event, {
      db$delete_event(con, input$delete_event)
      bump()
    })

    # --- Log display ---
    # reactable renders an interactive table. The delete column
    # uses a JS cell renderer to create a clickable button.
    #
    # Shiny.setInputValue() is the JS-to-R bridge: it creates
    # a custom input that fires observeEvent() above.
    # The ns() prefix is needed because we're inside a Shiny
    # module -- without it, the input wouldn't route correctly.
    ns <- session$ns
    delete_input_id <- ns("delete_event")

    output$log <- renderReactable({
      trigger()
      events <- db$get_todays_events(con)

      # Add a placeholder column for the delete buttons.
      # reactable needs it in the data frame even though
      # the cell content is rendered entirely by JS.
      events$delete <- NA

      reactable(
        events,
        columns = list(
          id = colDef(show = FALSE),
          time = colDef(name = "Time", width = 80),
          label = colDef(name = "Event"),
          delete = colDef(
            name = "",
            sortable = FALSE,
            width = 50,
            cell = JS(sprintf("
              function(cellInfo) {
                return React.createElement('button', {
                  onClick: function() {
                    Shiny.setInputValue('%s',
                      cellInfo.row.id,
                      {priority: 'event'}
                    )
                  },
                  style: {
                    border: 'none',
                    background: 'none',
                    cursor: 'pointer',
                    fontSize: '16px'
                  }
                }, '\\u274c')
              }
            ", delete_input_id))
          )
        ),
        pagination = FALSE,
        striped = TRUE,
        borderless = TRUE,
        compact = TRUE
      )
    })
  })
}

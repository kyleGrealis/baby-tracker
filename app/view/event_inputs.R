# app/view/event_inputs.R

box::use(
  shiny[
    actionButton, br, dateInput, div, h2, h4, hr,
    modalDialog, moduleServer, NS, observe, observeEvent,
    removeModal, selectInput, showModal, showNotification,
    tagList, textAreaInput, textInput,
    updateActionButton, updateTextAreaInput, wellPanel,
  ],
)

box::use(
  app / logic / db,
  app / logic / events,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
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
          actionButton(ns("both"), "Both 💦💩", class = "btn-both"),
        )
      ),
      div(
        class = "baby-cat food",
        h4("Feeding:"),
        div(
          class = "btn-row",
          actionButton(ns("food_start"), "Start 🍼", class = "btn-start"),
          actionButton(ns("food_stop"), "Stop 🛑", class = "btn-stop"),
        )
      ),
      div(
        class = "baby-cat sleep",
        h4("Napping:"),
        div(
          class = "btn-row",
          actionButton(ns("nap_start"), "Start 💤", class = "btn-start"),
          actionButton(ns("nap_stop"), "Stop 🛑", class = "btn-stop"),
        )
      ),
      br(),
      hr(),
      div(
        class = "baby-cat bed",
        h2("Bedtime", class = "bed"),
        div(
          class = "btn-row",
          actionButton(ns("night_start"), "Goodnight 🌛", class = "btn-night"),
          actionButton(ns("night_stop"), "Good morning 🌞", class = "btn-morning"),
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
      ),
      br(),
      hr(),
      actionButton(ns("retro_open"), "Add past event", class = "btn-retro")
    )
  )
}

#' Event inputs server
#'
#' Handles button clicks for logging care events (diapers,
#' feeding, naps, bedtime, notes) and the retroactive add modal.
#'
#' @param id Character. The module's namespace ID.
#' @param con DBI connection to the SQLite database.
#' @param bump Function that increments the reactive trigger,
#'   signaling other modules to refresh their data.
#' @param trigger Reactive value that fires on every data change,
#'   used to refresh button badge counts.
#' @export
server <- function(id, con, bump, trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update button labels with today's counts
    observe({
      trigger()
      counts <- db$get_today_counts(con)
      badge <- function(base, n) {
        if (n > 0) paste0(base, " (", n, ")") else base
      }
      updateActionButton(
        session, "poop",
        label = badge("Poopy 💩", counts$poop)
      )
      updateActionButton(
        session, "wet",
        label = badge("Wet 💦", counts$wet)
      )
      updateActionButton(
        session, "both",
        label = badge("Both 💦💩", counts$both)
      )
      updateActionButton(
        session, "food_start",
        label = badge("Start 🍼", counts$food_start)
      )
    })

    # Each button: write a row to the DB, then bump the trigger.
    observeEvent(input$poop, {
      db$log_event(con, "diaper", "poop", "Poopy diaper")
      bump()
    })
    observeEvent(input$wet, {
      db$log_event(con, "diaper", "wet", "Wet diaper")
      bump()
    })
    observeEvent(input$both, {
      db$log_event(con, "diaper", "both", "Wet & poopy diaper")
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
    observeEvent(input$nap_start, {
      db$log_event(con, "nap", "nap_start", "Napping started")
      bump()
    })
    observeEvent(input$nap_stop, {
      db$log_event(con, "nap", "nap_stop", "Napping ended")
      bump()
    })
    observeEvent(input$night_start, {
      db$log_event(con, "bed", "night_start", "Goodnight, Baby")
      bump()
    })
    observeEvent(input$night_stop, {
      db$log_event(con, "bed", "night_stop", "Good morning, Baby")
      bump()
    })
    observeEvent(input$submit, {
      db$log_event(
        con, "misc", "note", input$misc_text,
        notes = input$misc_text
      )
      updateTextAreaInput(session, "misc_text", value = "")
      bump()
    })

    # --- Retroactive add modal ---
    observeEvent(input$retro_open, {
      showModal(modalDialog(
        title = "Add past event",
        selectInput(
          ns("retro_type"), "Event type:",
          choices = events$event_choices
        ),
        dateInput(
          ns("retro_date"), "Date:",
          value = Sys.Date()
        ),
        textInput(
          ns("retro_time"), "Time (HH:MM):",
          value = format(Sys.time(), "%H:%M"),
          placeholder = "14:30"
        ),
        textAreaInput(
          ns("retro_notes"), "Notes (optional):",
          placeholder = "additional details...",
          height = "60px", resize = "none"
        ),
        footer = div(
          actionButton(
            ns("retro_cancel"), "Cancel",
            class = "btn btn-default"
          ),
          actionButton(
            ns("retro_submit"), "Add",
            class = "btn btn-wet"
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$retro_cancel, removeModal())

    observeEvent(input$retro_submit, {
      time_str <- input$retro_time
      if (!grepl("^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$", time_str)) {
        showNotification(
          "Invalid time. Use HH:MM (e.g. 14:30).",
          type = "error"
        )
        return()
      }
      ts <- paste0(input$retro_date, " ", time_str, ":00")
      info <- events$event_map[[input$retro_type]]
      notes <- if (nzchar(input$retro_notes)) {
        input$retro_notes
      } else {
        NA
      }
      db$log_event_at(
        con, ts, info$cat, input$retro_type,
        info$label, notes
      )
      removeModal()
      bump()
    })
  })
}

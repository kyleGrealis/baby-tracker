# app/view/event_log.R

box::use(
  reactable[
    colDef, JS, reactable, reactableOutput, renderReactable,
  ],
  shiny[
    actionButton, dateInput, dateRangeInput, div,
    eventReactive, modalDialog, moduleServer,
    NS, observe, observeEvent, reactive, reactiveVal,
    removeModal, req, selectInput, showModal, showNotification,
    span, tagList, textAreaInput, textInput,
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
    div(
      class = "output-log",
      div(
        class = "log-toolbar",
        div(
          class = "date-nav",
          actionButton(
            ns("prev_day"),
            "\u2190 Previous Day",
            class = "btn-link date-nav-link"
          ),
          span(class = "date-nav-sep", "|"),
          actionButton(
            ns("next_day"),
            "Next Day \u2192",
            class = "btn-link date-nav-link"
          )
        ),
        actionButton(
          ns("search_open"), "🔍",
          class = "btn-search"
        )
      ),
      reactableOutput(ns("log"))
    )
  )
}

#' Event log server
#'
#' Renders the reactable log, handles row-click editing,
#' inline deletion, date navigation, and the search modal.
#'
#' @param id Character. The module's namespace ID.
#' @param con DBI connection to the SQLite database.
#' @param trigger Reactive value (counter) that fires on data
#'   changes, used to invalidate and refresh the log table.
#' @param bump Function that increments the reactive trigger,
#'   signaling other modules to refresh their data.
#' @export
server <- function(id, con, trigger, bump) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Date navigation ────────────────────────────────
    viewing_date <- reactiveVal(Sys.Date())

    is_today <- reactive({
      viewing_date() == Sys.Date()
    })

    observeEvent(input$prev_day, {
      viewing_date(viewing_date() - 1)
    })

    observeEvent(input$next_day, {
      req(!is_today())
      viewing_date(viewing_date() + 1)
    })

    observe({
      session$sendCustomMessage("toggle-btn", list(
        id = ns("next_day"),
        disabled = is_today()
      ))
    })

    # ── Delete observer ────────────────────────────────
    observeEvent(input$delete_event, {
      db$delete_event(con, input$delete_event)
      bump()
    })

    # ── Edit modal (row click) ─────────────────────────
    observeEvent(input$edit_row, {
      row_idx <- input$edit_row
      rows <- db$get_events_for_date(con, viewing_date())
      row <- rows[row_idx, ]
      ts <- as.POSIXct(row$timestamp)
      session$userData$editing_event_id <- row$id

      showModal(modalDialog(
        title = "Edit event",
        selectInput(
          ns("edit_type"), "Event type:",
          choices = events$event_choices,
          selected = row$event_type
        ),
        dateInput(
          ns("edit_date"), "Date:",
          value = as.Date(row$timestamp)
        ),
        textInput(
          ns("edit_time"), "Time (HH:MM):",
          value = format(ts, "%H:%M"),
          placeholder = "14:30"
        ),
        textAreaInput(
          ns("edit_notes"), "Notes (optional):",
          value = if (is.na(row$notes)) "" else row$notes,
          height = "80px", resize = "none"
        ),
        footer = div(
          class = "edit-modal-footer",
          actionButton(
            ns("edit_delete"), "Delete",
            class = "btn btn-stop"
          ),
          div(
            actionButton(
              ns("edit_cancel"), "Cancel",
              class = "btn btn-default"
            ),
            actionButton(
              ns("edit_save"), "Save",
              class = "btn btn-wet"
            )
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$edit_cancel, removeModal())

    observeEvent(input$edit_save, {
      time_str <- input$edit_time
      if (!grepl("^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$", time_str)) {
        showNotification(
          "Invalid time. Use HH:MM (e.g. 14:30).",
          type = "error"
        )
        return()
      }
      ts <- paste0(input$edit_date, " ", time_str, ":00")
      info <- events$event_map[[input$edit_type]]
      notes <- if (nzchar(input$edit_notes)) {
        input$edit_notes
      } else {
        NA
      }
      db$update_event(
        con,
        session$userData$editing_event_id,
        ts, info$cat, input$edit_type, info$label, notes
      )
      removeModal()
      bump()
    })

    observeEvent(input$edit_delete, {
      db$delete_event(
        con, session$userData$editing_event_id
      )
      removeModal()
      bump()
    })

    # ── Log display ────────────────────────────────────
    delete_input_id <- ns("delete_event")
    edit_row_id <- ns("edit_row")

    output$log <- renderReactable({
      trigger()
      viewing_date()
      rows <- db$get_events_for_date(con, viewing_date())
      is_note <- rows$event_type == "note"
      rows$label[is_note] <- "NOTE"
      rows$notes[is.na(rows$notes)] <- ""
      rows$category <- toupper(rows$event_cat)
      rows$delete <- NA
      rows <- rows[, c(
        "id", "time", "category", "label", "notes",
        "delete", "event_cat", "event_type", "timestamp"
      )]

      reactable(
        rows,
        onClick = JS(sprintf("
          function(rowInfo, column) {
            if (column.id !== 'delete') {
              Shiny.setInputValue('%s',
                rowInfo.index + 1,
                {priority: 'event'}
              )
            }
          }
        ", edit_row_id)),
        columns = list(
          id = colDef(show = FALSE),
          event_cat = colDef(show = FALSE),
          event_type = colDef(show = FALSE),
          timestamp = colDef(show = FALSE),
          time = colDef(name = "Time", width = 65),
          category = colDef(
            name = "Category", width = 130
          ),
          label = colDef(name = "Event", width = 130),
          notes = colDef(name = "Notes"),
          delete = colDef(
            name = "",
            sortable = FALSE,
            width = 50,
            cell = JS(sprintf("
              function(cellInfo) {
                return React.createElement('button', {
                  onClick: function(e) {
                    e.stopPropagation()
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

    # ── Search modal ───────────────────────────────────
    observeEvent(input$search_open, {
      showModal(modalDialog(
        title = "Search Events",
        size = "l",
        div(
          class = "search-filters",
          selectInput(
            ns("search_cat"), "Category:",
            choices = c(
              "All" = "",
              "Diaper" = "diaper",
              "Feeding" = "food",
              "Nap" = "nap",
              "Bedtime" = "bed",
              "Notes" = "misc"
            ),
            selected = ""
          ),
          dateRangeInput(
            ns("search_dates"), "Date range:",
            start = Sys.Date() - 30,
            end = Sys.Date()
          ),
          textInput(
            ns("search_keyword"),
            "Keyword in notes:",
            placeholder = "search text..."
          ),
          actionButton(
            ns("search_run"), "Search",
            class = "btn btn-wet"
          )
        ),
        reactableOutput(ns("search_results")),
        footer = actionButton(
          ns("search_close"), "Close",
          class = "btn btn-default"
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$search_close, removeModal())

    search_data <- eventReactive(input$search_run, {
      db$search_events(
        con,
        category = input$search_cat,
        date_from = input$search_dates[1],
        date_to = input$search_dates[2],
        keyword = input$search_keyword
      )
    })

    output$search_results <- renderReactable({
      results <- search_data()
      results$notes[is.na(results$notes)] <- ""

      reactable(
        results[, c("date", "time", "label", "notes")],
        columns = list(
          date = colDef(name = "Date", width = 100),
          time = colDef(name = "Time", width = 65),
          label = colDef(name = "Event"),
          notes = colDef(name = "Notes")
        ),
        pagination = TRUE,
        defaultPageSize = 20,
        striped = TRUE,
        borderless = TRUE,
        compact = TRUE
      )
    })
  })
}

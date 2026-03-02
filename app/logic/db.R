# app/logic/db.R
# All database operations for Baby Care Tracker
#
# Uses an in-memory SQLite database -- each Shiny session starts
# fresh and data is lost on reload. DBI is R's universal interface
# for talking to databases; RSQLite is the SQLite driver.

box::use(
  DBI[dbConnect, dbDisconnect, dbExecute],
  dbplyr[...],
  dplyr[tbl, filter, collect, arrange, desc, mutate, select],
  RSQLite[SQLite],
)

#' Open an in-memory SQLite connection
#' Each call creates a brand-new empty database
#' @export
connect <- function() {
  dbConnect(SQLite(), ":memory:")
}

#' Safely close the database connection
#' Always pair with connect() -- open the pipe, do work, close it.
#' @export
disconnect <- function(con) {
  dbDisconnect(con)
}

#' Create the events table if it doesn't already exist
#' "IF NOT EXISTS" makes this safe to run every time the app starts
#' @export
init_db <- function(con) {
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS events (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      timestamp  TEXT    NOT NULL,
      event_cat  TEXT    NOT NULL,
      event_type TEXT    NOT NULL,
      label      TEXT    NOT NULL,
      notes      TEXT
    )
  ")
}

#' Insert a care event into the database
#'
#' The ? placeholders are "parameterized queries" -- they prevent
#' SQL injection (where malicious text in user input could run
#' unwanted SQL commands). The params list fills in the ?s safely.
#'
#' @param con DBI connection from connect().
#' @param event_cat Category: "diaper", "food", "sleep", "bed".
#' @param event_type Short key: "poop", "wet", "food_start", etc.
#' @param label Human-readable: "Poopy diaper", "Feeding started".
#' @param notes Free-text for note events. NULL for button events.
#' @export
log_event <- function(con, event_cat, event_type, label, notes = NA) {
  dbExecute(
    con,
    "INSERT INTO events (timestamp, event_cat, event_type, label, notes)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      event_cat,
      event_type,
      label,
      notes
    )
  )
}

#' Get today's events as a tibble, newest first
#'
#' This is where dbplyr shines. tbl() creates a *lazy* reference
#' to the SQL table -- no data is fetched yet. filter() and
#' arrange() build up a SQL query behind the scenes. collect()
#' actually runs the query and pulls the results into R as a
#' tibble you can work with normally.
#'
#' @param con DBI connection from connect().
#' @return A tibble with columns: id, time, label.
#' @export
get_todays_events <- function(con) {
  today <- format(Sys.Date(), "%Y-%m-%d")

  # This chain builds SQL like:
  #   SELECT * FROM events
  #   WHERE timestamp >= '2026-03-01'
  #   ORDER BY timestamp DESC
  # ...then collect() executes it and brings results into R
  tbl(con, "events") |>
    filter(timestamp >= today) |>
    arrange(desc(timestamp)) |>
    collect() |>
    mutate(time = format(as.POSIXct(timestamp), "%H:%M")) |>
    select(id, time, label)
}

#' Delete an event by its database ID
#' @param con DBI connection from connect().
#' @param event_id The integer id of the row to remove.
#' @export
delete_event <- function(con, event_id) {
  dbExecute(
    con,
    "DELETE FROM events WHERE id = ?",
    params = list(event_id)
  )
}

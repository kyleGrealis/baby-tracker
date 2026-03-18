# app/logic/db.R
# All database operations for Baby Care Tracker
#
# SQLite is a "file database" -- no server needed. The entire DB
# lives in a single file. DBI is R's universal interface for
# talking to databases; RSQLite is the SQLite driver.
#
# Two modes controlled by the DB_PATH environment variable:
#
# 1. Demo mode (default, DB_PATH not set):
#    Each Shiny session gets its own temporary copy of the seed
#    database (care_tracker.sqlite). Changes never leak between
#    visitors. The temp copy is deleted when the session ends.
#
# 2. Persistent mode (DB_PATH set in .Renviron):
#    All sessions share a single database file. Use this when
#    you fork the repo and want to keep your own data.

box::use(
  DBI[dbConnect, dbDisconnect, dbExecute, dbGetQuery],
  dbplyr[...],
  dplyr[arrange, collect, desc, filter, lead, mutate, select, tbl, ],
  RSQLite[SQLite],
)

# nolint start: object_name_linter
DB_PATH <- Sys.getenv("DB_PATH", "")
SEED_DB <- "care_tracker.sqlite"
# nolint end

#' Open a connection to the SQLite database
#'
#' In demo mode (no DB_PATH), copies the seed database to a temp
#' file so each session is isolated. In persistent mode, connects
#' directly to the file specified by DB_PATH.
#' @export
connect <- function() {
  if (nzchar(DB_PATH)) {
    dbConnect(SQLite(), DB_PATH)
  } else {
    tmp <- tempfile(fileext = ".sqlite")
    if (file.exists(SEED_DB)) {
      file.copy(SEED_DB, tmp)
    }
    dbConnect(SQLite(), tmp)
  }
}

#' Close the database connection
#'
#' In demo mode, also deletes the temp database file.
#' @export
disconnect <- function(con) {
  db_path <- con@dbname
  dbDisconnect(con)
  if (!nzchar(DB_PATH)) {
    unlink(db_path)
  }
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
#' @param event_cat Category: "diaper", "food", "nap", "bed", "misc".
#' @param event_type Short key: "poop", "wet", "food_start", etc.
#' @param label Human-readable: "Poopy diaper", "Feeding started".
#' @param notes Free-text for note events. NULL for button events.
#' @export
log_event <- function(con, event_cat, event_type, label, notes = NA) {
  dbExecute(
    con,
    "INSERT INTO events
       (timestamp, event_cat, event_type, label, notes)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      event_cat, event_type, label, notes
    )
  )
}

#' Get all events as a tibble, sorted chronologically
#'
#' @param con DBI connection from connect().
#' @return A tibble with all events.
#' @export
get_all_events <- function(con) {
  tbl(con, "events") |>
    arrange(timestamp) |>
    collect()
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

  tbl(con, "events") |>
    filter(timestamp >= today) |>
    arrange(desc(timestamp)) |>
    collect() |>
    mutate(
      time = format(as.POSIXct(timestamp), "%H:%M")
    ) |>
    select(id, time, label, event_cat, event_type, notes, timestamp)
}

#' Get events for a specific calendar date
#'
#' @param con DBI connection from connect().
#' @param date Date object. Events on this calendar day.
#' @return A tibble with columns: id, time, label, event_cat,
#'   event_type, notes, timestamp.
#' @export
get_events_for_date <- function(con, date) {
  day_start <- format(date, "%Y-%m-%d")
  day_end <- format(date + 1, "%Y-%m-%d")

  tbl(con, "events") |>
    filter(timestamp >= day_start, timestamp < day_end) |>
    arrange(desc(timestamp)) |>
    collect() |>
    mutate(
      time = format(as.POSIXct(timestamp), "%H:%M")
    ) |>
    select(id, time, label, event_cat, event_type, notes, timestamp)
}

#' Search events across the full database
#'
#' @param con DBI connection from connect().
#' @param category Optional event_cat filter.
#' @param date_from Optional Date. Start of range.
#' @param date_to Optional Date. End of range (inclusive).
#' @param keyword Optional substring match in notes.
#' @return A tibble of matching events.
#' @export
search_events <- function(
  con,
  category = NULL,
  date_from = NULL,
  date_to = NULL,
  keyword = NULL
) {
  q <- tbl(con, "events")

  if (!is.null(category) && nzchar(category)) {
    cat_val <- category
    q <- q |> filter(event_cat == cat_val)
  }
  if (!is.null(date_from)) {
    from_str <- format(date_from, "%Y-%m-%d")
    q <- q |> filter(timestamp >= from_str)
  }
  if (!is.null(date_to)) {
    to_str <- format(date_to + 1, "%Y-%m-%d")
    q <- q |> filter(timestamp < to_str)
  }

  result <- q |>
    arrange(desc(timestamp)) |>
    collect() |>
    mutate(
      date = format(as.POSIXct(timestamp), "%Y-%m-%d"),
      time = format(as.POSIXct(timestamp), "%H:%M")
    ) |>
    select(date, time, label, event_cat, event_type, notes)

  if (!is.null(keyword) && nzchar(keyword)) {
    kw <- tolower(keyword)
    result <- result |>
      filter(grepl(kw, tolower(notes), fixed = TRUE))
  }

  result
}

#' Insert a care event with a custom timestamp
#'
#' Same as log_event() but accepts an explicit timestamp instead
#' of using Sys.time(). Used for retroactive logging.
#'
#' @param con DBI connection from connect().
#' @param timestamp Character in "%Y-%m-%d %H:%M:%S" format.
#' @param event_cat Category: "diaper", "food", "nap", "bed".
#' @param event_type Short key: "poop", "wet", "food_start", etc.
#' @param label Human-readable: "Poopy diaper", "Feeding started".
#' @param notes Free-text for note events. NA for button events.
#' @export
log_event_at <- function(
  con, timestamp, event_cat, event_type, label, notes = NA
) {
  dbExecute(
    con,
    "INSERT INTO events
       (timestamp, event_cat, event_type, label, notes)
     VALUES (?, ?, ?, ?, ?)",
    params = list(
      timestamp, event_cat, event_type, label, notes
    )
  )
}

#' Update an existing event by ID
#'
#' @param con DBI connection from connect().
#' @param event_id The integer id of the event to update.
#' @param timestamp Character in "%Y-%m-%d %H:%M:%S" format.
#' @param event_cat Category: "diaper", "food", "nap", "bed".
#' @param event_type Short key: "poop", "wet", "food_start", etc.
#' @param label Human-readable: "Poopy diaper", "Feeding started".
#' @param notes Free-text notes. NA for no notes.
#' @export
update_event <- function(
  con, event_id, timestamp, event_cat, event_type, label, notes = NA
) {
  dbExecute(
    con,
    "UPDATE events
     SET timestamp = ?, event_cat = ?, event_type = ?,
         label = ?, notes = ?
     WHERE id = ?",
    params = list(
      timestamp, event_cat, event_type, label, notes, event_id
    )
  )
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

#' Compute rolling 7-day averages for summary cards
#'
#' Pulls the last 7 days of events, then computes:
#' - Poopy & wet diaper counts per day ("both" increments each)
#' - Feedings per day (one per food_start)
#' - Naps per day and total sleep hours per day (naps + nighttime)
#'
#' Sleep duration uses start/stop pairing: for each category
#' (nap, bed), events are sorted by time and each start is
#' matched with the next stop. Unpaired starts (still sleeping)
#' are ignored.
#'
#' @param con DBI connection from connect().
#' @return Named list: poopy, wet, feedings, naps, sleep_hrs
#' @export
get_weekly_summary <- function(con) {
  week_ago <- format(Sys.Date() - 7, "%Y-%m-%d")

  events <- tbl(con, "events") |>
    filter(timestamp >= week_ago) |>
    arrange(timestamp) |>
    collect()

  n_days <- 7

  # Diaper counts: "both" increments both poopy AND wet
  poopy <- sum(
    events$event_type %in% c("poop", "both")
  ) / n_days
  wet <- sum(
    events$event_type %in% c("wet", "both")
  ) / n_days

  # Feeding count: each food_start = one feeding
  feedings <- sum(
    events$event_type == "food_start"
  ) / n_days

  # Nap count: each nap_start = one nap
  naps <- sum(
    events$event_type == "nap_start"
  ) / n_days

  # Sleep hours: pair start/stop events for nap + bed categories
  sleep_hrs <- calc_sleep_hours(events, n_days)

  list(
    poopy = round(poopy, 1),
    wet = round(wet, 1),
    feedings = round(feedings, 1),
    naps = round(naps, 1),
    sleep_hrs = round(sleep_hrs, 1)
  )
}

#' Pair start/stop events and compute average daily sleep hours
#'
#' For each sleep category (nap, bed), sorts events by time,
#' then uses lead() to look ahead one row. When a start event's
#' next row is the matching stop, the duration is the difference
#' between their timestamps.
#'
#' lead() is a dplyr function that shifts a column "up" by one
#' row -- so for each row you can see what comes next. This
#' avoids manual loops for pairing.
#'
#' @param events Tibble with timestamp and event_type columns.
#' @param n_days Number of days to average over.
#' @return Numeric: average sleep hours per day.
calc_sleep_hours <- function(events, n_days) {
  sleep_types <- list(
    nap = c("nap_start", "nap_stop"),
    bed = c("night_start", "night_stop")
  )

  total_hrs <- 0

  for (types in sleep_types) {
    start_type <- types[1]
    stop_type <- types[2]

    pairs <- events |>
      filter(event_type %in% types) |>
      mutate(
        time = as.POSIXct(timestamp, tz = "")
      ) |>
      arrange(time) |>
      mutate(
        next_type = lead(event_type),
        next_time = lead(time)
      ) |>
      filter(
        event_type == start_type,
        next_type == stop_type
      )

    if (nrow(pairs) > 0) {
      durations <- as.numeric(
        difftime(pairs$next_time, pairs$time, units = "hours")
      )
      total_hrs <- total_hrs + sum(durations)
    }
  }

  total_hrs / n_days
}

#' Count today's diaper and feeding events for button badges
#'
#' @param con DBI connection from connect().
#' @return Named list: poop, wet, both, food_start counts.
#'   Poop/wet counts include "both" events.
#' @export
get_today_counts <- function(con) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  raw <- dbGetQuery(
    con,
    "SELECT event_type, COUNT(*) as n FROM events
     WHERE timestamp >= ?
       AND event_type IN ('poop', 'wet', 'both', 'food_start')
     GROUP BY event_type",
    params = list(today)
  )
  ct <- function(type) {
    val <- raw$n[raw$event_type == type]
    if (length(val) == 0) 0L else val
  }
  list(
    poop = ct("poop") + ct("both"),
    wet = ct("wet") + ct("both"),
    both = ct("both"),
    food_start = ct("food_start")
  )
}

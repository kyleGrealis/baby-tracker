# app/logic/events.R

box::use(
  dplyr[arrange, filter, group_by, lead, mutate, n, summarize, ],
  stats[setNames, ],
)

# --- Event type mapping ---
# Maps event_type keys to (cat, label) pairs. Used by the
# retroactive add and edit modals.
event_map <- list(
  note = list(cat = "misc", label = "Note"),
  poop = list(cat = "diaper", label = "Poopy diaper"),
  wet = list(cat = "diaper", label = "Wet diaper"),
  both = list(cat = "diaper", label = "Wet & poopy diaper"),
  food_start = list(cat = "food", label = "Feeding started"),
  food_stop = list(cat = "food", label = "Feeding ended"),
  nap_start = list(
    cat = "nap", label = "Napping started"
  ),
  nap_stop = list(
    cat = "nap", label = "Napping ended"
  ),
  night_start = list(
    cat = "bed", label = "Goodnight, Baby"
  ),
  night_stop = list(
    cat = "bed", label = "Good morning, Baby"
  )
)

# Choices vector for selectInput dropdowns
event_choices <- setNames(
  names(event_map),
  vapply(event_map, `[[`, character(1), "label")
)

# Note
# "note"

# Poopy diaper     Wet diaper      Both
# "poop"           "wet"           "both"

# Feeding started      Feeding ended
# "food_start"         "food_stop"

# Napping started      Napping ended
# "nap_start"          "nap_stop"

# Goodnight, Baby      Good morning, Baby
# "night_start"        "night_stop"


# Prepare events plot data (app/view/trends.R)
#' @param events Tibble. Event data filtered to category
prep_diapers <- function(events) {
  result <- events |>
    mutate(label = paste0(label, "s")) |>
    summarize(n = n(), .by = c(date, label)) |>
    group_by(label)

  attr(result, "y_label") <- "Number of Diapers"
  attr(result, "colors") <- c("#d7ccc8", "#f48fb1", "#90caf9")
  result
}

prep_feedings <- function(events) {
  result <- events |>
    filter(event_type == "food_start") |>
    mutate(event_type = "Feedings") |>
    summarize(n = n(), .by = c(date, event_type)) |>
    group_by(event_type)

  attr(result, "y_label") <- "Number of Feedings"
  result
}

prep_naps <- function(events) {
  result <- events |>
    filter(event_type %in% c("nap_start", "nap_stop")) |>
    mutate(time = as.POSIXct(timestamp)) |>
    arrange(time) |>
    mutate(
      next_type = lead(event_type),
      next_time = lead(time)
    ) |>
    filter(event_type == "nap_start", next_type == "nap_stop") |>
    mutate(
      hours = as.numeric(difftime(next_time, time, units = "hours"))
    ) |>
    summarize(n = round(sum(hours), 1), .by = date) |>
    mutate(series = "Nap hours") |>
    group_by(series)

  attr(result, "y_label") <- "Hours of Napping"
  result
}

prep_sleep <- function(events) {
  result <- events |>
    filter(event_type %in% c("night_start", "night_stop")) |>
    mutate(time = as.POSIXct(timestamp)) |>
    arrange(time) |>
    mutate(
      next_type = lead(event_type),
      next_time = lead(time)
    ) |>
    filter(
      event_type == "night_start", next_type == "night_stop"
    ) |>
    mutate(
      hours = as.numeric(difftime(next_time, time, units = "hours"))
    ) |>
    summarize(n = round(sum(hours), 1), .by = date) |>
    mutate(series = "Sleep hours") |>
    group_by(series)

  attr(result, "y_label") <- "Hours of Nightly Sleep"
  result
}

# app/logic/events.R

box::use(
  stats[setNames],
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

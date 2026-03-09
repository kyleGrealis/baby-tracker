# Baby Care Tracker

A Shiny app for tracking baby care events: diapers, feedings, naps, bedtime, and freeform notes. Built with [Rhino](https://appsilon.github.io/rhino/) and SQLite.

> Try the [live demo](https://shiny.kylegrealis.com/baby-tracker) to see it in action.

## Features

- **One tap logging** for diapers (poopy, wet, both), feedings, naps, and bedtime
- **Retroactive entries** to log events that happened earlier in the day
- **Daily event log** with date navigation to browse past days
- **Full database search** by category, date range, and keyword
- **7 day rolling averages** displayed as summary cards
- **Edit and delete** any event by tapping its row

The demo ships with sample data so you can explore every feature immediately. Each demo visitor gets their own isolated session, so feel free to add, edit, and delete events without worrying about other users.


## Getting Started

### Prerequisites

- R (>= 4.1.0)
- Node.js (>= 16) for building SCSS and JS assets

### Run the Demo Locally

```bash
git clone https://github.com/kyleGrealis/baby-tracker.git
cd baby-tracker
```

```r
renv::restore()   # install R dependencies
rhino::app()      # launch the app
```

The app starts in **demo mode** by default. Every browser session gets its own temporary copy of the included sample database. Nothing you do persists after closing the tab.


## Make It Yours

To switch from demo mode to persistent storage where your data is saved across sessions, create a `.Renviron` file in the project root with one line:

```
DB_PATH=my_baby.sqlite
```

Restart the app and you are all set. A new SQLite file is created automatically with an empty events table. If you want to start completely fresh without the sample data in the repo, delete `care_tracker.sqlite` before your first run.

That is the only required change. Everything else works out of the box.

### Optional Customizations

**App title:** Edit the two `"Baby Care Tracker"` strings in `app/main.R`.

**Bedtime labels:** The goodnight/good morning button labels live in two places:
- `app/logic/events.R` (the `event_map` list, used by modals)
- `app/view/event_inputs.R` (the `observeEvent` handlers for `night_start` and `night_stop`)

**Styling:** All styles live in `app/styles/main.scss`. Button colors, layout breakpoints, and fonts are all in that one file.


## Project Structure

```
app/
  main.R              Root Shiny module (UI shell + server wiring)
  logic/
    db.R              Database operations (connect, query, insert, update, delete)
    events.R          Event type definitions and dropdown choices
  view/
    event_inputs.R    Button panel + retroactive add modal
    event_log.R       Reactable table + edit modal + search modal
    summary_cards.R   7 day rolling average cards
  js/
    index.js          Custom Shiny message handler (button toggle)
  styles/
    main.scss         All app styling (desktop grid + mobile responsive)
care_tracker.sqlite   Seed database with sample events (used in demo mode)
```


## How the Database Works

The app uses SQLite, a file based database that needs no server. The `events` table has this schema:

| Column | Type | Description |
|--------|------|-------------|
| id | INTEGER | Auto incrementing primary key |
| timestamp | TEXT | Event time as `YYYY-MM-DD HH:MM:SS` |
| event_cat | TEXT | Category: diaper, food, nap, bed, misc |
| event_type | TEXT | Specific type: poop, wet, both, food_start, food_stop, nap_start, nap_stop, night_start, night_stop, note |
| label | TEXT | Human readable label shown in the log |
| notes | TEXT | Optional freeform text |

### Demo Mode vs Persistent Mode

When `DB_PATH` is **not set** (the default), each Shiny session copies `care_tracker.sqlite` to a temp file. The session operates on that copy, and the temp file is deleted when the session ends. This means every visitor starts with the same sample data and their changes are completely isolated.

When `DB_PATH` **is set**, all sessions connect to that single file. Data persists across restarts and is shared between concurrent users (ideal for a household where two parents use the same app).


## Development

During development, use `rhino::devmode()` to get live rebuilds of SCSS and JS on save. You do not need to run `rhino::build_sass()` or `rhino::build_js()` manually.

```r
# Start dev mode (rebuilds assets on save)
rhino::devmode()

# Lint checks
rhino::lint_r()
rhino::lint_js()
rhino::lint_sass()

# Run tests
rhino::test_r()
```

Before deploying, build the production assets:

```r
rhino::build_sass()
rhino::build_js()
```


## Deployment

This repo does not include deployment configuration because every hosting setup is different. A few common options:

- **shinyapps.io**: Follow the [shinyapps.io deployment guide](https://docs.posit.co/shinyapps.io/getting-started.html). Set `DB_PATH` in the app settings.
- **Posit Connect / Shiny Server**: Deploy via git or rsconnect. Set `DB_PATH` as an environment variable in your server config.
- **Self hosted (Raspberry Pi, VPS, etc.)**: Clone the repo on your server, install dependencies with `renv::restore()`, build assets, and point your Shiny Server site config at the project directory.

Whichever route you choose, make sure `DB_PATH` is set in the server environment so the app runs in persistent mode.


## Tech Stack

- [Rhino](https://appsilon.github.io/rhino/) for app structure and build tooling
- [Shiny](https://shiny.posit.co/) as the web framework
- [box](https://klmr.me/box/) for module imports (no `library()` calls)
- [RSQLite](https://rsqlite.r-dbi.org/) + [DBI](https://dbi.r-dbi.org/) for database operations
- [dbplyr](https://dbplyr.tidyverse.org/) + [dplyr](https://dplyr.tidyverse.org/) for query building
- [reactable](https://glin.github.io/reactable/) for the interactive data table


## License

MIT

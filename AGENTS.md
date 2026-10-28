# AGENTS.md

## Project Goals
- Recreate ESPN NBA data access (schedule, summaries, box scores, betting).
- Collect raw JSON for entire seasons.
- Parse raw data into tidy, analysis-ready tables.
- Ensure the pipeline is reproducible, auditable, testable, and scalable to new seasons.

## Definition of Done
For a given `season` and `season_type`, the pipeline can:
- Determine all expected completed game IDs.
- Confirm raw JSON exists (or explain why it does not).
- Parse raw JSON into stable tables.
- Validate schema and row counts.
- Reload results identically on reruns.

## Plan (Live Checklist)
### Phase A - Data Collection (Scrape & Store Raw)
- [x] Scrape works for a single game.
- [x] Scrape works for a week of games.
- [x] Scrape works for a full season.
- [x] Game IDs come from scoreboard/schedule endpoint.
- [x] Raw JSON saved one file per game.
- [x] Scraping is idempotent (skip if file exists unless `overwrite = TRUE`).
- [x] Robust HTTP handling (retries, backoff).
- [x] Configurable rate limiting (`pause_sec`).
- [x] Phase A tests (naming helpers, safe summary).

### Phase B - Inventory & Completeness (Manifest)
- [x] Compute expected completed games for season.
- [x] List scraped JSON files on disk.
- [x] Compare expected vs scraped (missing, duplicates, naming issues).
- [x] Detect data availability per game (team box, player box, betting).
- [x] Save manifest `manifest_<season>_<season_type>.csv`.
- [x] `validate_season()` fails if completeness < threshold.

### Phase C - Parsing (Raw JSON -> Tidy Tables)
- [x] Parsers accept already-loaded JSON (not file paths).
- [x] Missing sections return empty tibbles with correct columns.
- [x] Column names standardized (`janitor::clean_names`).
- [x] Explicit type casting (integers, numerics, dates).
- [x] Central schema map defines required/optional columns and NA defaults.
- [x] Target tables:
  - games (1 row per game)
  - team_box (2 rows per game)
  - player_box (N rows per game)
  - betting_* (0-N rows per game)

### Phase D - Data Quality Checks
- [x] Row counts: team_box = 2 rows per completed game.
- [x] Row counts: player_box > 0 rows per completed game.
- [x] Key integrity: no duplicate (game_id, team_id).
- [x] Key integrity: no duplicate (game_id, athlete_id).
- [x] Missingness report per column.
- [x] Spot-check early, mid, and late season samples.
- [x] Validation helper returns issues + samples for manual review.

### Phase E - Storage & Reproducibility
- [x] Write parsed tables to database (SQLite).
- [x] Add indices (game_id, team_id, athlete_id, game_date).
- [x] Incremental update strategy (detect new games, scrape->parse->upsert).
- [x] Schema version table.
- [x] Save parsed tables to RDS for quick reloads.

### Phase F - Testing & Regression Protection
- [x] Unit tests using saved JSON fixtures.
- [x] Tests do not depend on live ESPN endpoints.
- [x] Optional integration test (skipped by default).
- [x] Tests cover parsing columns, types, season validation.

## Agent Operating Rules
- Work incrementally: write function -> write test -> run tests.
- Never change output schemas without updating tests.
- Prefer deterministic outputs (no randomness).
- Log errors instead of crashing during collection.
- Fail only during validation, not mid-scrape.
- Assume future seasons will be scraped.

## Final Note
- Raw JSON = truth.
- Parsed tables = contract.
- Models and dashboards = consumers.
- Do not move to modeling until Phases A-D are green.

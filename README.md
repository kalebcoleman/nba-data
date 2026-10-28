# NBAData

End-to-end ESPN NBA data pipeline: raw JSON -> parsed tables -> RDS, SQLite, or CSV.

## Quickstart: raw JSON to parsed tables

```r
# 1) Collect raw JSON for a season
collect_raw_season(2024, raw_dir = "data/raw")

# Raw files are stored in:
# data/raw/<season>/summary_<season>_<YYYYMMDD>_<game_id>.json

# 2) Parse raw JSON into tidy tables
tables <- espn_nba_parse_raw_dir(2024, raw_dir = "data/raw")

# 3) Validate shape + keys
v <- validate_parsed_tables(tables, warn_only = TRUE)

# 4) Write parsed tables (choose one or many)
write_parsed_tables(tables, out_dir = "data/parsed", format = "rds", season = 2024)
write_parsed_tables(tables, out_dir = "data/parsed", format = "csv")
write_parsed_tables(tables, out_dir = "data/parsed", format = "sqlite")
```

## SQLite (recommended source of truth)

```r
con <- nba_db_connect(db = "data/sql/nbadata.sqlite")
nba_db_init(con)

tables <- espn_nba_parse_raw_dir(2024, raw_dir = "data/raw")
write_parsed_tables(tables, format = "sqlite", mode = "upsert", con = con)

nba_db_disconnect(con)
```

Schema versioning is tracked in `nbadata_meta` via `NBA_SCHEMA_VERSION` and migrations in `R/migrations.R`.

## One-call pipeline (collect -> parse -> store)

```r
out_2024 <- collect_parse_store(2024, raw_dir = "data/raw")
out_2025 <- collect_parse_store(
  2025,
  raw_dir = "data/raw",
  ingest_db = TRUE,
  db_path = "data/sql/nbadata.sqlite",
  db_mode = "upsert",
  validate = FALSE
)
```

## RDS -> SQLite ingest

```r
nba_db_ingest_rds(
  2026,
  parsed_dir = "data/parsed",
  db_path = "data/sql/nbadata.sqlite",
  validate = FALSE
)
```

## Manifest + missing reports

```r
m_2024 <- espn_nba_manifest(2024, season_type = "regular", raw_dir = "data/raw")
m_2025 <- espn_nba_manifest(2025, season_type = "regular", raw_dir = "data/raw")
```

This writes:
- `manifest_<season>_<season_type>.csv`
- `missing_boxscore_<season>_<season_type>.csv` (postponed/canceled excluded)

## Live integration test (optional)

```sh
NBDATA_LIVE=true Rscript -e "testthat::test_file('tests/testthat/test-integration-live.R')"
```

## Notes

- Parsed tables: `games`, `team_box`, `player_box`, and `betting_*`.
- Raw JSON is the source of truth; parsed tables are the contract.

## Data and terms of use

This repository contains code only. Raw JSON, parsed tables, and the SQLite
database are not committed; rebuild them locally with `collect_parse_store()`.
The coverage manifests in `data/raw/<season>/` are kept so you can see which
games were collected.

The data comes from ESPN's public site API (`site.api.espn.com`). That API is
undocumented and not licensed for redistribution, so treat any data you collect
as personal, non-commercial research data and do not republish it. The collector
drops editorial content (`news`, `article`, `videos`) before writing raw files.

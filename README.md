# GAMMa — Gap Analysis & Metacollection Management

GAMMa is an R/Shiny application that lets a curator assemble an occurrence
dataset for one plant taxon (from [GBIF](https://www.gbif.org) and/or their own
CSV/XLSX accession records), clean it interactively on a map and in tables,
and then run an *ex situ* conservation gap analysis (SRS / GRS / ERS / FCS)
with a downloadable HTML report. The metrics follow the
[GapAnalysis](https://github.com/CIAT-DAPA/GapAnalysis) R package methodology.

## Running locally

Requirements: R 4.6 (renv.lock pins 4.6.1; the Docker image uses the same)
and the system libraries used by `sf`/`terra` (GDAL, GEOS, PROJ, udunits). Then, from the repository root:

```r
install.packages("renv")
renv::restore()        # installs the exact package versions in renv.lock
shiny::runApp()        # or source("run.R")
```

`app.R` sources `global.R` (package attachment, static data loaded once per
process) and defines the UI/server; every module and helper lives in `R/`,
which Shiny auto-sources before evaluating `app.R`.

## Repository layout

| Path | Purpose |
|---|---|
| `app.R`, `global.R` | Entry point and process-level setup |
| `R/module_*.R` | Shiny modules: landing, data analysis (controls, map, tables), gap analysis, about |
| `R/gbif_functions.R` | Pure functions for the GBIF download pipeline |
| `R/gap_analysis_functions.R` | Local implementations of `SRSex`, `GRSex`, `ERSex` and score helpers |
| `R/leaflet_maps.R`, `R/utils.R` | Palettes, map builders, schema enforcement, footer |
| `R/fctn_read_upload_file.R` | CSV/XLSX upload parser |
| `R/report_render.R` | Renders the HTML report with litedown (no pandoc) |
| `reportTemplate.Rmd`, `reportTemplate.css` | Parameterised HTML report and its stylesheet |
| `VERSION` | App version shown in the footer and the report |
| `run.R`, `uploadtoshinyio.R` | Local run shortcut; shinyapps.io deploy script (not deployed) |
| `appData/` | Static data shipped with the app (see below) plus markdown/YAML copy |
| `www/` | CSS and images |
| `tests/testthat/` | Unit tests (`Rscript tests/testthat.R`) |
| `dev/` | Diagnostic scripts, not deployed |
| `preprocessing/` | One-off scripts that build the static layers, not deployed |
| `manifest.json`, `Dockerfile`, `.dockerignore` | Posit Connect git-backed deployment manifest; container image recipe and its exclusions |
| `.github/workflows/tests.yml` | CI: runs the unit tests on push / PR |

## Static data provenance

| File | Source |
|---|---|
| `appData/plant_taxonomy_lean.parquet` | Subset of the GBIF Backbone Taxonomy (Plantae): `taxonID, canonicalName, genericName, specificEpithet, infraspecificEpithet, taxonRank, taxonomicStatus`. Build script not yet in repo. |
| `appData/land_simple.gpkg` | Natural Earth 10 m land minus lakes, simplified to 10 % of vertices (`preprocessing/downloadWater.R`). |
| `appData/ecoregionsSimplified.gpkg` | Terrestrial ecoregions (`ECO_NAME`), simplified. Build script not yet in repo. |
| `appData/upload_sample*.csv/xlsx`, `Magnolia_acuminata_data*.csv` | Test fixtures for the upload flow and the unit tests. Excluded from the deploy bundle. |

## Working dataset schema

Everything downstream keys off one data frame with the columns
`Accession Number, Taxon Name, Current Germplasm Type, Collection Date,
Latitude, Longitude, Locality, Collector, issues, source, index`.
`merge_and_index()` in `R/utils.R` enforces it; `index` is a 1..n row id
regenerated on every mutation and is what map markers and table rows share.

## Tests

```r
Rscript tests/testthat.R
```

The suite covers the upload parser, schema enforcement, the GBIF filtering
pipeline (on a saved fixture, no network), golden gap-analysis scores on the
fixture datasets, the Shiny modules via `shiny::testServer`, and report
rendering. `test-ui_browser.R` drives the app in headless Chromium with
shinytest2 (skipped on CI and when no browser is installed).

`dev/diagnose_61.R` prints the record counts at every step of a GBIF download
for one taxon; useful when a user reports unexpected G/H numbers.

## Deployment

`uploadtoshinyio.R` deploys to shinyapps.io (`atlantabg` account) using
credentials in a git-ignored `secrets.yaml`; set `app_name` at the top of the
script to the app you want to update. `manifest.json` is generated with
`rsconnect::writeManifest()` for git-backed Posit Connect deployment
(regenerate it after changing `renv.lock` or adding/removing app files). The
per-directory `.rscignore` files keep fixtures, tests and preprocessing out of
the bundle (about 22 MB).

To run in a container: `docker build -t gamma . && docker run --rm -p 3838:3838 gamma`.
`.dockerignore` keeps the test fixtures and local cruft out of the image.

Set the version shown in the footer by editing `VERSION`.

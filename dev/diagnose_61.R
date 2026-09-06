# Diagnostic for issue #61: "different number of G values from summary to gbif pull".
# Logs the GBIF index counts and the number of records surviving each step of the
# download pipeline, and demonstrates the pre-refactor double counting.
#
# Usage (from the app root, needs network):
#   Rscript dev/diagnose_61.R                # Magnolia fraseri, slider = 200
#   Rscript dev/diagnose_61.R 3153837 500    # any taxonKey, any slider value
suppressPackageStartupMessages(source("global.R"))
source("R/gbif_functions.R")

args <- commandArgs(trailingOnly = TRUE)
taxon_key <- if (length(args) >= 1) as.numeric(args[1]) else 3153619  # Magnolia fraseri
limit     <- if (length(args) >= 2) as.integer(args[2]) else 200L

counts <- gbif_counts(taxon_key)
cat(sprintf("taxonKey %s   slider limit %d\n", format(taxon_key), limit))
cat(sprintf("GBIF index (hasCoordinate): total=%s  living=%s  iNaturalist=%s\n\n",
            counts$total, counts$living, counts$inat))

limits <- gbif_pool_limits(limit)
pool <- gbif_fetch(taxon_key, living_limit = limits$living, other_limit = limits$other,
                   progress = function(v, d) cat("  ", d, "\n"))
cat(sprintf("\nRaw pool: %d rows (living query limit %d + unfiltered query limit %d)\n",
            nrow(pool), limits$living, limits$other))
cat(sprintf("  LIVING_SPECIMEN rows in raw pool: %d  (unique gbifID: %d)  <- pre-refactor app counted the first number as G\n",
            sum(pool$basisOfRecord == "LIVING_SPECIMEN"),
            length(unique(pool$gbifID[pool$basisOfRecord == "LIVING_SPECIMEN"]))))
cat("  taxonomicStatus of raw pool:\n"); print(table(pool$taxonomicStatus, useNA = "ifany"))

# Reproduce what the pre-refactor app did: fetch ALL living specimens (limit 10000)
# and then an unfiltered pool for the remainder, without de-duplicating the living
# rows that the second query returns again.
old_living <- rgbif::occ_search(taxonKey = taxon_key, hasCoordinate = TRUE,
                                basisOfRecord = "LIVING_SPECIMEN", limit = 10000)$data
old_other  <- rgbif::occ_search(taxonKey = taxon_key, hasCoordinate = TRUE,
                                limit = max(0, 10000 - nrow(old_living)))$data
old_pool <- dplyr::bind_rows(old_living, old_other)
old_pool <- old_pool[old_pool$taxonomicStatus == "ACCEPTED", ]
old_pool <- old_pool[!old_pool$basisOfRecord %in% "FOSSIL_SPECIMEN", ]
old_G <- old_pool[old_pool$basisOfRecord == "LIVING_SPECIMEN", ]
cat(sprintf("
Pre-refactor path (10k + 10k pool): %d rows; LIVING_SPECIMEN rows = %d, unique gbifID among them = %d
",
            nrow(old_pool), nrow(old_G), length(unique(old_G$gbifID))))
cat(sprintf("  => the old app reported %d G for a taxon whose summary said %s living records (each living record was fetched twice and only the non-living half was de-duplicated)
",
            min(limit, nrow(old_G)), counts$living))

for (syn in c(FALSE, TRUE)) {
  g <- gbif_gather(taxon_key, limit = limit, include_synonyms = syn, pool = pool)
  cat(sprintf("\ninclude_synonyms = %s\n", syn))
  for (nm in names(g$steps)) cat(sprintf("  %-22s %6d\n", nm, g$steps[[nm]]))
  cat(sprintf("  => loaded %d records: %d G, %d H   (index says %s living before filters)\n",
              nrow(g$data), g$n_g, g$n_h, counts$living))
}

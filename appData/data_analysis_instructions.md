## Getting Started

This page is where you assemble and clean the working dataset used by the
Gap Analysis. Load records from either source below — they can be combined.

### 1. Load Occurrence Data

- **GBIF Data** — open the panel in the sidebar, choose a genus, specific
  epithet, rank and infraspecific epithet, set the maximum number of
  occurrences, then click **Gather GBIF Occurrences**. *Advanced options*
  let you filter by event date, drop iNaturalist records, include taxonomic
  synonyms, or take a random rather than most-recent sample.
- **Custom Data** — upload your own specimen records as CSV or Excel.
  See **View format requirements** for the expected columns.

### 2. Review Your Records

On the map, points are colored by source (GBIF vs. Upload) and by germplasm
type — **G** (germplasm, conserved) or **H** (herbarium/reference). Use the
layer control to show or hide each group. The counter in the bottom-left
summarizes how many of each you currently have.

In the tables, click the **accession number** of a GBIF record to open that
occurrence on [gbif.org](https://www.gbif.org) in a new tab — useful for
checking a record's locality, collector, or images before deciding whether
to keep it. Uploaded records show their accession number as plain text.

### 3. Remove Bad Records

Select the records you want to remove in any combination of these ways:

- **Click a point** on the map to select it; click it again to deselect.
- **Click a row** in the table to the right.
- **Draw a polygon or rectangle** with the map's drawing tools to select
  every point inside it.

Selections made on the map and in the tables stay in sync, and each new
selection adds to the current one. Then click **Delete Selection** to remove
the selected records. **Undo Delete** restores the last deletion, and
**Clear Selection** deselects everything without deleting.

### 4. Export or Continue

**Export Analysis Data** downloads the current working dataset. When you are
satisfied with it, move on to the **Gap Analysis** tab.

# About GAMMA
### The Gap Analysis Application

---

## Overview
The **(GAMMA)** is a web-based tool designed to help living collections managers, curators, anyone else interested in plant conservation assess the conservation value of their collections against a public record of observations of there species of interest. By treating geographic and ecological coverage as proxies for genetic diversity, this tool provides a comparable and quantifable estimate of how well an *ex situ* collection represents the wild diversity of a specific taxon. This conservation score if best used to rank the relative conservation status of a given taxon against taxa.

### Key Features
* **Visualize Coverage:** Generate interactive maps showing where a taxon exists in the wild versus where collected germplasm originated.
* **Quantify Completeness:** Calculate standardized metrics (SRS, GRS, ERS) that score the collection's representation of wild diversity.
* **Identify Gaps:** Pinpoint specific ecoregions and geographic areas that are under-represented in the current collection.
* **Secure & Temporary:** User-uploaded data is processed for the analysis session only and is never stored on the application server.

---

## Methodology

### The Science of Ex Situ Gap Analysis
Conservation gap analysis has long been used to assess how well protected areas conserve biodiversity. *Ex situ* gap analysis applies this same logic to living collections. It asks: **To what extent does the gene pool held in botanic gardens and gene banks capture the breadth of the taxon's wild gene pool?**

### Workflow
The application operates by comparing two datasets:
* **Germplasm (G):** Location data for living accessions (seeds or plants) held in conservation collections. This is provided by you, the user.
* **Reference (H):** A proxy for the full wild distribution of the taxon, typically derived from herbarium specimens and observational records. This is pulled automatically from the [Global Biodiversity Information Facility (GBIF)](https://www.gbif.org/).

### Metrics & Scores
The application utilizes the methodology developed by Carver et al. (2021) to generate three primary scores, ranging from **0 (no coverage)** to **100 (complete coverage)**.

#### 1. Sampling Representativeness Score (SRS)
* **What it measures:** The ratio of germplasm collections to wild reference records.
* **Interpretation:** A high score indicates that the taxon is well-sampled numerically, relative to its known occurrences.
* **Calculation:** `SRS = (Total G Records / Total H Records) * 100`

#### 2. Geographic Representativeness Score (GRS)
* **What it measures:** The physical area of the wild distribution covered by collections.
* **How it works:** Buffers (50km radius) are drawn around distinct G and H points. The score is the proportion of the H area covered by the G area.
* **Interpretation:** A high score implies that collectors have visited a wide breadth of the taxon physical range.

#### 3. Ecological Representativeness Score (ERS)
* **What it measures:** The diversity of environments captured in the collection.
* **How it works:** It overlays points onto a map of terrestrial ecoregions. It compares the number of ecoregions containing G records to the number of ecoregions containing H records.
* **Interpretation:** A high score suggests the collection includes populations adapted to the full range of environmental conditions the taxon has been observed to inhabit.

---

## Data Security & Taxonomy

### Data Security
This tool is designed as a **single-session application**.
* **No Retention:** Any CSV file you upload is held in temporary memory solely for the duration of your analysis.
* **Automatic Deletion:** Once your session ends (browser closed or timed out after ~15 minutes), all user data is permanently purged.
* **No Tracking:** We do not track or store the specific taxon or collections you analyze.

### Taxonomy
The app utilizes the **GBIF Backbone Taxonomy**. When running an analysis, use your descression on what taxon best matches your uploaded data. The application will not prevent you from upload data from different taxon, as it is understand taxonomic nomenclature is a dynamic process.

---

## Project Background

The need for this tool arose from the growing focus on **Metacollections**—combined holdings of a species maintained across multiple institutions. 

With support from the Institute of Museum and Library Services (IMLS), the Atlanta Botanical Garden led a multi-institutional team to translate these scripts into this web-based GAMMA interface.

### Acknowledgements

**Development Team:**
* Atlanta Botanical Garden
* Colorado State University
* The Morton Arboretum
* San Diego Botanic Garden
* Montgomery Botanical Center

**Funding:**
Development was made possible by the **Institute of Museum and Library Services (IMLS)** Grant #MG-252894-OMS-23, with additional support from the **United States Botanic Garden** and **BGCI-US**.

---

## References

**If you use this tool for publication or internal reporting, please cite it as:**
> Atlanta Botanical Garden. [Year]. Gap Analysis App (GAMMA). Botanic Gardens Conservation International. Available at https://atlantabg.shinyapps.io/GAMMA/.

**Key Methodology Papers:**
* **Carver, D.**, et al. (2021). GapAnalysis: an R package to calculate conservation indicators using spatial information. *Ecography*, 44: 1000-1009.
* **Khoury, C. K.**, et al. (2019). Comprehensiveness of conservation of useful wild plants: An ecological indicator for biodiversity and sustainable development. *Ecological Indicators*, 98, 420–429.
* **Beckman, E.**, et al. (2019). *Conservation Gap Analysis of Native U.S. Oaks*. Lisle, IL: The Morton Arboretum.
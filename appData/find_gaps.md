## Step 2: Find the gaps

The **Gap Analysis** page estimates how well the germplasm (**G**) records represent the taxon's wild or native distribution, following the *ex situ* conservation gap analysis framework of the [GapAnalysis R package](https://github.com/CIAT-DAPA/GapAnalysis). Geographic and ecological coverage are used as proxies for the genetic diversity captured in collections.

#### Estimating the range

Two methods are offered for estimating the wild range from the records with coordinates:

* **Buffer around records (default):** a circular buffer of the chosen distance is drawn around every record, and the union of those buffers, clipped to land, is taken as the range.
* **Convex hull around records:** the smallest convex polygon enclosing all records, clipped to land, is taken as the range. This fills the space between scattered records and needs at least three records at distinct locations. With this method the buffer distance is applied to the germplasm records only.

#### Three scores

* **Sampling Representativeness Score (SRS):** the ratio of germplasm accessions to reference records, using all records whether or not they have coordinates. Are there enough accessions relative to what is known of the taxon?
* **Geographic Representativeness Score (GRS):** the share of the estimated range that lies within the buffer distance of a germplasm record. Which parts of the range have never been collected from?
* **Ecological Representativeness Score (ERS):** the share of the ecoregions containing records that also contain a germplasm record. Which environments are missing from collections?

Each score runs from 0 (nothing represented) to 100 (fully represented). Their average is the **Final Conservation Score (FCS)**, which places the taxon in a priority category:

| FCS | Priority category | Meaning |
| :--- | :--- | :--- |
| **0 – 25** | **Urgent Priority (UP)** | Little or none of the range or its ecoregions is represented in collections. |
| **25 – 50** | **High Priority (HP)** | Some representation, but large geographic or ecological gaps remain. |
| **50 – 75** | **Medium Priority (MP)** | Reasonably represented; targeted collecting can still close specific gaps. |
| **75 – 100** | **Low Priority (LP)** | Well represented *ex situ*. |

#### Reading the map

The map shows the estimated range, the buffers around germplasm records, the **GRS gap** (the part of the range farther than the buffer distance from any germplasm record) and the **ERS regions** (ecoregions coloured by whether a germplasm buffer reaches them). Global protected-area boundaries can be switched on to see where collecting may need permits. Together these layers point to the places and environments where new collecting would add the most to the metacollection.

> Any change to the working dataset on the Data Analysis page clears the current results; re-run the analysis after cleaning.

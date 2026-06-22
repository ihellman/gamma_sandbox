# About the Gap Analysis & Metacollection Management (GAMMa) Tool

The Gap Analysis & Metacollection Management (GAMMa) tool estimates the extent to which an *ex situ* collection for a taxon is representative of its wild or native distribution, using geographic and ecological coverage as proxies for genetic diversity. 

Areas of wild or native distribution not represented in a living collection, i.e., gaps in *ex situ* collections, can be identified through the tool and then prioritized for future collecting to increase representation in *ex situ* collections.

Curators and collections managers at botanic gardens, genebanks, and other *ex situ* repositories can use this tool to assess the representativeness of their collections, or that of metacollections (i.e., multiple collections across several institutions, in combination) with the aim of building collections more fully representing plant diversity.

The tool enables ready access to taxon-level data in the Global Biodiversity Information Facility (GBIF). User data can also be uploaded to the tool; this data is not stored within the application and thus kept private for your use only.

Use this tool to:
* Visualize and quantify the geographic and ecological coverage (i.e., representation) of a single taxon in an *ex situ* collection or metacollection
* Identify geographic and ecological gaps in the *ex situ* collection or metacollection

## GAMMa General Information

### What does the tool do?
It compares accession-level data of an *ex situ* collection for a single taxon to wild or native occurrence records of that taxon. The tool calculates the degree of representation, or overlapping area, of the existing *ex situ* accession(s) in comparison to the taxon’s wild or native distribution. A report is generated which provides a map of the representation as well as the coverage calculations.

[Click here for a step-by-step tutorial.](#)

### What is conservation gap analysis?
Conservation gap analysis is a method of assessing the degree of representation of species in conservation systems, in this case using spatial approaches. *Ex situ* conservation gap analysis analyzes the extent to which *ex situ* conservation collections (i.e. mature plant, seed, etc., collections in botanic gardens, genebanks, and other repositories) are conserving the full range of diversity of a taxon. 

The *ex situ* conservation gap analysis methodology has been under development over many years by many contributing researchers and organizations. More information and select publications on the research methodology are listed below.

### How is genetic diversity estimated?
The GAMMa tool does not use direct metrics of genetic diversity (e.g., allelic richness or heterozygosity) of individuals in a living collection. Instead, it uses geographic and ecological information as proxies for genetic diversity.

### Why should an ex situ collection attempt to represent the full range of wild or native genetic diversity?
Genetic diversity is essential for species to adapt to changing environmental conditions. Unfortunately, plants suffer from many threats that reduce population size and erode the genetic variation needed for long-term survival. *Ex situ* collections can safeguard genetic diversity that would otherwise be lost. Genetically diverse collections support conservation, scientific study, species reintroduction and wider ecological restoration, and plant breeding/crop improvement. 

This tool aims to empower collection managers to identify opportunities to increase the breadth of their collections and contribute more effectively to the *ex situ* and *in situ* conservation and sustainable use of plant diversity.

### What type of data does the tool use?
The tool compares *ex situ* accession-level data with wild or native occurrence data from the Global Biodiversity Information Facility (GBIF) and/or uploaded by the user.

* **Accession data:** Users upload their own accession data as a spreadsheet in .csv format, with this template format. The data uploaded by users is not stored in the tool, and is only held temporarily for the purpose of running the analysis.
* **Wild occurrence data:** Users can query an in-tool GBIF search function to retrieve occurrence records, or can upload occurrence data that has been gathered elsewhere. (Learn more about GBIF data here.)

[Introduction/Explainer/Overview Video]

## In-tool Terms and Metrics

### Input data types (GBIF and Custom Data uploads)
* **(G) Germplasm records:** Wild collection data (i.e. accession data) associated with living samples (e.g., entire plants or seeds) in an *ex situ* repository (i.e., botanical garden, genebank, etc.). These are provided by the user and/or are flagged in the GBIF dataset which filters the “Basis of Record” field and assigns “living specimen” records as G.
* **(H) Reference records:** Wild or native occurrences that have supporting records (e.g. herbarium voucher, human observation, material collection, etc). These can be pulled from GBIF within the tool and/or be provided by the user.

### Analysis Calculations
* **Estimating wild or native range.** Two methods can be used:
  * **Buffer method:** a user-defined buffer is created around each H collection coordinate point to estimate wild or native range
  * **Convex hull method:** XXXX
* **Estimating geographic area represented by an ex situ living collection record:** a user-defined buffer is created around each G collection coordinate point to estimate geographic and ecological areas already collected.

### Output Calculations
Using the input data, the gap analysis tool produces three *ex situ* conservation scores (all bound between 0-100) assessing the geographic and ecological representativeness of the *ex situ* collection:

* **The Sampling Representativeness Score ex situ (SRS ex situ)** calculates the ratio of germplasm accessions (G) available in *ex situ* repositories to reference/voucher (H) records for each taxon, making use of all compiled records irrespective of whether they include coordinates. If there are more G than H records, SRS ex situ is set to the maximum score of 100.
* **The Geographic Representativeness Score ex situ (GRS ex situ)** uses user-defined buffers created around each G collection coordinate point to estimate geographic areas collected within the estimated range of each taxon and then calculates the proportion of the range covered by these buffers.
* **The Ecological Representativeness Score ex situ (ERS ex situ)** calculates the proportion of terrestrial ecoregions represented within the G buffered areas out of the total number of ecoregions occupied by the potential range. The layer used for estimating the ERS ex situ contains 814 distinct terrestrial ecoregions worldwide (Olson et al., 2001).
* **The Final Conservation Score ex situ (FCS ex situ)** is derived by calculating the average of the three *ex situ* conservation metrics.

### Interpreting the Final Conservation Score ex situ (FCS ex situ):

| Score Range | Priority Level | Description |
| :--- | :--- | :--- |
| 0 - 25 | Urgent Priority | Species is critically under-represented in collections. Urgent further conservation action required. |
| 26 - 50 | High Priority | Some representation exists, but significant gaps remain in terms of specific geographic areas or ecoregions. |
| 51 - 75 | Medium Priority | Considerable representation exists, but some gaps remain in terms of specific geographic areas or ecoregions. |
| 76 - 100 | Low Priority | Species is fairly well-conserved *ex situ*, but a few gaps remain in terms of specific geographic areas or ecoregions. |

## Taxonomy
To search and upload reference records from GBIF, the GAMMa tool uses the GBIF Backbone Taxonomy, which is assembled from 105 sources including The World Checklist of Vascular Plants (WCVP) and the International Plant Names Index. Click here to learn more about GBIF Backbone Taxonomy.

## Data Use
The Gap Analysis tool does not store any user-uploaded data. Analyses are performed in a single session, and will time out after one hour of non use. Sensitive information is therefore not retained—but users must also re-run analyses after the session times out.

## Citing this Tool
Please cite as:
Carver, D., Byrne, A., Coffey, E.E.D., Good, K., Handley, V., Khoury, C.K., Linsky, J., Norris, S., Phipps, S., Toppila, R. (2026). Gap Analysis And Metacollection Management Tool (GAMMa). [Computer Software] Atlanta Botanical Garden. https://atlantabg.shinyapps.io/GAMMA_dev/ (Version BETA). Accessed on DD/MM/YYYY.

## Project Background
Botanic gardens, genebanks, and other *ex situ* repositories play a vital role in biodiversity conservation, ecological restoration, research, education, and outreach. Staff at these institutions are increasingly working to understand and expand the conservation and use value of collections through assessment and curation while also widening the impact of those collections through collaboration with other organizations. Metacollections - combined holdings of species across multiple institutions, managed collaboratively for conservation and research - are arising as a key tool to address challenges in *ex situ* plant conservation, including a lack of coordination among institutions, redundancy and gaps in geographic and ecological coverage, uneven taxonomic representation, loss of wild-origin data and genetic lineages, and limited long-term institutional capacity.

In order to establish and expand metacollections of high conservation and use value, an understanding of both the status of and gaps within the collection are needed. *Ex situ* conservation gap analysis can provide this information through compilation and ecogeographical analysis of accession data. Workflows to gather, analyze, and present conservation gap analyses which inform collection development have been created over recent decades, including for example by Ramírez-Villegas et al. (2010), Castañeda-Álvarez et al. (2016), Khoury et al. (2019), Khoury et al. (2020) and Carver et al. (2021), and have been implemented for wild relatives of crops and useful wild plants more widely. 

These and adapted workflows were created to address the needs of other rare and threatened plant groups often of focus in the botanic garden sector. These workflows supported the publication of the Conservation Gap Analysis of Native U.S. Oaks and other genus level gap analyses (Beckman et al. 2019, 2021), inspiring the use of this methodology to assess and strengthen *ex situ* living metacollections for rare and threatened species. A multi-species focused gap analysis methodology guidance document was created by The Morton Arboretum. Multiple reports have been published to date using this methodology within the Global Conservation Consortia, however accessible tools and training to allow institutions of different sizes and capacities to apply these methodologies more broadly were identified as a need within the plant conservation community.

In 2023, the Atlanta Botanical Garden, The Morton Arboretum, and The Montgomery Botanical Center initiated a multi-year project supported by the Institute of Museum and Library Services (Award #: MG-252894-OMS-23) and BGCI-US/US Botanic Garden to develop a user-friendly, web-based application (GAMMa) to make an *ex situ* gap analysis methodology more accessible to the wider botanical community. The project has facilitated several workshops between 2024 and 2027 for collections managers, developed guidance on the use of the application for metacollections management, and produced conservation gap analyses supporting collections efforts for various species.

## Acknowledgements
Development of this version of the Gap Analysis and Metacollection Management tool was supported by:
* Botanic Gardens Conservation International-US
* Institute of Museum and Library Services
* United States Botanic Garden

The Gap Analysis and Metacollection Management tool was developed by a core project team from the following organizations:
* Colorado State University - Geospatial Data Centroid
* Atlanta Botanical Garden
* The Morton Arboretum
* Montgomery Botanical Center
* New York Botanical Garden
* Botanic Gardens Conservation International-US

## References & Relevant reading
* Beckman, E., Meyer, A., Denvir, A., Gill, D., Man, G., Pivorunas, D., Shaw, K., & Westwood, M. (2019). Conservation Gap Analysis of Native U.S. Oaks. Lisle, IL: The Morton Arboretum.
* Beckman, E., Meyer, A., Pivorunas, D., Hoban, S., & Westwood, M. (2021). Conservation Gap Analysis of American Beech. Lisle, IL: The Morton Arboretum.
* Cano, Á., Powell, J., Aiello, A.S. et al. (2025), Insights from a century of data reveal global trends in ex situ living plant collections. Nat Ecol Evol 9, 214–224. https://doi.org/10.1038/s41559-024-02633-z
* Carver, D., Khoury, C.K., Frances, A., McCarry, N., Diaz-Garcia. L., Galarneau, E., Gora, S., Haidet, M., Heinitz, C., Knapp, W., Meyer, A., Miller, A., Mims, R., Sapkota, S., Spurrier, C., and Wen, J. (2026), Conservation gap analysis for wild grapevines ( Vitis L.) of the Americas. Plants People Planet
* Carver, D., Sosa, C.C., Khoury, C.K., Achicanoy, H.A., Diaz, M.V., Sotelo, S., Castañeda-Álvarez, N.P. and Ramirez-Villegas, J. (2021), GapAnalysis: an R package to calculate conservation indicators using spatial information. Ecography, 44: 1000-1009. https://doi.org/10.1111/ecog.05430
* Castañeda-Álvarez NP, Khoury CK, Achicanoy HA, Bernau V, Dempewolf H, Eastwood RJ, Guarino L, Harker RH, Jarvis A, Maxted N, Mueller JV, Ramírez-Villegas J, Sosa CC, Struik PC, Vincent H, and Toll J (2016) Global conservation priorities for crop wild relatives. Nature Plants 2(4): 16022. doi: 10.1038/nplants.2016.22.. https://doi.org/10.1038/nplants.2016.22
* Griffith, M.P., Emily Beckman, Taylor Callicrate, John Clark, Teodoro Clase, Susan Deans, Michael Dosmann, Jeremie Fant, Xavier Gratacos, Kayri Havens, Sean Hoban, Matt Lobdell, Francisco Jiménez-Rodriguez, Andrea Kramer, Robert Lacy, Tracy Magellan, Joyce Maschinski, Alan W. Meerow, Abby Meyer, Vanessa Sanchez, Emma Spence, Pedro Toribio, Seana Walsh, Murphy Westwood, Jordan Wood. (2019), Toward the Metacollection: Safeguarding Plant Diversity and Coordinating Conservation Collections. BGCI-US. https://www.bgci.org/resources/bgci-tools-and-resources/toward-the-metacollection-coordinating-conservation-collections-to-safeguard-plant-diversity/
* Khoury CK, Amariles D, Soto JS, Diaz MV, Sotelo S, Sosa CC, Ramírez-Villegas J, Achicanoy HA, Velásquez-Tibatá J, Guarino L, León B, Navarro-Racines C, Castañeda-Álvarez NP, Dempewolf H, Wiersema JH, and Jarvis A (2019) Comprehensiveness of conservation of useful wild plants: an operational indicator for biodiversity and sustainable development targets. Ecological Indicators 98: 420-429. doi: 10.1016/j.ecolind.2018.11.016
* Khoury, C. K., Carver, D., Greene, S. L., Williams, K. A., Achicanoy, H. A.,Schori, M., Leon, B., Wiersema, J. H., & Frances, A. (2020). Crop wild relatives of the United States require urgent conservation action. Proceedings of the National Academy of Sciences,117(52), 33351–33357. https://doi.org/10.1073/pnas.2007029117
* Olson, David M., Eric Dinerstein, Eric D. Wikramanayake, Neil D. Burgess, George V. N. Powell, Emma C. Underwood, Jennifer A. D'amico, Illanga Itoua, Holly E. Strand, John C. Morrison, Colby J. Loucks, Thomas F. Allnutt, Taylor H. Ricketts, Yumiko Kura, John F. Lamoreux, Wesley W. Wettengel, Prashant Hedao, Kenneth R. Kassem, Terrestrial Ecoregions of the World: A New Map of Life on Earth: A new global map of terrestrial ecoregions provides an innovative tool for conserving biodiversity, BioScience, Volume 51, Issue 11, November 2001, Pages 933–938, https://doi.org/10.1641/0006-3568(2001)051[0933:TEOTWA]2.0.CO;2
* Ramírez-Villegas J, Khoury C, Jarvis A, Debouck DG, and Guarino L (2010) A gap analysis methodology for collecting crop genepools: a case study with Phaseolus beans. PLoS One 5(10): e13497. doi: 10.1371/journal.pone.0013497. https://doi.org/10.1371/journal.pone.0013497
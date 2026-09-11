## Step 1: Gather your data

A gap analysis starts from a working dataset of occurrence records for one taxon. GAMMa builds that dataset from two sources, which can be used on their own or combined.

**Reference records from GBIF.** The Global Biodiversity Information Facility aggregates herbarium specimens, field observations and living-collection records from institutions worldwide. Choose a genus, species and, if needed, an infraspecific name in the sidebar of the **Data Analysis** page; the tool resolves it against the GBIF Backbone Taxonomy, reports how many georeferenced records exist, and downloads up to the number you set with the *Max Occurrences* slider. Living specimens held by botanic gardens and genebanks are always downloaded first and treated as germplasm (**G**); everything else is treated as a reference record (**H**). *Advanced options* let you restrict the download by date, exclude iNaturalist observations, include synonyms, relax the scientific-name check, or change how reference records are chosen.

**Your own accession data.** Upload a CSV or Excel file of accessions from your collection, or from a metacollection assembled across several institutions. The file needs one row per accession with the columns listed under **View format requirements**; an example file that matches the format exactly can be downloaded from the same panel. Uploaded rows are labelled by their *Current Germplasm Type* column, so a file can carry both **G** (living material in your collection) and **H** (reference or voucher records) rows.

**Review and clean.** Every record appears on the map and in the tables, coloured by source. Click points or rows, or draw a shape on the map, to select records that are mislocated, duplicated or otherwise unwanted, then delete them. One level of undo is available. The cleaned dataset is what the gap analysis uses, and it can be exported at any time.

> Only records with latitude and longitude can be mapped and used for the geographic and ecological scores. Records without coordinates are kept and still count towards the sampling score.

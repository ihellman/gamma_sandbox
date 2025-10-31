library(readr)
library(sf)
library(terra)
library(tictoc)

land <- terra::vect("appData/land/landNoLakes.gpkg")

ecoRegions <-terra::vect("appData/ecoregionsSimplified.gpkg")


gapPoints <- read_csv("C:/Users/ianhe/Downloads/Magnolia_acuminata_data.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  vect()


pointsBuffer <- gapPoints %>%
  terra::buffer(width = as.numeric(100) * 1000) |>
      terra::intersect(land)

h_buffer <- terra::aggregate(
      pointsBuffer[pointsBuffer$`Current Germplasm Type` == "H", ])

g_buffer <- terra::aggregate(
      pointsBuffer[pointsBuffer$`Current Germplasm Type` == "G", ])


grsex_buffers <- terra::erase(x = h_buffer,
                            y = g_buffer)



    p1 <- pointsBuffer
    g1 <- grsex_buffers
    # total area
    totalArea <- p1 |>
      terra::aggregate() |> 
      terra::expanse(unit="km")
    print("total area")
    print(totalArea)
    # calculate areas outside of gbuffer
    gapArea <- g1 |>
      terra::aggregate() |>
      terra::expanse(unit="km")
    print("gap area")
    print(gapArea)
    #calculate GRSex score
    ## total - gap area give the area covered by G buffers.
    difference <- totalArea - gapArea
    if(difference == 0){
      grsex_score <- 0
    }else{
      grsex_score <- ((totalArea - gapArea)/totalArea)*100
    }
   
    

  
    # define point objects
    d1 <- gapPoints
    print("d1")
    # print(head(d1))
    # determine the eco regions present in the
    inter <- terra::intersect(x = d1, y = ecoRegions) 
    print("inter")
    # print(head(inter))
    
    # select ecoregions of interest
    ecoCodes <- unique(inter$ECO_NAME)
    # select ecoregions of interest
    ersex_Ecos <- ecoRegions[ecoRegions$ECO_NAME %in% ecoCodes, ]
    print("ecos")


 
    currentEcos <- ersex_Ecos
    print("current Ecos")
    # print(currentEcos$ECO_NAME)
    # determine ecoregions with G buffers  
    gs <- terra::aggregate(g_buffer)
    gEco <- terra::intersect(x = gs, y = currentEcos)
    print("gEcos")
    print(gEco$ECO_NAME)
    
    # Pull unique Ids and subset the G values 
    ecoIDs <- unique(currentEcos$ECO_NAME[!currentEcos$ECO_NAME %in% gEco$ECO_NAME])
    print("ecoIDs")
    # print(ecoIDs)
    # select from 
    ersex_missingEcos <- currentEcos[currentEcos$ECO_NAME %in% ecoIDs]
  

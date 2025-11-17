library(readr)
library(sf)
library(terra)
library(tictoc)
library(mapview)

land <- terra::vect("C:/Users/ianhe/OneDrive/Documents/Github/gap-analysis-shiny-app-main/appData/land/landNoLakes.gpkg")


ecoRegions <-terra::vect("C:/Users/ianhe/OneDrive/Documents/Github/gap-analysis-shiny-app-main/appData/ecoregionsSimplified.gpkg")


# gapPoints <- read_csv("C:/Users/ianhe/OneDrive/Documents/Github/gamma_sandbox/simple_app_no_conditional/appData/Magnolia_acuminata_data.csv") %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   vect()

gapPoints <- read_csv("C:/Users/ianhe/Downloads/Magnolia_acuminata_data_log.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  vect()



# Buffer arond points and exclude water from buffer
bufferDist <- 100
pointsBuffer <- gapPoints %>%
  terra::buffer(width = as.numeric(bufferDist) * 1000) |>
      terra::intersect(land)

# H Buffers dissolve
h_buffer <- terra::aggregate(
      #pointsBuffer[pointsBuffer$`Current Germplasm Type` == "H", ]
      pointsBuffer[pointsBuffer$Current.Germplasm.Type == "H", ]
    )

# G buffer dissolve
g_buffer <- terra::aggregate(
      #pointsBuffer[pointsBuffer$`Current Germplasm Type` == "G", ]
      pointsBuffer[pointsBuffer$Current.Germplasm.Type == "G", ]  
    )

# Erase h_buffer from g_buffer
grsex_buffers <- terra::erase(x = h_buffer,
                            y = g_buffer)




# Scores
#SRSEX Score:



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
   
    

    # determine the eco regions present in the
    inter <- terra::intersect(x = gapPoints, y = ecoRegions) 
    print("inter")
    # print(head(inter))
    
    # select ecoregions of interest
    ecoCodes <- unique(inter$ECO_NAME)
    # select ecoregions of interest
    ersex_Ecos <- ecoRegions[ecoRegions$ECO_NAME %in% ecoCodes, ]



    
  mapview(g_buffer) + mapview(ersex_Ecos, col.regions = "red")



    currentEcos <- ersex_Ecos
    print("current Ecos")
    # print(currentEcos$ECO_NAME)
    # determine ecoregions with G buffers  
    #gs <- terra::aggregate(g_buffer)
    gEco <- terra::intersect(x = g_buffer, y = currentEcos)
    print("gEcos")
    print(gEco$ECO_NAME)

    mapview(g_buffer) + mapview(gEco, col.regions = "red")

  # determine missing ecos 
    # Pull unique Ids and subset the G values 
    ecoIDs <- unique(currentEcos$ECO_NAME[!currentEcos$ECO_NAME %in% gEco$ECO_NAME])
    print("ecoIDs")
    # print(ecoIDs)
    # select from 
    ersex_missingEcos <- currentEcos[currentEcos$ECO_NAME %in% ecoIDs]
  

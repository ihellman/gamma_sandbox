# 1. Load Libraries
pacman::p_load(rnaturalearth, sf, rmapshaper, ggplot2, tmap)
tmap_mode("view")
export1 <- "appData/land_detail.gpkg"
export2 <- "appData/land_simple.gpkg"
# getting errors with the intersection without this setting might need to project the data to get better results
sf::sf_use_s2(FALSE)
if (!file.exists(export1) | !file.exists(export2)) {
  # 1. Download Data
  # 'land' gives us the continents (excluding ocean)
  # 'lakes' gives us the internal water bodies
  # using the most percise data and scale 10
  world_land <- ne_download(
    scale = 10,
    type = 'land',
    category = 'physical',
    returnclass = 'sf'
  )
  lakes <- ne_download(
    scale = 10,
    type = 'lakes',
    category = 'physical',
    returnclass = 'sf'
  )
  # union to form a single feature
  one_lake <- sf::st_make_valid(sf::st_union(lakes))

  # remove all the lake areas from the world_land object
  pure_land <- st_difference(world_land, one_lake)
  # rending taking a long time so simplfying on second step
  valid_land <- st_make_valid(pure_land)
  #visual check
  # qtm(pure_land)

  # export # export the results
  sf::st_write(
    obj = valid_land,
    dsn = export1,
    delete_dsn = TRUE
  )

  # simplfy and export lower res object
  ## droping to 10% of the points
  simple_land <- ms_simplify(pure_land, keep = 0.1, keep_shapes = TRUE)
  #visual check
  # qtm(simple_land)
  # export # export the results
  sf::st_write(
    obj = simple_land,
    dsn = export2,
    delete_dsn = TRUE
  )
}

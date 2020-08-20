world_shp <- readOGR(dsn = file.path(dropbox_file_path, "Data", "world_shapefile", "RawData"),
        layer = "ne_50m_admin_0_countries")

world_shp@data <- world_shp@data %>%
  dplyr::select(ISO_A2, ISO_A3) %>%
  dplyr::rename(geo = ISO_A2)

saveRDS(world_shp,
        file.path(dropbox_file_path, "Data", "world_shapefile", "FinalData",
                  "world_sdf.Rds"))



library(rnaturalearth)
library(sp)

world_ne <- ne_countries()
world_ne@data <- world_ne@data %>%
  dplyr::select(iso_a2, continent, name) %>%
  dplyr::rename(geo = iso_a2)

world_ne_sf <- world_ne %>% st_as_sf()

saveRDS(world_ne_sf,
        file.path(dropbox_file_path, "Data", "world_shapefile", "FinalData",
                  "world_ne.Rds"))


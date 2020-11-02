
world_sp <- readOGR(file.path(dropbox_file_path, "Data", "world_shapefile", 
                              "RawData",
                              "TM_WORLD_BORDERS-0.3.shp"))

world_sp_s <- ms_simplify(world_sp, keep_shapes=T, keep = 0.075)
#world_sp_s <- gSimplify(world_sp, tol=0.05)
#world_sp_s$id <- 1:length(world_sp_s)
#world_sp_s@data <- world_sp@data
#world_sp_s <- world_sp

saveRDS(world_sp_s, file.path(dropbox_file_path, "Data", "world_shapefile", 
                            "FinalData",
                            "TM_WORLD_BORDERS-0.3_simplified.Rds"))




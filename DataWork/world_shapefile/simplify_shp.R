
world_sp <- readOGR(file.path(dropbox_file_path, "Data", "world_shapefile", 
                              "RawData",
                              "TM_WORLD_BORDERS-0.3.shp"))

world_sp_s <- ms_simplify(world_sp, keep_shapes=T)

saveRDS(world_sp_s, file.path(dropbox_file_path, "Data", "world_shapefile", 
                            "FinalData",
                            "TM_WORLD_BORDERS-0.3_simplified.Rds"))


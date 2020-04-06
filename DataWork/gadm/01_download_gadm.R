# Download GADM

setwd(file.path(dropbox_file_path, "Data", "GADM", "RawData"))
getData('GADM', country='BRA', level=0)
getData('GADM', country='BRA', level=1)
getData('GADM', country='BRA', level=2)
getData('GADM', country='BRA', level=3)

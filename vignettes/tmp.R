library(dplyr)
library(eurostat)
library(sf)
library(tmap)

# Download attribute data from Eurostat
sp_data <- eurostat::get_eurostat("tgs00026",
		                  time_format = "raw",
                                  stringsAsFactors = FALSE) %>% 
  # subset to have only a single row per geo
  dplyr::filter(time == 2010, nchar(geo) == 4) %>% 
  # categorise
  dplyr::mutate(income = cut_to_classes(values, n = 5))

print("Download geospatial data from GISCO")
geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
				   nuts_level = 2,
				   year = 2013)

#print("merge with attribute data with geodata")
#map_data <- inner_join(geodata, sp_data)

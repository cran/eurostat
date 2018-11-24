## ----setup, include=FALSE------------------------------------------------
# Global options
library(knitr)
opts_chunk$set(fig.path="fig/")

## ----install, eval=FALSE-------------------------------------------------
#  install.packages("eurostat")

## ----install2, eval=FALSE------------------------------------------------
#  library(devtools)
#  install_github("ropengov/eurostat")

## ---- echo=FALSE---------------------------------------------------------
library(eurostat)

## ---- echo=FALSE,comment=NA----------------------------------------------
cat(paste0(library(help = "eurostat")$info[[2]], collapse = "\n"))

## ----get_eurostat_toc, warning=FALSE, message=FALSE----------------------
# Load the package
library(eurostat)
library(rvest)

# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
library(knitr)
kable(head(toc))

## ----search_eurostat, warning=FALSE, message=FALSE-----------------------
# info about passengers
kable(head(search_eurostat("passenger transport")))

## ----get_id, warning=FALSE, message=FALSE, results='asis'----------------
# For the original data, see
# http://ec.europa.eu/eurostat/tgm/table.do?tab=table&init=1&plugin=1&language=en&pcode=tsdtr210
id <- search_eurostat("Modal split of passenger transport", 
        	             type = "table")$code[1]
print(id)

## ----get_eurostat, warning=FALSE, message=FALSE, results='asis'----------
dat <- get_eurostat(id, time_format = "num")

## ----str_dat, warning=FALSE, message=FALSE-------------------------------
str(dat)

## ----head_dat, warning=FALSE, message=FALSE, results='asis'--------------
kable(head(dat))

## ----get_eurostat_json, warning=FALSE, message=FALSE, results='asis', eval=FALSE----
#  dat2 <- get_eurostat(id, filters = list(geo = c("EU28", "FI"), lastTimePeriod=1), time_format = "num")
#  kable(dat2)

## ----json_labels, warning=FALSE, message=FALSE, results='asis', eval=FALSE----
#  datl2 <- get_eurostat(id, filters = list(geo = c("EU28", "FI"),
#                                           lastTimePeriod = 1),
#                        type = "label", time_format = "num")
#  kable(head(datl2))

## ----labels, warning=FALSE, message=FALSE, results='asis'----------------
datl <- label_eurostat(dat)
kable(head(datl))

## ----name_labels, eval = FALSE-------------------------------------------
#  label_eurostat_vars(names(datl))

## ----vehicle_levels, eval = FALSE----------------------------------------
#  levels(datl$vehicle)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
data(efta_countries)
kable(efta_countries)

## ----eu_12---------------------------------------------------------------
dat_eu12 <- subset(datl, geo == "European Union (current composition)" & time == 2012)
kable(dat_eu12, row.names = FALSE)

## ----eu_vehicles_table---------------------------------------------------
library("tidyr")
dat_eu_0012 <- subset(dat, geo == "EU28" & time %in% 2000:2012)
dat_eu_0012_wide <- spread(dat_eu_0012, vehicle, values)
kable(subset(dat_eu_0012_wide, select = -geo), row.names = FALSE)

## ----trains_table--------------------------------------------------------
dat_trains <- subset(datl, geo %in% c("Austria", "Belgium", "Finland", "Sweden")
                     & time %in% 2000:2012 
                     & vehicle == "Trains")

dat_trains_wide <- spread(dat_trains, geo, values) 
kable(subset(dat_trains_wide, select = -vehicle), row.names = FALSE)

## ----trains_plot, fig.width=6, fig.height=3------------------------------
library(ggplot2)
p <- ggplot(dat_trains, aes(x = time, y = values, colour = geo)) 
p <- p + geom_line()
print(p)

## ----plotGallery, warning=FALSE, message=FALSE, fig.width=6, fig.height=6----
library(tidyr)
library(plotrix)
library(eurostat)
library(dplyr)
library(tidyr)

# All sources of renewable energy are to be grouped into three sets
 dict <- c("Solid biofuels (excluding charcoal)" = "Biofuels",
 "Biogasoline" = "Biofuels",
 "Other liquid biofuels" = "Biofuels",
 "Biodiesels" = "Biofuels",
 "Biogas" = "Biofuels",
 "Hydro power" = "Hydro power",
 "Tide, Wave and Ocean" = "Hydro power",
 "Solar thermal" = "Wind, solar, waste and Other",
 "Geothermal Energy" = "Wind, solar, waste and Other",
 "Solar photovoltaic" = "Wind, solar, waste and Other",
 "Municipal waste (renewable)" = "Wind, solar, waste and Other",
 "Wind power" = "Wind, solar, waste and Other",
 "Bio jet kerosene" = "Wind, solar, waste and Other")
# Some cleaning of the data is required
 energy3 <- get_eurostat("ten00081") %>%
 label_eurostat(dat) %>%
 filter(time == "2013-01-01",
 product != "Renewable energies") %>%
 mutate(nproduct = dict[as.character(product)], # just three categories
 geo = gsub(geo, pattern=" \\(.*", replacement="")) %>%
 select(nproduct, geo, values) %>%
 group_by(nproduct, geo) %>%
 summarise(svalue = sum(values)) %>%
 group_by(geo) %>%
 mutate(tvalue = sum(svalue),
 svalue = svalue/sum(svalue)) %>%
 filter(tvalue > 1000) %>% # only large countries
 spread(nproduct, svalue)
 
# Triangle plot
 par(cex=0.75, mar=c(0,0,0,0))
 positions <- plotrix::triax.plot(as.matrix(energy3[, c(3,5,4)]),
                     show.grid = TRUE,
                     label.points= FALSE, point.labels = energy3$geo,
                     col.axis="gray50", col.grid="gray90",
                     pch = 19, cex.axis=0.8, cex.ticks=0.7, col="grey50")

 # Larger labels
 ind <- which(energy3$geo %in%  c("Norway", "Iceland","Denmark","Estonia", "Turkey", "Italy", "Finland"))
 df <- data.frame(positions$xypos, geo = energy3$geo)
 points(df$x[ind], df$y[ind], cex=2, col="red", pch=19)
 text(df$x[ind], df$y[ind], df$geo[ind], adj = c(0.5,-1), cex=1.5)

## ----maps1-1, eval=TRUE, fig.width=8, fig.height=8-----------------------
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

# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
				   nuts_level = 2,
				   year = 2013)

# merge with attribute data with geodata
map_data <- inner_join(geodata, sp_data)

## ----map1ex, eval=TRUE, warning=FALSE------------------------------------
map1 <- tmap::tm_shape(geodata) +
  tmap::tm_fill("lightgrey") +
  tmap::tm_shape(map_data) +
  tmap::tm_grid() +
  tmap::tm_polygons("income", title = "Disposable household\nincomes in 2010",  
                    palette = "Oranges")
print(map1)  

## ----maps1-2, eval=FALSE, fig.width=8, fig.height=8----------------------
#  # Interactive
#  tmap_mode("view")
#  map1
#  
#  # Set the mode back to normal plotting
#  tmap_mode("plot")
#  print(map1)

## ----maps2, fig.width=8, fig.height=8, warning=FALSE---------------------
library(eurostat)
library(dplyr)
library(sf)
library(RColorBrewer)

# Downloading and manipulating the tabular data
print("Let us focus on year 2014 and NUTS-3 level")
euro_sf2 <- get_eurostat("tgs00026", time_format = "raw",
                         stringsAsFactors = FALSE,
			 filter = list(time = "2014")) %>% 
 
  # Subset to NUTS-3 level
  dplyr::filter(grepl("PL",geo)) %>% 
  # label the single geo column
  mutate(label = paste0(label_eurostat(.)[["geo"]], "\n", values, "€"),
         income = cut_to_classes(values))

print("Download geospatial data from GISCO")
geodata <- get_eurostat_geospatial(output_class = "sf", resolution = "60", nuts_level = 2, year = 2013)

# Merge with attribute data with geodata
map_data <- inner_join(geodata, euro_sf2)

# plot map
map2 <- tm_shape(geodata) +
  tm_fill("lightgrey") +
  tm_shape(map_data, is.master = TRUE) +
  tm_polygons("income", title = "Disposable household incomes in 2014",
              palette = "Oranges", border.col = "white") + 
  tm_text("NUTS_NAME", just = "center") + 
  tm_scale_bar()
map2

## ----maps3, fig.width=8, fig.height=8, dev='CairoPNG'--------------------
library(sp)
library(eurostat)
library(dplyr)
library(RColorBrewer)
dat <- get_eurostat("tgs00026", time_format = "raw", stringsAsFactors = FALSE) %>% 
  # subsetting to year 2014 and NUTS-2 level
  dplyr::filter(time == 2014, nchar(geo) == 4) %>% 
  # classifying the values the variable
  dplyr::mutate(cat = cut_to_classes(values))

# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(output_class = "spdf", resolution = "10", nuts_level = 2, year = 2013)

# merge with attribute data with geodata
geodata@data <- left_join(geodata@data, dat)

# plot map
sp::spplot(obj = geodata, "cat", main = "Disposable household income",
	   xlim = c(-22,34), ylim = c(35,70), 
           col.regions = c("dim grey", brewer.pal(n = 5, name = "Oranges")),
	   col = "white", usePolypath = FALSE)

## ----maps4, fig.width=8, fig.height=8, dev='CairoPNG'--------------------
library(eurostat)
library(dplyr)
library(ggplot2)
dat <- get_eurostat("tgs00026", time_format = "raw", stringsAsFactors = FALSE) %>% 
  # subsetting to year 2014 and NUTS-2 level
  dplyr::filter(time == 2014, nchar(geo) == 4) %>% 
  # classifying the values the variable
  dplyr::mutate(cat = cut_to_classes(values))

# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "2", year = 2013)

# merge with attribute data with geodata
map_data <- inner_join(geodata, dat)

ggplot(data=map_data) + geom_sf(aes(fill=cat),color="dim grey", size=.1) + 
    scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "euro")) +
  labs(title="Disposable household income in 2014",
       caption="(C) EuroGeographics for the administrative boundaries 
                Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
  theme_light() + theme(legend.position=c(.8,.8)) +
  coord_sf(xlim=c(-12,44), ylim=c(35,70))


## ----rsdmx, fig.width=8, fig.height=8, dev='CairoPNG'--------------------
library(rsdmx)

# Data set URL
url <- "http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/cdh_e_fos/..PC.FOS1.BE/?startperiod=2005&endPeriod=2011"

# Read the data from eurostat
d <- readSDMX(url)

# Convert to data frame and show the first entries
df <- as.data.frame(d)

kable(head(df))

## ----citation, message=FALSE, eval=TRUE, echo=TRUE-----------------------
citation("eurostat")

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()


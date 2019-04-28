# This file is NOT required pre-reading for the R Course on the 11th.

# The materials in this file can instead be considered extesion materials.

# In this file I demonstrate how I aquired and prepared the geographic data that we will visualise in the course.

# To run the code below you will need the following four packages installed:
  # raster
  # tidyverse
  # sf
  # maps

# There is a namespace conflict between raster and tidyverse so I don't load the raster package.
# Instead I call functions from the raster package using the `package.name::function.name( )` syntax

# The other two package we can load:

library(tidyverse)
library(sf)

# `ch.dem` & `ch.boundaries` were downloaded from public servers with the `raster::getData()` command:

ch.dem <- raster::getData(name = 'alt', country = 'CHE')

ch.dem

st_crs(ch.dem)

ch.boundaries <- raster::getData(name = 'GADM', country = 'CHE', level = 1)

ch.boundaries

st_crs(ch.boundaries)

# the global rivers and lakes data were downloaded from <https://www.hydrosheds.org/>

# and read into R as follows (both of these files are huge: 1.2Gb and 1.1Gb respectively):

world.rivers.sf <- st_read(dsn = '~/Downloads/GloRiC_v10_shapefile/GloRiC_v10_shapefile/GloRiC_v10.shp')

st_crs(world.rivers.sf)

world.lakes.sf <- st_read(dsn = '~/Downloads/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp')

st_crs(world.lakes.sf)

# I then downloaded the Swiss border:

ch.border <- raster::getData(name = 'GADM', country = 'CHE', level = 0)

# and converted it to a simple features object:

ch.border.sf <- st_as_sf(x = ch.border)

ggplot() +
  geom_sf(data = ch.border.sf)

# before progressing it is important to check whether these object use the same coordinate reference system:

st_crs(ch.dem) == st_crs(ch.boundaries)

st_crs(ch.dem) == st_crs(world.rivers.sf)

st_crs(ch.dem) == st_crs(world.lakes.sf)

st_crs(ch.dem) == st_crs(ch.border)

# now that I've checked that I extract the portions of the lakes and rivers data are contained within the Swiss border:

ch.rivers <- st_intersection(x = world.rivers.sf, y = ch.border.sf)

ch.lakes <- st_intersection(x = world.lakes.sf, y = ch.border.sf)

# a quick visual check:

ggplot() +
  geom_sf(data = ch.border.sf) + 
  geom_sf(data = ch.rivers, colour = 'blue', alpha = 0.1) +
  geom_sf(data = ch.lakes, fill = 'blue', colour = 'blue')

# You'll notice that the lakes that intersect the Swiss border have been cut off at the border.

# If we wanted all the lakes in a box that bounds Switzerland we could instead use:

ch.bbox.lakes <- st_crop(x = world.lakes.sf, y = st_bbox(ch.border.sf))

ggplot() +
  geom_sf(data = ch.border.sf) + 
  geom_sf(data = ch.rivers, colour = 'blue', alpha = 0.1) +
  geom_sf(data = ch.bbox.lakes, fill = 'blue', colour = 'blue')


# Preparing some points data describing the 5 most populous cities in Switzerland
# These data are provided by the 'maps' package

library(maps)
data(world.cities)

ch.5.cities.sf <- filter(world.cities, country.etc == 'Switzerland') %>%
  arrange(desc(pop)) %>%
    slice(1:5) %>%
      st_as_sf(x = .,
               coords = c('long', 'lat'),
               crs = 4326
      ) %>%
        st_transform(crs = st_crs(ch.dem)) %>%
          mutate(pop.lab.num = round(x = pop/1e3, digits = 0),
                pop.lab = glue::glue("~{pop.lab.num}k")
          )

ch.5.cities.sf

# Reproject all objects to LV95 = EPSG:2056

ch.boundaries <- st_as_sf(x = ch.boundaries) %>%
                   st_transform(crs = 2056)

ch.rivers <- st_transform(x = ch.rivers, crs = 2056)

ch.lakes <- st_transform(x = ch.lakes, crs = 2056)

ch.5.cities.sf <- st_transform(x = ch.5.cities.sf, crs = 2056)

ch.dem <- raster::projectRaster(ch.dem,
                                crs = st_crs(ch.5.cities.sf) %>%
                                        purrr::pluck('proj4string')
                  )              

# Finally I write out the data we will use in the course as a .RData file and a .tif:

save(list = c('ch.boundaries', 'ch.rivers', 'ch.lakes', 'ch.5.cities.sf'), file = '~/jan_r_course/geospatial_visualisation/ch_map_data.RData')

raster::writeRaster(x = ch.dem, filename = '~/jan_r_course/geospatial_visualisation/ch_dem.tif')




library(tidyverse)
library(sf)
library(raster)
library(getlandsat)
library(mapview)
library(osmdata)

bb = read_csv('data/uscities.csv') %>%
  filter(city == 'Palo') %>%
  st_as_sf(coords = c('lng','lat'), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc()

mapview(bb)

bwgs = st_transform(bb, 4326)

mapview(bwgs)

osm = opq(bwgs) %>% 
  add_osm_feature('natural') %>%
  osmdata_sf()

mapview(osm$osm_polygons)

scenes = lsat_scenes()

bbwgs = st_bbox(bwgs)

down = scenes %>%
  filter(min_lat <= bbwgs$ymin, max_lat >= bbwgs$ymax,
         min_lon <= bbwgs$xmin, max_lon >= bbwgs$xmax,
         as.Date(acquisitionDate) == as.Date('2016-09-26'))

write.csv(down, file = 'data/palo-flood.csv', row.names = F)


#~~~~~~~~~~~~~~~~~~~~~#

meta = read_csv('data/palo-flood.csv')

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0('B',1:6,'.TIF$', collapse = '|'), file)) %>%
  arrange(file) %>% 
  pull(file)

st = sapply(files, lsat_image) 

s = stack(st) %>% setNames(paste0('band', 1:6))

cropper = bb %>% st_as_sf() %>% st_transform(crs(s))

r = crop(s, cropper)
plot(r)

plotRGB(r, r = 4, g = 3, b = 2)
plotRGB(r, r = 5, g = 4, b = 3)

par(mfrow = c(1,2))
plotRGB(r, r = 4, g = 3, b = 2, stretch = 'hist')
plotRGB(r, r = 5, g = 4, b = 3, stretch = 'hist')
dev.off()

ndvi = (r$band5 - r$band4)/(r$band5 + r$band4)

palette = colorRampPalette(c('blue', 'white', 'red'))

plot(ndvi, col = palette(256))

thresholding = function(x) {ifelse(x<=0,1,NA)}


flood = calc(ndvi, thresholding)

plot(flood)

ss = stars::st_as_stars(flood)

library(leaflet)

leaflet() %>% addTiles() %>% leafem::addStarsImage(ss)


flood[flood == 0 ] = NA

flood



install.packages("mapview")








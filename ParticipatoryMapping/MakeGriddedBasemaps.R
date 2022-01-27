library(ggmap)
library(sf)
library(tidyverse)
register_google(key="AIzaSyCh-KGA6T9qaPF-wjfnBV8hb7zzGYMlGTw")

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

####Make fishnet
initial<-st_transform(Pemba, 3857) %>% dplyr::select(NAME_3, geometry)
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)
grid <- st_make_grid(target,
                     cellsize=1000, #In meters
                    
                     crs = st_crs(initial),
                     what = "polygons",
                     square = TRUE
)

# To sf
grid <- st_sf(index = 1:length(lengths(grid)), grid) # Add index
cent_grid <- st_centroid(grid)
cent_merge <- st_join(cent_grid, initial["index_target"], left = F)
grid_new <- inner_join(grid, st_drop_geometry(cent_merge))

# Fishnet
Fishgeom <- aggregate(grid_new,
                      by = list(grid_new$index_target),
                      FUN = min,
                      do_union = FALSE
)

# Lets add the df
Fishnet <- left_join(
  Fishgeom %>% dplyr::select(index_target),
  st_drop_geometry(initial)
) %>%
  dplyr::select(-index_target)

plot(st_geometry(Fishnet), main = "Fishnet")

##########Plotting function for merging ggmap and sf

# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
Pemba_3857 <- st_transform(Pemba, 3857)

map <- get_googlemap("pemba island", scale=2)


# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map <- get_map("pemba island", scale=2,maptype = "terrain",source="stamen" )
map <- ggmap_bbox(map)

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = Pemba_3857, fill=NA, inherit.aes = FALSE)


####Just one area

map <- get_map("mjimbini tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map)
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mjimbini"), 
          color="lightgrey",size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mjimbini"), 
          color=alpha("darkgrey",0.4),size=0.5,fill=NA, inherit.aes = FALSE)+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), 
                         style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.2, "in"), )


map2 <- get_googlemap(center = c(lon = 39.725, lat = -4.973),scale=2, zoom=14, maptype = "hybrid")
map2 <- ggmap_bbox(map2)
ggmap(map2) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kifundi"), 
          color="grey",size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kifundi"), 
          color=alpha("lightgrey",0.3),size=0.5,fill=NA, inherit.aes = FALSE)+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.2, "in"), 
                                    style = north_arrow_fancy_orienteering)





##Piloting
map <- get_map(center=c(lon = 39.70, lat = -4.904), scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map)
ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Makangale"), 
          color=alpha("lightgrey",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Makangale"), 
          color=alpha("darkgrey",0.4),size=0.5,fill=NA, inherit.aes = FALSE)+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.2, "in"), )





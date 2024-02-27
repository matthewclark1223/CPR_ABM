library(ggmap)
library(sf)
library(tidyverse)
library(ggspatial)
register_google(key="REMOVED")

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

####Make fishnet
initial<-st_transform(Pemba, 3857) %>% dplyr::select(NAME_3, geometry)
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)
grid <- st_make_grid(target,
                     cellsize=500, #In meters
                    
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


####Specific shehia

#Ukunjwi
map2 <- get_googlemap(center = c(lon = 39.697, lat = -5.04),scale=2, zoom=14, maptype = "hybrid")
map2 <- ggmap_bbox(map2)
ggmap(map2) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Ukunjwi"), 
          color=alpha("white",0.3),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Ukunjwi"), 
          color=alpha("lightgrey",0.6),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Tibirinzi

map <- get_map("tibirinzi tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Tibirinzi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Tibirinzi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


# Gando
map <- get_googlemap(center = c(lon = 39.702, lat = -4.99),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Gando"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Gando"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Ziwani

map <- get_googlemap(center = c(lon = 39.76, lat = -5.185),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Ziwani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Ziwani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


##Msuka Magharibi
map <- get_map("Msuka Magharibi tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Msuka Magharibi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Msuka Magharibi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

##Msuka Mashariki
map <- get_map("Msuka Mashariki tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Msuka Mashariki"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Msuka Mashariki"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

# Kisiwani
map <- get_googlemap(center = c(lon = 39.76, lat = -5.16),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kisiwani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kisiwani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Mtambwe Kaskazini
map <- get_googlemap(center = c(lon = 39.71, lat = -5.096),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mtambwe Kaskazini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mtambwe Kaskazini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )



#Mtambwe Kusini
map <-  get_googlemap(center = c(lon = 39.71, lat = -5.1385),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mtambwe Kusini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mtambwe Kusini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Kangagani
map <-  get_googlemap(center = c(lon = 39.84, lat = -5.152),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kangagani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kangagani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Kiuyu Minungwini
map<- get_googlemap(center = c(lon = 39.82, lat = -5.145),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kiuyu Minungwini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kiuyu Minungwini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Junguni
map <- get_map("Junguni Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Junguni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Junguni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Kambini
map <- get_googlemap(center = c(lon = 39.81, lat = -5.139),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kambini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kambini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Shengejuu
map <- get_map("Shengejuu Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Shengejuu"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Shengejuu"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Kifundi
map <- get_googlemap(center = c(lon = 39.715, lat = -4.974),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kifundi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kifundi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Wingwi Mjananza
map <- get_map("Wingwi Mjananza Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Wingwi Mjananza"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Wingwi Mjananza"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Mtemani
map <- get_map("Mtemani Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mtemani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mtemani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )
#Tondooni
map <- get_googlemap(center = c(lon = 39.69, lat = -4.945),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Tondooni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Tondooni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Tumbe Magharibi
map <- get_googlemap(center = c(lon = 39.783, lat = -4.95),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Tumbe Magharibi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Tumbe Magharibi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


##Tumbe Mashariki
map <- get_googlemap(center = c(lon = 39.805, lat = -4.96),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Tumbe Mashariki"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Tumbe Mashariki"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )




#Kilindi
map <-get_googlemap(center = c(lon = 39.71, lat = -5.265),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kilindi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kilindi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )





#Wesha
map <- get_googlemap(center = c(lon = 39.735, lat = -5.221),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Wesha"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Wesha"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Makangale
map <- get_googlemap(center = c(lon = 39.702, lat = -4.902),scale=2, zoom=14, maptype = "satellite")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Makangale"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Makangale"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Wingwi Mapofu
map <- get_googlemap(center = c(lon = 39.810, lat = -5.015),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Wingwi Mapofu"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Wingwi Mapofu"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Majenzi
map <- get_map("Majenzi Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Majenzi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Majenzi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Mjini Wingwi
map <- get_map("Mjini Wingwi Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mjini Wingwi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mjini Wingwi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Kiuyu Mbuyuni
map <- get_googlemap(center = c(lon = 39.86, lat = -4.942),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kiuyu Mbuyuni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kiuyu Mbuyuni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Sizini
map <- get_googlemap(center = c(lon = 39.808, lat = -4.99),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Sizini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Sizini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Kinowe
map <- get_googlemap(center = c(lon = 39.77, lat = -4.945),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kinowe"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kinowe"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Shumba Mjini
map <-  get_googlemap(center = c(lon = 39.823, lat = -4.9415),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Shumba Mjini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Shumba Mjini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )



#Ndagoni_1
map <- get_googlemap(center = c(lon = 39.70, lat = -5.211),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Ndagoni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Ndagoni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()#+
  #ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
   #                                 pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
    #                                style = north_arrow_fancy_orienteering)+
  #ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
   #                           pad_y = unit(0.3, "in"), )

#Ndagoni_2
map <- get_googlemap(center = c(lon = 39.675, lat = -5.211),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Ndagoni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Ndagoni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Kibokoni
map <- get_map("Kibokoni Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kibokoni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kibokoni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Mgelema
map <- get_googlemap(center = c(lon = 39.71, lat = -5.30),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mgelema"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mgelema"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Chonga
map <- get_map("Chonga Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Chonga"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Chonga"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Mkoani shehia

#Mbuguani
map <- get_map("Mbuguani Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mbuguani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mbuguani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Mbuyuni
map <- get_map("Mbuyuni Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mbuyuni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mbuyuni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Kuukuu
map <- get_map("Kuukuu Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kuukuu"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kuukuu"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Kangani
map <- get_map("Kangani Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kangani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kangani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Chumbageni
map <- get_googlemap(center = c(lon = 39.695, lat = -5.331),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Chumbageni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Chumbageni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Wambaa
map <- get_googlemap(center = c(lon = 39.675, lat = -5.30),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Wambaa"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Wambaa"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Chokocho
map <- get_map("Chokocho Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Chokocho"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Chokocho"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Michenzani
map <- get_map("Michenzani Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Michenzani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Michenzani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Makombeni
map <- get_map("Makombeni Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Makombeni"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Makombeni"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )


#Chambani
map <- get_googlemap(center = c(lon = 39.785, lat = -5.35),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Chambani"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Chambani"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Stahabu
map <- get_map("Stahabu Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Stahabu"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Stahabu"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Shidi
map <- get_map("Shidi Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Shidi"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Shidi"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Kengeja
map <- get_googlemap(center = c(lon = 39.725, lat = -5.428),scale=2, zoom=14, maptype = "hybrid")
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Kengeja"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Kengeja"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

#Mjimbini
map <- get_map("Mjimbini Pemba tanzania", scale=2,zoom=14,maptype = "hybrid" )
map <- ggmap_bbox(map) #this function created above

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = filter(Pemba_3857,NAME_3 =="Mjimbini"), 
          color=alpha("white",0.6),size=1,fill=NA, inherit.aes = FALSE)+
  geom_sf(data = filter(Fishnet,NAME_3 =="Mjimbini"), 
          color=alpha("lightgrey",0.8),size=0.3,fill=NA, inherit.aes = FALSE)+
  theme_void()+
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                    pad_x = unit(0., "in"), pad_y = unit(0.4, "in"), 
                                    style = north_arrow_fancy_orienteering)+
  ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_col= "white",
                              pad_y = unit(0.3, "in"), )

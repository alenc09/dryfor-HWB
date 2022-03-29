# Tue Nov 09 11:46:47 2021 ------------------------------
#Script para montar mapa de forest proximate people na Caatinga

#library
library(raster)
library(geobr)
library(dplyr)
library(here)
library(sf)
library(ggplot2)


#data----
read_biomes(simplified = F) %>%
  filter(name_biome == "Caatinga") %>%
  glimpse -> caat_shp

read.csv(file = here("data/aam6527_Bastin_Database-S1.csv"),
         sep = ";") -> df_plots

raster(x = here("data/bra_ppp_2020_UNadj_constrained.tif")) -> br_pop
raster(x = here("data/pop_caat_polybr_1000.tif")) -> pop_caat_polybr_1000
raster(x = here("data/caat_rural_ppp_2000_1km_Aggregated_5880.tif")) -> WP_caat_rural_pop_2000_5880
raster(x = here("data/caat_rural_ppp_2007_1km_Aggregated_5880.tif")) -> WP_caat_rural_pop_2007_5880
raster(x = here("data/caat_pop_rural_landscan_2000.tif")) -> LS_caat_pop_rural_2000_5880
raster(x = here("data/caat_pop_rural_landscan_2007.tif")) -> LS_caat_pop_rural_2007_5880

#data manipulation----
## Caatinga shape----
st_transform(caat_shp, crs = 4326) -> caat_shp_wgs84 #coordinate system change to wgs84
as_Spatial(caat_shp_wgs84) -> caat_shp_wgs84_sp #object class change to be used in crop and mask functions
st_transform(caat_shp, crs = 5880) ->caat_shp_polybr

##plots----
df_plots %>% #filtering only plots in Caatinga
  filter(
    location_x >= -45.07814 & location_x <= -35.06698,
    location_y >= -16.71264 & location_y <= -2.748264,
    land_use_category == "forest"
  ) %>%
  glimpse -> df_plots_caat
st_as_sf(
  x = df_plots_caat,
  coords = c("location_x", "location_y"),
  crs = 4326
) -> df_plots_caat_points
st_transform(x = df_plots_caat_points, crs = 5880)-> df_plots_caat_points_polybr

df_plots_caat_points_polybr[caat_shp_polybr,] ->plot_caat_polybr #only forested plots inside caatinga

##buffers----
st_buffer(x = plot_caat_polybr, dist = 1000)%>%
  st_union()%>%
  as_Spatial()->buff_1km_union

st_buffer(x = plot_caat_polybr, dist = 5000)%>%
  st_union()%>%
  as_Spatial()->buff_5km_union 

st_buffer(x = plot_caat_polybr, dist = 10000)%>%
  st_union()%>%
  as_Spatial()-> buff_10km_union

#Data analysis----
##rural population only----
raster::extract(x = pop_caat_rural_polybr,
                y = buff_1km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_1km
raster::extract(x = pop_caat_rural_polybr,
                y = buff_5km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_5km
raster::extract(x = pop_caat_rural_polybr,
                y = buff_10km_union,
                fun = sum,
                na.rm = FALSE) -> people_rural_10km

raster::extract(x = WP_caat_rural_pop_2000_5880,
                y= all_buff,
                fun = sum,
                na.rm = T) -> wP_pop_rural_2000_buff
raster::extract(x = WP_caat_rural_pop_2007_5880,
                y= all_buff,
                fun = sum,
                na.rm = T) -> wP_pop_rural_2007_buff

raster::extract(x = LS_caat_pop_rural_2000_5880,
                y= all_buff,
                fun = sum,
                na.rm = T) -> LS_pop_rural_2000_buff
raster::extract(x = LS_caat_pop_rural_2007_5880,
                y= all_buff,
                fun = sum,
                na.rm = T) -> LS_pop_rural_2007_buff

##all caatinga population----
raster::extract(x = pop_caat_polybr_1000,
                y = buff_1km_union,
                fun = sum,
                na.rm = FALSE) -> people_1km
raster::extract(x = pop_caat_polybr_1000,
                y = buff_5km_union,
                fun = sum,
                na.rm = FALSE) -> people_5km
raster::extract(x = pop_caat_polybr_1000,
                y = buff_10km_union,
                fun = sum,
                na.rm = FALSE) -> people_10km
## Results ----
c("rural only", "total")-> Population
c(people_rural_1km, people_1km) -> pop1km
c(people_rural_5km, people_5km) -> pop5km
c(people_rural_10km, people_10km) -> pop10km
data.frame(Population, pop1km, pop5km, pop10km)-> df_results

df_results%>%
  as_tibble()->df_results
df_results$pop1km<- as.numeric(df_results$pop1km)
df_results$pop5km<- as.numeric(df_results$pop5km)
df_results$pop10km<- as.numeric(df_results$pop10km)

# data visualization ----

#plot(br_pop)
plot(pop_caat_wgs84)
plot(pop_caat_polybr_1000)
plot(pop_caat_rural_polybr)
plot(pop_caat_polybr_1000 < 130 & pop_caat_polybr_1000 > 0)
#plot(buff_10km_union)
#plot(caat_shp_polybr$geom)
#plot(plot_caat_polybr, add = T)

ggplot() +
  geom_raster(data = pop_caat_rural_tibble, aes(x=x, y=y, fill=bra_ppp_2020_UNadj_constrained))+
  geom_sf(data = caat_shp_polybr, fill= NA, colour = "black", size = .3) +
  geom_sf(data = plot_caat_polybr, size =0.1) +
  geom_sf(data=st_as_sf(buff_1km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_5km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_10km_union), fill = NA, col = "red") +
  scale_fill_viridis_c(name = "Rural population in Caatinga") +
  theme(panel.background = element_blank(),
        axis.title = element_blank()
        ) -> caat_pop_rural.fig

ggplot() +
  geom_raster(data = pop_caat_rural_tibble, aes(x=x, y=y, fill=bra_ppp_2020_UNadj_constrained))+
  geom_sf(data = caat_shp_polybr, fill= NA, colour = "black", size = .3) +
  geom_sf(data = plot_caat_polybr, size =0.1) +
  geom_sf(data=st_as_sf(buff_1km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_5km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_10km_union), fill = NA, col = "red") +
  coord_cartesian(xlim = c(6500000,7000000), ylim = c(9000000,9500000))+
  coord_sf(xlim = c(6800000,7000000), ylim = c(9000000,9200000))+
  scale_fill_viridis_c(name = "Rural population in Caatinga") +
  theme(panel.background = element_blank(),
        axis.title = element_blank()
  )

ggplot() +
  geom_raster(data = pop_caat_tibble, aes(x=x, y=y, fill=bra_ppp_2020_UNadj_constrained))+
  geom_sf(data = caat_shp_polybr, fill= NA, colour = "black", size = .3) +
  geom_sf(data = plot_caat_polybr, size =0.1) +
  geom_sf(data=st_as_sf(buff_1km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_5km_union), fill = NA, col = "red") +
  geom_sf(data=st_as_sf(buff_10km_union), fill = NA, col = "red") +
  scale_fill_viridis_c()+
  theme(panel.background = element_blank(),
        axis.title = element_blank()
        ) -> caat_pop.fig



#data export----
writeRaster(x = pop_caat_polybr_1000, filename = here("data/pop_caat_polybr_1000.tif"))
st_write(obj = caat_shp, dsn = "D:/lucas_alencar/downloads/caat_shp.shp")

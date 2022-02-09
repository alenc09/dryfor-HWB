# Wed Feb  9 14:16:12 2022 ------------------------------
#script para delimitar os setores censitários (IBGE - 2010) da Caatinga

#libaries####
library(sf)
library(here)
libary(dplyr)

#data####
#sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados

##import####
list.files(path = here("data/unzip"), pattern=".shp$", full.names=TRUE)%>%
  lapply(X = .,read_sf)%>%
  do.call(what = rbind) %>%
  .[caat_shp,] -> sc_caat
st_read(dsn = here("data/sc_rural_caat.shp"))-> sc_rural_caat

##transformation####
sc_caat%>%
  dplyr::filter(TIPO== "RURAL") ->sc_rural_caat

## visualization ####
plot(sc_rural_caat)

# data export ####
st_write(obj = sc_rural_caat, dsn = here("data/sc_rural_caat.shp"))
         
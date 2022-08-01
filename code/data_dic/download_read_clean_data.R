library(dataDownloader)
library(tidyverse)


# meta turfID -------------------------------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "clean_data",
         remote_path = "Site")

metadata <- read_csv("clean_data/Three-D_metaturfID.csv") %>% 
  select(warming, turfID)


# CFlux data -------------------------------------------------------------

get_file(node = "pk4bg",
          file = "Three-D_24h-cflux_vikesland_2022.csv",
          path = "clean_data",
          remote_path = "C-Flux")

cflux_vikesland<-read.csv("clean_data/Three-D_24h-cflux_vikesland_2022.csv")
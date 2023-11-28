# packages ----

library(tidyverse)
library(sf)

# directories ---- 

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, "/")[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = "/")
data.dir <- paste0(home.dir, '/Data')
res.dir <- paste0(home.dir, '/Results')
imd.dir <- paste0(data.dir, '/Index Multiple Deprivation')
data.spatial.dir <- paste0(data.dir, '/shapeFiles')
res.data.expl.dir <- paste0(res.dir, '/Data Exploration')

# shape file ----

poly.lsoa <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS11_LSOA')
poly.nat <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS21_NAT')

# neighbourhood structure ----

poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.nat.england <- poly.nat %>%  dplyr::filter(str_detect(CTRY21CD, '^E'))
lsoa.nbs <- spdep::poly2nb(pl = as(poly.lsoa.england, 'Spatial'))

# lookup file ----

setwd(data.dir)
lookup.01.11 <- read.csv('rawData/UK Gov/LSOA01_LSOA11_LAD11_lookUp.csv')
lookup.11.21 <- read.csv('rawData/UK Gov/LSOA11_LSOA21_LAD22_lookUp.csv')

# imd files ----

setwd(imd.dir)
imd.04 <- readxl::read_excel('LSOA01_2004_IMD.xls', sheet = 'IMD 2004')
imd.07 <- readxl::read_excel('LSOA01_2007_IMD.xls', sheet = 'IMD 2007')
imd.10 <- readxl::read_excel('LSOA01_2010_IMD.xls', sheet = 'IMD 2010')
imd.15 <- readxl::read_excel('LSOA11_2015_IMD.xlsx', sheet = 'IMD 2015')
imd.19 <- readxl::read_excel('LSOA11_2019_IMD.xlsx', sheet = 'IMD2019')

# sort data ----

## 2004 imd ----

imd.04.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.04 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'SOA',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define new rank & deprivation
                     dplyr::arrange(dplyr::desc(imdScore)) %>% 
                     dplyr::mutate(imdRank = 1:n(),
                                   DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.04.missing.lsoa <- 
  imd.04.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2004')

if(nrow(imd.04.missing.lsoa) > 0){
  imd.04.sorted$DEPRIVATION[is.na(imd.04.sorted$DEPRIVATION)] <-
    sapply(lsoa.nbs[which(is.na(imd.04.sorted$DEPRIVATION))],
           FUN = function(x) { mean(imd.04.sorted$DEPRIVATION[x], na.rm = TRUE) %>% round() })
}  

## 2007 imd ----

imd.07.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.07 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define new rank & deprivation
                     dplyr::arrange(dplyr::desc(imdScore)) %>% 
                     dplyr::mutate(imdRank = 1:n(),
                                   DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.07.missing.lsoa <- 
  imd.07.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2007')

if(nrow(imd.07.missing.lsoa) > 0){
  imd.07.sorted$DEPRIVATION[is.na(imd.07.sorted$DEPRIVATION)] <-
    sapply(lsoa.nbs[which(is.na(imd.07.sorted$DEPRIVATION))],
           FUN = function(x) { mean(imd.07.sorted$DEPRIVATION[x], na.rm = TRUE) %>% round() })
}  

## 2010 imd ----

imd.10.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.10 %>% 
                     ## Rename
                     dplyr::rename('LSOA01CD' = 'LSOA CODE',
                                   'imdScore' = 'IMD SCORE') %>% 
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(imdScore = imdScore %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define new rank & deprivation
                     dplyr::arrange(dplyr::desc(imdScore)) %>% 
                     dplyr::mutate(imdRank = 1:n(),
                                   DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.10.missing.lsoa <- 
  imd.10.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2010')

if(nrow(imd.10.missing.lsoa) > 0){
  imd.10.sorted$DEPRIVATION[is.na(imd.10.sorted$DEPRIVATION)] <-
    sapply(lsoa.nbs[which(is.na(imd.10.sorted$DEPRIVATION))],
           FUN = function(x) { mean(imd.10.sorted$DEPRIVATION[x], na.rm = TRUE) %>% round() })
}  

## 2015 imd ----

imd.15.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.15 %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdRank' = 'Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdRank = imdRank %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define new deprivation
                     dplyr::arrange(imdRank) %>% 
                     dplyr::mutate(imdRank = 1:n(),
                                   DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.15.missing.lsoa <- 
  imd.15.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2015')

if(nrow(imd.15.missing.lsoa) > 0){
  imd.15.sorted$DEPRIVATION[is.na(imd.15.sorted$DEPRIVATION)] <-
    sapply(lsoa.nbs[which(is.na(imd.15.sorted$DEPRIVATION))],
           FUN = function(x) { mean(imd.15.sorted$DEPRIVATION[x], na.rm = TRUE) %>% round() })
}  

## 2019 imd ----

imd.19.sorted <- 
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(., 
                   # sort and join imd 2004
                   imd.19 %>% 
                     ## Rename
                     dplyr::rename('LSOA11CD' = 'LSOA code (2011)',
                                   'imdRank' = 'Index of Multiple Deprivation (IMD) Rank') %>% 
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD (does nothing for 2015+)
                     dplyr::summarise(imdRank = imdRank %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define new deprivation
                     dplyr::arrange(imdRank) %>% 
                     dplyr::mutate(imdRank = 1:n(),
                                   DEPRIVATION = dplyr::ntile(x = imdRank, n = 10)),
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

imd.19.missing.lsoa <- 
  imd.19.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'IMD 2019')

if(nrow(imd.19.missing.lsoa) > 0){
  imd.19.sorted$DEPRIVATION[is.na(imd.19.sorted$DEPRIVATION)] <-
    sapply(lsoa.nbs[which(is.na(imd.19.sorted$DEPRIVATION))],
           FUN = function(x) { mean(imd.19.sorted$DEPRIVATION[x], na.rm = TRUE) %>% round() })
}  

# combine all ----

imd.01.06 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2001:2006) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   imd.04.sorted %>% 
                     dplyr::select(LSOA11CD, DEPRIVATION) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

imd.07.09 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2007:2009) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   imd.07.sorted %>% 
                     dplyr::select(LSOA11CD, DEPRIVATION) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

imd.10.14 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2010:2014) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   imd.10.sorted %>% 
                     dplyr::select(LSOA11CD, DEPRIVATION) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

imd.15.18 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2015:2018) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   imd.15.sorted %>% 
                     dplyr::select(LSOA11CD, DEPRIVATION) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

imd.19.21 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2019:2021) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   imd.19.sorted %>% 
                     dplyr::select(LSOA11CD, DEPRIVATION) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

imd.01.21 <-
  dplyr::bind_rows(imd.01.06,
                   imd.07.09,
                   imd.10.14,
                   imd.15.18,
                   imd.19.21)

imd.02.21 <-
  imd.01.21 %>% 
  dplyr::filter(YEAR %in% 2002:2021)

imd.01.21.missing <-
  dplyr::bind_rows(imd.04.missing.lsoa,
                   imd.07.missing.lsoa,
                   imd.10.missing.lsoa,
                   imd.15.missing.lsoa,
                   imd.19.missing.lsoa)

nrow(poly.lsoa.england)*length(2001:2021) == nrow(imd.01.21)
nrow(poly.lsoa.england)*length(2002:2021) == nrow(imd.02.21)

# plot ----

## arguments ----

text.size <- 20
width <- height <- 10

# theme for map plots
myMapTheme <- function(...){
  ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                 axis.text.x = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.text.y = ggplot2::element_blank(),
                 axis.ticks.y = ggplot2::element_blank(),
                 legend.text.align=0,
                 legend.key=element_rect(fill=NA),
                 panel.background=element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 ...)
}

## 2004 imd ----

imd.04.plot.data <-
  imd.04.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(IMD = '2004 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

imd.04.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.04.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.04.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.04.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.04.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2007 imd ----

imd.07.plot.data <-
  imd.07.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(IMD = '2007 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

imd.07.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.07.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.07.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.07.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.07.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2010 imd ----

imd.10.plot.data <-
  imd.10.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(IMD = '2010 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

imd.10.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.10.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.10.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.10.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.10.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2015 imd ----

imd.15.plot.data <-
  imd.15.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(IMD = '2015 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

imd.15.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.15.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.15.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.15.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.15.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2019 imd ----

imd.19.plot.data <-
  imd.19.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(IMD = '2019 IMD',
         DEPRIVATION = DEPRIVATION %>% 
           factor(levels = 1:10,
                  labels = c('1 (Most Deprived)', 2:9, '10 (Least Deprived)')))

imd.19.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     imd.19.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DEPRIVATION),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'DEPRIVATION', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

imd.19.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = imd.19.plot.data, aes(fill = DEPRIVATION), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Deprivation', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(imd.19.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ IMD) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')


# save ----

setwd(data.dir)
save(imd.02.21, file = 'Organised Data/LSOA11_2002_2021_IMD.rda')

setwd(res.data.expl.dir)
print(x = xtable::xtable(x = imd.01.21.missing),
      include.rownames = FALSE,
      file = 'LSOA11_2002_2021_DEPRVATION_MISSING.txt')
ggplot2::ggsave(imd.04.plot,
                filename = paste0('LSOA11_IMD_2004_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.07.plot,
                filename = paste0('LSOA11_IMD_2007_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.10.plot,
                filename = paste0('LSOA11_IMD_2010_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.15.plot,
                filename = paste0('LSOA11_IMD_2015_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(imd.19.plot,
                filename = paste0('LSOA11_IMD_2019_PLOT.png'),
                width = width, height = height)
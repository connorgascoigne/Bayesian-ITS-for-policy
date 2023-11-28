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
ethn.dir <- paste0(data.dir, '/Ethnic Diversity')
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

# ethnic diversity files ----

setwd(ethn.dir)
ed.01 <- 
  read.csv('LSOA01_ETHNIC_DIVERSITY.csv', skip = 7) %>% 
  # remove last three rows as they are empty
  dplyr::filter(row_number() <= n() - 3) 
ed.11 <- read.csv('LSOA11_ETHNIC_DIVERSITY.csv')
ed.21 <- read.csv('LSOA21_ETHNIC_DIVERSITY.csv')

# sort data ----

## 2001 census ----

ed.01.sorted <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.01 %>% 
                     ## make proportions
                     dplyr::mutate(
                       ## make this numeric as it had text in one entry
                       Total = All.categories..Ethnic.group %>% as.numeric(),
                       ## geographical identifiers
                       LSOA01CD = mnemonic,
                       LSOA01NM = X2001.super.output.areas...lower.layer,
                       ## ethnic proportions
                       ### include Chinese in the Asian group
                       Asian = (Asian.Asian.British + Chinese.Other..Chinese) / Total,
                       Black = (Black.Black.British) / Total,
                       Mixed = (Mixed) / Total,
                       ### only other and include other white (which is Gypsy, Irish Traveler and Roma)
                       Other = (White..Other + Chinese.Other..Other) / Total,
                       ### remove other white
                       White = (White..British + White..Irish) / Total) %>%
                     ## join the LSOA11CD
                     dplyr::left_join(., 
                                      lookup.01.11 %>% 
                                        dplyr::select(LSOA01CD, LSOA11CD),
                                      by = 'LSOA01CD',
                                      relationship = 'many-to-many') %>%
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(Asian = Asian %>% mean(),
                                      Black =  Black %>% mean(),
                                      Mixed =  Mixed %>% mean(),
                                      Other =  Other %>% mean(),
                                      White =  White %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define diversity
                     dplyr::mutate(DIVERSITY = dplyr::case_when(0 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.06 ~ 4,
                                                                0.06 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.12 ~ 3,
                                                                0.12 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.24 ~ 2,
                                                                0.24 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other <= 1.00 ~ 1,
                                                                TRUE ~ NA)),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.01.missing.lsoa <- 
  ed.01.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'Census 2001')

if(nrow(ed.01.missing.lsoa) > 0){
  ed.01.sorted$DIVERSITY[is.na(ed.01.sorted$DIVERSITY)] <-
    sapply(lsoa.nbs[which(is.na(ed.01.sorted$DIVERSITY))],
           FUN = function(x) { mean(ed.01.sorted$DIVERSITY[x], na.rm = TRUE) %>% round() })
}

## 2011 census ----

ed.11.sorted <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.11 %>% 
                     ## make proportions
                     dplyr::mutate(
                       ## geographical identifiers
                       LSOA11CD = geography.code,
                       LSOA11NM = geography,
                       ## total
                       total = Ethnic.Group..All.categories..Ethnic.group..measures..Value,
                       ## ethnic proportions
                       Asian = (Ethnic.Group..Asian..measures..Value) / total,
                       Black = (Ethnic.Group..Black..measures..Value) / total,
                       Mixed = (Ethnic.Group..Mixed..measures..Value) / total,
                       ## only other and include other white (which is Gypsy, Irish Traveler and Roma)
                       Other = 
                         (Ethnic.Group..Other..measures..Value + 
                            Ethnic.Group..White..Gypsy.or.Irish.Traveller..measures..Value + 
                            Ethnic.Group..White..Other.White..measures..Value )/total,
                       ## remove other white
                       White = 
                         (Ethnic.Group..White..English.Welsh.Scottish.Northern.Irish.British..measures..Value + 
                            Ethnic.Group..White..Irish..measures..Value) / total) %>%
                     ## average over repeated any potential repeated LSOAs (non for 2011 census)
                     dplyr::summarise(Asian = Asian %>% mean(),
                                      Black =  Black %>% mean(),
                                      Mixed =  Mixed %>% mean(),
                                      Other =  Other %>% mean(),
                                      White =  White %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define diversity
                     dplyr::mutate(DIVERSITY = dplyr::case_when(0 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.09 ~ 4,
                                                                0.09 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.18 ~ 3,
                                                                0.18 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.36 ~ 2,
                                                                0.36 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other <= 1.00 ~ 1,
                                                                TRUE ~ NA)),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.11.missing.lsoa <- 
  ed.11.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'Census 2011')

if(nrow(ed.11.missing.lsoa) > 0){
  ed.11.sorted$DIVERSITY[is.na(ed.11.sorted$DIVERSITY)] <-
    sapply(lsoa.nbs[which(is.na(ed.11.sorted$DIVERSITY))],
           FUN = function(x) { mean(ed.11.sorted$DIVERSITY[x], na.rm = TRUE) %>% round() })  
}

## 2021 census ----

ed.21.sorted <-
  # all the LSOA11CD
  data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD) %>% 
  dplyr::left_join(.,
                   # sort and join ed 2001
                   ed.21 %>%
                     ## group by year and make proportions
                     dplyr::mutate(
                       ## geographical identifiers
                       LSOA21CD = Lower.layer.Super.Output.Areas.Code,
                       LSOA21NM = Lower.layer.Super.Output.Areas) %>% 
                     ## remove does not apply code
                     dplyr::filter(Ethnic.group..20.categories..Code > 0) %>% 
                     ## select only relavent columns and expand
                     dplyr::select(LSOA21CD, LSOA21NM, `Ethnic.group..20.categories.`, Observation) %>% 
                     tidyr::pivot_wider(., 
                                        names_from = 'Ethnic.group..20.categories.', 
                                        values_from = 'Observation') %>% 
                     dplyr::mutate(TOTAL = dplyr::select(., -'LSOA21CD', -'LSOA21NM') %>% rowSums()) %>% 
                     dplyr::mutate(
                       ## ethnic proportions
                       Asian = 
                         (`Asian, Asian British or Asian Welsh: Bangladeshi` +
                            `Asian, Asian British or Asian Welsh: Chinese` +
                            `Asian, Asian British or Asian Welsh: Indian` +
                            `Asian, Asian British or Asian Welsh: Pakistani` +
                            `Asian, Asian British or Asian Welsh: Other Asian`) / 
                         TOTAL,
                       Black = 
                         (`Black, Black British, Black Welsh, Caribbean or African: African` +
                            `Black, Black British, Black Welsh, Caribbean or African: Caribbean` +
                            `Black, Black British, Black Welsh, Caribbean or African: Other Black`) / 
                         TOTAL,
                       Mixed = 
                         (`Mixed or Multiple ethnic groups: White and Asian` +
                            `Mixed or Multiple ethnic groups: White and Black African` +
                            `Mixed or Multiple ethnic groups: White and Black Caribbean` +
                            `Mixed or Multiple ethnic groups: Other Mixed or Multiple ethnic groups`) /
                         TOTAL,
                       ### only other and include other white (which is Gypsy, Irish Traveler and Roma)
                       Other = 
                         (`Other ethnic group: Arab` +
                            `Other ethnic group: Any other ethnic group` + 
                            `White: Gypsy or Irish Traveller` +
                            `White: Roma`)/
                         TOTAL,
                       ### remove other white
                       White = 
                         (`White: English, Welsh, Scottish, Northern Irish or British` +
                            `White: Irish`) / 
                         TOTAL) %>% 
                     dplyr::left_join(., 
                                      lookup.11.21 %>% 
                                        dplyr::select(LSOA21CD, LSOA11CD), 
                                      by = 'LSOA21CD',
                                      'many-to-many') %>%
                     ## average over repeated LSOAs because multiple LSOA01CD got to LSOA11CD
                     dplyr::summarise(Asian = Asian %>% mean(),
                                      Black =  Black %>% mean(),
                                      Mixed =  Mixed %>% mean(),
                                      Other =  Other %>% mean(),
                                      White =  White %>% mean(),
                                      .by = 'LSOA11CD') %>% 
                     ## define diversity
                     dplyr::mutate(DIVERSITY = dplyr::case_when(0 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.12 ~ 4,
                                                                0.12 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.24 ~ 3,
                                                                0.24 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other < 0.48 ~ 2,
                                                                0.48 <= Asian + Black + Mixed + Other & Asian + Black + Mixed + Other <= 1.00 ~ 1,
                                                                TRUE ~ NA)),
                   # join by the newly created LSOA11CDs 
                   by = 'LSOA11CD') %>% 
  # join spatial polygons to impute missing data
  dplyr::left_join(.,
                   poly.lsoa.england,
                   by = 'LSOA11CD') %>%
  sf::st_as_sf()

ed.21.missing.lsoa <- 
  ed.21.sorted %>% 
  dplyr::filter(rowSums(is.na(.)) > 0) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(LSOA11CD, LSOA11NM) %>% 
  dplyr::mutate(GROUP = 'Census 2011')

if(nrow(ed.21.missing.lsoa) > 0){
  ed.21.sorted$DIVERSITY[is.na(ed.21.sorted$DIVERSITY)] <-
    sapply(lsoa.nbs[which(is.na(ed.21.sorted$DIVERSITY))],
           FUN = function(x) { mean(ed.21.sorted$DIVERSITY[x], na.rm = TRUE) %>% round() })
}


# combine all ----

ed.01.10 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2001:2010) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   ed.01.sorted %>% 
                     dplyr::select(LSOA11CD, Asian, Black, Mixed, Other, White, DIVERSITY) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

ed.11.20 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2011:2020) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   ed.11.sorted %>% 
                     dplyr::select(LSOA11CD, Asian, Black, Mixed, Other, White, DIVERSITY) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

ed.21.21 <-
  expand.grid(LSOA11CD = poly.lsoa.england$LSOA11CD,
              YEAR = 2021:2021) %>% 
  dplyr::left_join(.,
                   # only select relavent columns and join
                   ed.21.sorted %>% 
                     dplyr::select(LSOA11CD, Asian, Black, Mixed, Other, White, DIVERSITY) %>% 
                     sf::st_drop_geometry(),
                   by = 'LSOA11CD')

ed.01.21 <-
  dplyr::bind_rows(ed.01.10,
                   ed.11.20,
                   ed.21.21)

ed.02.21 <-
  ed.01.21 %>% 
  dplyr::filter(YEAR %in% 2002:2021)

ed.01.21.missing <-
  dplyr::bind_rows(ed.01.missing.lsoa,
                   ed.11.missing.lsoa,
                   ed.21.missing.lsoa)

nrow(poly.lsoa.england)*length(2001:2021) == nrow(ed.01.21)
nrow(poly.lsoa.england)*length(2002:2021) == nrow(ed.02.21)

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

## 2001 census ----

ed.01.plot.data <-
  ed.01.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(CENSUS = '2001 Census',
         DIVERSITY = DIVERSITY %>% 
           factor(levels = 1:4,
                  labels = c('1 (Most Diverse)', 2:3, '4 (Least Diverse)')))

ed.01.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     ed.01.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DIVERSITY),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

ed.01.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = ed.01.plot.data, aes(fill = DIVERSITY), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(ed.01.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ CENSUS) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2011 census ----

ed.11.plot.data <-
  ed.11.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(CENSUS = '2011 Census',
         DIVERSITY = DIVERSITY %>% 
           factor(levels = 1:4,
                  labels = c('1 (Most Diverse)', 2:3, '4 (Least Diverse)')))

ed.11.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     ed.11.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DIVERSITY),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

ed.11.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = ed.11.plot.data, aes(fill = DIVERSITY), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(ed.11.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ CENSUS) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')

## 2021 census ----

ed.21.plot.data <-
  ed.21.sorted %>% 
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22CD), by = 'LSOA11CD') %>% 
  mutate(CENSUS = '2021 Census',
         DIVERSITY = DIVERSITY %>% 
           factor(levels = 1:4,
                  labels = c('1 (Most Diverse)', 2:3, '4 (Least Diverse)')))

ed.21.plot.london <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = 
                     ed.21.plot.data %>% 
                     dplyr::filter(str_detect(LAD22CD, "^E09")), 
                   aes(fill = DIVERSITY),
                   colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  myMapTheme(legend.position = 'none')

ed.21.plot <- 
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = poly.nat.england, fill = 'white') +
  ggplot2::geom_sf(data = ed.21.plot.data, aes(fill = DIVERSITY), colour = NA) +
  ggplot2::scale_fill_viridis_d(name = 'Diversity', direction = 1, option = 'G', na.value = 'red') +
  # add london in seperately
  ggplot2::geom_rect(aes(xmin = 490000, xmax = 560000, ymin = 150000, ymax = 210000), color = 'red', linewidth = 1, fill = NA) +
  ggplot2::annotation_custom(ggplot2::ggplotGrob(ed.21.plot.london), xmin = 82700, xmax = 300000, ymin = 160000, ymax = 450000) +
  ggplot2::facet_grid(~ CENSUS) +
  myMapTheme(text = element_text(size = text.size),
             legend.title = element_blank(),
             legend.position = 'bottom')


# save ----

setwd(data.dir)
save(ed.02.21, file = 'Organised Data/LSOA11_2002_2021_ETHNIC_DIVERSITY.rda')

setwd(res.data.expl.dir)
print(x = xtable::xtable(x = ed.01.21.missing),
      include.rownames = FALSE,
      file = 'LSOA11_2002_2021_ETHNIC_DIVERSITY_MISSING.txt')
ggplot2::ggsave(ed.01.plot,
                filename = paste0('LSOA11_CENSUS_2001_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(ed.11.plot,
                filename = paste0('LSOA11_CENSUS_2011_PLOT.png'),
                width = width, height = height)
ggplot2::ggsave(ed.21.plot,
                filename = paste0('LSOA11_CENSUS_2021_PLOT.png'),
                width = width, height = height)

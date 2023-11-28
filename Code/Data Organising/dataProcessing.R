# packages ----

library(tidyverse)
library(haven)
library(readxl)
library(sf)

# directories ----

# extract file location of this script
code.path <- rstudioapi::getActiveDocumentContext()$path
code.path.splitted <- strsplit(code.path, '/')[[1]]

# retrieve directories
home.dir <- paste(code.path.splitted[1: (length(code.path.splitted)-3)], collapse = '/')
code.dir <- paste0(home.dir, '/Code')
data.dir <- paste0(home.dir, '/Data')
data.survey.dir <- paste0(data.dir, '/rawData/STATA')
data.survey.general.dir <- paste0(data.survey.dir, '/UKDA-6614-stata/stata/stata13_se/ukhls/')
data.survey.lad.dir <- paste0(data.survey.dir, '/UKDA-6666-stata/stata/stata13/ukhls/')
data.survey.lsoa.dir <- paste0(data.survey.dir, '/UKDA-7248-stata/stata/stata13/ukhls/')
data.organised.dir <- paste0(data.dir, '/Organised Data')
data.spatial.dir <- paste0(data.dir, '/shapeFiles')

# organised data folder
if(!dir.exists(paths = data.organised.dir)) {
  dir.create(path = data.organised.dir)
}

# import functions ----

source(paste0(code.dir, '/functions.R'))

# ukhls data ----

## house hold survey ----

### list of files to import ----

list.hh <- list.files(path = data.survey.general.dir, pattern = '*_hhresp.dta')

### load in the files ----

hh <- lapply(paste0(data.survey.general.dir, list.hh), haven::read_dta)
hh <- 
  lapply(hh, function(x) {
    x %>%
      # use this to select the columns to include/exclude
      dplyr::select(
        # see the following:
        ## https://www.understandingsociety.ac.uk/content/understanding-society-key-variables
        ## https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/household-income-variables
        # identifier
        dplyr::ends_with('idp'),
        dplyr::ends_with('pno'),
        # sampling & interview variables
        dplyr::ends_with('ivfio'),
        dplyr::ends_with('ivfho'),
        dplyr::ends_with('indmode'),
        dplyr::ends_with('modetype'),
        dplyr::ends_with('hholdmodedv'),
        dplyr::ends_with('psu'),
        dplyr::ends_with('strata'),
        dplyr::ends_with('idp'),
        dplyr::ends_with('idp'),
        dplyr::ends_with('indpxus_lw'),
        dplyr::ends_with('indinus_lw'),
        # interview facts
        dplyr::ends_with('intdatd_dv'),
        dplyr::ends_with('intdatm_dv'),
        dplyr::ends_with('intdaty_dv'),
        # residents variable
        dplyr::ends_with('country'),
        dplyr::ends_with('gor_dv'),
        dplyr::ends_with('urban_dv'),
        # socio-economic & demographic characteristics
        dplyr::ends_with('doby_dv'),
        dplyr::ends_with('age_dv'),
        dplyr::ends_with('sex_dv'),
        dplyr::ends_with('ukborn'),
        dplyr::ends_with('bornuk_dv'),
        dplyr::ends_with('racel_dv'),
        dplyr::ends_with('ethn_dv'),
        dplyr::ends_with('mastat_dv'),
        dplyr::ends_with('nchild_dv'),
        dplyr::ends_with('hiqual_dv'),
        dplyr::ends_with('qfhigh_dv'),
        dplyr::ends_with('scend_dv'),
        dplyr::ends_with('jbstat'),
        dplyr::ends_with('jbsoc00_cc'),
        dplyr::ends_with('jbnssec8_dv'),
        dplyr::ends_with('jbnssec5_dv'),
        dplyr::ends_with('jbnssec3_dv'),
        # health & well-being
        dplyr::ends_with('sf1'),
        dplyr::ends_with('scsf1'),
        dplyr::ends_with('sf12mcs_dv'),
        dplyr::ends_with('sf12pcs_dv'),
        dplyr::ends_with('health'),
        dplyr::ends_with('scghq1_dv'),
        dplyr::ends_with('scghq2_dv'),
        dplyr::ends_with('sclfsato'),
        # individual & family background
        dplyr::ends_with('j1soc00_cc'),
        dplyr::ends_with('maid'),
        dplyr::ends_with('macob'),
        dplyr::ends_with('maedqf'),
        dplyr::ends_with('masoc90_cc'),
        dplyr::ends_with('masoc00_cc'),
        dplyr::ends_with('masoc10_cc'),
        dplyr::ends_with('paid'),
        dplyr::ends_with('pacob'),
        dplyr::ends_with('paedqf'),
        dplyr::ends_with('pasoc90_cc'),
        dplyr::ends_with('pasoc00_cc'),
        dplyr::ends_with('pasoc10_cc'),
        # household-level characteristics
        dplyr::ends_with('hhsize'),
        dplyr::ends_with('nkids_dv'),
        dplyr::ends_with('hhtype_dv'),
        dplyr::ends_with('tenure_dv'),
        # individual & household income
        dplyr::ends_with('fimnnet_dv'),
        dplyr::ends_with('fimnlabnet_dv'),
        dplyr::ends_with('fimnmisc_dv'),
        dplyr::ends_with('fimnprben_dv'),
        dplyr::ends_with('fimninvnet_dv'),
        dplyr::ends_with('fimnpen_dv'),
        dplyr::ends_with('fimnsben_dv')
      )
  })
names(hh) <- gsub('\\.dta$', '', list.hh)

### extract dataframes from list ----

list2env(hh, .GlobalEnv)
# rm(hh)

## individual survey ----

list.ind <- list.files(path = data.survey.general.dir, pattern = '*_indresp.dta')

### load in the files ----

ind <- lapply(paste0(data.survey.general.dir, list.ind), haven::read_dta)
ind <- 
  lapply(ind, function(x) {
    x %>%
      select(
        # see the following:
        ## https://www.understandingsociety.ac.uk/content/understanding-society-key-variables
        ## https://www.understandingsociety.ac.uk/documentation/mainstage/user-guides/main-survey-user-guide/household-income-variables
        # identifier
        dplyr::ends_with('idp'),
        # dplyr::ends_with('pno'),
        # sampling & interview variables
        dplyr::ends_with('ivfio'),
        dplyr::ends_with('ivfho'),
        dplyr::ends_with('indmode'),
        dplyr::ends_with('modetype'),
        dplyr::ends_with('hholdmodedv'),
        # dplyr::ends_with('psu'),
        # dplyr::ends_with('strata'),
        dplyr::ends_with('idp'),
        dplyr::ends_with('idp'),
        dplyr::ends_with('indpxus_lw'),
        dplyr::ends_with('indinus_lw'),
        dplyr::ends_with('indinus_xw'),
        # interview facts
        dplyr::ends_with('intdatd_dv'),
        dplyr::ends_with('intdatm_dv'),
        dplyr::ends_with('intdaty_dv'),
        # residents variable
        # dplyr::ends_with('country'),
        dplyr::ends_with('gor_dv'),
        dplyr::ends_with('urban_dv'),
        # socio-economic & demographic characteristics
        dplyr::ends_with('doby_dv'),
        dplyr::ends_with('age_dv'),
        dplyr::ends_with('sex_dv'),
        dplyr::ends_with('ukborn'),
        dplyr::ends_with('bornuk_dv'),
        dplyr::ends_with('racel_dv'),
        dplyr::ends_with('ethn_dv'),
        dplyr::ends_with('mastat_dv'),
        dplyr::ends_with('nchild_dv'),
        dplyr::ends_with('hiqual_dv'),
        dplyr::ends_with('qfhigh_dv'),
        dplyr::ends_with('scend_dv'),
        dplyr::ends_with('jbstat'),
        dplyr::ends_with('jbsoc00_cc'),
        dplyr::ends_with('jbnssec8_dv'),
        dplyr::ends_with('jbnssec5_dv'),
        dplyr::ends_with('jbnssec3_dv'),
        # health & well-being
        dplyr::ends_with('sf1'),
        dplyr::ends_with('scsf1'),
        dplyr::ends_with('sf12mcs_dv'),
        dplyr::ends_with('sf12pcs_dv'),
        dplyr::ends_with('health'),
        dplyr::ends_with('scghq1_dv'),
        dplyr::ends_with('scghq2_dv'),
        dplyr::ends_with('sclfsato'),
        # individual & family background
        dplyr::ends_with('j1soc00_cc'),
        dplyr::ends_with('maid'),
        dplyr::ends_with('macob'),
        dplyr::ends_with('maedqf'),
        dplyr::ends_with('masoc90_cc'),
        dplyr::ends_with('masoc00_cc'),
        dplyr::ends_with('masoc10_cc'),
        dplyr::ends_with('paid'),
        dplyr::ends_with('pacob'),
        dplyr::ends_with('paedqf'),
        dplyr::ends_with('pasoc90_cc'),
        dplyr::ends_with('pasoc00_cc'),
        dplyr::ends_with('pasoc10_cc'),
        # household-level characteristics
        # dplyr::ends_with('hhsize'),
        dplyr::ends_with('nkids_dv'),
        dplyr::ends_with('hhtype_dv'),
        dplyr::ends_with('tenure_dv'),
        # individual & household income
        dplyr::ends_with('fimnnet_dv'),
        dplyr::ends_with('fimnlabnet_dv'),
        dplyr::ends_with('fimnmisc_dv'),
        dplyr::ends_with('fimnprben_dv'),
        dplyr::ends_with('fimninvnet_dv'),
        dplyr::ends_with('fimnpen_dv'),
        dplyr::ends_with('fimnsben_dv')
      )
  })
names(ind) <- gsub('\\.dta$', '', list.ind)

### extract dataframes from list ----

list2env(ind, .GlobalEnv)
# rm(ind)

## local authority district survey ----

list.lad <- list.files(path = data.survey.lad.dir, pattern = '*_oslaua_protect.dta')

### load in the files ----

lad <- lapply(paste0(data.survey.lad.dir, list.lad), haven::read_dta)
lad <- lapply(lad, function(x) {x})
names(lad) <- gsub('\\.dta$', '', list.lad)

### extract dataframes from list ----

list2env(lad, .GlobalEnv)
# rm(lad)

## lower layer super output area survey ----

list.lsao <- list.files(path = data.survey.lsoa.dir, pattern = '*_lsoa11_protect.dta')

### load in the files ----

lsao <- lapply(paste0(data.survey.lsoa.dir, list.lsao), haven::read_dta)
lsao <- lapply(lsao, function(x) {x})
names(lsao) <- gsub('\\.dta$', '', list.lsao)

### extract dataframes from list ----

list2env(lsao, .GlobalEnv)
# rm(lsao)

## combine data ----

### individual and household per wave ----

num.wave <- length(list.hh)

merg.terms <- 
  data.frame(ind = gsub('\\.dta$', '', list.ind),
             hh = gsub('\\.dta$', '', list.hh),
             lad = gsub('\\.dta$', '', list.lad),
             lsao = gsub('\\.dta$', '', list.lsao),
             by = paste0(gsub('\\_indresp.dta$', '', list.ind), '_hidp'))

u <- 
  apply(merg.terms, 1,
        function(x){
          final <- 
            (parse(text = x[1]) %>% eval) %>%
            dplyr::left_join(., (parse(text = x[2]) %>% eval), 
                             by = x[5] %>% as.character()) %>%
            dplyr::left_join(., (parse(text = x[3]) %>% eval), 
                             by = x[5] %>% as.character()) %>%
            dplyr::left_join(., (parse(text = x[4]) %>% eval), 
                             by = x[5] %>% as.character())
          final 
        })
names(u) <- paste0('u', 1:num.wave)

### all waves ----  

usoc <- Reduce(function(x,y) merge(x, y, by = 'pidp', all = TRUE), u)

## clean up environment ----

rm(list = ls(pattern = '[0-9]$'), ind, hh, lad)
rm(list = ls(pattern = 'resp'))

## clean up data frame ----

# renaming for pivoting
## remove _dv
names(usoc) <- str_replace(names(usoc), '_(dv)$', '')
names(usoc) <- str_replace(names(usoc), '_(cc)$', '')

# select weights
## most recent longitudinal
longitudinal.weight <-
  u[[paste0('u', num.wave)]] %>%
  dplyr::select(pidp, 
                dplyr::contains('indpxus_lw'),
                dplyr::contains('indinus_lw')) %>%
  dplyr::rename('longitudinalWeightProxy' = paste0(letters[num.wave], '_indpxus_lw'),
                'longitudinalWeight' = paste0(letters[num.wave], '_indinus_lw'))
## first cross sectional
cross.sectional.weight <-
  u[[paste0('u', 1)]] %>%
  dplyr::select(pidp, 
                dplyr::contains('indinus_xw')) %>%
  dplyr::rename('crossSectionalWeight' = paste0(letters[1], '_indinus_xw'))

# final formatting
## drop all the weighting columns
### Wave 1 does not have one and they are not needed due to the left_join later
## pivot longer each variable for waves are now long
## include the most recent weights using the pidp
usoc.long <- 
  usoc %>%
  select(-dplyr::contains(c('_indinus_xw', '_indpxus_lw', '_indinus_lw'))) %>%
  tidyr::pivot_longer(cols = -pidp, names_to = c('wave', '.value'), names_sep = '_') %>%
  dplyr::left_join(., longitudinal.weight, by = 'pidp') %>%
  dplyr::left_join(., cross.sectional.weight, by = 'pidp') %>%
  dplyr::relocate(c('hidp', 'crossSectionalWeight', 'longitudinalWeightProxy', 'longitudinalWeight'), .before = wave) %>%
  dplyr::mutate(interviewDate = paste(intdaty, intdatm, intdatd, sep = '-') %>% as.Date(., '%Y-%m-%d'),
                wave = wave %>% as.factor() %>% as.numeric()) %>%
  dplyr::relocate(interviewDate, .before = intdatd) %>%
  dplyr::select(-intdaty, -intdatm, -intdatd)

## include additional data ----

setwd(data.dir)

### spatial ----

# spatial polygons at lsoa and lad levels
poly.lsoa <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS11_LSOA')
poly.lad <- sf::st_read(dsn = data.spatial.dir, layer = 'ONS22_LAD')

# england only
poly.lsoa.england <- poly.lsoa %>%  dplyr::filter(str_detect(LSOA11CD, '^E'))
poly.lad.england <- poly.lad %>%  dplyr::filter(str_detect(LAD22CD, '^E'))

# make a-mats
lsoa.mat <- spdep::poly2nb(as(poly.lsoa.england, 'Spatial'))
lsoa.mat <- spdep::nb2mat(lsoa.mat, zero.policy = TRUE)
colnames(lsoa.mat) <- rownames(lsoa.mat) <- paste0('lsoa_', 1:dim(lsoa.mat)[1])
lsoa.names <- data.frame(LSOA11CD = poly.lsoa.england$LSOA11CD,
                         LSOA11NM = poly.lsoa.england$LSOA11NM,
                         Internal = rownames(lsoa.mat))

lad.mat <- spdep::poly2nb(as(poly.lad.england, 'Spatial'))
lad.mat <- spdep::nb2mat(lad.mat, zero.policy = TRUE)
colnames(lad.mat) <- rownames(lad.mat) <- paste0('lad_', 1:dim(lad.mat)[1])
lad.names <- data.frame(LAD22CD = poly.lad.england$LAD22CD,
                        LAD22NM = poly.lad.england$LAD22NM,
                        Internal = rownames(lad.mat))

### confounders ----

#### raw data ----

# look up from 2011 lsoa to 2021 lsoa and 2021 ltla
lookup.01.11 <- read.csv('rawData/UK Gov/LSOA01_LSOA11_LAD11_lookUp.csv')
lookup.11.21 <- read.csv('rawData/UK Gov/LSOA11_LSOA21_LAD22_lookUp.csv')

# raw imd data
load('Organised Data/LSOA11_2002_2021_IMD.rda')
# raw ethncity data
load('Organised Data/LSOA11_2002_2021_ETHNIC_DIVERSITY.rda')

#### sorted data ----

imd.02.21.sorted <-
  imd.02.21 %>%
  dplyr::mutate(DEPRIVATION = DEPRIVATION %>% factor()) %>%
  dplyr::select(LSOA11CD, YEAR, DEPRIVATION)

ed.02.21.sorted <-
  ed.02.21 %>%
  dplyr::mutate(DIVERSITY = DIVERSITY %>% factor()) %>%
  dplyr::select(LSOA11CD, YEAR, DIVERSITY)

#### combine with final data ----

# final data
data.final <-
  usoc.long %>%
  # # un comment for more clear output for checking
  # dplyr::select(pidp, wave, interviewDate, oslaua, lsoa11) %>%
  # remove NAs from interview and LAD11CD
  tidyr::drop_na(interviewDate, oslaua) %>%
  # ukhls naming convention to ons naming convention
  dplyr::rename('LSOA11CD' = 'lsoa11', 'LAD11CD' = 'oslaua') %>%
  # only england
  dplyr::filter(stringr::str_detect(LSOA11CD, '^E')) %>%
  # include LAD21 and LSOA21 codes and names
  dplyr::left_join(., lookup.11.21 %>% dplyr::select(LSOA11CD, LAD22NM, LAD22CD), by = 'LSOA11CD', relationship = 'many-to-many') %>%
  # join imd and diversity data
  ## need YEAR variable
  dplyr::mutate(YEAR = year(interviewDate)) %>%
  ## joins
  dplyr::left_join(., imd.02.21.sorted, by = c('LSOA11CD', 'YEAR'), relationship = 'many-to-many') %>%
  dplyr::left_join(., ed.02.21.sorted, by = c('LSOA11CD', 'YEAR'), relationship = 'many-to-many')

# saving ----

setwd(data.organised.dir)
saveRDS(object = data.final, file = 'ukhls_final.rds')
saveRDS(object = usoc.long, file = 'ukhls_dropout.rds')
save(lsoa.mat, lad.mat, file = 'aMat_england.rds')
save(lsoa.names, lad.names, file = 'aMat_names_england.rds')

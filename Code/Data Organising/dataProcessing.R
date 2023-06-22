# packages ----

library(tidyverse)
library(haven)
library(readxl)
library(sf)

# directories ----

# extract file location of this script
codePath <- rstudioapi::getActiveDocumentContext()$path
codePathSplitted <- strsplit(codePath, "/")[[1]]

# retrieve directories
homeDir <- paste(codePathSplitted[1: (length(codePathSplitted)-3)], collapse = "/")
codeDir <- paste0(homeDir, '/Code')
dataDir <- paste0(homeDir, '/Data')
surveyDir <- paste0(dataDir, '/rawData/STATA')
surveyDir_general <- paste0(surveyDir, '/UKDA-6614-stata/stata/stata13_se/ukhls/')
surveyDir_la <- paste0(surveyDir, '/UKDA-6666-stata/stata/stata13/ukhls/')
surveyDir_lsoa <- paste0(surveyDir, '/UKDA-7248-stata/stata/stata13/ukhls/')
dataDir_organised <- paste0(dataDir, '/Organised Data')

# organised data folder
if(!dir.exists(paths = dataDir_organised)) {
  dir.create(path = dataDir_organised)
}

# import functions ----

source(paste0(codeDir, '/functions.R'))

# ukhls data ----

## house hold survey ----

### list of files to import ----

listhh <- list.files(path = surveyDir_general, pattern = '*_hhresp.dta')

### load in the files ----

hh <- lapply(paste0(surveyDir_general, listhh), haven::read_dta)
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
names(hh) <- gsub('\\.dta$', '', listhh)

### extract dataframes from list ----

list2env(hh, .GlobalEnv)
# rm(hh)

## individual survey ----

listind <- list.files(path = surveyDir_general, pattern = '*_indresp.dta')

### load in the files ----

ind <- lapply(paste0(surveyDir_general, listind), haven::read_dta)
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
names(ind) <- gsub('\\.dta$', '', listind)

### extract dataframes from list ----

list2env(ind, .GlobalEnv)
# rm(ind)

## local authority survey ----

listla <- list.files(path = surveyDir_la, pattern = '*_oslaua_protect.dta')

### load in the files ----

la <- lapply(paste0(surveyDir_la, listla), haven::read_dta)
la <- lapply(la, function(x) {x})
names(la) <- gsub('\\.dta$', '', listla)

### extract dataframes from list ----

list2env(la, .GlobalEnv)
# rm(la)

## lower layer super output area survey ----

listlsao <- list.files(path = surveyDir_lsoa, pattern = '*_lsoa11_protect.dta')

### load in the files ----

lsao <- lapply(paste0(surveyDir_lsoa, listlsao), haven::read_dta)
lsao <- lapply(lsao, function(x) {x})
names(lsao) <- gsub('\\.dta$', '', listlsao)

### extract dataframes from list ----

list2env(lsao, .GlobalEnv)
# rm(lsao)

## combine data ----

### individual and household per wave ----

nWave <- length(listhh)

mergTerms <- 
  data.frame(ind = gsub('\\.dta$', '', listind),
             hh = gsub('\\.dta$', '', listhh),
             la = gsub('\\.dta$', '', listla),
             lsao = gsub('\\.dta$', '', listlsao),
             by = paste0(gsub('\\_indresp.dta$', '', listind), '_hidp'))

u <- 
  apply(mergTerms, 1,
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
names(u) <- paste0('u', 1:nWave)

### all waves ----  

usoc <- Reduce(function(x,y) merge(x, y, by = 'pidp', all = TRUE), u)

## clean up environment ----

rm(list = ls(pattern = '[0-9]$'), ind, hh, la)
rm(list = ls(pattern = 'resp'))

## clean up data frame ----

# renaming for pivoting
## remove _dv
names(usoc) <- str_replace(names(usoc), '_(dv)$', '')
names(usoc) <- str_replace(names(usoc), '_(cc)$', '')

# select weights
## most recent longitudinal
longitudinalWeight <-
  u[[paste0('u', nWave)]] %>%
  dplyr::select(pidp, 
                dplyr::contains('indpxus_lw'),
                dplyr::contains('indinus_lw')) %>%
  dplyr::rename('longitudinalWeightProxy' = paste0(letters[nWave], '_indpxus_lw'),
                'longitudinalWeight' = paste0(letters[nWave], '_indinus_lw'))
## first cross sectional
crossSectionalWeight <-
  u[[paste0('u', 1)]] %>%
  dplyr::select(pidp, 
                dplyr::contains('indinus_xw')) %>%
  dplyr::rename('crossSectionalWeight' = paste0(letters[1], '_indinus_xw'))

# final formatting
## drop all the weighting columns
### Wave 1 does not have one and they are not needed due to the left_join later
## pivot longer each variable for waves are now long
## include the most recent weights using the pidp
usocLong <- 
  usoc %>%
  select(-dplyr::contains(c('_indinus_xw', '_indpxus_lw', '_indinus_lw'))) %>%
  tidyr::pivot_longer(cols = -pidp, names_to = c('wave', '.value'), names_sep = '_') %>%
  dplyr::left_join(., longitudinalWeight, by = 'pidp') %>%
  dplyr::left_join(., crossSectionalWeight, by = 'pidp') %>%
  dplyr::relocate(c('hidp', 'crossSectionalWeight', 'longitudinalWeightProxy', 'longitudinalWeight'), .before = wave) %>%
  dplyr::mutate(interviewDate = paste(intdaty, intdatm, intdatd, sep = '-') %>% as.Date(., "%Y-%m-%d"),
                wave = wave %>% as.factor() %>% as.numeric()) %>%
  dplyr::relocate(interviewDate, .before = intdatd) %>%
  dplyr::select(-intdaty, -intdatm, -intdatd)

## include additional data ----

### spatial ----

# spatial at lsoa level for deprivation and diversity
onsShapePath <- 'Data/shapeFiles_ons2/'
lsoaPoly <- 
  sf::st_read(paste0(onsShapePath, 'ons21_GBR_LSOA_shp/ons21_GBR_LSOA.shp'))
lsoaPoly_England <- 
  lsoaPoly %>% 
  dplyr::filter(str_detect(LSOA21CD, "^E"))

# look up from 2011 lsoa to 2021 lsoa and 2021 ltla
lsoa11_lookup <- 
  read.csv('Data/rawData/UK Gov/lsoa11_to_lsao21_and_ltla22.csv') %>% 
  dplyr::rename(LAD21NM = 'LAD22NM',
                LAD21CD = 'LAD22CD') %>% 
  dplyr::filter(str_detect(LSOA11CD, "^E"))

### raw data ----

# raw imd data
imd2011 <- 
  read.csv('Data/rawData/UK Gov/imd2011_lsoa.csv') %>%
  dplyr::select(`LSOA.code..2011.`,
                `LSOA.name..2011.`,
                `Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.`) %>%
  dplyr::rename('LSOA11CD' = `LSOA.code..2011.`,
                'LSOA11NM' = `LSOA.name..2011.`,
                'imdRank' = `Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.`)

# raw ethncity data
ethnicity2021 <- 
  read.csv(file = 'Data/rawData/UK Gov/ethnicity2021_lsoa.csv', stringsAsFactors = TRUE) %>%
  dplyr::rename('LSOA21CD' = 'Lower.layer.Super.Output.Areas.Code',
                'ethnicityCD' = 'Ethnic.group..20.categories..Code',
                'count' = 'Observation') %>%
  # aggregate counts over bame and non-bame
  dplyr::mutate(ethnicityGroup = dplyr::if_else(ethnicityCD %in% 13:17, 'nonBameCount', 'bameCount')) %>% 
  dplyr::group_by(LSOA21CD, ethnicityGroup) %>% 
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_wider(names_from = ethnicityGroup, 
                     values_from = count) %>% 
  dplyr::mutate(
    # percentages
    bamePercentage = bameCount/(bameCount + nonBameCount),
    nonBamePercentage = nonBameCount/(bameCount + nonBameCount),
    # rank based on percentage
    bameRank = bamePercentage %>% dplyr::desc() %>% dplyr::dense_rank())

### sorted data ----

# deprivation data
deprivationData <- 
  lsoaPoly_England %>% 
  dplyr::select(LSOA21CD, LSOA21NM) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::left_join(., lsoa11_lookup %>% dplyr::select(LSOA11CD, LAD21CD, LAD21NM, LSOA21CD), by = 'LSOA21CD') %>% 
  dplyr::left_join(., imd2011, by = 'LSOA11CD') %>% 
  dplyr::select(LSOA11CD, LSOA21CD, LSOA21NM, LAD21NM, imdRank) %>%
  # include tiles
  dplyr::mutate(imd10 = dplyr::ntile(imdRank, 10) %>% factor(., labels = 1:10),
                imd5 = dplyr::ntile(imdRank, 5) %>% factor(., labels = 1:5),
                imd4 = dplyr::ntile(imdRank, 4) %>% factor(., labels = 1:4))

# diversity data
diversityData <-
  lsoaPoly_England %>% 
  dplyr::select(LSOA21CD, LSOA21NM) %>% 
  sf::st_drop_geometry()  %>% 
  dplyr::left_join(., lsoa11_lookup %>% dplyr::select(LSOA11CD, LAD21CD, LAD21NM, LSOA21CD), by = 'LSOA21CD') %>% 
  dplyr::left_join(., ethnicity2021, by = 'LSOA21CD') %>% 
  dplyr::select(LSOA11CD, LSOA21CD, LSOA21NM, LAD21NM, bamePercentage, bameRank) %>%
  # include tiles
  dplyr::mutate(bame10 = cut((1-bamePercentage), breaks = seq(from = 0, to = 1, by = 0.1)) %>% factor(., labels = 1:10),
                bame5 = cut((1-bamePercentage), breaks = seq(from = 0, to = 1, by = 0.2)) %>% factor(., labels = 1:5),
                bame4 = cut((1-bamePercentage), breaks = seq(from = 0, to = 1, by = 0.25)) %>% factor(., labels = 1:4))

### combine with final data ----

# final data 
dataFinal <-
  usocLong %>%
  # # un comment for more clear output for checking
  # dplyr::select(pidp, wave, interviewDate, oslaua, lsoa11) %>%
  # remove NAs from interview and LAD11CD
  tidyr::drop_na(interviewDate, oslaua) %>%
  # ukhls naming convention to ons naming convention
  dplyr::rename('LSOA11CD' = 'lsoa11', 'LAD11CD' = 'oslaua') %>%
  # only england
  dplyr::filter(stringr::str_detect(LSOA11CD, '^E')) %>%
  # include LAD21 and LSOA21 codes and names
  dplyr::left_join(., lsoa11_lookup %>% dplyr::select(LSOA11CD, LAD21NM, LAD21CD, LAD21CD, LSOA21CD, LSOA21NM), by = 'LSOA11CD', relationship = 'many-to-many') %>% 
  # include imd11 data
  dplyr::left_join(., deprivationData %>% dplyr::select(-LSOA21CD, -LSOA21NM, -LAD21NM), by = 'LSOA11CD', relationship = 'many-to-many') %>%
  # include bame21 data
  dplyr::left_join(., diversityData %>% dplyr::select(-LSOA21CD, -LSOA21NM, -LAD21NM), by = 'LSOA11CD', relationship = 'many-to-many')


# spatial data ----

ltlaPoly <- 
  sf::st_read(paste0(onsShapePath, 'ons21_GBR_LTLA_shp/ons21_GBR_LTLA.shp'))
ltlaPoly_England <- 
  ltlaPoly %>% 
  dplyr::filter(str_detect(LAD21CD, "^E"))

aMat_england <- getAmat(geo = ltlaPoly_England, names = ltlaPoly_England$LAD21NM)
colnames(aMat_england) <- rownames(aMat_england) <- ltlaPoly_England$LAD21NM

# saving ----

## final save ----

saveRDS(object = dataFinal,
        file = paste0(dataDir_organised, '/ukhls_final.rds'))

saveRDS(object = usocLong,
        file = paste0(dataDir_organised, '/ukhls_dropout.rds'))

saveRDS(aMat_england, 
        file = paste0(dataDir_organised, '/aMat_england.rds'))

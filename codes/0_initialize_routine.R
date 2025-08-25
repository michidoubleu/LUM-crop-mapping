##### CODE 0: initialisation for cluster

#### Author: Michael Wögerer
#### Date: Dec/2024

prior.path <- "./input/priors/new/"
EEA.path <- "./input/EEA_1km/"
LUM.path <- "./input/LUM_data/"
mapping.path <- "./input/mappings/"
LUM.energy.path <- "./input/LUM_fit_with_energy_levels_and_new_FM/"
SimU.path <- "./input/SIMU_LAEA/5arcmin_simu_world_ETRS_1989_LAEA_wsimuID.shp"
#### cluster setup


  curr.year <- as.character(clustergrid[JOB,"years"])
  capri.reg <- as.character(clustergrid[JOB,"capri.regs"])
  country <- as.character(clustergrid[JOB,"countries"])
  lama.reg <- as.character(clustergrid[JOB,"lama.regs"])

  # curr.year <- "2010"
  # capri.reg <- "AT120000"
  # country <- "Austria"
  # lama.reg <- "AT12"

##### identify curr prior
prior <- list.files(prior.path, full.names = TRUE)
prior <- prior[grepl(curr.year,prior)]

#### identify curr LUM map
LUM <- list.files(LUM.path, full.names = TRUE)
LUM <- LUM[grepl(curr.year,LUM)]
LUM <- LUM[!grepl(".ovr",LUM)]
LUM <- LUM[!grepl(".qml",LUM)]
LUM <- LUM[!grepl(".aux",LUM)]

#### identify curr LUMenergy map
LUMenergy <- list.files(LUM.energy.path, full.names = TRUE)
LUMenergy <- LUMenergy[grepl(curr.year,LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".ovr",LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".qml",LUMenergy)]
LUMenergy <- LUMenergy[!grepl(".aux",LUMenergy)]

#### identify curr EEA map
EEA.grid <- list.files(EEA.path, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(country,EEA.grid)]
EEA.grid <- list.files(EEA.grid, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(".shp",EEA.grid)]

#### irrigation map
irrig.path <- list.files("input/irrigation/", full.names = T)

#### load mapping for LUM-croparea
lum_crop_map <- c(seq(2001,2005,1),seq(3001,3005,1))

#### load mapping for country
EEA_NUTS <- read.csv(paste0(mapping.path,"EEAref_LAMAnuts_mapping_oct17.csv"))
length.curr.nuts <- nchar(lama.reg)
curr.EEA_NUTS <- EEA_NUTS %>% filter(substr(NUTS_ID,1,length.curr.nuts)==lama.reg)

#### load mapping for country
# crop_mapping <- read.csv(paste0(mapping.path,"GLOB_crop_itemmapping.csv"))
# colnames(crop_mapping)[1] <- "GLOBIOM"
# setDT(crop_mapping)

crop_mapping <- read.csv(paste0(mapping.path,"CAPRI_GLOB_mapping.csv"))
colnames(crop_mapping)[1] <- "CAPRI"
setDT(crop_mapping)



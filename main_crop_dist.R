##### CODE MAIN: main file to apply settings and launch model.

### Loop and cluster integration done at this stage
### can be easily adapted to local execution by replacing cluster grid with loop

#### Author: Michael Wögerer
#### Date: Jan/2025

rm(list = ls())
gc()

## Load libraries and run settings
library(dplyr)
library(tidyr)
library(sf)
library(readr)
library(terra)
library(exactextractr)
library(countrycode)
library(data.table)
library(downscalr)
library(stringr)

args <- commandArgs(trailingOnly=TRUE)
JOB <- ifelse(.Platform$GUI == "RStudio",199,as.integer(args[[1]]))
dir.create("output")


## settings for the run(s)
CLUSTER <- TRUE
mixed.targets <- TRUE
GLOB.target <- TRUE
#### IF TARGETS NEED TO BE REPROCESSED
#source("./codes/test_cropdata_load.R")

clustergrid <- readRDS(file="output/cluster_cropdist.rds")
clustergrid <- clustergrid %>% arrange(desc(years),lama.regs) %>% filter(countries=="Austria")

if(CLUSTER==FALSE){
  clustergrid <- clustergrid[JOB,]
}


if(CLUSTER==FALSE){

  for (JOB in 1:nrow(clustergrid)) {
    JOB=1
    rm(list = setdiff(ls(), c("JOB", "clustergrid", "CLUSTER", "mixed.targets", "GLOB.target")))
    gc()  # Run garbage collection to free up memory
    message("Currently at ", JOB, " / ", nrow(clustergrid))

    ## ROUTINE STARTS HERE
    tryCatch({
      source("./codes/0_initialize_routine.R", local = TRUE)
      source("./codes/1_prepare_startmap_Linda_LUM.R", local = TRUE)
      source("./codes/2_prepare_priors_Storm.R", local = TRUE)
      source("./codes/3_load_EPIC.R", local = TRUE)
      source("./codes/4_prepare_CAPRI_targets.R", local = TRUE)
      source("./codes/5_area_matching.R", local = TRUE)
      source("./codes/6_production_matching.R", local = TRUE)

      saveRDS(list(final.alloc, show.res, result),
            file = paste0("output/res_v1/", lama.reg, "_", curr.year, "_res_v1.rds"))

    }, error = function(e) {
      message("Error in iteration ", JOB, ": ", conditionMessage(e))
      saveRDS("failed", file = paste0("output/res_v1/","FAILED_" ,lama.reg, "_", curr.year, "_res_v1.rds"))
    })
  }

} else {
  save.image("input/temp_image.RData")

  #### cleaning of output folder before cluster use
  file.remove(list.files("./output/", pattern = "CDDS_", full.names = TRUE))

  source("./codes/S1_update_config.R")
  system("run_CDDS.bat")

}

source("./codes/7_summarise_CDDS.R")

# source("./codes/8_aggreg_res_SIMU_LU_grid.R")











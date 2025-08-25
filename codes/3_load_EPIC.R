##### CODE 5: EPIC matiching

#### Author: Michael Wögerer
#### Date: 03/02/2025
source("input/mappings/FAO_eurostat_EPIC_GLOBIOM_crop_mapping.R")

# # Read the shapefile (slow, but only once)
# EPIC_shp <- read_sf("./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.shp")
#
# # Save it as an RDS file
# saveRDS(EPIC_shp, file = "./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.rds")

EPIC_shp <- readRDS("./input/EPIC/MAP/EUEPIC_SimUID_1k_updt.rds")
EPIC_shp <- st_crop(EPIC_shp,ext(temp.EEA))

# Ensure CRS is the same
temp.EEA <- st_transform(temp.EEA, st_crs(EPIC_shp))
# Perform spatial join (assign CELLCODE to each point in EPIC_shp)
EPIC_mapping <- st_join(EPIC_shp, temp.EEA, left = FALSE) %>%
  select(CELLCODE, SimUID) %>% st_drop_geometry()


YLDG_data <- readRDS(file="input/EPIC_yields.rds")

YLDG_data <- YLDG_data %>% filter(SimUID %in% unique(EPIC_mapping$SimUID))


final_yields <- EPIC_mapping %>% full_join(YLDG_data %>% dplyr::select(SimUID, Scen, CROP, YLD))




# Step 1: Identify missing CELLCODE/SimUID rows
missing_rows <- final_yields %>%
  filter(is.na(Scen) & is.na(CROP))

# Step 2: Compute average YLD for each Scen and CROP over all available data
avg_yields <- final_yields %>%
  filter(!is.na(Scen) & !is.na(CROP) & !is.na(YLD)) %>%
  group_by(Scen, CROP) %>%
  summarise(avg_YLD = mean(YLD, na.rm = TRUE), .groups = "drop")

# Step 3: Create a full combination of missing CELLCODE/SimUID with all Scens and Crops
expanded_rows <- missing_rows %>%
  select(CELLCODE, SimUID) %>%
  distinct() %>%
  crossing(avg_yields)  # Generates all combinations

# Step 4: Bind back to original dataset
final_yields <- final_yields %>%
  filter(!is.na(Scen) & !is.na(CROP)) %>%  # Keep original non-missing rows
  bind_rows(expanded_rows %>%
              rename(YLD = avg_YLD))       # Add filled-in rows

final_yields <- final_yields %>% left_join(eurostat_GLOBIOM_mapping %>% dplyr::select(EPIC_abr, GLOBIOM_abr) %>% rename("CROP"="EPIC_abr")) %>% dplyr::select(CELLCODE, Scen, GLOBIOM_abr, YLD)



##### Hopefully temporal fix: Add yield for crops outside of new EPIC runs
### Load GLOBIOM crop data
final.cropdat.old <- readRDS( file = "input/cropyieldsOLD_GLOBIOM.rds")
### add SimU to cellcode mapping
simu.shp <- read_sf(SimU.path)
simu.shp <- st_crop(simu.shp,ext(temp.EEA))
simu.shp <- simu.shp %>% dplyr::select(-uniqueID)


simu_mapping <- st_join(simu.shp, temp.EEA, left = FALSE) %>%
  select(CELLCODE, simuID) %>% st_drop_geometry() %>% rename("SimUID"="simuID")


final.cropdat.old <- left_join(simu_mapping %>% mutate(SimUID=as.character(SimUID)),final.cropdat.old) %>% dplyr::select(-SimUID) %>% filter(!GLOBIOM_abr%in%unique(final_yields$GLOBIOM_abr))


final_yields <- final_yields %>% bind_rows(final.cropdat.old)








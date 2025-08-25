##### CODE A4: Show country res

#### Author: Michael Wögerer
#### Date: 28/01/2025

## choose country
res.country <- "AT"

result.files <- list.files("output/res_v1/", full.names = T)
result.files <- result.files[grepl(res.country,result.files)]
result.files <- result.files[!grepl("FAILED",result.files)]


file <- result.files[1]
final.areas <- NULL
for(file in result.files){

  temp.res <- readRDS(file)
  temp.areas <- temp.res[[1]]

  final.areas <- final.areas %>% bind_rows(temp.areas)

}

curr.country <- unique(clustergrid$countries[substr(clustergrid$lama.regs,1,2)==substr(res.country,1,2)])
EEA.grid <- list.files(EEA.path, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(curr.country,EEA.grid)]
EEA.grid <- list.files(EEA.grid, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(".shp",EEA.grid)]

temp.EEA <- st_read(EEA.grid, quiet = TRUE)

LAMA.NUTS <- read_sf("input/NUTS_LAMASUS/shp_nuts.shp")

LAMA.NUTS <- LAMA.NUTS %>% filter(LEVL_CODE==0, NUTS_ID!=res.country)

final.areas.grouped <- final.areas %>% filter(year=="2018") %>% group_by(lu.to, region) %>% mutate(value=sum(value))%>% group_by(CELLCODE, lu.to)  %>% summarise(value=mean(value))
final.areas <- final.areas %>% filter(year=="2018") %>% group_by(CELLCODE) %>% summarize(value=sum(value))



# Join the result data to the spatial object
temp.EEA.forplot <- temp.EEA %>%
  left_join(final.areas)

# Get the bounding box of the map data
bbox <- st_bbox(temp.EEA.forplot)

# Use the bounding box to set the limits of the plot
ggplot(data = temp.EEA.forplot) +
  geom_sf(aes(fill = value), color = NA) +  # Color polygons by `sum_value`
  scale_fill_viridis_c(option = "viridis", name = paste("Share of arable cropland")) +
  theme_minimal() +
  labs(title = paste("Map of arable cropland share per 1km² pixel"), x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", axis.text = element_blank(),
        axis.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(barwidth = 20, barheight = 1)) +
  geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))

ggsave("output/plots/AT_arable.png", last_plot(), width = 8, height = 6, dpi = 300, units = "in")



crop.for.plot <- "Whea"
# Join the result data to the spatial object
temp.EEA.forplot <- temp.EEA %>%
  left_join(final.areas.grouped %>% filter(lu.to==crop.for.plot))

# Use the bounding box to set the limits of the plot
ggplot(data = temp.EEA.forplot) +
  geom_sf(aes(fill = value), color = NA) +  # Color polygons by `sum_value`
  scale_fill_viridis_c(option = "viridis", name = paste("Wheat area [km²]")) +
  theme_minimal() +
  labs(title = paste("NUTS2 harvested area of wheat"), x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", axis.text = element_blank(),
        axis.title = element_blank(), plot.title = element_blank(), legend.title = element_text(size = 16), legend.text = element_text(size = 14) ) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.4)) +
  geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))

ggsave("output/plots/AT_regional.png", last_plot(), width = 5, height = 3, dpi = 300, units = "in")

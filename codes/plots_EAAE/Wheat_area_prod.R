##### CODE A4: Show country res

#### Author: Michael Wögerer
#### Date: 28/01/2025

## choose country
res.country <- "AT"

result.files <- list.files("output/res_v1/", full.names = T)
result.files <- result.files[grepl(res.country,result.files)]
result.files <- result.files[!grepl("FAILED",result.files)]
#result.files <- result.files[grepl("2018",result.files)]

file <- result.files[1]
final.areas <- NULL
final.unharmo <- NULL
for(file in result.files){

    temp.res <- readRDS(file)
    temp.areas <- temp.res[[1]]
   #unharmo.area <- temp.res[[3]]
    final.unharmo <- final.unharmo %>% bind_rows(unharmo.area)
    final.areas <- final.areas %>% bind_rows(temp.areas)

}

curr.country <- unique(clustergrid$countries[substr(clustergrid$lama.regs,1,2)==substr(res.country,1,2)])
EEA.grid <- list.files(EEA.path, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(curr.country,EEA.grid)]
EEA.grid <- list.files(EEA.grid, full.names = TRUE)
EEA.grid <- EEA.grid[grepl(".shp",EEA.grid)]

temp.EEA <- st_read(EEA.grid, quiet = TRUE)


source("codes/5_load_epic.R")


LAMA.NUTS <- read_sf("input/NUTS_LAMASUS/shp_nuts.shp")

LAMA.NUTS <- LAMA.NUTS %>% filter(LEVL_CODE==0, NUTS_ID!=res.country)


final.areas <- final.areas %>% left_join(final_yields %>% rename("lu.to"="GLOBIOM_abr", "LUM.class"="Scen"))
final.areas$production=final.areas$value*final.areas$YLD
final.areas$area <- final.areas$value
final.areas <- final.areas %>% dplyr::select(-value) %>%
  pivot_longer(cols = c(area, production),
               names_to = "measure",
               values_to = "value")



# Join the result data to the spatial object
temp.EEA.forplot <- temp.EEA %>%
  left_join(final.areas %>% group_by(CELLCODE, year, lu.to, measure) %>% summarise(value=sum(value,na.rm=T)))

# Filter for a specific `lu.to` value (e.g., "OCER")
lu_to_value <- "Whea"
temp.EEA.filtered <- temp.EEA.forplot %>%
  filter(lu.to == lu_to_value)

# # Plot using ggplot
# ggplot(data = temp.EEA.filtered) +
#   geom_sf(aes(fill = value), color = NA) + # Color polygons by `sum_value`
#   scale_fill_viridis_c(option = "viridis", name = paste("Sum Value for", lu_to_value)) +
#   theme_minimal() +
#   labs(title = paste("Map of", lu_to_value, "Values"), x = "Longitude", y = "Latitude") +
#   theme(legend.position = "bottom")

library(ggplot2)
library(patchwork)
library(scales)  # For `squish()`


# Get the bounding box of the map data
bbox <- st_bbox(temp.EEA.filtered)

# # Create the first plot for `area`
# p1 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="area")) +
#   geom_sf(aes(fill = value), color = NA) +
#   scale_fill_viridis_c(option = "viridis", name = "Area [km²]",
#                        limits = c(0, 0.8), breaks=seq(0,0.8,0.2), oob = scales::squish) +  # Caps values at 0 and 1
#   theme_minimal() +
#   facet_wrap(~year, ncol =1)+
#   labs(title = paste("Map of", lu_to_value, "- Area"), x = "Longitude", y = "Latitude") +
#   theme(legend.position = "bottom")
#
# # Create the second plot for `production`
# p2 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="production")) +
#   geom_sf(aes(fill = value), color = NA) +
#   scale_fill_viridis_c(option = "viridis", name = "Production [1000t]",
#                        limits = c(0, 5), breaks=seq(0,5,1), oob = scales::squish) +  # Caps values at 0 and 1
#   theme_minimal() +
#   facet_wrap(~year, ncol =1)+
#   labs(title = paste("Map of", lu_to_value, "- Production"), x = "Longitude", y = "Latitude") +
#   theme(legend.position = "bottom")

p1 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="area")) +
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c(option = "viridis", name = "Area\n[km²] ",
                       limits = c(0, 0.4), breaks = seq(0, 0.4, 0.1),
                       oob = scales::squish) +
  facet_wrap(~year, ncol = 1, strip.position = "left") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",   # keep strips outside plot panel
    strip.text.y.left = element_text(angle = 90), # keep horizontal labels
    strip.switch.pad.grid = unit(0.1, "cm")
  ) +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 0.4)) +
  geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))



# Create the second plot for `production`
p2 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="production")) +
  geom_sf(aes(fill = value), color = NA) +  # Color polygons by `sum_value`
  scale_fill_viridis_c(option = "viridis", name = "Production\n[1000t]",
                       limits = c(0, 3), breaks=seq(0,3,1), oob = scales::squish) +  # Caps values at 0 and 5
  theme_minimal() +
  facet_wrap(~year, ncol = 1, strip.position = "right") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5),
    strip.placement = "outside",   # keep strips outside plot panel
    strip.text.y.left = element_text(angle = 90), # keep horizontal labels
    strip.switch.pad.grid = unit(0.1, "cm")
  ) +
  guides(fill = guide_colorbar(barwidth = 5, barheight = 0.4)) +
  geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))
















#
# # Create the first plot for `area`
# p1 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="area")) +
#   geom_sf(aes(fill = value), color = NA) +  # Color polygons by `sum_value`
#   scale_fill_viridis_c(option = "viridis", name = "Area\n[km²] ",
#                        limits = c(0, 0.4), breaks=seq(0,0.4,0.1), oob = scales::squish) +  # Caps values at 0 and 0.8
#   theme_minimal() +
#   facet_wrap(~year, ncol =1) +
#  # labs(title = paste("Wheat - Area"), x = "Longitude", y = "Latitude") +
#   theme(legend.position = "bottom") +
#   guides(fill = guide_colorbar(barwidth = 6, barheight = 0.4)) +
#   # Adding grey polygons and borders from LAMA.NUTS
#   geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
#   theme(legend.position = "bottom", axis.text = element_blank(),
#         axis.title = element_blank(), plot.title = element_text(hjust = 0.5)) +  # Fill polygons with lightgrey and borders with darkgrey
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"]))
#
# # Create the second plot for `production`
# p2 <- ggplot(data = temp.EEA.filtered %>% filter(measure=="production")) +
#   geom_sf(aes(fill = value), color = NA) +  # Color polygons by `sum_value`
#   scale_fill_viridis_c(option = "viridis", name = "Production\n[1000t]",
#                        limits = c(0, 3), breaks=seq(0,3,1), oob = scales::squish) +  # Caps values at 0 and 5
#   theme_minimal() +
#   facet_wrap(~year, ncol =1) +
#  # labs(title = paste("Wheat - Production"), x = "Longitude", y = "Latitude") +
#   theme(legend.position = "bottom", axis.text = element_blank(),
#         axis.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   guides(fill = guide_colorbar(barwidth = 6, barheight = 0.4)) +
#   # Adding grey polygons and borders from LAMA.NUTS
#   geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
#   theme(legend.position = "bottom") +  # Fill polygons with lightgrey and borders with darkgrey
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
#            ylim = c(bbox["ymin"], bbox["ymax"]))

# Stack the maps vertically
wrap_plots(p1, p2, nrow = 1)

wrap_plots(p1 + theme(plot.margin = margin(0, 1, 0, 0)),
           p2 + theme(plot.margin = margin(0, 0, 0, 1)),
           nrow = 1)

ggsave(paste0("output/plots/",res.country,"_final.png"), last_plot(), width = 5, height = 4, dpi = 300, units = "in")










# Join the result data to the spatial object
temp.EEA.unharmo <- temp.EEA %>%
  left_join(final.unharmo %>% rename("CELLCODE"="ns"))

# Filter for a specific `lu.to` value (e.g., "OCER")
lu_to_value <- "Whea"
temp.EEA.unharmo <- temp.EEA.unharmo %>%
  filter(lu.to == lu_to_value)





library(ggplot2)
library(patchwork)
library(scales)  # For `squish()`


# Get the bounding box of the map data
bbox <- st_bbox(temp.EEA.unharmo)


# Plot using ggplot
ggplot(data = temp.EEA.unharmo) +
  geom_sf(aes(fill = sum_value), color = NA) +  # Color polygons by `sum_value`
  scale_fill_viridis_c(option = "viridis", name = "Area [km²]  ",
                       limits = c(0, 0.4), breaks=seq(0,0.4,0.1), oob = scales::squish) +  # Caps values at 0 and 5
  theme_minimal() +
  labs(title = paste("Map of unharmonized wheat area 2018"), x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom", axis.text = element_blank(),
        axis.title = element_blank(), plot.title = element_blank(), legend.title = element_text(size = 16), legend.text = element_text(size = 14) ) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.4)) +
  # Adding grey polygons and borders from LAMA.NUTS
  geom_sf(data = LAMA.NUTS, fill = "lightgrey", color = "darkgrey", linewidth = 1) +
  theme(legend.position = "bottom") +  # Fill polygons with lightgrey and borders with darkgrey
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]))


ggsave("output/plots/AT_wheat.png", last_plot(), width = 5, height = 3, dpi = 300, units = "in")

##
## 15-degree grid map
## 
## ------------------------------------------------------------------------- ###

### Libraries ####
library(dplyr)
library(sf)
library(ggplot2)

### Read data ####
world <- rnaturalearth::ne_countries(returnclass = "sf")
# plot(world)

grid <- sf::st_read("./data/grid_15.gpkg")
# plot(grid)

### Map ####

# sf::sf_use_s2(FALSE)

map <-
  ggplot() +
  geom_sf(data = world, colour = NA, fill = "grey80") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

map <-
  map + 
  geom_sf(data = grid, fill = NA, colour = "black") +
  geom_sf_text(data = grid, aes(label = id)) + 
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = c(seq(from = -180, to = 180, by = 15))) + 
  scale_y_continuous(breaks = c(seq(from = -90, to = 90, by = 15))) + 
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(map,
       filename = "./results/map-15-degree-labelled-grid-cells.pdf",
       height = 14, width = 22, units = "cm", dpi = 200)

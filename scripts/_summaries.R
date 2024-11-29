##
## Summaries -- Random sample
## 
## ------------------------------------------------------------------------- ###

### Libraries ####
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(ggplot2)
library(patchwork)
library(ggalluvial)

### Data ####

## Grid ---------------------------------------------------- ##
grid15 <- sf::st_read("./data/grid_15.gpkg")

## All files ----------------------------------------------- ##

### "at-sea surveys"

# List file names
files_at_sea <- list.files(path = "/home/nicholas/Dropbox/Nico_PhD/at_sea_ship_surveys/")

# Split 'files' vector into a data.frame with four columns
files_at_sea <- 
  files_at_sea %>% 
  stringr::str_split_fixed(pattern = "_", n = 4) %>% 
  as.data.frame()

# Remove the last 9 rows, which are from folders/.doc file names
files_at_sea <- head(files_at_sea, -9)

# Rename columns to meaningful names
files_at_sea <- 
  files_at_sea %>% 
  dplyr::transmute(year = as.numeric(V1),
                   authors = V2,
                   journal = V3,
                   title = gsub('.{4}$', '', V4)) %>%
  dplyr::filter(year >= 1971) %>%
  dplyr::filter(year < 2022)

### "methods"
files_methods <- 
  list.files(path = "/home/nicholas/Dropbox/Nico_PhD/at_sea_methods/")

# Split 'files_yet_to_be_screened' vector into a data.frame with four columns
files_methods <- 
  files_methods %>% 
  stringr::str_split_fixed(pattern = "_", n = 4) %>% 
  as.data.frame()

# Remove the last 3 rows, which are from folders and tidy up column names
files_methods <- 
  head(files_methods, -3) %>% 
  dplyr::transmute(year = as.numeric(V1),
                   authors = V2,
                   journal = V3,
                   title = gsub('.{4}$', '', V4))

#### "to be screened"
files_yet_to_be_screened_for_refs <- 
  list.files(path = "/home/nicholas/Dropbox/Nico_PhD/")

# Split 'files_yet_to_be_screened' vector into a data.frame with four columns
files_yet_to_be_screened_for_refs <- 
  files_yet_to_be_screened_for_refs %>% 
  stringr::str_split_fixed(pattern = "_", n = 4) %>% 
  as.data.frame()

# Remove the last 8 rows, which are from folder or .doc names and tidy up column names
files_yet_to_be_screened_for_refs <- 
  head(files_yet_to_be_screened_for_refs, -8) %>% 
  dplyr::transmute(year = as.numeric(V1),
                   authors = V2,
                   journal = V3,
                   title = gsub('.{4}$', '', V4))

files_all <- 
  rbind(files_at_sea,
        files_methods,
        files_yet_to_be_screened_for_refs) %>% 
  dplyr::arrange(year) %>% 
  dplyr::mutate(five_yr_bin = dplyr::case_when(
    year >= 1971 & year<= 1975 ~ "1971_1975",
    year >= 1976 & year<= 1980 ~ "1976_1980",
    year >= 1981 & year<= 1985 ~ "1981_1985",
    year >= 1986 & year<= 1990 ~ "1986_1990",
    year >= 1991 & year<= 1995 ~ "1991_1995",
    year >= 1996 & year<= 2000 ~ "1996_2000",
    year >= 2001 & year<= 2005 ~ "2001_2005",
    year >= 2006 & year<= 2010 ~ "2006_2010",
    year >= 2011 & year<= 2015 ~ "2011_2015",
    year >= 2016 & year<= 2020 ~ "2016_2020"
  ), .before = year)

## --------------------------------------------------------- ##
## --------------------------------------------------------- ##
## Random selection ---------------------------------------- ##
## --------------------------------------------------------- ##
## --------------------------------------------------------- ##

data_random <- 
  read.csv("./data/random-selection_edited.csv") %>%
  dplyr::group_by(title) %>% 
  dplyr::mutate(id = cur_group_id(), .before = everything()) %>% 
  dplyr::ungroup()

data_random_pretty <- 
  data_random %>% 
  dplyr::mutate(preferential_hemisphere = case_when(
    preferential_hemisphere == "both" ~ "Both",
    preferential_hemisphere == "north" ~ "North hemisphere",
    preferential_hemisphere == "south" ~ "South hemisphere"
    )) %>% 
  dplyr::mutate(preferential_habitat = case_when(
    preferential_habitat == "coastal" ~ "Coastal",
    preferential_habitat == "oceanic" ~ "Oceanic"
  )) %>% 
  dplyr::mutate(sampling_protocol = case_when(
    sampling_protocol == "other" ~ "Other",
    sampling_protocol == "biomass" ~ "BIOMASS",
    sampling_protocol == "tasker" ~ "Tasker et al. 1984",
    sampling_protocol == "buckland" ~ "Buckland et al. 2001",
    sampling_protocol == "spear" ~ "Spear et al. 1992",
    sampling_protocol == "tasker, gould_forsell" ~ "Tasker et al. 1984",
    sampling_protocol == "gould_forsell" ~ "Gould & Forsell 1989"
  )) %>% 
  dplyr::mutate(method = case_when(
    method == "log_book" ~ "Log book",
    method == "strip_transect" ~ "Strip transect",
    method == "distance_sampling" ~ "Distance sampling",
    method == "max_numer" ~ "Maximum number"
  )) %>% 
  dplyr::mutate(type_of_count = case_when(
    type_of_count == "not specified" ~ "Not specified",
    type_of_count == "continuous" ~ "Countinuos",
    type_of_count == "continuous_snapshot" ~ "Continuous and Snapshot",
    type_of_count == "snapshot" ~ "Snapshot",
    type_of_count == "radial" ~ "Radial"
  )) %>% 
  dplyr::mutate(attraction_bias = case_when(
    attraction_bias == "none" ~ "None",
    attraction_bias == "research_fishing" ~ "Research fishing",
    attraction_bias == "commercial_fishing" ~ "Commercial fishing", 
    attraction_bias == "research_fishing, none" ~ "Research fishing",
    attraction_bias == "commercial_fishing, none" ~ "Commercial fishing", 
    attraction_bias == "chumming" ~ "Chumming"
  )) %>% 
  tidyr::separate_longer_delim(study_purpose, delim = ", ") %>% 
  dplyr::mutate(study_purpose = case_when(
    study_purpose == "distribution" ~ "Distribution",
    study_purpose == "natural_history" ~ "Natural history",
    study_purpose == "predator_prey" ~ "Predator-Prey",
    study_purpose == "ecology" ~ "Ecology",
    study_purpose == "community_ecology" ~ "Community ecology",
    study_purpose == "method" ~ "Method",
    study_purpose == "abundance" ~ "Abundance",
    study_purpose == "conservation" ~ "Conservation"
  )) %>% 
  tidyr::separate_longer_delim(stats_technique, delim = ", ") %>% 
  dplyr::mutate(stats_technique = case_when(
    stats_technique == "qualitative" ~ "Qualitative",
    stats_technique == "hypothesis_testing" ~ "Hypothesis testing",
    stats_technique == "modelling" ~ "Modelling",
    stats_technique == "multivariate" ~ "Multivariate",
    stats_technique == "spatial_stats" ~ "Spatial statistics"
    ))

### Summaries --- playing around a bit... ####

## (i) Temporal trends ####

# a) Histogram: Yearly number of publications (per hemisphere)
# a.1) Total
# a.2) Random sample, per hemisphere

pubs_per_year_total <-
  ggplot(data = files_all,
         aes(x = year)) +
  geom_bar(fill = "steelblue") + 
  xlab("") + ylab("Number of publications") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))

pubs_per_year_sample <-
  ggplot(data = (data_random_pretty %>% 
                   dplyr::distinct(id, .keep_all = TRUE)),
         aes(x = year_publication, fill = preferential_hemisphere)) +
  geom_bar(position = "stack") + 
  scale_fill_manual(values = c("khaki", "olivedrab3", "lightcoral"), name = "") + 
  xlab("") + ylab("Number of publications") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.position = "top",
        legend.text = element_text(size = 8))

histogram_pubs_per_year <-
  pubs_per_year_total / 
  pubs_per_year_sample + 
  patchwork::plot_annotation(tag_levels = 'A')

# ggsave(histogram_pubs_per_year,
#        filename = "./results/histogram-number-pubs-per-year.pdf",
#        height = 12, width = 12, units = "cm", dpi = 200)

rm(histogram_pubs_per_year, pubs_per_year_total, pubs_per_year_sample)

# b) Heatmap: Month x Year, by preferential hemisphere
# --- Can I make this happen??? Probably worth trying for the publication

## (ii) Spatial patterns of effort ####

# a) Histogram: Number of grid cells by study 
#    (gives an idea of spatial scale of sampling)

n_grids_per_study <-
  data_random %>% 
  dplyr::select(title, study_area_grids) %>% 
  tidyr::separate_longer_delim(study_area_grids, delim = ", ") %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::group_by(title) %>% 
  dplyr::summarise(n_grids = n_distinct(study_area_grids))

histogram_grids_per_study <-
  ggplot(data = n_grids_per_study,
         aes(x = n_grids)) +
  geom_bar(fill = "steelblue") + 
  scale_x_continuous(breaks = c(0:28), 
                     labels = c("", as.character(seq(1:7)), "", "9", rep("", 2), "12", rep("", 5), 
                                "18", "", "20", rep("", 2), "23", rep("", 4), "28")) + 
  xlab("Number of grids") + ylab("Number of studies") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank())

# ggsave(histogram_grids_per_study,
#        filename = "./results/histogram-grids-per-study.pdf",
#        height = 6, width = 12, units = "cm", dpi = 200)

rm(n_grids_per_study, histogram_grids_per_study)

# b) Barplot: Oceanic x Coastal

habitat_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, preferential_habitat) %>% 
  dplyr::summarise(n_habitat = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_habitat <-
  ggplot(habitat_per_5yr, 
         aes(x = five_yr_bin, y = n_habitat, fill = preferential_habitat)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = c("forestgreen", "lightgreen"), na.value = "grey80",
                    name = "") + 
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 8))

# ggsave(barplot_habitat,
#        filename = "./results/barplot-percent-costal-oceanic.pdf",
#        height = 8, width = 12, units = "cm", dpi = 200)

rm(habitat_per_5yr, barplot_habitat)

# c) Number of publications per 15x15 grid cells; total and per 5-yr bin

studies_per_grids_per_5yr <-
  data_random %>% 
  dplyr::select(five_yr_bin, title, study_area_grids) %>% 
  tidyr::separate_longer_delim(study_area_grids, delim = ", ") %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, study_area_grids) %>% 
  dplyr::summarise(n_studies = n_distinct(title)) %>% 
  dplyr::mutate(study_area_grids = as.integer(study_area_grids)) %>% 
  dplyr::ungroup()

studies_grids_total <-
  data_random %>% 
  dplyr::select(five_yr_bin, title, study_area_grids) %>% 
  tidyr::separate_longer_delim(study_area_grids, delim = ", ") %>% 
  dplyr::distinct(.keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, study_area_grids) %>% 
  dplyr::summarise(n_studies = n_distinct(title)) %>% 
  dplyr::mutate(study_area_grids = as.integer(study_area_grids)) %>% 
  dplyr::group_by(study_area_grids) %>% 
  dplyr::summarise(n_studies = sum(n_studies)) %>% 
  dplyr::mutate(five_yr_bin = "Total", .before = everything()) %>% 
  dplyr::ungroup()

studies_per_grid <- rbind(studies_grids_total, studies_per_grids_per_5yr)

studies_per_grid <- 
  dplyr::left_join(grid15, studies_per_grid,
                   by = join_by("id" == "study_area_grids")) %>% 
  dplyr::filter(! is.na(five_yr_bin)) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin))

map_studies_per_grid <- 
  ggplot() + 
  geom_sf(data = studies_per_grid, 
          aes(fill = n_studies)) + 
  geom_sf(data = rnaturalearth::ne_countries(returnclass = "sf"),
          fill = "grey80", colour = "grey80") + 
  # scale_fill_gradient(low = "black", high = "yellow") +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "") + 
  facet_wrap(~ five_yr_bin, ncol = 3, nrow = 4) + 
  theme_bw() +
  theme(legend.position = c(0.81, 0.13),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white"))

# ggsave(map_studies_per_grid,
#        filename = "./results/map-studies-per-grid.pdf",
#        height = 13, width = 15, units = "cm", dpi = 200)

rm(studies_per_grids_per_5yr, studies_grids_total, 
   studies_per_grid, map_studies_per_grid)

## (iii) Study aims & Analysis ####
# a) Stacked % histogram: by 5-yr bin [col "study_purpose"]

study_purpose_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, study_purpose, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, study_purpose) %>% 
  dplyr::summarise(n_study_purpose = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_study_purpose <-
  ggplot((study_purpose_per_5yr %>% 
            dplyr::mutate(five_yr_bin = paste0("     ", five_yr_bin))), 
         aes(x = five_yr_bin, y = n_study_purpose, fill = study_purpose)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = palette.colors(palette = "Classic Tableau")[1:8],
                    name = "") + 
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  scale_x_discrete(position = "top") +
  xlab("") + ylab("") + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1), 
        axis.text.y = element_text(size = 9),
        legend.position = "left",
        legend.text = element_text(size = 8))

# ggsave(barplot_study_purpose,
#        filename = "./results/barplot-percent-study-purpose-5yr.pdf",
#        height = 8, width = 13, units = "cm", dpi = 200)

# rm(study_purpose_per_5yr, barplot_study_purpose)

# b) Stacked % histogram: "stats_technique"

stats_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, stats_technique, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, stats_technique) %>% 
  dplyr::summarise(n_stats = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_stats <-
  ggplot(stats_per_5yr, 
         aes(x = five_yr_bin, y = n_stats, fill = stats_technique)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(# palette.colors(palette = "Polychrome 36")[1:5]
                    values = c("#5A5156", "#F6222E", "#E4E1E3", "#FE00FA","#16FF32"), 
                    name = "") + 
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("") + 
  coord_flip() +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(size = 8),
        plot.margin = margin(l=0))

# ggsave(barplot_stats,
#        filename = "./results/barplot-percent-stats-5yr.pdf",
#        height = 10, width = 13, units = "cm", dpi = 200)

# rm(stats_per_5yr, barplot_stats)

# c) Alluvial plot, 'study_purpose'<>'stats_technique'

alluvial_data <-
  data_random_pretty %>% 
  dplyr::distinct(id, study_purpose, stats_technique) %>% 
  dplyr::group_by(study_purpose, stats_technique) %>% 
  dplyr::summarise(n_alluvial = n()) %>% 
  dplyr::ungroup()

plot_alluvial <- 
  ggplot(alluvial_data,
       aes(y = n_alluvial, axis1 = study_purpose, axis2 = stats_technique)) +
  geom_alluvium(aes(fill = study_purpose), width = 1/12) +
  geom_stratum(width = 1/12,
               fill = c(rev(palette.colors(palette = "Classic Tableau")[1:8]),
                        rev(c("#5A5156", "#F6222E", "#E4E1E3", "#FE00FA","#16FF32"))),
               color = "black") +
  scale_x_discrete(limits = c("study_purpose", "stats_technique"), 
                   labels = c("Study purpose", "Stats"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = palette.colors(palette = "Classic Tableau")[1:8],
                    name = "") + 
  ylab("") + 
  # coord_flip() + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

### Patchwork

purpose_and_stats <- 
  (barplot_study_purpose + plot_spacer() + barplot_stats) +
  plot_alluvial + 
  patchwork::plot_annotation(tag_levels = list(c("A", "", "B"))) + 
  patchwork::plot_layout(widths = c(5,-1.15,5,7))

# ggsave(purpose_and_stats,
#        filename = "./results/patchwork_purpose-and-stats.pdf",
#        height = 9, width = 28, units = "cm", dpi = 200)

rm(study_purpose_per_5yr, barplot_study_purpose, 
   stats_per_5yr, barplot_stats, 
   alluvial_data, plot_alluvial, 
   purpose_and_stats)

## (iv) Methods ####

# a) col 'method' 

method_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, method, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, method) %>% 
  dplyr::summarise(n_method = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_method <-
  ggplot(method_per_5yr, 
         aes(x = five_yr_bin, y = n_method, fill = method)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = c("#3283FE", "#FEAF16", "#B00068"), # palette.colors(palette = "Polychrome 36")[6:8]
                    na.value = "grey", name = "") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 8))

# b) col 'sampling_protocol'

protocol_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, sampling_protocol, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, sampling_protocol) %>% 
  dplyr::summarise(n_protocol = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_protocol <-
  ggplot(protocol_per_5yr, 
         aes(x = five_yr_bin, y = n_protocol, fill = sampling_protocol)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = palette.colors(palette = "Accent")[6:1],
                    na.value = "grey", name = "") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_blank(),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 8))

# c) col "type_of_count"

typeofcount_per_5yr <-
  data_random_pretty %>% 
  dplyr::distinct(id, type_of_count, .keep_all = TRUE) %>% 
  dplyr::group_by(five_yr_bin, type_of_count) %>% 
  dplyr::summarise(n_typeofcount = n()) %>% 
  dplyr::mutate(five_yr_bin = gsub(pattern = "_",
                                   replacement = "-", x = five_yr_bin)) %>% 
  dplyr::ungroup()

barplot_typeofcount <-
  ggplot(typeofcount_per_5yr, 
         aes(x = five_yr_bin, y = n_typeofcount, fill = type_of_count)) +
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = palette.colors(palette = "Set 2")[2:6],
                    na.value = "grey", name = "") +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(axis.text = element_text(size = 8), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 8))

### Patchwork these last three plots
methods_plot <- 
  barplot_method /
  barplot_protocol /
  barplot_typeofcount +
  patchwork::plot_annotation(tag_levels = 'A')

# ggsave(methods_plot,
#        filename = "./results/patchwork_methods.pdf",
#        height = 20, width = 16, units = "cm", dpi = 200)

rm(method_per_5yr, barplot_method,
   protocol_per_5yr, barplot_protocol,
   typeofcount_per_5yr, barplot_typeofcount,
   methods_plot)

# d) col "attraction_bias"

data_random_pretty %>% 
  dplyr::distinct(id, attraction_bias, .keep_all = TRUE) %>% 
  dplyr::group_by(attraction_bias) %>% 
  dplyr::summarise(n = n())

# Chumming               2
# Commercial fishing     8
# None                  73
# Research fishing      17
# NA                     2

# e) number of 'NA' in cols "dist_boat", "ship_speed", "height_above_water", "n_sides", "n_observers"

View(data_random_pretty %>% 
       dplyr::distinct(id, dist_boat, .keep_all = TRUE) %>% 
       dplyr::group_by(dist_boat) %>% 
       dplyr::summarise(n = n()) %>% 
       dplyr::arrange(desc(n)))

## Top 6
# 300m             42
# not specified    20
# unlimited         8
# 500m              7
# 100m              5
# 150m              4

View(data_random_pretty %>% 
       dplyr::distinct(id, ship_speed, .keep_all = TRUE) %>% 
       dplyr::group_by(ship_speed) %>% 
       dplyr::summarise(n = n()) %>% 
       dplyr::arrange(desc(n)))

## The important info is:
# not specified    58
# NA                4

View(data_random_pretty %>% 
       dplyr::distinct(id, height_above_water, .keep_all = TRUE) %>% 
       dplyr::group_by(height_above_water) %>% 
       dplyr::summarise(n = n()) %>% 
       dplyr::arrange(desc(n)))

## The important info is:
# not specified    66
# NA                4

data_random_pretty %>% 
  dplyr::distinct(id, n_sides, .keep_all = TRUE) %>% 
  dplyr::group_by(n_sides) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n))

# n_sides           n
# 1                44
# not specified    27
# 2                21
# 4                 5
# NA                3
# 1-2               2

data_random_pretty %>% 
  dplyr::distinct(id, n_observers, .keep_all = TRUE) %>% 
  dplyr::group_by(n_observers) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n))

# n_observers       n
# 1                58
# not specified    22
# 2                12
# 2-3               4
# NA                3
# 1-2               2
# 1 or 2            1

# f) number (%) studies that analysed birds "on_water_only"

data_random_pretty %>% 
  dplyr::distinct(id, on_water_only, .keep_all = TRUE) %>% 
  dplyr::group_by(on_water_only) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(desc(n))

# on_water_only        n
# "no"                66
# "not specified"     17
# "yes"               14
# NA                   3
# ""                   1
# "yes_foraging"       1

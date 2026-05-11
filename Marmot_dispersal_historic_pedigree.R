install.packages("ggraph")
library("dplyr") 
library("ggplot2")
library("tidyr")
library("readxl")
library("igraph")
library("ggraph")
library("sf")

setwd("/Users/aiyonnebryant/Desktop/EEB_331") # set to your own local directory!

# set up plot aesthetics we can use for all figures
eeb_theme <- function(base_size = 9, base_family = "Arial") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      # Set all text to Arial
      text = element_text(family = "Arial"),
      # Remove grid lines and background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      # Plot title
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = base_size, hjust = 0.5, margin = margin(b = 10)),
      # Axes
      axis.line = element_line(color = "black"),
      axis.text = element_text(size = 9, color = "black"),
      axis.title = element_text(size = 9),
      # Legend
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_blank(),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9),
    ) 
}

# upload historic pedigree
ped_data_historic <- read_excel("Historical_Marmot_pedigree_1962.xlsx")
head(ped_data_historic)

# read in metadata for life history (colony) information 
colonies <- read.csv("sophie_sires_col.csv")
head(colonies)

# Filter historic pedigree for uid, dam, sire, and sex
ped_clean_historic <- ped_data_historic %>%
  select(uid, dam, sire, sex) 

# Join with colony data
ped_final_historic <- ped_clean_historic %>%
  left_join(colonies %>% select(uid, col_area), by = "uid")
ped_final_historic <- ped_final_historic %>%
  filter(col_area != "NA")
head(ped_final_historic)
nrow(ped_final_historic) # n = 2013

# Add dam and sire colony columns 
ped_final_historic <- ped_final_historic %>%
  mutate(
    dam_col = colonies$col_area[match(dam, colonies$uid)],
    sire_col = colonies$col_area[match(sire, colonies$uid)]
  )
head(ped_final_historic)

ped_final_historic <- ped_final_historic %>%
  mutate(
    # Create the natal_proxy column based on availability
    natal_col_proxy = case_when(
      !is.na(dam_col) ~ dam_col,              # Priority 1: Use Mom
      is.na(dam_col) & !is.na(sire_col) ~ sire_col, # Priority 2: Use Dad if Mom is NA
      TRUE ~ NA_character_                    # Otherwise: Stay NA
    ),
    
    # Define is_disperser based on the proxy
    is_disperser = case_when(
      is.na(natal_col_proxy) ~ NA,            # Can't determine if origin is unknown
      col_area != natal_col_proxy ~ TRUE,     # Different colony = Dispersed
      col_area == natal_col_proxy ~ FALSE     # Same colony = Resident
    )
  )
head(ped_final_historic)

plot_data_historic <- ped_final_historic %>%
  drop_na(is_disperser, sex) %>%
  filter(sex != "NA") %>%
  mutate(
    # Convert logical to more readable labels
    Dispersal_Status = if_else(is_disperser, "Dispersed", "Resident")
  )
head(plot_data_historic)
nrow(plot_data_historic)
# n = 1573

## Sex-Biased Dispersal Bar Chart ##
# Calculate the total counts per sex for the labels
sex_totals <- plot_data_historic %>%
  group_by(sex) %>%
  summarise(total_n = n())
sex_totals

# Plot
dispersal_by_sex_hist <- ggplot(plot_data_historic, aes(x = sex, fill = Dispersal_Status)) +
  geom_bar(position = "fill") + 
  eeb_theme() + 
  geom_text(data = sex_totals, 
            aes(x = sex, y = 1.02, label = paste0("n = ", total_n)), 
            inherit.aes = FALSE, # Prevent it from looking for 'Dispersal_Status'
            vjust = 0,
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.1)) + 
  scale_fill_manual(values = c("Dispersed" = "hotpink", "Resident" = "#B2DF8A")) + 
  labs(
    title = "Dispersal Events by Sex",
    x = "Sex",
    y = "Percentage of Individuals",
    fill = "Status"
  ) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, margin = margin(r = 15)))
dispersal_by_sex_hist

# save bar plot as a png
ggsave("dispersal_by_sex_hist.png", plot = dispersal_by_sex_hist, width = 8, height = 8, units = "in", dpi=500, bg="white")

### Significance test ###

# Create the table
dispersal_table <- table(plot_data_historic$sex, plot_data_historic$Dispersal_Status)
prop.table(dispersal_table, margin = 1) #show table 

# Convert dispersed to 0/1 for the model
plot_data_historic$dispersed_binary <- ifelse(plot_data_historic$is_disperser == "TRUE", 1, 0)

# Run the model
mod <- glm(dispersed_binary ~ sex, data = plot_data_historic, family = binomial)

# Check results
summary(mod)

### Dispersal Network ###
# Create a lookup table for renaming colonies and grouping by upad vs down valley
colony_groups <- tribble(
  ~original,          ~display_name,    ~valley_section,
  "river_rivermound", "River Mound",    "Down Valley",
  "river_sagemound",  "River Mound",    "Down Valley",
  "bench",            "Bench",          "Down Valley",
  "river_southmound", "River",          "Down Valley",
  "river",            "River",          "Down Valley",
  "horsemound",       "Horse Mound",    "Down Valley",
  "rvannex",          "River Annex",    "Down Valley",
  "gothictown",       "Gothic Town",    "Down Valley",
  "avalanche",        "Avalanche",      "Down Valley",
  "mm_aspen",         "Marmot Meadow",  "Up Valley",
  "mm_main",          "Marmot Meadow",  "Up Valley",
  "talus",            "Stonefield",     "Up Valley",
  "boulder",          "Boulder",        "Up Valley",
  "picnic_lower",     "Picnic",         "Up Valley",
  "picnic_upper",     "Picnic",         "Up Valley",
  "northpk",          "North Picnic",   "Up Valley",
  "cliff_upper",      "Cliff",          "Up Valley"
)

# Rename colonies in plot_data 
dispersal_data_hist <- plot_data_historic %>%
  left_join(colony_groups, by = c("natal_col_proxy" = "original")) %>%
  rename(natal_display = display_name,
         natal_group   = valley_section) %>%
  left_join(colony_groups, by = c("col_area" = "original")) %>%
  rename(sampling_display = display_name,
         sampling_group   = valley_section)

# Df with edges
dispersal_data_hist <- dispersal_data_hist %>%
  filter(natal_display != sampling_display) %>%
  group_by(natal_display, sampling_display) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(from = natal_display, to = sampling_display)

# Build node table with group info for coloring
node_groups <- colony_groups%>%
  select(display_name, valley_section) %>%
  distinct() %>%
  rename(name = display_name, group = valley_section)

# Create graph object
g_hist <- graph_from_data_frame(dispersal_data_hist, directed = TRUE, 
                           vertices = node_groups)

# Plot
dispersal_network_hist <- ggraph(g_hist, layout = "fr") +
  geom_edge_arc(arrow = arrow(length = unit(4, "mm"), type = "closed"),
                start_cap = circle(5, "mm"),    # increased to push arrows clear of nodes
                end_cap = circle(6, "mm"),
                color = "black",
                strength = 0.2) +
  geom_node_point(aes(color = group), size = 6) +
  geom_node_label(aes(label = name, color = group),
                  size = 3.5,
                  repel = TRUE,                 # repel pushes labels away from each other
                  max.overlaps = Inf,           # ensures no labels are dropped
                  box.padding = 1.2,            # increases space between label and node
                  point.padding = 0.8,
                  family = "Times New Roman") +
  scale_color_manual(
    values = c("Down Valley" = "magenta", "Up Valley" = "darkorchid3"),
    name = "Valley Section"
  ) +
  labs(title = "Marmot Dispersal Network") +
  eeb_theme()+ 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
dispersal_network_hist
#colors()

# save dispersal network as a png
ggsave("dispersal_network_hist.png", plot = dispersal_network_hist, width = 8, height = 8, units = "in", dpi=500, bg="white")

## make multi-panel plot ##
title <- ggdraw() + 
  draw_label("Sex-Biased Dispersal Inferred from Historic Pedigree",
             fontface = "bold",
             size = 14,
             fontfamily = "Arial")

# Combine title + your two-panel grid
hist_ped_dispersal_by_sex_plots <- plot_grid(
  dispersal_by_sex_hist,
  dispersal_network_hist, 
  ncol = 2, nrow = 1
)

hist_ped_dispersal_by_sex_plots <- plot_grid(
  title, 
  hist_ped_dispersal_by_sex_plots,
  ncol = 1,
  rel_heights = c(0.1, 1)     # title gets ~10% of vertical space
)
hist_ped_dispersal_by_sex_plots

ggsave("hist_ped_dispersal_by_sex_plots.png", plot = hist_ped_dispersal_by_sex_plots, width = 17, height = 8, units = "in", dpi=500, bg="white")

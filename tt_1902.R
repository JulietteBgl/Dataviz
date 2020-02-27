# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("ggplot2") # data visualisation
library("ggalt") # dumbell chart
library("forcats") # factor manipulation
library('ggsci') # color palette
library("cowplot") # export plots
library("forcats") # factor manipulation
library('sf')
library('echarts4r')
library("GGally")
library("tidyr")
library("rnaturalearth") # to get world data

# Data --------------------------------------------------------------------

# Dataset TT
tuesdata <- tidytuesdayR::tt_load('2020-02-18') 
food_consumption <- tuesdata[[1]]

# ajout de la catégorie d'aliment + recodage pays
food_consumption <- food_consumption %>% 
  mutate(category = 
           case_when(food_category %in% c("Pork", "Poultry", "Beef", "Lamb & Goat") ~ "Meat",
                     food_category %in% c("Eggs", "Milk - inc. cheese") ~ "Eggs, Milk, Cheese",
                     food_category == "Fish" ~ "Fish",
                     food_category %in% c("Wheat and Wheat Products", "Rice", "Soybeans", "Nuts inc. Peanut Butter") ~ "Grains",
                     ),
         country = recode_factor (country,
                                  `USA` = "United States",
                                  `Czech Republic`= "Czech Rep.",
                                  `South Korea`= "Korea")  
         )

# ordre des facteurs
food_consumption$category <- factor(food_consumption$category, levels = c("Meat", "Fish", "Eggs, Milk, Cheese", "Grains"))

# Donnée aggrégée ---------------------------------------------------------

# Pollution par pays
pollution_par_pays <- food_consumption %>% 
  group_by(country) %>% 
  summarise(consumption = sum(consumption),
            co2_emmission = sum(co2_emmission))

# Pollution par catégorie et par pays
pollution_par_pays_et_category <- food_consumption %>% 
  group_by(country, category) %>% 
  summarise(consumption = sum(consumption),
            co2_emmission = sum(co2_emmission))

# Map ---------------------------------------------------------------------

#world_map <- st_read('TidyTuesday/TM_WORLD_BORDERS-0.3/')
world_map <- ne_countries(scale = "medium", returnclass = "sf") 

food_consumption_map <- merge(
  x = world_map,
  y = pollution_par_pays,
  by.x = "name",
  by.y = "country",
  all.x = TRUE
)

# Remove Antartica
food_consumption_map <- food_consumption_map %>%
  filter(geounit != "Antarctica")
ggplot(data = map_test) + 
  geom_sf(mapping = aes(fill = co2_emmission))

# tests Mathieu 
map_test <- st_simplify(food_consumption_map, dTolerance = 2, preserveTopology = T)

# Map
(map <- ggplot(data = food_consumption_map) + 
  geom_sf(mapping = aes(fill = co2_emmission)) +
  labs(
    title = "",
    fill = "CO2 emission\n (kg CO2/person/year)"
  ) +
    theme_void() +
  #scale_fill_gradient(low = "#006600", high = "#990000", na.value = "grey") +
  theme(
    plot.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "#f7f7f7", color = NA),
    text = element_text(family = "Gibson", colour = "gray20", size = 12),
    plot.title = element_text(hjust = 0.5, face = "plain" )
  ))

# export plot
save_plot(
  filename = 'TidyTuesday/outputs/map.png',
  plot = map,
  base_height = 15,
  base_width = 15 * 1.61
)

# Solution TT -------------------------------------------------------------

pollution_par_pays %>%
  e_charts(country) %>%
  e_map(co2_emmission) %>%
  e_visual_map(min=0, max=2000) %>%
  e_title("Total CO2 emissions due to food products \n (kg CO2/person/year)", left = "center") %>%
  e_theme("chalk") #chalk / infographic /halloween

# Pays les plus pollueurs -------------------------------------------------

pollution_par_pays %>% 
  top_n(10, co2_emmission) %>% 
  arrange(desc(co2_emmission))

# Top 5 des pays les plus pollueurs
top_5 <- pollution_par_pays %>%  
  top_n(5, co2_emmission) %>% 
  arrange(desc(co2_emmission)) %>% 
  select(country) %>% 
  mutate(type = "top_5")

# Bottom 5 des pays les moins pollueurs
bottom_5 <- pollution_par_pays %>%  
  top_n(-5, co2_emmission) %>% 
  select(country) %>% 
  mutate(type = "bottom_5")


# Sources de pollution selon niveau de pollution --------------------------

# Dumbbell

# Préparation des données

# High 5
dumbbell <- food_consumption %>%
  filter(country %in% top_5$country) %>%
  group_by(country, category) %>% 
  mutate(sum_consumption_country = sum(consumption),
         sum_co2_country = sum(co2_emmission)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(min_consumption = min(sum_consumption_country), max_consumption = max(sum_consumption_country))

theme_set(theme_classic())

(gg1 <- ggplot(data = dumbbell, aes(
    x = min_consumption,
    xend = max_consumption,
    y = fct_relevel(category, "Grains", "Eggs, Milk, Cheese","Fish", "Meat"), 
    group = category
  )) +
  geom_dumbbell(color = "#a3c4dc",
                size = 1,
                colour_x = "#0e668b", 
                colour_xend = "#0e668b",
                size_x = 2,
                size_xend = 2) +
  #scale_x_continuous(label = percent) +
  labs(
    x = "CO2 emission (Kg CO2/person/year)",
    y = NULL,
    title = "Distribution of C02 emission per type of food",
    subtitle = "Min - Max C02 for the 5 countries with the highest CO2 emission rates"
  ) +
  theme(
    text = element_text(family = "Gibson", colour = "gray10"),
    plot.title = element_text(hjust = 0.5, face = "plain"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.line = element_line(color = "gray30"),
    panel.background = element_rect(fill = "#f7f7f7"),
    axis.title.x = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    axis.ticks = element_blank(),
    legend.position = "top",
    panel.border = element_blank()
  )) 


## Less 5

dumbbell2 <- food_consumption %>%
  filter(country %in% bottom_5$country) %>%
  group_by(country, category) %>% 
  mutate(sum_consumption_country = sum(consumption),
         sum_co2_country = sum(co2_emmission)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(min_consumption = min(sum_consumption_country), max_consumption = max(sum_consumption_country))

(gg2 <- ggplot(data = dumbbell2, aes(
  x = min_consumption,
  xend = max_consumption,
  y = fct_relevel(category, "Grains", "Eggs, Milk, Cheese","Fish", "Meat"), 
  group = category
)) +
    geom_dumbbell(color = "#a3c4dc",
                  size = 1,
                  colour_x = "#0e668b", 
                  colour_xend = "#0e668b",
                  size_x = 2,
                  size_xend = 2) +
    #scale_x_continuous(label = percent) +
    labs(
      x = "CO2 emission (Kg CO2/person/year)",
      y = NULL,
      title = "Distribution of C02 emission per type of food",
      subtitle = "Min - Max C02 for the 5 countries with the lowest CO2 emission rates"
    ) +
    theme(
      text = element_text(family = "Gibson", colour = "gray10"),
      plot.title = element_text(hjust = 0.5, face = "plain"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#f7f7f7"),
      axis.line = element_line(color = "gray30"),
      panel.background = element_rect(fill = "#f7f7f7"),
      axis.title.x = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
      axis.ticks = element_blank(),
      legend.position = "top",
      panel.border = element_blank()
    )) 


# Test both ---------------------------------------------------------------

all_c <- rbind(top_5, bottom_5)

dumbell3 <- merge(
  x = all_c,
  y = food_consumption,
  by = "country"
)

dumbell3 <- dumbell3 %>%
  group_by(country, category) %>% 
  mutate(sum_consumption_country = sum(consumption),
         sum_co2_country = sum(co2_emmission)) %>% 
  ungroup() %>% 
  group_by(category) %>% 
  mutate(min_consumption = min(sum_consumption_country), max_consumption = max(sum_consumption_country))

ggplot(data = dumbell3, aes(
  x = min_consumption,
  xend = max_consumption,
  y = fct_relevel(category, "Grains", "Eggs, Milk, Cheese","Fish", "Meat"), 
  group = category
)) +
  geom_point(aes(colour = type))
    geom_dumbbell(color = "#a3c4dc",
                  size = 1,
                  colour_x = "#0e668b", 
                  colour_xend = "#0e668b",
                  size_x = 2,
                  size_xend = 2) +
    #scale_x_continuous(label = percent) +
    labs(
      x = "CO2 emission (Kg CO2/person/year)",
      y = NULL,
      title = "Distribution of C02 emission per type of food",
      subtitle = "Min - Max C02 for the 5 countries with the lowest CO2 emission rates"
    ) +
    theme(
      text = element_text(family = "Gibson", colour = "gray10"),
      plot.title = element_text(hjust = 0.5, face = "plain"),
      plot.subtitle = element_text(hjust = 0.5),
      plot.background = element_rect(fill = "#f7f7f7"),
      axis.line = element_line(color = "gray30"),
      panel.background = element_rect(fill = "#f7f7f7"),
      axis.title.x = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
      axis.ticks = element_blank(),
      legend.position = "top",
      panel.border = element_blank()
    )

# Focus sur les consommateurs de viande -----------------------------------

# top 10 pollueurs
top_10 <- pollution_par_pays %>%  
  top_n(10, co2_emmission) %>% 
  arrange(desc(co2_emmission)) %>% 
  select(country)

# préparation des données
plot_meat <- food_consumption %>% 
  filter(category == "Meat") %>% 
  group_by(country, food_category) %>% 
  mutate(sum_consumption = sum(consumption),
         group = ifelse(
           test = country %in% top_10$country,
           yes = 0,
           no = 1
         )) %>% 
  select(-co2_emmission, -consumption, -category)

# long to wide
plot_meat <- spread(
  data = plot_meat,
  key = food_category,
  value = sum_consumption
)

plot_meat$group <- factor(plot_meat$group)

(m <- plot_meat %>%
  arrange(desc(group)) %>%
  ggparcoord(
    columns = 3:6, 
    groupColumn = 2, 
    order = "anyClass",
    showPoints = TRUE, 
    alphaLines = 1, 
    scale = "globalminmax"
  ) + 
  scale_color_manual(values=c( "#69b3a2", "#E8E8E8") ) +
 # theme_ipsum()+
  theme(
    legend.position="Default",
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
    plot.subtitle = element_text(size = 12, face = "plain", hjust = 0.5),
    axis.line.y = element_line(colour = "gray80", size = .3), 
    axis.line.x = element_line(colour = "gray80", size = .3),
    text = element_text(family = "Gibson", colour = "gray20"),
    axis.text = element_text(family = "Gibson", size = 14, colour = "gray40"),
    axis.title = element_text(size = 12)
    #axis.line = element_line(color = "gray30"),
  ) +
  labs(
    x = "",
    y = "Consumption (kg/person/year)",
    title = "Meet consumption",
    subtitle = "The higlighted lines represent the countries with the highest CO2 emissions"
  ))


# Lien entre émissions et conso -------------------------------------------

food_consumption %>%
  filter(category == "Meat") %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = food_category)) +
  geom_point(size = 0.7)

# Meat

levels(food_consumption$category) # OK

(c1 <- food_consumption %>%
  arrange(desc(category)) %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = category)) +
  geom_point(size = 0.7) +
  labs(
    x = "Consumption",
    y = "CO2 emissions",
    title = "Meat"#,
    #subtitle = "Meat"
  ) +
  scale_colour_manual(values = c("#69b3a2", "gray90", "gray90", "gray90")) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30"),
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.line = element_line(color = "gray30"),
    panel.background = element_rect(fill = "#f7f7f7"),
    axis.title = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 15, y = 700, family = "Gibson", size = 6, color = "gray40",
           label = "Lamb\n & Goat") +
  annotate("text", x = 55, y = 1200, family = "Gibson", size = 6, color = "gray40",
           label = "Beef") +
  annotate("text", x = 40, y = 280, family = "Gibson", size = 6, color = "gray40",
           label = "Pork") +
  annotate("text", x = 70, y = 150, family = "Gibson", size = 6, color = "gray40",
           label = "Poultry"))

# Fish
# reorder factors
food_consumption_fish <- food_consumption
food_consumption_fish$category <- factor(food_consumption$category, levels = c("Fish", "Meat", "Eggs, Milk, Cheese", "Grains"))

(c2 <- food_consumption_fish %>%
  arrange(desc(category)) %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = category)) +
  geom_point(size = 0.7) +
  labs(
    x = "Consumption",
    y = "CO2 emissions",
    title = "Fish"#,
    #subtitle = "Fish"
  ) +
  scale_colour_manual(values = c("#69b3a2", "gray90", "gray90", "gray90")) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30"),
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.line = element_line(color = "gray30"),
    panel.background = element_rect(fill = "#f7f7f7"),
    axis.title = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ))

# Eggs and Cheese

food_consumption_grains %>%
  filter(category == "Eggs, Milk, Cheese") %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = food_category)) +
  geom_point(size = 0.7)

# reorder factors
food_consumption_eggs <- food_consumption
food_consumption_eggs$category <- factor(food_consumption$category, 
                                         levels = c("Eggs, Milk, Cheese", "Fish", "Meat", "Grains"))

(c3 <- food_consumption_eggs %>%
  arrange(desc(category)) %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = category)) +
  geom_point(size = 0.7) +
  labs(
    x = "Consumption",
    y = "CO2 emissions",
    title = "Eggs, Milk, Cheese"#,
    #subtitle = "Eggs, Milk, Cheese"
  ) +
  scale_colour_manual(values = c("#69b3a2", "gray90", "gray90", "gray90")) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30"),
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.line = element_line(color = "gray30"),
    panel.background = element_rect(fill = "#f7f7f7"),
    axis.title = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 300, y = 550, family = "Gibson", size = 6, color = "gray40",
           label = "Milk and cheese") +
  annotate("text", x = 40, y = 20, family = "Gibson", size = 6, color = "gray40",
           label = "Eggs"))

# Grains

food_consumption_grains %>%
  filter(category == "Grains") %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = food_category)) +
  geom_point(size = 0.7)


food_consumption_grains <- food_consumption
food_consumption_grains$category <- factor(food_consumption$category, levels = c("Grains", "Fish", "Meat", "Eggs, Milk, Cheese"))

(c4 <- food_consumption_grains %>%
  arrange(desc(category)) %>%
  ggplot(aes(x = consumption,
             y = co2_emmission,
             color = category)) +
  geom_point(size = 0.7) +
  labs(
    x = "Consumption",
    y = "CO2 emissions",
    title = "Grains"#,
    #subtitle = "Grains"
  ) +
  scale_colour_manual(values = c("#69b3a2", "gray90", "gray90", "gray90")) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30"),
    plot.title = element_text(size = 16, face = "plain", hjust = 0.5),
    plot.background = element_rect(fill = "#f7f7f7"),
    axis.line = element_line(color = "gray30"),
    panel.background = element_rect(fill = "#f7f7f7"),
    axis.title = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dashed"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    legend.position = "none"
  ) +
  annotate("text", x = 10, y = 80, family = "Gibson", size = 6, color = "gray40",
           label = "Nuts") +
  annotate("text", x = 190, y = 180, family = "Gibson", size = 6, color = "gray40",
           label = "Rice") +
  annotate("text", x = 30, y = 10, family = "Gibson", size = 6, color = "gray40",
           label = "Soybeans") +
  annotate("text", x = 150, y = 80, family = "Gibson", size = 6, color = "gray40",
           label = "Wheat"))

# Mix plots
plot_grid(c1, c2, c3, c4)

# Final plot --------------------------------------------------------------

title <- ggdraw() + 
  draw_label("CO2 EMISSIONS DUE TO FOOD PRODUCTS", 
             fontfamily = "Gibson", 
             fontface = 'bold',
             size = 24,
             colour = "gray30") +
  theme(
    plot.background = element_rect(fill = "#f7f7f7")
  )

title2 <- ggdraw() + 
  draw_label("Total CO2 emissions due to food products", 
             fontfamily = "Gibson", 
             fontface = 'plain',
             size = 16,
             colour = "gray30") +
  theme(
    plot.background = element_rect(fill = "#f7f7f7", color = NA
                                   )
  )

title3 <- ggdraw() + 
  draw_label("Relation between CO2 emission and consumption", 
             fontfamily = "Gibson", 
             fontface = 'plain',
             size = 16,
             colour = "gray30") +
  theme(
    plot.background = element_rect(fill = "#f7f7f7", color = NA
                                   )
  )

source <- ggdraw() + 
  draw_label("Source: Nu3 | Dataviz: @JulietteBgl", 
             fontfamily = "Gibson", 
             fontface = 'plain',
             size = 10,
             colour = "gray10") +
  theme(
    plot.background = element_rect(fill = "#f7f7f7", color = NA
    )
  )

tt <- plot_grid(
  title,
  rel_heights=c(0.1, 1, 0.1),
  ncol = 1,
  plot_grid(
    ncol = 2,
    # colonne de gauche
    plot_grid(
      ncol = 1,
      title2,
      rel_heights=c(0.2, 0.8, 1.2),
      map, 
      m),
    # colonne de droite
    plot_grid(
      ncol = 1,
      title3,
      rel_heights = c(0.1, 1),
      plot_grid(c1, c2, c3, c4, ncol = 2)
    )
  ),
  source
  )


# export plot
save_plot(
  filename = 'TidyTuesday/outputs/tt_1902.png',
  plot = tt,
  base_height = 15,
  base_width = 15 * 1.61
)


# To do
# Cartes geo
# chart relié sans la selection sur les tops consommateurs de viande
# revoir les conclusions boeuf vs cheese
# Grid plot de grid plots





# AFC ---------------------------------------------------------------------

library('FactoMineR')
library('tidyr')
library('factoextra')

# long to wide
afc <- spread(food_consumption %>% select(country, food_category, co2_emmission), 
              key = food_category, 
              value = co2_emmission)

# Ajout de noms de lignes
names <- afc[,1]
afc <- afc[,-1]
afc <- as.data.frame(afc)
row.names(afc) <- names$country

# Selection des pays

# Top 5 conso des pays dans chaque catégorie de food
top_5_food <- food_consumption %>% 
  group_by(food_category) %>% 
  arrange(desc(consumption)) %>% 
  mutate(rank = seq(n())) %>% 
  filter(rank %in% 1:5)

unique(top_5_food$country)

# Top 50 conso tous produits confondus
top_5_all <- food_consumption %>% 
  group_by(country) %>% 
  summarise(consumption = sum(consumption)) %>% 
  top_n(50, consumption)


# AFC - all
resCA <- CA(afc)

plot1 <- fviz_ca_biplot(resCA, 
               col.row ="steelblue", 
               col.col = "darkred", 
               #row.size = ,
               select.row = list(cos2 = 0.6)) + ##69b3a2
  theme_minimal() +
  labs(
    title = "Correspondence Analysis",
    subtitle = "Filter on countries with cos2 > 0.6"
  ) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30", size = 24))

# AFC - sélection sur top 5 food
resCA2 <- CA(afc[row.names(afc) %in% top_5_food$country,])

plot2 <- fviz_ca_biplot(resCA2, 
               col.row ="steelblue", 
               col.col = "darkred", 
               select.row = list(cos2 = 0.5) ##69b3a2
) +
  theme_minimal() +
  labs(
    title = "Correspondence Analysis",
    subtitle = "Filters applied: top 5 countries per food catégory & countries with cos2 > 0.5"
  ) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30", size = 24))

# AFC - sélection sur top 50 conso
resCA3 <- CA(afc[row.names(afc) %in% top_5_all$country,])

plot3 <- fviz_ca_biplot(resCA3, 
               col.row ="steelblue", 
               col.col = "darkred", 
               select.row = list(cos2 = 0.5) ##69b3a2
) +
  theme_minimal() +
  labs(
    title = "Correspondence Analysis",
    subtitle = "Filters applied: top 50 of the most consuming countries & countries with cos2 > 0.5"
  ) +
  theme(
    text = element_text(family = "Gibson", colour = "gray30", size = 24))


# export plot
save_plot(
  filename = 'TidyTuesday/outputs/afc1.png',
  plot = plot1,
  base_height = 15,
  base_width = 15 * 1.61
)

# export plot
save_plot(
  filename = 'TidyTuesday/outputs/afc2.png',
  plot = plot2,
  base_height = 15,
  base_width = 15 * 1.61
)


# export plot
save_plot(
  filename = 'TidyTuesday/outputs/afc3.png',
  plot = plot3,
  base_height = 15,
  base_width = 15 * 1.61
)


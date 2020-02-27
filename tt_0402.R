
# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("ggplot2") # data visualisation

library("forcats") # factor manipulation
library('ggsci') #color palette
library("cowplot") # export plots

# Data --------------------------------------------------------------------

#devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-02-04') 

attendance <- tuesdata[[1]]
standings <- tuesdata[[2]]
games <- tuesdata[[3]]

# Préparation de la donnée ------------------------------------------------

# Ajout de la moyenne par équipe

games <- games %>% 
  mutate(points_differential = pts_win - pts_loss)

prep <- games %>% 
  mutate(group = ifelse(
    test = week %in% c("WildCard", "Division", "ConfChamp"),
    yes = "Playoffs",
    no = ifelse(
      test = week == "SuperBowl",
      yes = "SuperBowl",
      no = "Regular Season"
    ))
  ) %>% 
  group_by(group) %>% 
  mutate(points_differential_group = mean(points_differential),
         points_differential_global = mean(games$points_differential)
         ) %>% 
  select(year, week, home_team, away_team, group, pts_win, pts_loss, points_differential, points_differential_group, points_differential_global)

# ordre des facteurs

prep$group <- factor(prep$group, levels = c("SuperBowl", "Playoffs", "Regular Season"))


# Viz ---------------------------------------------------------------------

# max point diff superbowl
max_sb <- prep %>% 
  filter(group == "SuperBowl", points_differential == max(points_differential)) %>% 
  select(year, home_team, away_team, points_differential)

# max point diff regular season
max_rs <- prep %>% 
  filter(group == "Regular Season", points_differential == max(points_differential)) %>% 
  select(year, home_team, away_team, points_differential)

# max point diff playoffs
max_po <- prep %>% 
  filter(group == "Playoffs", points_differential == max(points_differential)) %>% 
  select(year, home_team, away_team, points_differential)
  
# Base
g <- ggplot(data = prep, 
            mapping = aes(x = group, 
                          y = points_differential, 
                          color = group)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 62)#,
                     #expand = c(0.005, 0.005),
                     #breaks = c(0, 20, 40, 60, 80, 100),
  #                   labels = c('0%', '20%', '40%', '60%', '80%', '100%')
  ) +
  scale_color_uchicago() +
  labs(x = NULL,
       y = NULL, 
       subtitle = 'Point per game differential from 2000 to 2019, per type of game') +
  theme_set(theme_light(base_size = 50)) +
  theme(legend.position = "none",
        text = element_text(family = "Gibson", colour = "gray80"),
        panel.border = element_blank(),
        axis.line.y = element_blank(), 
        axis.line.x = element_line(colour = "gray80", size = .3),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Gibson", size = 30, colour = "gray80"),
        panel.grid = element_blank(), #element_line(size = 1),
        title = element_text(family = "Gibson", size = 50,  hjust = 1),
        plot.title = element_text(hjust=0.5, vjust=0.5),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        plot.subtitle = element_text(family = "Gibson", size = 30,  hjust = 0.5)
        ) +
  ggtitle(label = 'NFL - Point per game differential')

# arrows
arrows <- data.frame(
  x1 = c(3.4, 2.25, 1.35, 1.35),
  x2 = c(3.4, 2.05, 1.75, 1.75),
  y1 = c(20, 30, 42, 42),
  y2 = c(unique(prep$points_differential_global)+0.5, 14.5, 40.4, 37.5)
)

# all

set.seed(1234)
plot <- g +
  geom_segment(mapping = aes(x = group, xend = group,
                             y = points_differential_global, 
                             yend = points_differential_group),
               size = 1.5) +
  geom_hline(aes(yintercept = points_differential_global), color = "gray80", size = 0.9) +
  geom_jitter(size = 7, alpha = 0.6, width = 0.2) +
  geom_point(mapping = aes(x = fct_reorder(group, points_differential_group, .fun = mean), y = points_differential_group), size = 15) +
  annotate("text", x = 3.5, y = 20, family = "Gibson", size = 8, color = "gray40",
           label = glue::glue("Avg. point differential: {unique(round(prep$points_differential_global, 1))}")) +
  annotate("text", x = 2.4, y = 30, family = "Gibson", size = 8, color = "gray40",
           label = "Avg. point differential \nper type of game") +
  annotate("text", x = 1.35, y = 49, family = "Gibson", size = 8, color = "gray40",
           label = "Point differential per game") +
  annotate("text", x = 1, y = 8, family = "Gibson", size = 8, color = "gray80", fontface = 2,
           label = glue::glue("{unique(round(prep[prep$group == 'SuperBowl',]$points_differential_group, 1))}")) +
  annotate("text", x = 2.1, y = 13, family = "Gibson", size = 8, color = "gray80", fontface = 2,
           label = glue::glue("{unique(round(prep[prep$group == 'Playoffs',]$points_differential_group, 1))}")) +
  annotate("text", x = 3.1, y = 13, family = "Gibson", size = 8, color = "gray80", fontface = 2,
           label = glue::glue("{unique(round(prep[prep$group == 'Regular Season',]$points_differential_group, 1))}")) +
  annotate("text", x = 0.7, y = 35, family = "Gibson", size = 6, color = "gray80",
           label = glue::glue("{max_sb[,2]}\n {max_sb[,3]} \nagainst {max_sb[,4]}\n Point differential: {max_sb[,5]}")) +
  annotate("text", x = 2.7, y = 59, family = "Gibson", size = 6, color = "gray80",
           label = glue::glue("{max_rs[,2]}\n {max_rs[,3]} \nagainst {max_rs[,4]}\n Point differential: {max_rs[,5]}")) +
  annotate("text", x = 0.5, y = 57, family = "Gibson", size = 6, color = "gray40",
           label = glue::glue("Source:Pro Football Reference|Viz: @JulietteBgl")) +
  geom_curve(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.5,
             color = "gray40", curvature = -0.3)


# export plot
save_plot(
  filename = 'tt_0402.png',
  plot = plot,
  base_height = 15,
  base_width = 15 * 1.61
)


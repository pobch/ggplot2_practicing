library(ggalt)
library(ggplot2)
library(maps)
library(dplyr)
library(readr)
library(scales)
library(extrafont)

# IMPORT FONTS
font_import(pattern = '[r/R]oboto') # IMPORT ROBOTO FONT
font_import() # IMPORT ALL FONTS
loadfonts(device = 'win')

# GET DATA
df.vc_totals <- read_csv("http://sharpsightlabs.com/wp-content/uploads/2016/09/vc_investment_totals.csv")

# GET WORLD MAP
map.world <- map_data ("world")

# CREATE THE WORLD MAP PLOT
ggplot() +
  geom_polygon(data = map.world, aes(x = long , y = lat, group = group), 
               fill = "#002035",
               col = "#114151") +
  geom_point(data = df.vc_totals, 
             aes(x = longitude , 
                 y = latitude, 
                 size = vc_investment_millions), 
             col = 'orange',
             alpha = 0.5) +
  geom_point(data = df.vc_totals,
             aes(x = longitude,
                 y = latitude,
                 size = vc_investment_millions),
             shape = 1, 
             alpha = 0.7,
             col = 'grey10') +
  scale_size_continuous(range = c(1,20), 
                        breaks = c(500,1000, 2000,6000), 
                        name="Venture Capital Investment\n(USD, Millions)\n") +
  labs(title = 'Venture Capital Investment by City') +
  theme(panel.background = element_rect(fill = '#000727'),
        panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = 'Roboto Condensed Light'),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.1,0.3),
        legend.text = element_text(color = '#CCCCCC', size = 11),
        legend.title = element_text(color = '#CCCCCC', size = 12),
        plot.title = element_text(color = '#CCCCCC', 
                                  hjust = .5, vjust = -5, 
                                  family = 'Roboto Medium', 
                                  size = 20),
        plot.background = element_rect(fill = '#000727'))
ggsave('3_worldmap.png', height = 200, width = 400, units = 'mm')

# CREATE THE BAR CHART 
ggplot(data = df.vc_totals, 
       aes(x = reorder(metro, vc_investment_millions), 
           y = vc_investment_millions)) +
  geom_bar(stat = 'identity', fill = '#114151') +
  geom_text(aes(label = comma(vc_investment_millions)),
            hjust = 1.2,
            col = '#CCCCCC') +
  labs(y = 'Millions of Dollars', title = 'Venture Capital Investment by City') +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, max(df.vc_totals$vc_investment_millions)*1.1)) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = 'Gill Sans MT', size = 15),
        plot.title = element_text(size = 28, color = "#555555"))
ggsave('3_barChart.png', units = 'mm', height = 200, width = 220)



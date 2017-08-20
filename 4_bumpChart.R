library(dplyr)
library(ggplot2)
library(scales)

# GET THE DATA
url.world_ports <- url("http://sharpsightlabs.com/wp-content/datasets/world_ports.RData")
load(url.world_ports)
glimpse(df.world_ports)


# PLOT
rank_n = 15

df.world_ports %>% 
  filter(rank <= rank_n) %>% 
  mutate(china_flag = ifelse(economy == 'China', T, F),
         china_labels = ifelse(china_flag == T, port, 'other')) %>% 
  ggplot(aes(x = year, y = rank, group = port_label)) +
  geom_line(aes(col = china_labels, alpha = china_flag), size = 2) +
  geom_point(aes(col = china_labels, alpha = china_flag), size = 3.2) +
  geom_point(aes(alpha = china_flag) , col = '#FFFFFF', size = 1) +
  scale_y_reverse(breaks = c(1,5,10,15)) +
  scale_alpha_discrete(range = c(.4,.9)) +
  scale_color_manual(values = c("#4e79a5",
                                "#f18f3b",
                                "#af7a0a",
                                "#e0585b",
                                "#5aa155",
                                "#edc958",
                                "#77b7b2",
                                "#BBBBBB")) +
  scale_x_discrete(expand = c(.3,.3)) +
  geom_text(data = . %>% 
              filter(year == '2004', rank <= rank_n),
            aes(label = port_label, x = '2004', col = china_labels), 
            hjust = 1, 
            nudge_x = -0.2,
            size = 4,
            family = 'Roboto Black') +
  geom_text(data = . %>% 
              filter(year == '2014' & rank <= rank_n),
            aes(label = port_label, x = '2014', col = china_labels),
            hjust = 0,
            nudge_x = 0.2,
            size = 4,
            family = 'Roboto Black') +
  labs(title = "Top Chinese ports increased rank\nsubstantially from 2004 to 2014") +
  labs(subtitle = "(Port ranks, by volume)") +
  labs(x = "Year", y = "Rank") +
  theme(panel.grid.major.x = element_line(color = "#FFFFFF")) +  
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "None") +
  theme(text = element_text(family = 'Gill Sans MT'),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15, color = '#888888'))
ggsave('4_bumpChart.png', units = 'mm', height = 150, width = 200)

library(ggplot2)

data("mtcars")
mtcars$car.names = rownames(mtcars)
mtcars$mpg.z = round((mtcars$mpg - mean(mtcars$mpg)) / sd(mtcars$mpg) , 2)
mtcars$mpg.type = ifelse(mtcars$mpg.z > 0, 'Above', 'Below')
# mtcars = mtcars[order(mtcars$mpg.z), ]
mtcars$car.names = reorder(mtcars$car.names, mtcars$mpg.z)

# Flipped bar chart
theme_set(theme_bw())
ggplot(data = mtcars, aes(x = car.names, y = mpg.z)) +
  geom_bar(stat = 'identity', width = 0.5, aes(fill=mpg.type)) +
  labs(title = 'Normalised mileage') +
  scale_fill_discrete(name = 'Mileage', 
                      labels = c('Above average', 'Below average')) +
  ylim(-2.5, 2.5) +
  coord_flip()
ggsave('2_flippedBar.png', units = 'mm', width = 200, height = 120)  

# Lollipop chart
ggplot(data = mtcars, aes(x = car.names, y = mpg.z)) +
  geom_point(size = 4, col = 'tomato2') +
  geom_segment(aes(x = car.names,
                   xend = car.names,
                   y = 0,
                   yend = mpg.z),
               col = 'tomato2') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_text(aes(label = mpg.z,
                y = ifelse(mpg.z < 0, mpg.z - 0.2, mpg.z + 0.2)), 
            col = 'tomato2', size = 3) +
  labs(title = 'Lollipop chart',
       subtitle = 'Normalised mileage') +
  coord_flip() +
  ylim(-2.5,2.5)
ggsave('2_lollipop.png', units = 'mm', width = 200, height = 150)


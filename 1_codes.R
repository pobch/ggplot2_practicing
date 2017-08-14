library(ggplot2)
data(mpg)
mpg
theme_set(theme_classic())

# count chart
ggplot(data = mpg, aes(cty, hwy)) +
  geom_count(col = 'tomato', show.legend = F) +
  labs(title = 'Counts Plot',
       subtitle = 'city vs highway mileage')
ggsave('1_countPlot.png', units = 'mm', width = 100, height = 100)

# geom_jitter
ggplot(data = mpg[mpg$manufacturer %in% c('ford', 'honda', 'toyota'),], aes(displ, cty)) +
  labs(title='Bubble chart',
       subtitle='Displacement vs City Mileage') +
  geom_jitter(aes(col=manufacturer, size=hwy), alpha = 0.6) 
ggsave('1_bubbleChart.png', units = 'mm', width = 120, height = 100)

# corr plot
library(ggcorrplot)
data(mtcars)
corr = round(cor(mtcars),2)
corr
ggcorrplot(corr, hc.order = T,
           type = 'lower',
           lab = T,
           lab_size = 2.5,
           method = 'circle',
           ggtheme = theme_bw,
           colors = c('tomato2', 'white', 'springgreen4'))
ggsave('1_corrplot.png', units = 'mm', width = 120, height = 100)

# dot plot
cty_mpg = aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) = c("make", "mileage")  # change column names
cty_mpg = cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make = factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)

theme_set(theme_classic())
ggplot(data = cty_mpg, aes(x=make, y=mileage)) +
  geom_segment(aes(x = make,
                   xend = make,
                   y = 0,
                   yend = max(mileage)),
               linetype='dashed',
               size = 0.2,
               col = 'grey') +
  geom_point(col = 'tomato2', size = 3) +
  labs(title='Dot Plot',
       subtitle='Make vs Avg. Mileage') +
  coord_flip()
ggsave('1_dotPlot.png', units = 'mm', width = 130, height = 100)

# slope chart
library(scales)
df = read.csv("https://raw.githubusercontent.com/selva86/datasets/master/gdppercap.csv")
colnames(df) = c("continent", "1952", "1957")
left_label = paste(df$continent, comma(round(df$`1952`)),sep=", ")
right_label = paste(df$continent, comma(round(df$`1957`)),sep=", ")
df$class = ifelse((df$`1957` - df$`1952`) < 0, "red", "green")
p = ggplot(df) + 
  geom_segment(aes(x=1, xend=2, y=`1952`, yend=`1957`, col=class), size=.75, show.legend=F) +
  geom_vline(xintercept=1, linetype="dashed", size=.2) +
  geom_vline(xintercept=2, linetype="dashed", size=.2) +
  scale_color_manual(values = c("red"="tomato2", "green"="springgreen3")) +  # color of lines
  scale_y_continuous(labels = comma, limits = c(0,(1.1*(max(df$`1952`, df$`1957`))))) +
  labs(x="", y="Mean GdpPerCap") +  # Axis labels
  xlim(.5, 2.5)
# Add texts
p = p + geom_text(label=left_label, y=df$`1952`, x=rep(1, nrow(df)), hjust=1.1, size=3.5)
p = p + geom_text(label=right_label, y=df$`1957`, x=rep(2, nrow(df)), hjust=-0.1, size=3.5)
p = p + geom_text(label="1952", x=1, y=1.1*(max(df$`1952`, df$`1957`)), hjust=1.2, size=5)  # title
p = p + geom_text(label="1957", x=2, y=1.1*(max(df$`1952`, df$`1957`)), hjust=-0.1, size=5)  # title
# Theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank())
ggsave('1_slope.png', units = 'mm', width = 130, height = 200)


# Zane Dax
# Nov 30, 2021





library(tidyverse)
library(ggplot2)
library(lubridate)
library(showtext)
library(ggtext)
library(viridis)

font_add_google(family = "Poppins","Poppins")
showtext_auto()

cricket <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

cricket %>% janitor::clean_names()

# 1 Day             721
# 2 Day and night   516



cricket %>% 
  select(time_of_day, ground_country) %>% 
  group_by(ground_country, time_of_day) %>% 
  count(time_of_day) %>% 
  arrange( desc(n)) %>% 
  filter(time_of_day =="Day") %>% 
  ggplot(
    aes(x= n,
        y= fct_reorder(as.factor(ground_country) , n),
        fill= n)
  )+
  geom_col()+
  ggdark::dark_mode()+
  scale_fill_viridis_c(option = 'C', 
                       name=''
                       )+
  labs(title = "\nCricket playing countries ground game (daytime)",
       caption = "\n#TidyTuesday | @StarTrek_Lt  Nov 30 2021\n",
       y= "",
       x="count",
       )+
  theme(
    text = element_text(family = "Poppins"),
    plot.title = element_text(size = 14, 
                              hjust = 0.5,
                              face = 'bold'),
    plot.caption = element_text(hjust = 0,
                                size = 11,
                                color = 'grey50'),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 12),
    legend.text = element_text()
  )

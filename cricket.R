
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




#-----------------------------
# Dec 1 2021



library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(showtext)



font_add_google(family = "Raleway","Raleway")
showtext_auto()

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

matches$match_date

matches = matches %>% 
  mutate(team1 = as_factor(team1),
         team2 = as_factor(team2),
         team1_away_or_home = as_factor(team1_away_or_home),
         team2_home_away = as_factor(team2_home_away),
         winner = as_factor(winner),
         venue = as_factor(venue),
         ground_city = as_factor(ground_city),
         match_date = mdy(match_date),
         toss = as_factor(toss)
         )

matches %>% 
  select(score_team1, margin) %>% 
  drop_na(margin) %>% 
  mutate(avg_margin =  mean(margin) ) 

matches %>% 
  select(team1, team1_away_or_home) %>% 
  group_by(team1_away_or_home, team1) %>% 
  count()






# ------------------ series
matches %>% 
  select(venue, series) %>% 
  group_by(series) %>% 
  count() %>% 
  arrange( desc(n)) %>% 
  filter(n > 10) %>% 
  ggplot(
    aes(x= n,
        y= fct_reorder(series, n) ,
        fill= n)
  )+
  geom_col()+
  scale_fill_distiller(type = 'div', palette = 8)+ # 1,2,3
  # scale_fill_viridis_c(option = 'C')+
  ggdark::dark_mode()+
  labs(
    title = "Cricket Series by count",
    caption = "#TidyTuesday | @StarTrek_Lt | Dec 1 2021",
    y="",
    x="count"
  )+
  theme(
    text = element_text(family = "Raleway" ),
    legend.title = element_blank(),
    plot.title = element_text(size=15,
                              hjust = 0.5,
                              face = 'bold'),
    plot.caption = element_text(size= 12,
                                colour = 'grey50',
                                hjust = 0),
    axis.text.y = element_text(size = 12,
                               face = 'bold'),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )






matches %>% 
  select(venue, series) %>% 
  group_by(venue) %>% 
  count() %>% 
  arrange( desc(n)) %>% 
  filter( n > 20) %>% 
  ggplot(
    aes(x= n,
        y= fct_reorder(venue, n) ,
        fill= n)
  )+
  geom_col()+
  scale_fill_distiller(type = 'div', palette = 3)+ # 1,2,3
  # scale_fill_viridis_c(option = 'C')+
  ggdark::dark_mode()+
  labs(
    title = "Cricket venue count",
    subtitle = "venue counts > 20",
    caption = "#TidyTuesday | @StarTrek_Lt | Dec 1 2021",
    y="",
    x="count"
  )+
  theme(
    text = element_text(family = "Raleway" ),
    legend.title = element_blank(),
    plot.title = element_text(size=15,
                              hjust = 0.5,
                              face = 'bold'),
    plot.caption = element_text(size= 12,
                                colour = 'grey50',
                                hjust = 0),
    axis.text.y = element_text(size = 12,
                               face = 'bold'),
    axis.title.x = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )


























# I could only get ggwaffle to work, not library(waffle)
# devtools::install_github("liamgilbey/ggwaffle")

# I really wanted to use FontAwesome, but there's a bug with RStudio. 
# I may go and do one with base R for funsies
# spot the one really nasty fix


library(tidyverse)
library(lubridate)
library(ggwaffle)
library(nord)
library(emojifont)
library(extrafont)


life_data <- expand_grid(
  m1 = month.name,
  year = 1986:2020) %>% 
  mutate(month = as_factor(m1),
         month = fct_relevel(m1, month.name)) %>% 
  arrange(year, month) %>%  
  group_by(year) %>% 
  mutate(month_number = row_number()) %>% 
  ungroup() %>% 
  filter(!(year == 1986 & month_number <6),
         !(year == 2020 & month_number >7))




life_data2 <- life_data %>% 
  rowwise() %>% 
  mutate(year_month = paste0(c(year, month_number), collapse ="_")) %>% 
  ungroup() %>% 
  mutate(period = case_when(
    year_month == "1986_6" ~ "Preverbal",
    year_month == "1987_3" ~ "Young Child",
    year_month == "1991_7" ~ "Primary Schooling",
    year_month == "1998_8" ~ "Secondary Schooling",
    year_month == "2004_8" ~ "Glasgow Uni",
    year_month == "2009_6" ~ "Childcare Worker",
    year_month == "2010_1" ~ "PhD, Edinburgh Uni/SAC",
    year_month == "2013_5" ~ "Animal Behaviour Researcher, SAC",
    year_month == "2016_6" ~ "Education Researcher, Edinburgh Uni"
  )) %>% 
  fill(period) %>% 
  mutate(period = fct_inorder(period))




# waffle_iron should in theory convert life_data2 to an x,y, group dataframe
# But it doesn't want to count my data so
# the waffle expects x to be year, y to be month, group to be period
# not sure how it automatically knows to row 12
# I guess its picking up on there only being 12 levels max

# I go through EmojiOne fonts one by one to find the emoji I want
# e.g. search_emoji('book')

chart <- 
  life_data2 %>% 
  mutate(label = case_when(period == "Preverbal" ~ emoji('baby'),
                           period == "Young Child" ~ emoji('girl'),
                           period == "Primary Schooling" ~ emoji('open_book'),
                           period == "Secondary Schooling" ~ emoji('books'),
                           period == "Glasgow Uni" ~ emoji('woman_student'),
                           period == "Childcare Worker" ~ emoji('children_crossing'),
                           period == "PhD, Edinburgh Uni/SAC" ~ emoji('cow'),
                           period == "Animal Behaviour Researcher, SAC" ~ emoji('mortar_board'),
                           period == "Education Researcher, Edinburgh Uni" ~ emoji('woman'))) %>% 
  ggplot(aes(x = year, y = month)) + 
  geom_text(aes(label=label, color = period), family='EmojiOne', size=6) +
  coord_equal(clip = "off") +
  scale_color_nord(palette = "afternoon_prarie") +
  theme_waffle() +
  theme(plot.background = element_rect(fill = "#faf5f5", color = "#f0f0f0"),
        legend.position = "none") +
  labs (x = NULL, y = NULL) +
  annotate("text", label = "Preverbal", x = 1987, y = 15, size = 6, colour = "#486090",
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 1986, xend = 1986, y = 14.5, yend = 12.3), arrow = arrow(length = unit(0.03, "npc")), color = "#486090") +
  annotate("text", label = "Young Child", x = 1990, y = 13.7, size = 6, colour = "#6078A8", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 1989, xend = 1988, y = 13.1, yend = 12.3), curvature = -0.2, arrow = arrow(length = unit(0.03, "npc")), color = "#6078A8") +
  annotate("text", label = "Primary School", x = 1991.5, y = -1.1, size = 6, colour = "#7890A8", 
           family = "Bradley Hand ITC", fontface = "bold") +
  geom_curve(aes(x = 1994, xend = 1993, y = -1, yend = 0.5), curvature = -0.3, arrow = arrow(length = unit(0.03, "npc")), color = "#7890A8") +
  annotate("text", label = "Secondary School", x = 1998, y = -0.1, size = 6, 
           colour = "#90A8C0", family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2004, y = -0.5, yend = 0.5), curvature = 0.3, arrow = arrow(length = unit(0.03, "npc")), color = "#90A8C0") +
  annotate("text", label = "Glasgow Uni", x = 2003, y = 14, size = 6, colour = "#F0D8C0", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2004.5, y = 13.5, yend = 12.5), curvature = -0.3, arrow = arrow(length = unit(0.03, "npc")), color ="#F0D8C0") +
  annotate("text", label = "Childcare Worker", x = 2009, y = 13, size = 6, colour = "#D6BBCF", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  annotate("text", label = "PhD", x = 2012, y = -0.1, size = 6, colour = "#A8C0C0", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 2011.2, xend = 2010, y = -0.1, yend = 0.5), curvature = -0.2, arrow = arrow(length = unit(0.03, "npc")), color ="#A8C0C0") +
  annotate("text", label = "Ethology Lecturer", x = 2016, y = 14, size = 6, colour = "#C0D8D8", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 2018, xend = 2014, y = 13.3, yend = 12.2), curvature = -0.1, arrow = arrow(length = unit(0.03, "npc")), color ="#C0D8D8") +
  annotate("text", label = "Education Researcher", x = 2015, y = -1.1, size = 6, colour = "#A8A890", 
           family = "Bradley Hand ITC", fontface = "bold" ) +
  geom_curve(aes(x = 2015, xend = 2018, y = -0.8, yend = 0.5), curvature = 0.3, arrow = arrow(length = unit(0.03, "npc")), color ="#A8A890") +
  annotate("text", label = "My Life in Months", x = 2003, y = 18, size = 9, colour = "#486090", 
           family = "Bradley Hand ITC", fontface = "bold", fontface = "bold" ) +
  annotate("text", label = "Each cell = 1 month, Each col = 1 Year", x = 2003, y = 16.5, size = 7, colour = "#486090", family = "Bradley Hand ITC") +
  annotate("text", label = "https://github.com/jillymackay/mylifeinmonths", x = 2015, y = -3, size = 3, colour = "#486090", family = "Calibri Light" ) +
  annotate("text", label = "an ugly fix", x = 2003, y = 18.5, size = 3, colour = "#f0f0f0") 






ggsave("life_in_months.png", plot = chart, device = "png", dpi = 300)






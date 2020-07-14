# I could only get ggwaffle to work, not library(waffle)
# devtools::install_github("liamgilbey/ggwaffle")


library(tidyverse)
library(lubridate)
library(ggwaffle)
library(nord)


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
    year_month == "1993_7" ~ "Primary Schooling",
    year_month == "1998_8" ~ "Secondary Schooling",
    year_month == "2004_8" ~ "Glasgow Uni",
    year_month == "2009_6" ~ "Childcare Worker",
    year_month == "2010_1" ~ "PhD, Edinburgh Uni",
    year_month == "2013_5" ~ "Lecturer, Scotland's Agricultural College",
    year_month == "2016_6" ~ "Education Researcher, Edinburgh Uni"
  )) %>% 
  fill(period) %>% 
  mutate(period = fct_inorder(period))




# waffle_iron should in theory conver to an x,y, group dataframe
# But it doesn't want to count my data so
# the waffle expects x to be year, y to be mnth, group to be period
# not sure how it automatically knows to row 12
# I guess its picking up on there only being 12 levels

chart <- life_data2 %>% 
  ggplot(aes(x = year, y = month, fill = period)) + 
  geom_waffle() +
  coord_equal(clip = "off") +
  scale_fill_nord(palette = "afternoon_prarie") +
  theme_waffle() +
  theme(plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7")) +
  labs (x = NULL, y = NULL)



ggsave("life_in_months.png", plot = chart, device = "png", width = 25, height = 15, dpi = 300)

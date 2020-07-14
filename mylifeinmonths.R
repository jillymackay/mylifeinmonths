# I could only get ggwaffle to work, not library(waffle)
# devtools::install_github("liamgilbey/ggwaffle")


library(tidyverse)
library(lubridate)
library(ggwaffle)
library(forcats)


life_data <- expand_grid(
  m1 = month.name,
  year = 1986:2020) %>% 
  mutate(month = as_factor(m1),
         month = fct_relevel(m1, month.name)) %>% 
  arrange(year, month) %>%  
  group_by(year) %>% 
  mutate(month_number = row_number()) %>% 
  ungroup() %>% 
  filter(!(year == 1986 & month_number <6))



life_data2 <- life_data %>% 
  rowwise() %>% 
  mutate(year_month = paste0(c(year, month_number), collapse ="_")) %>% 
  ungroup() %>% 
  mutate(period = case_when(
    year_month == "1986_6" ~ "Preverbal",
    year_month == "1987_3" ~ "Young Child",
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





waffle_data <- waffle_iron(mpg, aes_d(group = class))

life_waffle <- waffle_iron(life_data2, aes_d(group = period))



# the waffle expects x to be year, y to be mnth, group to be period

ggplot(life_data2, aes(x = year, y = month_number, fill = period)) + 
  geom_waffle(n_rows = 12) +
  coord_equal() +
  scale_x_continuous(limits = c(-0.5))

# example

life_data2 %>% 
  count(period)  %>% 
  ggplot(aes(fill = period, values = n)) +
  geom_waffle(color = "#F7F7F7", n_rows = 12, size = 1, flip = FALSE)


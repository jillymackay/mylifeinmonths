# I could only get ggwaffle to work, not library(waffle)
# devtools::install_github("liamgilbey/ggwaffle")

# I really wanted to use FontAwesome, but there's a bug with RStudio. 
# I may go and do one with base R for funsies
# spot the one really nasty fix


library(tidyverse)
library(lubridate)
library(ggwaffle)
library(MetBrewer)
library(emojifont)
library(extrafont)
library(showtext)
font_add_google("Short Stack", family = "special")
showtext_auto()


life_data <- expand_grid(
  m1 = month.name,
  year = 1986:2026) |> 
  mutate(month = as_factor(m1),
         month = fct_relevel(m1, month.name)) |>  
  arrange(year, month) |>  
  group_by(year) |> 
  mutate(month_number = row_number()) |>  
  ungroup() |> 
  filter(!(year == 1986 & month_number <6),
         !(year == 2026 & month_number >11))  |> 
  rowwise() |> 
  mutate(year_month = paste0(c(year, month_number), collapse ="_")) |> 
  ungroup() |> 
  mutate(period = case_when(
    year_month == "1986_6" ~ "Infant",
    year_month == "1987_6" ~ "Toddler",
    year_month == "1991_7" ~ "Primary Schooling",
    year_month == "1998_8" ~ "Secondary Schooling",
    year_month == "2004_8" ~ "Glasgow Uni",
    year_month == "2009_6" ~ "Childcare Worker",
    year_month == "2010_1" ~ "PhD, Edinburgh Uni/SAC",
    year_month == "2013_5" ~ "Animal Behaviour Researcher, SAC",
    year_month == "2016_6" ~ "Education Researcher, Edinburgh Uni",
    year_month == "2022_1" ~ "Maternity Leave",
    year_month == "2023_2" ~ "Senior Lecturer, Edinburgh Uni",
    year_month == "2024_10" ~ "Maternity Leave",
    year_month == "2025_10" ~ "Senior Lecturer, Edinburgh Uni"
  )) |> 
  fill(period) |> 
  mutate(period = fct_inorder(period))




# waffle_iron should in theory convert life_data2 to an x,y, group dataframe
# But it doesn't want to count my data so
# the waffle expects x to be year, y to be month, group to be period
# not sure how it automatically knows to row 12
# I guess its picking up on there only being 12 levels max

# I go through EmojiOne fonts one by one to find the emoji I want
# e.g. search_emoji('book')


colourlist <- c("#5b859e", "#1e395f", "#4c825d",
                "#1e5a46", "#df8d71", "#af4f2f",
                "#d48f90", "#732f30", "#ab84a5",
                "#59385c", "#d8b847", "#b38711")


life_data |> 
  mutate(label = case_when(period == "Infant" ~ emoji('baby_symbol'),
                           period == "Toddler" ~ emoji('child'),
                           period == "Primary Schooling" ~ emoji('school'),
                           period == "Secondary Schooling" ~ emoji('open_book'),
                           period == "Glasgow Uni" ~ emoji('mortar_board'),
                           period == "Childcare Worker" ~ emoji('children_crossing'),
                           period == "PhD, Edinburgh Uni/SAC" ~ emoji('cow'),
                           period == "Animal Behaviour Researcher, SAC" ~ emoji('cow2'),
                           period == "Education Researcher, Edinburgh Uni" ~ emoji('woman'),
                           period == "Maternity Leave" ~ emoji('baby'),
                           period == "Senior Lecturer, Edinburgh Uni" ~ emoji('woman'))) %>% 
  ggplot(aes(x = year, y = month)) + 
  geom_text(aes(label=label, color = period), family="OpenMoji", size=8) +
  coord_equal(clip = "off") +
  scale_colour_met_d("Redon") +
  #scale_color_nord(palette = "moose_pond") +
  theme_waffle() +
  theme(plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.position = "none") +
  labs (x = NULL, y = NULL) +
  annotate("text", label = "Infant", x = 1987, y = 15, size = 6, colour = "#5b859e",
           family = "Short Stack" ) +
  geom_curve(aes(x = 1986, xend = 1986, y = 14.5, yend = 12.3), 
             arrow = arrow(length = unit(0.03, "npc")), color = "#5b859e") +
  annotate("text", label = "Toddler", x = 1990, y = 13.7, size = 6, colour = "#1e395f", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 1989, xend = 1988, y = 13.1, yend = 12.3), curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#1e395f")+
  annotate("text", label = "Primary School", x = 1991.5, y = -1.1, size = 6, colour = "#4c825d", 
           family = "Calibri Light", fontface = "bold") +
  geom_curve(aes(x = 1994, xend = 1993, y = -1, yend = 0.5), curvature = 0.3, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#4c825d") +
  annotate("text", label = "Secondary School", x = 2000, y = -0.1, size = 6, 
           colour = "#1e5a46", family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2003.5, y = -0.5, yend = 0.5), curvature = 0.3, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#1e5a46") +
  annotate("text", label = "Glasgow Uni", x = 2003, y = 14, size = 6, colour = "#df8d71", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2004.5, y = 13.5, yend = 12.5), 
             curvature = -0.3, arrow = arrow(length = unit(0.03, "npc")), color ="#af4f2f") +
  annotate("text", label = "Childcare Worker", x = 2009, y = 13, size = 6, colour = "#af4f2f", 
           family = "Calibri Light", fontface = "bold" ) +
  annotate("text", label = "PhD", x = 2012, y = -0.1, size = 6, colour = "#d48f90", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2011.3, xend = 2011, y = -0.1, yend = 0.5), curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc")), color ="#d48f90") +
  annotate("text", label = "Lecturer\n(Ethology)", x = 2016, y = 14, size = 6, colour = "#732f30", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2015, xend = 2014, y = 13.3, yend = 12.2), 
             curvature = -0.1, arrow = arrow(length = unit(0.03, "npc")), color ="#732f30") +
    annotate("text", label = "Lecturer\n(Vet Science Education)", x = 2019, y = -1.1, size = 6, colour = "#ab84a5", 
             family = "Calibri Light", fontface = "bold" ) +
    geom_curve(aes(x = 2019, xend = 2020, y = -0.5, yend = 0.5), curvature = -0.3,
               arrow = arrow(length = unit(0.03, "npc")), color ="#ab84a5") +
     annotate("text", label = "Mat Leave\n (1 + 2)", x = 2022, y = 14, size = 6, colour = "#59385c", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2022, xend = 2022.3, y = 13, yend = 12), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#59385c") +
  geom_curve(aes(x = 2022, xend = 2024, y = 13, yend = 12), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#59385c") +
    annotate("text", label = "Working Parent", x = 2025, y = -1.1, size = 6, colour = "#d8b847", 
             family = "Calibri Light", fontface = "bold" ) +
    geom_curve(aes(x = 2024, xend = 2024, y = -1, yend =0), curvature = -0.3,
               arrow = arrow(length = unit(0.03, "npc")), color ="#d8b847") +
  annotate("text", label = "My Life in Months", x = 2003, y = 18, size = 9, colour = "#1e395f", 
           family = "Calibri Light", fontface = "bold", fontface = "bold" ) +
  annotate("text", label = "Each cell = 1 month, Each col = 1 Year", x = 2003, 
           y = 16.5, size = 7, colour = "#1e395f", family = "Calibri Light") +
  annotate("text", label = "https://github.com/jillymackay/mylifeinmonths", x = 2015, 
           y = -3, size = 5, colour = "#486090", family = "Calibri Light" ) 





ggsave("life_in_months.png", device = "png", dpi = 300)








fibre_reproduce <- expand_grid(
  m1 = month.name,
  year = 1986:2026) |> 
  mutate(month = as_factor(m1),
         month = fct_relevel(m1, month.name)) |>  
  arrange(year, month) |>  
  group_by(year) |> 
  mutate(month_number = row_number()) |>  
  ungroup() |> 
  filter(!(year == 1986 & month_number <6),
         !(year == 2026 & month_number >11))  |> 
  rowwise() |> 
  mutate(year_month = paste0(c(year, month_number), collapse ="_")) |> 
  ungroup() |> 
  mutate(period = case_when(
    year_month == "1986_6" ~ "Infant",
    year_month == "1987_6" ~ "Toddler",
    year_month == "1991_7" ~ "Primary Schooling",
    year_month == "1998_8" ~ "Secondary Schooling",
    year_month == "2000_6" ~ "Knit",
    year_month == "2000_7" ~ "Secondary Schooling",
    year_month == "2004_8" ~ "Glasgow Uni",
    year_month == "2009_6" ~ "Childcare Worker",
    year_month == "2010_1" ~ "PhD, Edinburgh Uni/SAC",
    year_month == "2013_5" ~ "Animal Behaviour Researcher, SAC",
    year_month == "2016_6" ~ "Education Researcher, Edinburgh Uni",
    year_month == "2022_1" ~ "Maternity Leave",
    year_month == "2023_2" ~ "Senior Lecturer, Edinburgh Uni",
    year_month == "2024_10" ~ "Maternity Leave",
    year_month == "2025_10" ~ "Senior Lecturer, Edinburgh Uni"
  )) |> 
  fill(period) |> 
  mutate(period = fct_inorder(period))




fibre_reproduce |> 
  mutate(label = case_when(period == "Infant" ~ emoji('baby_symbol'),
                           period == "Toddler" ~ emoji('child'),
                           period == "Primary Schooling" ~ emoji('school'),
                           period == "Secondary Schooling" ~ emoji('open_book'),
                           period == "Glasgow Uni" ~ emoji('mortar_board'),
                           period == "Childcare Worker" ~ emoji('children_crossing'),
                           period == "PhD, Edinburgh Uni/SAC" ~ emoji('cow'),
                           period == "Animal Behaviour Researcher, SAC" ~ emoji('cow2'),
                           period == "Education Researcher, Edinburgh Uni" ~ emoji('woman'),
                           period == "Maternity Leave" ~ emoji('baby'),
                           period == "Senior Lecturer, Edinburgh Uni" ~ emoji('woman'),
                           period == "Knit" ~ emoji('yarn'))) %>% 
  ggplot(aes(x = year, y = month)) + 
  geom_text(aes(label=label, color = period), family="OpenMoji", size=8) +
  coord_equal(clip = "off") +
  scale_colour_met_d("Redon") +
  theme_waffle() +
  theme(plot.background = element_rect(fill = "#ffffff", color = "#ffffff"),
        legend.position = "none") +
  labs (x = NULL, y = NULL) +
  annotate("text", label = "Infant", x = 1987, y = 15, size = 6, colour = "#5b859e",
           family = "Short Stack" ) +
  geom_curve(aes(x = 1986, xend = 1986, y = 14.5, yend = 12.3), 
             arrow = arrow(length = unit(0.03, "npc")), color = "#5b859e") +
  annotate("text", label = "Toddler", x = 1990, y = 13.7, size = 6, colour = "#1e395f", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 1989, xend = 1988, y = 13.1, yend = 12.3), curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#1e395f")+
  annotate("text", label = "Primary School", x = 1991.5, y = -1.1, size = 6, colour = "#4c825d", 
           family = "Calibri Light", fontface = "bold") +
  geom_curve(aes(x = 1994, xend = 1993, y = -1, yend = 0.5), curvature = 0.3, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#4c825d") +
  annotate("text", label = "Secondary School", x = 2000, y = -0.1, size = 6, 
           colour = "#1e5a46", family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2003.5, y = -0.5, yend = 0.5), curvature = 0.3, 
             arrow = arrow(length = unit(0.03, "npc")), color = "#1e5a46") +
  annotate("text", label = "Glasgow Uni", x = 2003, y = 14, size = 6, colour = "#af4f2f", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2002.5, xend = 2004.5, y = 13.5, yend = 12.5), 
             curvature = -0.3, arrow = arrow(length = unit(0.03, "npc")), color ="#af4f2f") +
  annotate("text", label = "Childcare Worker", x = 2009, y = 13, size = 6, colour = "#d48f90", 
           family = "Calibri Light", fontface = "bold" ) +
  annotate("text", label = "PhD", x = 2012, y = -0.1, size = 6, colour = "#732f30", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2011.3, xend = 2011, y = -0.1, yend = 0.5), curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc")), color ="#732f30") +
  annotate("text", label = "Lecturer\n(Ethology)", x = 2016, y = 14, size = 6, colour = "#ab84a5", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2015, xend = 2014, y = 13.3, yend = 12.2), 
             curvature = -0.1, arrow = arrow(length = unit(0.03, "npc")), color ="#ab84a5") +
  annotate("text", label = "Lecturer\n(Vet Science Education)", x = 2019, y = -1.1, size = 6, colour = "#59385c", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2019, xend = 2020, y = -0.5, yend = 0.5), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#59385c") +
  annotate("text", label = "Mat Leave\n (1 + 2)", x = 2022, y = 14, size = 6, colour = "#d8b847", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2022, xend = 2022.3, y = 13, yend = 12), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#d8b847") +
  geom_curve(aes(x = 2022, xend = 2024, y = 13, yend = 12), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#d8b847") +
  annotate("text", label = "Working Parent", x = 2025, y = -1.1, size = 6, colour = "#b38711", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 2024, xend = 2024, y = -1, yend =0), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#b38711") +
  annotate("text", label = "Started Fibre\nArts", x = 1998, y = 14, size = 6, colour = "#df8d71", 
           family = "Calibri Light", fontface = "bold" ) +
  geom_curve(aes(x = 1998, xend = 2000, y = 13, yend =6.8), curvature = -0.3,
             arrow = arrow(length = unit(0.03, "npc")), color ="#df8d71") +
  annotate("text", label = "My Life in Months", x = 2003, y = 18, size = 9, colour = "#1e395f", 
           family = "Calibri Light", fontface = "bold", fontface = "bold" ) +
  annotate("text", label = "Each cell = 1 month, Each col = 1 Year", x = 2003, 
           y = 16.5, size = 7, colour = "#1e395f", family = "Calibri Light") +
  annotate("text", label = "https://github.com/jillymackay/mylifeinmonths", x = 2015, 
           y = -3, size = 5, colour = "#486090", family = "Calibri Light" ) 




ggsave("fibre.png", device = "png", dpi = 300)



library(tidyverse)
library(googlesheets4)
ndays <- as.Date("2023-11-03") - as.Date("2021-04-01") + 1

# number of users ---------------------------------------------------------
datvis <- read_csv("data/data-export.csv", skip = 8, n_max = ndays) %>%
  select(day = `Nth day`,
         users = Users) %>%
  mutate(day = as.numeric(day),
         date = as.Date("2021-04-01") + day) %>% 
  filter(day > 353)

gvis <- datvis %>%
  ggplot(aes(date)) +
  annotate("rect",
           xmin = c(as.Date("2022-07-25") - 7, as.Date("2022-03-21"), as.Date("2023-02-27") - 7, as.Date("2023-07-24") - 7),
           xmax = c(as.Date("2022-10-21"), as.Date("2022-05-27"), as.Date("2023-05-26"), as.Date("2023-10-20")),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.4,
           fill = "grey80") +
  geom_line(aes(y = users)) +
  annotate("text",
           x = c(as.Date("2022-07-18"), as.Date("2022-10-21"), as.Date("2022-05-27"), 
                 as.Date("2023-02-20"), as.Date("2023-07-17"), as.Date("2023-05-26"), as.Date("2023-10-20")),
           y = 75,
           color = "grey40",
           size = 3,
           label = c("Week 0", "Week 12", "Week 12", "Week 0", "Week 0", "Week 12", "Week 12"),
           angle = 90) +
  annotate("text",
           x = c(as.Date("2022-04-20"), as.Date("2022-09-01"), as.Date("2023-04-15"), as.Date("2023-09-06")),
           y = 75,
           label = c("Semester 1", "Semester 2", "Semester 1", "Semester 2")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        panel.grid.major.y = element_line(color = "grey", linetype = "dashed")) +
  labs(x = "", y = "", title = "Number of users for the Learn R website")

ggsave("images/numberofusers.jpg", width = 8, height = 4, dpi = 600)

# engagement --------------------------------------------------------------
dateng <- read_csv("data/data-export.csv", skip = 1910, n_max = ndays) %>%
  select(day = `Nth day`,
         engagement = `Average engagement time`) %>%
  mutate(day = as.numeric(day),
         date = as.Date("2021-04-01") + day) %>% 
  filter(day > 353)

dat <- datvis %>%
  left_join(dateng, by = "date")

median(dat$engagement/dat$users, na.rm = TRUE)

ggplot(dat, aes(engagement/users)) +
  geom_histogram(color = "white") +
  scale_x_log10(labels = scales::comma_format()) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        panel.grid.major = element_line(color = "grey", linetype = "dashed")) +
  labs(x = "", y = "Frequency", title = "Average engagement time per user across days (in minutes)",
       subtitle = "Most users spend about 11 minutes on the website")

ggsave("images/engagement.jpg", width = 8, height = 4, dpi = 600)

# survey results ----------------------------------------------------------

dat <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eeeNgn4T87UFSX6fYX-4Zg_-ZSV8uIete6Lz4m7ySis/edit?usp=sharing")
dat2 <- dat %>% 
  select(rate = `How would you rate the overall effectiveness of this resource for you?`) %>% 
  filter(rate > 1) %>% 
  mutate(rate = factor(rate, levels = 1:10)) 

ggplot(dat2, aes(rate)) + geom_bar() +
  scale_x_discrete(drop = FALSE) +
  theme(plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks.length = unit(0, "cm"),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        panel.grid.major = element_line(color = "grey", linetype = "dashed")) +
  labs(x = "Rating from 1 (Not very effective) to 10 (very effective)",
       y = "Frequency",
       title = "How would you rate the overall effectiveness of this resource for you?",
       subtitle = "Average rating is 7.6 and median rating is 8.0 out of 10")

ggsave("images/ratings.jpg", width = 8, height = 4, dpi = 600)

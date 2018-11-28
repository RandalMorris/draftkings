library(tidyverse)
library(lubridate)

dat <- 
  read_csv("./data/all_games.csv") %>% 
  janitor::clean_names() %>% 
  filter(complete.cases(.), dk_salary > 0)

dat <- dat %>% mutate(ratio = dk_points/dk_salary)

ggplot(data = dat) +
  geom_point(aes(x = dk_salary, y = dk_points, color = as.factor(week))) +
  facet_wrap(facets = ~year, nrow = 2)

ggplot(data = dat) + 
  geom_point(aes(x = dk_salary, y = dk_points, color = name)) + 
  theme(legend.position = "none") +
  facet_wrap(~pos, ncol = 3)

ggplot(data = dat) + 
  geom_point(aes(x = dk_salary, y = ratio, color = name)) + 
  theme(legend.position = "none") +
  facet_wrap(~pos, ncol = 3) + 
  geom_hline(yintercept = .005) +
  geom_vline(xintercept = 7500)

ggplot(data = dat) +
  geom_line(aes(x = week, ratio, color = as.factor(year))) + 
  facet_wrap(~team, ncol = 6)

ggplot() +
  geom_line(data = filter(dat, name == "Roethlisberger, Ben", year %in% 2016:2017), aes(x = week, y = ratio)) +
  geom_line(data = filter(dat, name == "Fournette, Leonard", year %in% 2016:2017), aes(x = week, y = ratio)) +
  geom_line(data = filter(dat, name == "Bell, Le'Veon", year  %in% 2016:2017), aes(x = week, y = ratio)) +
  geom_line(data = filter(dat, name == "Brady, Tom", year %in% 2016:2017), aes(x = week, y = ratio)) +
  facet_wrap(~year)



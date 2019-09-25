library(tidyverse)
library(readr)
library(readxl)

dat <- read_csv("./data/preds.csv")

dat <- 
  dat %>% 
  filter((dk_salary.x > 2500 & pos.x == "TE") |
           (dk_salary.x > 4700 & pos.x == "QB") |
           (dk_salary.x > 3000 & pos.x == "WR") |
           (dk_salary.x > 3000 & pos.x == "RB") |
           (dk_salary.x > 0 & pos.x == "Def"),
         projected_points > 0,
         !player %in% c("BlakeBortles_QB", "A.J.Green_WR", "joshDoctson_WR", "JoshDoctson_WR", "DavidMoore_WR", "AlexErickson_WR")) %>%
  rename(franchise = team.x)

sample(x = dat$player, size = 9, prob = dat$projected_points)

# filter non starters with high salary, injured, and arbitrary
qbs <- dat %>% filter(pos.x == "QB")
rbs <- dat %>% filter(pos.x == "RB")
wr <- dat %>% filter(pos.x == "WR")
te <- dat %>% filter(pos.x == "TE")
dst <- dat %>% filter(pos.x == "Def")

lst <- 
  list(
    qbs, # 1 qb
    rbs, rbs, # 2 rbs
    wr, wr, wr, # 3 wrs
    te, # 1 te
    rbind(rbs, wr), # 1 flex
    dst # 1 def/specialteams
  )

sample_position <- 
  function(position) {
    # position <- position %>% filter(AvgPointsPerGame > 5) # filter out players making no impact
    Name <- with(position, sample(x = player, size = 1, prob = projected_points))
    position %>% filter(player == Name)
  }

teams <- NULL
for (j in 1:1000) {
  
  success <- FALSE
  while (!success) {
    
    team <- NULL
    for (i in lst) {
      team <- rbind(team, sample_position(i))
    }
    cond1 <- sum(team$dk_salary.x) > 49000 # try to use almost all of the money
    cond2 <- sum(team$dk_salary.x) <= 50000 # max spend
    cond3 <- !team$player %>% duplicated %>% any() # playoffs have to have team dupes
    
    # print(c(cond1, cond2, cond3))
    
    success <- all(cond1, cond2, cond3)
    
  }
  
  print(j)
  
  team <- 
    team %>% 
    mutate(team_i = j, 
           team_points = sum(projected_points), 
           team_salary = sum(dk_salary.x),
           pos = factor(pos.x, levels = c("QB", "RB", "WR", "TE", "Def"))) %>%
    select(team_i, franchise, oppt, wk=week, year=year, player, projected_points, dk_salary = dk_salary.x, pos = pos.x, team_points, team_salary) %>%
    arrange(pos, desc(projected_points))
  
  teams <- rbind(teams, team)
  
}

qplot(unique(teams$team_points))

teams <- teams %>% arrange(desc(team_points), pos, desc(projected_points))

View(teams)

teams_filtered <- 
  teams %>% 
  group_by(team_i, franchise) %>% 
  mutate(frn_count = n()) %>% 
  group_by(team_i, franchise, pos) %>% 
  mutate(frn_pos_count = n()) %>% 
  ungroup() %>% 
  group_by(team_i) %>% 
  filter(all(frn_pos_count < 2)) %>% 
  ungroup()

View(teams_filtered)

# build lineup based on starting qb
teams_filtered %>% group_by(team_i) %>% filter(any(player == "TomBrady_QB")) %>% ungroup() %>% top_n(1, team_points)
teams_filtered %>% group_by(team_i) %>% filter(any(player == "GardnerMinshew_QB")) %>% ungroup() %>% top_n(1, team_points)


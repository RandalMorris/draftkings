library(tidyverse)
library(readr)
library(readxl)

dat <- read_csv("DKSalaries.csv")

sample(x = dat$Name, size = 9, prob = dat$Salary)

qbs <- dat %>% filter(`Roster Position` == "QB", Salary > 4500, !Name == "Jameis Winston")
rbs <- dat %>% filter(`Roster Position` == "RB/FLEX", Salary > 4000)
wr <- dat %>% filter(`Roster Position` == "WR/FLEX", Salary > 4000, 
                     !Name == "A.J. Green", 
                     !Name == "Torrey Smith", 
                     !Name == "Marvin Jones Jr.",
                     !Name == "Keelan Cole")
te <- dat %>% filter(`Roster Position` == "TE/FLEX", Salary > 2500)
dst <- dat %>% filter(`Roster Position` == "DST")

lst <- 
  list(
    qbs, 
    rbs, rbs,
    wr, wr, wr,
    te,
    rbind(rbs, wr),
    dst
  )

sample_position <- 
  function(position) {
    position <- position %>% filter(AvgPointsPerGame > 5)
    player <- with(position, sample(x = Name, size = 1, prob = Salary))
    position %>% filter(Name == player)
  }

success <- FALSE
while (!success) {
  
  team <- NULL
  for (i in lst) {
    team <- rbind(team, sample_position(i))
  }
  cond1 <- sum(team$Salary) > 49500
  cond2 <- sum(team$Salary) <= 50000
  cond3 <- !team$Name %>% duplicated %>% any()
  
  print(c(cond1, cond2, cond3))
  
  success <- all(cond1, cond2, cond3)
  
}
team
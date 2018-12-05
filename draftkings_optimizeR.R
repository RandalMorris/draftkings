library(lpSolveAPI)
library(readr)

dat <- read_csv("./data/preds.csv")

find_teams <- function(train, cap = 50000) {
  ## set constraints to use
  defense <- ifelse(train$position == "Def", 1, 0)
  qb <- ifelse(train$position == "QB", 1, 0)
  wr <- ifelse(train$position == "WR", 1, 0)
  te <- ifelse(train$position == "TE", 1, 0)
  rb <- ifelse(train$position == "RB", 1, 0)
  
  lpfantasy <- make.lp(0, nrow(train))
  
  set.objfn(lpfantasy, train$projected_points)
  
  set.type(lpfantasy, seq(1, nrow(train), by=1), type = c("binary"))
  
  dk_total <- defense + qb + wr + rb + te
  add.constraint(lpfantasy, defense, "=", 1)
  add.constraint(lpfantasy, qb, "=", 1)
  add.constraint(lpfantasy, wr, "<=", 4)
  add.constraint(lpfantasy, wr, ">=", 3)
  add.constraint(lpfantasy, rb, "<=", 3)
  add.constraint(lpfantasy, rb, ">=", 2)
  add.constraint(lpfantasy, te, "=", 1)
  add.constraint(lpfantasy, dk_total, "=", 9)
  
  add.constraint(lpfantasy, train$salary, "<=", cap)
  
  lp.control(lpfantasy, sense='max')
  
  team <- data.frame(matrix(0, 1, ncol(train) + 2))
  colnames(team) <- c(colnames(train), "TeamSalary", "TotalPoints")
  
  solve(lpfantasy)
  if(solve(lpfantasy) != 0)
    stop("Optimization failed at some step")
  
  team_select <- subset(data.frame(train, get.variables(lpfantasy)), get.variables.lpfantasy. == 1)
  team_select$get.variables.lpfantasy. <- NULL
  team_select$TeamSalary <- sum(team_select$salary)
  team_select$TotalPoints <- sum(team_select$projected_points)
  team <- rbind(team, team_select)
  team <- team[-1,]
  rownames(team) <- NULL
  
  team %>% 
    mutate(position = factor(position, levels = c("QB", "RB", "WR", "TE", "Def")),
           ActualPoints = sum(dk_points_raw)) %>% 
    arrange(position, desc(salary))
  
}


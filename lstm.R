library(keras)
library(tidyverse)
library(lubridate)
library(abind)
library(data.table)

setwd("/home/david/draftkings")
source("scraping.R")

team_city <- 
  read_csv("./data/teams.csv") %>% 
  select(Team, City) %>% 
  mutate(Name = paste0(unlist(lapply(str_split(City, ", "), function(data) data[[1]])), " Defense"))

score <- 
  read_csv("./data/1223_DKSalaries_12games.csv") %>% 
  janitor::clean_names() %>% 
  rename(player_key = name_id) %>%
  mutate(name = if_else(roster_position == "DST", paste(tolower(team_abbrev), "Def", sep = "_"), name),
         name = sapply(name, 
                       function(data) {
                         ifelse(data %in% team_city$Team,
                                pull(filter(team_city, data==Team), Name),
                                data)}),
         name = str_replace(name , " ", ""),
         player_key = if_else(roster_position == "DST", name, player_key))

# agrepl("Todd Gurley", "Todd Gurley II") fuzzy matching to fix this..
Name <- function(player_name) {
  
  lgical <- !grepl("Defense", player_name)
  p_list <- str_split(player_name, ", ")
  
  ifelse(
    lgical,
    
    lapply(p_list, function(data) paste0(data[2], data[1])) %>% unlist(),
    
    player_name
  )
}

dat <- 
  read_csv("./data/all_games.csv") %>% 
  janitor::clean_names() %>% 
  filter(complete.cases(.), dk_salary > 0, !oppt == "-") %>%
  mutate(name = Name(name), player_key = paste(name, pos, sep = "_")) %>%
  select(-gid) %>%
  mutate(team = str_replace(team, "sdg", "lac"),
         team = str_replace(team, "stl", "lar"),
         team = str_replace(team, "tam", "tb"),
         team = str_replace(team, "gnb", "gb"),
         team = str_replace(team, "jac", "jax"),
         team = str_replace(team, "nwe", "ne"),
         team = str_replace(team, "nor", "no"),
         team = str_replace(team, "sfo", "sf"),
         team = str_replace(team, "kan", "kc"),
         oppt = str_replace(oppt, "sdg", "lac"),
         oppt = str_replace(oppt, "stl", "lar"),
         oppt = str_replace(oppt, "tam", "tb"),
         oppt = str_replace(oppt, "gnb", "gb"),
         oppt = str_replace(oppt, "jac", "jax"),
         oppt = str_replace(oppt, "nwe", "ne"),
         oppt = str_replace(oppt, "nor", "no"),
         oppt = str_replace(oppt, "sfo", "sf"),
         oppt = str_replace(oppt, "kan", "kc"),
         name = if_else(pos == "Def", paste(team, pos, sep = "_"), name),
         player_key = if_else(pos == "Def", paste(team, pos, sep = "_"), player_key))

match_nm <- function(nm) {
  
  t1 <- Sys.time()
  for (p in unique(score$name)) {
    
    if (!agrep(nm, p, value = T) %>% length() == 0) {
      return(data.frame(name = nm, name_match = agrep(nm, p, value = T)))
    }
  }
}

t <- lapply(unique(dat$name), match_nm)
t_df <- NULL
for (i in t) {
  t_df <- rbind(t_df, i)
}
t_df %>% filter(grepl("Todd", name))

team_names <- function(event, team) {
  
  parsed_game_info <- str_split(event, "@| ")
  
  df_g <- NULL
  for (i in 1:length(parsed_game_info)) {
    
    g <- parsed_game_info[[i]] 
    g[1:2] <- tolower(g[1:2])
    
    tm <- tolower(team[[i]])
    
    g <- tibble(team = if_else(g[1]==tm, g[1], g[2]),
                oppt = if_else(g[1]==tm, g[2], g[1]),
                h_a = if_else(g[1]==tm, "a", "h"),
                dt = g[3],
                game_time = g[4],
                tz = g[5]
    )
    
    df_g <- bind_rows(df_g, g) %>% as_tibble()
    
  }
  return(df_g)
  
}

score <- 
  score %>% 
  mutate(week = 15, year = 2018, pos = str_replace(position, "DST", "Def")) %>% 
  bind_cols(team_names(score$game_info, score$team_abbrev)) %>%
  select(week, year, name, pos, team, h_a, oppt, dk_salary = salary)

score <- 
  score %>% 
  # mutate(i = row_number()) %>% 
  left_join(t_df, by = c("name" = "name_match")) %>%
  # mutate(j = row_number()) %>%
  mutate(name.y = as.character(name.y),
         name = if_else(grepl("Def", name), name, name.y),
         dk_points = NA, 
         player_key = if_else(grepl("Def", name), name, paste(name, pos, sep = "_"))) %>%
  select(-name.y) %>%
  filter(!name == "Max McCaffrey", !duplicated(.))
#inner_join(distinct(dat[dat$year == 2018,], name, pos, team), by = c("name.y" = "name"))

wk13_2018 <- score
# wk13_2018 <- dat %>% filter(year == 2018 & week == 14)
# dat <- dat %>% filter(!(year == 2018 & week >= 14))
# dat <- rbind(dat, wk13_2018) #
# dat <- dat %>% filter(!week == 12 | !year == 2018)

scaled <- function(data) {
  
  mn_points <- mean(data[["dk_points"]], na.rm = T)
  sd_points <- sd(data[["dk_points"]], na.rm = T)
  mn_salary <- mean(data[["dk_salary"]], na.rm = T)
  sd_salary <- sd(data[["dk_salary"]], na.rm = T)
  
  data <- 
    data %>%
    mutate(dk_points_raw = dk_points,
           dk_salary_raw = dk_salary,
           dk_points = (dk_points-mn_points)/sd_points,
           dk_salary = (dk_salary-mn_salary)/sd_salary)
  return(list(df = data, mn_points = mn_points, sd_points = sd_points, sd_salary = sd_salary, mn_salary = mn_salary))
}

scaled_dat <- scaled(dat)
dat <- scaled_dat$df
wk_2018 <- scaled(wk13_2018)$df

# dat <- dat %>% mutate(player_key = if_else(pos == "Def", paste(team, pos, sep = "_"), player_key))
# wk13_2018$df <- wk13_2018$df %>% mutate(player_key = if_else(pos == "Def", paste(team, pos, sep = "_"), player_key))

# tmp <- dat %>% filter(name == "TomBrady" | name == "BlakeBortles" | name == "LeonardFournette") ## yes I am a jags fan

create_array <- function(data) {
  t1 <- Sys.time()
  
  years <- sort(unique(data[["year"]]))
  players <- sort(unique(data[["player_key"]]))
  
  dk_x <- array(numeric(), dim = c(0, 17, 8)); dk_y <- array(numeric(), dim = c(0, 17, 1))
  teams <- sort(unique(data[["team"]]))
  positions <- sort(unique(data[["pos"]]))
  ref <- data.frame(s = 1:17)
  
  for (y in years) {
    
    for (p in players) {
      
      dk_i <- array(numeric(), dim = c(1, 17, 0))
      dk_j <- array(numeric(), dim = c(1, 17, 0))
      
      # print(y)
      # print(p)
      
      x <- data %>% filter(year == y, player_key == p)
      ## need to create windowed matrix here with zero padding for missing weeks (earlier in season)
      
      x_expand <- 
        ref %>% 
        left_join(x, by = c("s" = "week")) %>%
        rename(wk = s)
      
      x_expand <- sapply(x_expand, function(data) replace_na(data, "unk")) %>% as.data.frame()
      
      # print(x_expand)
      for (i in names(x_expand)[-1]) {
        # print(i)
        if (i == "dk_points") {
          dataX <- c("unk", x_expand[[i]][-17])
          dataY <- x_expand[[i]]
          # print(dataa)
          dk_array_y <- array(dataY, dim = c(1,17,1))
          dk_array <- array(dataX, dim = c(1,17,1))
          # print(dk_array)
          dk_i <- abind(dk_i, dk_array_y, along = 3)
          dk_j <- abind(dk_j, dk_array, along = 3)
          }
        else {
          dataX <- x_expand[[i]]
          dk_array <- array(dataX, dim = c(1,17,1))
          dk_j <- abind(dk_j, dk_array, along = 3)
          }
          
      }
      # print(dk_j)
      
      dk_x <- abind(dk_x, dk_j, along = 1)
      dk_y <- abind(dk_y, dk_i, along = 1)
      
      print(list(c(y, p), dim(dk_j), dim(dk_x), dim(dk_i), dim(dk_y)))
    }
    
  }
  
  t2 <- Sys.time()
  return(list(dk_x = dk_x, dk_y = dk_y, t2-t1))
}


train_set <- create_array(select(dat, week, year, player_key, pos, team, h_a, oppt, dk_points, dk_salary))
test_set <- create_array(select(wk_2018, week, year, player_key, pos, team, h_a, oppt, dk_points, dk_salary))

train_x <- train_set$dk_x
train_y <- train_set$dk_y
test_x <- test_set$dk_x
test_y <- test_set$dk_y

set.seed(42)
shffl <- sample(1:dim(train_x)[1])

train_x <- train_x[shffl,,]
train_y <- train_y[shffl,,]


## for given DK_Salary what is expected DK_points (include more features, examples below)
keras::k_clear_session()

main_input <- layer_input(shape = dim(sets$train$x)[-1], name = "main_input")

lstm <- 
  main_input %>%
  layer_lstm(units = 17, return_sequences = T) %>%
  # layer_lstm(units = 4, activity_regularizer = regularizer_l2(), return_sequences = T) %>% ## lstm return_sequences = T
  layer_lstm(units = 17)#, activity_regularizer = regularizer_l2(l = 0.01)) ## lstm return_sequences = T


aux_output <- ## for training lstm smoothly
  lstm %>%
  layer_dense(units = 1, name = "aux_output", activation = 'linear')

features_input <- layer_input(shape = 1198, name = "features_input")

features_k <-
  features_input %>%
  layer_dense(units = 64, activation = 'elu') %>%
  layer_dropout(rate = 0.2) # lot of input features

features_output <-
  features_k %>%
  layer_dense(units = 1, name = "features_output", activation = 'linear')

main_output <- 
  layer_concatenate(c(lstm, features_k)) %>% ## concat
  layer_dense(units = 64, activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  layer_dropout(rate = 0.4) %>%
  # layer_activity_regularization(l2 = 0.01) %>%
  # layer_dense(units = 8, activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  # layer_dropout(rate = 0.2) %>%
  # layer_activity_regularization(l2 = 0.01) %>%
  layer_dense(units = 1, name = "main_output", activation = 'linear') ## return the expected DK_points

model <-
  keras_model(inputs = c(main_input, features_input),
              outputs = c(main_output, aux_output, features_output))

model %>% compile(
  optimizer = optimizer_adam(lr = 0.001),
  loss = "mae",
  loss_weights = c(1.0, 0.2, 0.1)
)

history <- fit(model, x = list(sets$train$x, sets$train$features), y = list(sets$train$y, sets$train$y, sets$train$y), 
               validation_split = 0.3,
               # validation_data = list(list(sets$validation_data$x, sets$validation_data$features),
               #                        list(sets$validation_data$y, sets$validation_data$y, sets$validation_data$y)),
               epochs = 1000,
               callbacks = callback_early_stopping(monitor = "val_main_output_loss", patience = 10),
               # view_metrics = TRUE, 
               batch_size = 2^10
) # add callback for saving model (in case it takes forever)

history_df <- 
  rbind(
    data.frame(data = "training", 
               epochs = 1:length(history$metrics$val_main_output_loss), main_output_loss = history$metrics$main_output_loss),
    data.frame(data = "validation", 
               epochs = 1:length(history$metrics$val_main_output_loss), main_output_loss = history$metrics$val_main_output_loss))

ggplot(data = history_df, aes(x = epochs, y = main_output_loss, color = data)) + 
  geom_point() + geom_line()

p <- model %>% predict(list(sets$test$x, sets$test$features))
p <- p[[1]]

p <- exp(p) - sets$adj


wk13_preds <- 
  x %>% 
  mutate(player = str_replace(player, "NA ", "")) %>%
  filter(year == 2018, wk == 15) %>% 
  select(1, 18, 19, 20) %>%
  # filter(player_key %in% wk13_players) %>% 
  mutate(# salary = sets$test$features[,"salary"]*scaled_dat$sd_salary+scaled_dat$mn_salary, 
         projected_points = as.vector(p*scaled_dat$sd_points+scaled_dat$mn_points),
         position = unlist(lapply(str_split(player, "_"), function(data) data[[2]]))) %>%
  select(-`16`) %>% 
  inner_join(wk13_2018$df, by = c("player" = "player_key")) %>%
  mutate(salary = dk_salary_raw)

qplot(data = wk13_preds, x = salary, projected_points) + 
  geom_smooth()

ggplot(data = wk13_preds, aes(x = salary, projected_points)) + 
  geom_point() +
  facet_wrap(~position) +
  geom_smooth(method = lm, formula = y ~ x)

qplot(data = wk13_preds, x = projected_points, dk_points_raw) + 
  geom_abline()

qplot(wk13_preds$projected_points)

write_csv(wk13_preds, "./data/preds.csv")

source("./draftkings_optimizeR.R")

a <- 
  find_teams(cap = 50000, wk13_preds %>% 
               filter(!player %in% c("CurtisSamuel_WR", "Le'VeonBell_RB", "OdellBeckhamJr._WR", "Jeff WilsonJr._RB", "MarshawnLynch_RB", "BlakeBortles_QB"), 
                      salary > 2500,
                      !team %in% c("hou", "cle", "nyj", "den", "nyg"))) %>% 
  select(player, salary, projected_points, dk_points_raw, team, h_a, oppt, 
         TeamSalary, TotalPoints, ActualPoints)

View(a)

b <- 
  find_teams(cap = 50000-6700, wk13_preds %>% 
               filter(!player %in% c("Le'VeonBell_RB", "OdellBeckhamJr._WR", "Jeff WilsonJr._RB", "MarshawnLynch_RB", "BlakeBortles_QB", "SaquonBarkley_RB"), # obj is out vs titans 
                      salary > 2500,
                      !team %in% c("hou", "cle", "nyj", "den", "nyg"))) %>% # obj..
  select(player, salary, projected_points, dk_points_raw, team, h_a, oppt, 
         TeamSalary, TotalPoints, ActualPoints)
View(b)

c <- 
  find_teams(cap = 50000-6700, wk13_preds %>% 
               filter(!player %in% c("ToddGurley_RB", "AdamThielen_WR", "EzekielElliott_RB", "Le'VeonBell_RB", "OdellBeckhamJr._WR", "Jeff WilsonJr._RB", "MarshawnLynch_RB", "BlakeBortles_QB", "SaquonBarkley_RB"), 
                      salary > 2500,
                      !team %in% c("hou", "cle", "nyj", "den", "nyg"))) %>% # obj.. 
  select(player, salary, projected_points, dk_points_raw, team, h_a, oppt, 
         TeamSalary, TotalPoints, ActualPoints)
  
View(c)

library(keras)
library(tidyverse)
library(lubridate)
library(abind)
library(data.table)

setwd("/home/david/draftkings")
#source("scraping.R")

team_city <- 
  read_csv("./data/teams.csv") %>% 
  select(Team, City) %>% 
  mutate(Name = paste0(unlist(lapply(str_split(City, ", "), function(data) data[[1]])), " Defense"))

score <- 
  read_csv("./data/09222019_DK_Salaries.csv") %>% 
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
         player_key = str_replace(player_key, "PatrickMahomes II_QB", "PatrickMahomes_QB"),
         name = str_replace(name, "PatrickMahomes II", "PatrickMahomes"),
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
  mutate(week = 3, year = 2019, pos = str_replace(position, "DST", "Def")) %>% 
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
  filter(!name == "Max McCaffrey", !name == "MaxMcCaffrey", !duplicated(.))
#inner_join(distinct(dat[dat$year == 2018,], name, pos, team), by = c("name.y" = "name"))

wk13_2018 <- score
# wk13_2018 <- dat %>% filter(year == 2018 & week == 14)
# dat <- dat %>% filter(!(year == 2018 & week >= 14))
dat <- rbind(dat, wk13_2018) #
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
wk13_2018 <- scaled(wk13_2018)

# dat <- dat %>% mutate(player_key = if_else(pos == "Def", paste(team, pos, sep = "_"), player_key))
# wk13_2018$df <- wk13_2018$df %>% mutate(player_key = if_else(pos == "Def", paste(team, pos, sep = "_"), player_key))

# tmp <- dat %>% filter(name == "TomBrady" | name == "BlakeBortles" | name == "LeonardFournette") ## yes I am a jags fan

create_array <- function(data) {
  t1 <- Sys.time()
  
  years <- sort(unique(data[["year"]]))
  players <- sort(unique(data[["player_key"]]))
  
  dk_salary <- NULL; dk_salary_j <- NULL
  dk_points <- NULL; dk_points_j <- NULL
  ref <- NULL
  
  for (i in 1:18) {
    ref_i <- data.frame(wk = rep(i, 18), s = seq(from = i-17, to = i))
    ref <- rbind(ref, ref_i)
  }
  
  for (y in years) {
    
    for (p in players) {
      print(y)
      print(p)
      x <- data %>% filter(year == y, player_key == p)
      ## need to create windowed matrix here with zero padding for missing weeks (earlier in season)
      
      x_expand <- 
        ref %>% 
        left_join(x, by = c("s" = "week")) %>% 
        group_by(wk) %>% 
        mutate(i = seq(length(wk))) %>% 
        ungroup()
      
      dk_salary_j <- 
        x_expand %>%
        select(wk, dk_salary, i) %>% 
        spread(key = i, value = dk_salary, fill = 0) %>%
        mutate(year = y, player = p)
      
      dk_points_j <- 
        x_expand %>% 
        select(wk, dk_points, i) %>% 
        spread(key = i, value = dk_points, fill = 0) %>%
        mutate(`0` = 0) %>%
        mutate(year = y, player = p) %>%
        select(wk, `0`, everything())
        
      
      dk_salary <- rbind(dk_salary, dk_salary_j)
      dk_points <- rbind(dk_points, dk_points_j)
      
      print(list(dim(dk_salary), dim(dk_salary_j), dim(dk_points), dim(dk_points_j)))
    }
    
  }
  
  t2 <- Sys.time()
  return(list(dk_salary = dk_salary, dk_points = dk_points, t2-t1))
}


# l <- create_array(dat)
# 
# l_wk13_2018 <- create_array(wk13_2018$df)

# indices <- rowSums(l[[1]][2:18]) > 0# & rowSums(!l[[1]][2:18] == 0) > 5
# wk13_players <- l_wk13_2018[[1]] %>% filter(rowSums(l_wk13_2018[[1]][2:18]) > 0) %>% pull(player) %>% unique()

x <- dat %>% select(dk_salary, player=player_key, wk=week,  year)
y <- dat %>% pull(dk_points) %>% as.array()


salaries <- wk13_2018$df %>% select(dk_salary, player=player_key, wk=week, year)

features <- 
  left_join(select(x, player, wk, year), dat, by = c("player" = "player_key", "wk" = "week", "year")) %>% 
  mutate(bbi = if_else(is.na(name), 1, 0)) %>% ##bye week | benched | injured
  mutate_all(funs(replace_na(., 0))) %>%
  rename(salary = dk_salary_raw) %>%
  distinct()

features <- model.matrix(data = features, dk_points_raw ~ as.factor(year) + pos + h_a + oppt + team + 
                           #bbi + 
                           wk + player + salary)[, -1]
features <- as.data.table(features)

cols <- names(features)
fn_scale <- function(data) (data-mean(data))/sd(data)
features[ , (cols) := lapply(.SD, fn_scale), .SDcols = cols]
features <- as.matrix(features)

ord <- with(x, order(year, wk, player))

x <- x[ord,]
y <- y[ord]
features <- features[ord,]

set.seed(42)
shffl <- sample(1:nrow(x))

# x <- x[shffl,]
# y <- y[shffl]
# features <- features[shffl,]

# zeros_rm <- rowSums(x[,2:18])==0

# x <- x[!zeros_rm,]
# y <- y[!zeros_rm]
# features <- features[!zeros_rm,]
# x <- x[!is.na(y),]
# features <- features[!is.na(y),]
# y <- y[!is.na(y)]

create_sets <- function(x, y, features, yr, week) { # create training and test set for sanity checking predictions (most recent week)
  print(1)
  wk_2018_indices <- with(x, year == yr & wk == week)
  
  # (rowSums(x %>% select(2:18) == 0) < 12) %>% qplot()
  # val_indices <- rowSums(x %>% select(2:18) == 0) < 13
  # val_indices <- 1:length(val_indices) %in% sample(which(val_indices), 5000)
  print(2)
  x <- x %>% select(-wk, -year, -player) %>% as.matrix()
  print(3)
  x_train <- x[!(wk_2018_indices)] %>% as.matrix()#  | val_indices 
  y_train <- y[!(wk_2018_indices)] #  | val_indices
  features_train <- features[!(wk_2018_indices),] #  | val_indices
  print(4)
  adj <- abs(min(y_train)) + 0.001
  y_train <- log(y_train+adj)
  print(5)
  x_test <- x[wk_2018_indices] %>% as.matrix()
  y_test <- y[wk_2018_indices]
  features_test <- features[wk_2018_indices,]
  
  # x_val <- x[val_indices,]
  # y_val <- y[val_indices]
  # features_val <- features[val_indices,]
  print(6)
  print(class(x_train))
  print(class(x_test))
  x_train <- array_reshape(x_train, dim = c(dim(x_train)[1], 1, 1))
  x_test <- array_reshape(x_test, dim = c(dim(x_test)[1], 1, 1))
  # x_val <- array_reshape(x_val, dim = c(dim(x_val)[1], dim(x_val)[2], 1))
  print(7)
  return(list(train = list(x = x_train, y = y_train, features = features_train),
              # validation_data = list(x = x_val, y = y_val, features = features_val),
              test = list(x = x_test, y = y_test, features = features_test), adj = adj))
  
}

sets <- create_sets(x, y, features, 2019, 3)

## for given DK_Salary what is expected DK_points (include more features, examples below)
keras::k_clear_session()

main_input <- layer_input(batch_shape = c(1, dim(sets$train$x)[-1]), name = "main_input")

lstm <- 
  main_input %>%
  layer_lstm(units = 30, activity_regularizer = regularizer_l2(), return_sequences = T, stateful = T) %>%
  #layer_lstm(units = 4, activity_regularizer = regularizer_l2(), return_sequences = T) %>% ## lstm return_sequences = T
  layer_lstm(units = 30, activity_regularizer = regularizer_l2(), stateful = T)#, activity_regularizer = regularizer_l2(l = 0.01)) ## lstm return_sequences = T


aux_output <- ## for training lstm smoothly
  lstm %>%
  layer_dense(units = 1, name = "aux_output", activation = 'linear')

features_input <- layer_input(batch_shape = c(1, dim(sets$train$features)[2]), name = "features_input")

features_k <-
  features_input %>%
  layer_dense(units = 32, activation = 'elu', batch_size = 1) %>%
  layer_dropout(rate = 0.2)# lot of input features
  #layer_activity_regularization(l2 = 0.01) 

features_output <-
  features_k %>%
  layer_dense(units = 1, name = "features_output", activation = 'linear')

main_output <- 
  layer_concatenate(c(lstm, features_k)) %>% ## concat
  layer_dense(units = 32, activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  layer_dropout(rate = 0.2) %>%
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

df_batches <- count(x, year, wk) %>% select(year, n)
df_batches <- df_batches[-dim(df_batches)[1], ]
years <- df_batches %>% pull(year) %>% unique()

for (e in 1:1000) {

  prev <- 0
  
  for (yr in (years)) {
    
    i <- 0
    j <- 0
    
    if (!yr == prev) {
      
      reset_states(model)
      cat("\nresetting_states\n")
      
    }
    
    batches <- df_batches %>% filter(year == yr) %>% pull(n)
    prev <- yr
    
    for (batch in batches) { 
      
      i <- sum(i, batch)
        
      x_train <- sets$train$x[j:i, , , drop=F]
      x_features <- sets$train$features[j:i, , drop=F]
      y_train <- sets$train$y[j:i, drop=F]
      
      cat(j, ", ", i, "\n")
      
      j <- i+1
      
      
      history <- fit(model,
                       x = list(x_train, x_features),
                       y = list(y_train, y_train, y_train),
                       validation_split = 0.1,
                       # validation_data = list(list(sets$validation_data$x, sets$validation_data$features),
                       #                        list(sets$validation_data$y, sets$validation_data$y, sets$validation_data$y)),
                       epochs = 1,
                       # view_metrics = TRUE,
                       batch_size = 1,
                       callback = list(
                         callback_model_checkpoint('./checkpoints/model.hdf5'))
      ) # add callback for saving model (in case it takes forever)
  
      # history <- c(history, history_i)
      
      cat("\n", e, "---", yr, "---", batch, "---", dim(x_train), "---", dim(x_features), "---", dim(y_train), "\n")
        
    }
    
  }
  
}
      
reset_states(model)

model <- load_model_hdf5("./checkpoints/model.hdf5")

# history_df <- 
#   rbind(
#     data.frame(data = "training", 
#                epochs = 1:length(history$metrics$val_main_output_loss), main_output_loss = history$metrics$main_output_loss),
#     data.frame(data = "validation", 
#                epochs = 1:length(history$metrics$val_main_output_loss), main_output_loss = history$metrics$val_main_output_loss))
# 
# ggplot(data = history_df, aes(x = epochs, y = main_output_loss, color = data)) + 
#   geom_point() + geom_line()

p <- model %>% predict(list(sets$test$x, sets$test$features), batch_size = 1)
p <- p[[1]]

p <- exp(p) - sets$adj


wk13_preds <- 
  score %>% 
  mutate(player = str_replace(player_key, "NA ", "")) %>%
  select(player, dk_salary, pos, team) %>%
  # filter(player_key %in% wk13_players) %>% 
  mutate(# salary = sets$test$features[,"salary"]*scaled_dat$sd_salary+scaled_dat$mn_salary, 
         projected_points = as.vector(p*scaled_dat$sd_points+scaled_dat$mn_points),
         position = unlist(lapply(str_split(player, "_"), function(data) data[[2]]))) %>%
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
wk13_preds <- read_csv("./data/preds.csv")
source("./draftkings_optimizeR.R")

a <- 
  find_teams(cap = 50000, train = wk13_preds %>% 
               filter((dk_salary.x > 2500 & pos.x == "TE") |
                      (dk_salary.x > 4700 & pos.x == "QB") |
                      (dk_salary.x > 3000 & pos.x == "WR") |
                      (dk_salary.x > 3000 & pos.x == "RB") |
                      (dk_salary.x > 0 & pos.x == "Def")) %>%
               filter(!player == "AntonioBrown_WR", !player == "DonteMoncrief_WR")) %>%
  select(player, salary, projected_points, dk_points_raw, team.x, h_a, oppt, 
         TeamSalary, TotalPoints, ActualPoints)

View(a)
print(a)
cat(a)

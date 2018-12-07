library(keras)
library(tidyverse)
library(lubridate)
library(abind)
library(data.table)

setwd("/home/david/draftkings")

team_city <- 
  read_csv("./data/teams.csv") %>% 
  select(Team, City) %>% 
  mutate(Name = paste0(unlist(lapply(str_split(City, ", "), function(data) data[[1]])), " Defense"))

score <- 
  read_csv("./data/DKSalaries12_4.csv") %>% 
  janitor::clean_names() %>% 
  mutate(week = 13, year = 2018) %>% 
  rename(player_key = name_id) %>%
  mutate(name = sapply(name, 
                       function(data) {
                         ifelse(data %in% team_city$Team,
                         pull(filter(team_city, data==Team), Name),
                         data)}))

# agrepl("Todd Gurley", "Todd Gurley II") fuzzy matching to fix this..
Name <- function(player_name) {
  
  lgical <- !grepl("Defense", player_name)
  p_list <- str_split(player_name, ", ")
  
  ifelse(
    lgical,
    
    lapply(p_list, function(data) paste(data[2], data[1])) %>% unlist(),
    
    player_name
  )
}

dat <- 
  read_csv("./data/all_games.csv") %>% 
  janitor::clean_names() %>% 
  filter(complete.cases(.), dk_salary > 0) %>%
  mutate(name = Name(name), player_key = paste(name, pos, sep = "_")) %>%
  select(-gid)

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
    
    g <- str_replace(g, "tb", "tam"); tm <- str_replace(tm, "tb", "tam")
    g <- str_replace(g, "gb", "gnb"); tm <- str_replace(tm, "gb", "gnb")
    g <- str_replace(g, "jax", "jac"); tm <- str_replace(tm, "jax", "jac")
    
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
  mutate(week = 13, year = 2018, pos = str_replace(position, "DST", "Def")) %>% 
  bind_cols(team_names(score$game_info, score$team_abbrev)) %>%
  select(week, year, name, pos, team, h_a, oppt, dk_salary = salary)

score <- 
  score %>% 
  inner_join(t_df, by = c("name" = "name_match")) %>%
  select(-name) %>% rename(name = name.y) %>% 
  mutate(dk_points = NA, player_key = paste(name, pos, sep = "_")) %>%
  filter(!name == "Max McCaffrey")
  #inner_join(distinct(dat[dat$year == 2018,], name, pos, team), by = c("name.y" = "name"))

wk13_2018 <- score
wk13_2018 <- dat %>% filter(year == 2018 & week == 13)
dat <- dat %>% filter(!(year == 2018 & week >= 13))
dat <- rbind(dat, score)
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

# tmp <- dat %>% filter(name == "Brady, Tom" | name == "Bortles, Blake") ## yes I am a jags fan

create_array <- function(data) {
  t1 <- Sys.time()
  
  years <- sort(unique(data[["year"]]))
  players <- sort(unique(data[["player_key"]]))
  
  dk_salary <- NULL; dk_salary_j <- NULL
  dk_points <- NULL; dk_points_j <- NULL
  ref <- NULL
  
  for (i in 1:17) {
    ref_i <- data.frame(wk = rep(i, 17), s = seq(from = i-16, to = i))
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
        mutate(year = y, player = p)
      
      dk_salary <- rbind(dk_salary, dk_salary_j)
      dk_points <- rbind(dk_points, dk_points_j)
    }
    
  }
  
  t2 <- Sys.time()
  return(list(dk_salary = dk_salary, dk_points = dk_points, t2-t1))
}


l <- create_array(dat)
l_wk13_2018 <- create_array(wk13_2018$df)

# indices <- rowSums(l[[1]][2:18]) > 0# & rowSums(!l[[1]][2:18] == 0) > 5
# wk13_players <- l_wk13_2018[[1]] %>% filter(rowSums(l_wk13_2018[[1]][2:18]) > 0) %>% pull(player) %>% unique()

x <- l[[1]]# %>% filter(indices)
y <- 
  l[[2]] %>% # select(-wk, -year, -player) %>% filter(indices) %>% 
  pull(`17`)

features <- 
  left_join(select(x, player, wk, year), dat, by = c("player" = "player_key", "wk" = "week", "year")) %>% 
  mutate(bbi = if_else(is.na(name), 1, 0)) %>% ##bye week | benched | injured
  mutate_all(funs(replace_na(., 0)))

features <- model.matrix(data = features, dk_points_raw ~ as.factor(year) + pos + h_a + oppt + team + bbi)[, -1]
features <- as.data.table(features)

cols <- names(features)
fn_scale <- function(data) (data-mean(data))/sd(data)
features[ , (cols) := lapply(.SD, fn_scale), .SDcols = cols]
features <- as.matrix(features)

ord <- with(x, order(year, wk, player))

x <- x[ord,]
y <- y[ord]
features <- features[ord,]

# set.seed(42)
# shffl <- sample(1:nrow(x))
# 
# x <- x[shffl,]
# y <- y[shffl]
# features <- features[shffl,]

create_sets <- function(x, y, features, yr, week) { # create training and test set for sanity checking predictions (most recent week)
    
    wk_2018_indices <- with(x, year == yr & wk == week)
    
    x <- x %>% select(-wk, -year, -player) %>% as.matrix()
    
    adj <- abs(min(y)) + 0.001
    y <- log(y+adj)
    
    x_train <- x[!wk_2018_indices,]
    y_train <- y[!wk_2018_indices]
    features_train <- features[!wk_2018_indices,]
    
    x_test <- x[wk_2018_indices,]
    y_test <- y[wk_2018_indices]
    features_test <- features[wk_2018_indices,]
    
    x_train <- array_reshape(x_train, dim = c(dim(x_train)[1], dim(x_train)[2], 1))
    x_test <- array_reshape(x_test, dim = c(dim(x_test)[1], dim(x_test)[2], 1))
    
    return(list(train = list(x = x_train, y = y_train, features = features_train), 
                test = list(x = x_test, y = y_test, features = features_test), adj = adj))
    
}
    
sets <- create_sets(x, y, features, 2018, 13)


## for given DK_Salary what is expected DK_points (include more features, examples below)
keras::k_clear_session()

main_input <- layer_input(shape = dim(sets$train$x)[-1], name = "main_input")

lstm <- 
  main_input %>%
  layer_lstm(units = 17, activity_regularizer = regularizer_l2(), return_sequences = T) %>%
  # layer_lstm(units = 4, activity_regularizer = regularizer_l2(), return_sequences = T) %>% ## lstm return_sequences = T
  layer_lstm(units = 3)#, activity_regularizer = regularizer_l2(l = 0.01)) ## lstm return_sequences = T


aux_output <- ## for training lstm smoothly
  lstm %>%
  layer_dense(units = 1, name = "aux_output", activation = 'linear')

features_input <- layer_input(shape = 89, name = "features_input")

# features_k <- 
#   features_input %>%
#   layer_dense(units = 16, activation = 'elu') #%>%
#   #layer_activity_regularization(l2 = 0.01)
# 
# features_output <- 
#   features_k %>% 
#   layer_dense(units = 1, name = "features_output", activation = 'linear')

main_output <- 
  layer_concatenate(c(lstm, features_input)) %>% ## concat
  layer_dense(units = 16, activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  #layer_activity_regularization(l2 = 0.01) %>%
  # layer_dense(units = 32, input_shape = dim(features), activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  # layer_activity_regularization(l2 = 0.01) %>%
  layer_dense(units = 1, name = "main_output", activation = 'linear') ## return the expected DK_points

model <-
  keras_model(inputs = c(main_input, features_input),
              outputs = c(main_output, aux_output))

model %>% compile(
  optimizer = "adam",
  loss = "mae",
  loss_weights = c(1.0, 0.2)
)

history <- fit(model, x = list(sets$train$x, sets$train$features), y = list(sets$train$y, sets$train$y), 
               validation_split = 0.1, 
               epochs = 200,
               # view_metrics = TRUE, 
               batch_size = 2^10
               ) # add callback for saving model (in case it takes forever)

history_df <- 
  rbind(
    data.frame(data = "training", 
               epochs = 1:history$params$epochs, main_output_loss = history$metrics$main_output_loss),
    data.frame(data = "validation", 
               epochs = 1:history$params$epochs, main_output_loss = history$metrics$val_main_output_loss))

ggplot(data = history_df, aes(x = epochs, y = main_output_loss, color = data)) + 
  geom_point() + geom_line()

p <- model %>% predict(list(sets$test$x, sets$test$features))
p <- p[[1]]

p <- exp(p) - sets$adj


wk13_preds <- 
  x %>% 
  filter(year == 2018, wk == 13) %>% 
  select(1, 18, 19, 20) %>%
  # filter(player_key %in% wk13_players) %>% 
  mutate(salary = `17`*scaled_dat$sd_salary+scaled_dat$mn_salary, 
         projected_points = as.vector(p*scaled_dat$sd_points+scaled_dat$mn_points),
         position = unlist(lapply(str_split(player, "_"), function(data) data[[2]]))) %>%
  select(-`17`) %>% 
  inner_join(wk13_2018$df, by = c("player" = "player_key"))

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

find_teams(wk13_preds) %>% 
  select(player, salary, projected_points, dk_points_raw, team, h_a, oppt, 
         TeamSalary, TotalPoints, ActualPoints) %>%
  View()

library(keras)
library(tidyverse)
library(lubridate)
library(abind)
library(data.table)

setwd("/home/david/draftkings")

score <- 
  read_csv("./data/DKSalaries11_30.csv") %>% 
  janitor::clean_names() %>% 
  mutate(week = 13, year = 2018) %>% 
  rename(player_key = name_id)

# agrepl("Todd Gurley", "Todd Gurley II") fuzzy matching to fix this..
Name <- function(player_name) {
  
  p_list <- str_split(player_name, ", ")
  
  lapply(p_list, function(data) {
    paste(data[2], data[1])
  }) %>% unlist()
}

dat <- 
  read_csv("./data/all_games.csv") %>% 
  janitor::clean_names() %>% 
  filter(complete.cases(.), dk_salary > 0) %>%
  mutate(name = Name(name))

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

score

wk12_2018 <- dat %>% filter(week == 12, year == 2018)
# dat <- dat %>% filter(!week == 12 | !year == 2018)

scaled <- function(data) {

  mn_points <- mean(data[["dk_points"]])
  sd_points <- sd(data[["dk_points"]])
  mn_salary <- mean(data[["dk_salary"]])
  sd_salary <- sd(data[["dk_salary"]])
  
  data <- 
    data %>%
    mutate(dk_points_raw = dk_points,
           dk_salary_raw = dk_salary,
           dk_points = (dk_points-mn_points)/sd_points,
           dk_salary = (dk_salary-mn_salary)/sd_salary)
  return(data)
}

dat <- scaled(dat)
wk12_2018 <- scaled(wk12_2018)

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
l_wk12_2018 <- create_array(wk12_2018)

indices <- rowSums(l[[1]][2:18]) > 0# & rowSums(!l[[1]][2:18] == 0) > 5
x <- l[[1]] %>% filter(indices)
y <- l[[2]] %>% select(-wk, -year, -player) %>% filter(indices) %>% pull(`17`)
features <- 
  left_join(select(x, player, wk, year), dat, by = c("player" = "player_key", "wk" = "week", "year")) %>% 
  mutate(bbi = if_else(is.na(gid), 1, 0)) %>% ##bye week | benched | injured
  mutate_all(funs(replace_na(., 0)))

features <- model.matrix(data = features, dk_points_raw ~ as.factor(year) + pos + h_a + oppt + team + bbi + wk)[, -1]
features <- apply(features, 2, function(data) (data-mean(data)/sd(data)))

ord <- with(x, order(year, player, wk))

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
    
    wk12_2018_indices <- with(x, year == yr & wk == week)
    
    x <- x %>% select(-wk, -year, -player) %>% as.matrix()
    
    adj <- abs(min(y)) + 0.001
    y <- log(y+adj)
    
    x_train <- x[!wk12_2018_indices,]
    y_train <- y[!wk12_2018_indices]
    features_train <- features[!wk12_2018_indices,]
    
    x_test <- x[wk12_2018_indices,]
    y_test <- y[wk12_2018_indices]
    features_test <- features[wk12_2018_indices,]
    
    x_train <- array_reshape(x_train, dim = c(dim(x_train)[1], dim(x_train)[2], 1))
    x_test <- array_reshape(x_test, dim = c(dim(x_test)[1], dim(x_test)[2], 1))
    
    return(list(train = list(x = x_train, y = y_train, features = features_train), 
                test = list(x = x_test, y = y_test, features = features_test), adj))
    
}
    
sets <- create_sets(x, y, features, 2018, 12)


## for given DK_Salary what is expected DK_points (include more features, examples below)
keras::k_clear_session()

main_input <- layer_input(shape = dim(sets$train$x)[-1], name = "main_input")

lstm <- 
  main_input %>%
  # layer_lstm(units = 4, activity_regularizer = regularizer_l2(), return_sequences = T) %>%
  # layer_lstm(units = 4, activity_regularizer = regularizer_l2(), return_sequences = T) %>% ## lstm return_sequences = T
  layer_lstm(units = 2, activity_regularizer = regularizer_l2(l = 0.01)) ## lstm return_sequences = T


aux_output <- ## for training lstm smoothly
  lstm %>%
  layer_dense(units = 1, name = "aux_output", activation = 'linear')

features_input <- layer_input(shape = 82, name = "features_input")

main_output <- 
  layer_concatenate(c(lstm, features_input)) %>% ## concat
  layer_dense(units = 4, input_shape = dim(features), activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
  layer_activity_regularization(l2 = 0.01) %>%
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
               epochs = 1000, 
               # view_metrics = TRUE, 
               batch_size = 2^10) # add callback for saving model (in case it takes forever)
plot(history)

p <- model %>% predict(list(sets$test$x, sets$test$features))
p <- p[[1]]

p <- exp(p) - adj

qplot((p*sd_points)+mn_points)


qplot(p*sd_points+mn_points, sets$test$y*sd_points+mn_points) + geom_smooth() + geom_abline()
qplot(sets$test$x[,17,1]*sd_salary+mn_salary, p*sd_points+mn_points) + geom_smooth()



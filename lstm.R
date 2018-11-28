library(keras)
library(tidyverse)
library(lubridate)
library(abind)
library(data.table)

setwd("/home/david/draftkings")


dat <- 
  read_csv("./data/all_games.csv") %>% 
  janitor::clean_names() %>% 
  filter(complete.cases(.), dk_salary > 0) %>%
  mutate(player_key = paste(pos, gid, name, sep = "_"))

mn_points <- mean(dat$dk_points)
sd_points <- sd(dat$dk_points)
mn_salary <- mean(dat$dk_salary)
sd_salary <- sd(dat$dk_salary)

dat <- 
  dat %>% 
  mutate(dk_points_raw = dk_points,
         dk_salary_raw = dk_salary,
         dk_points = (dk_points-mn_points)/sd_points,
         dk_salary = (dk_salary-mn_salary)/sd_salary)

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

indices <- rowSums(l[[1]][2:18]) > 0 & rowSums(!l[[1]][2:18] == 0) > 5
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

x <- x %>% select(-wk, -year, -player) %>% as.matrix()

# set.seed(42)
# shffl <- sample(1:nrow(x))
# 
# x <- x[shffl,]
# y <- y[shffl]
# features <- features[shffl,]

x <- array_reshape(x, dim = c(dim(x)[1], dim(x)[2], 1))


## for given DK_Salary what is expected DK_points (include more features, examples below)
keras::k_clear_session()

main_input <- layer_input(shape = dim(x)[-1], name = "main_input")

lstm <- 
  main_input %>%
  layer_lstm(units = 32, activity_regularizer = regularizer_l2(), return_sequences = T) %>%
  # layer_lstm(units = 32, activity_regularizer = regularizer_l2(), return_sequences = T) %>% ## lstm return_sequences = T
  layer_lstm(units = 32, activity_regularizer = regularizer_l2()) ## lstm return_sequences = T


aux_output <- ## for training lstm smoothly
  lstm %>%
  layer_dense(units = 1, name = "aux_output", activation = 'linear')

features_input <- layer_input(shape = 81, name = "features_input")

main_output <- 
  layer_concatenate(c(lstm, features_input)) %>% ## concat
  layer_dense(units = 16, input_shape = dim(features), activation = 'elu') %>%  ## add in home/away, onehot vector for player, onehot vector for team, onehot vector for opponent team
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

adj <- abs(min(y)) + 0.001
log_y <- log(y+adj)

history <- fit(model, x = list(x, features), y = list(y, y), 
               validation_split = 0.1, 
               epochs = 1000, 
               # view_metrics = TRUE, 
               batch_size = 2^10) # add callback for saving model (in case it takes forever)
plot(history)

p <- model %>% predict(list(x, features))
p <- p[[1]]

p <- exp(p) - adj

qplot((p*sd_points)+mn_points)

points2018 <- l[[2]] %>% filter(rowSums(l[[1]][2:18]) > 0) %>% mutate(pred = (p*sd_points)+mn_points) %>% filter(year == 2018, wk == 10)
salary2018 <- l[[1]] %>% filter(rowSums(l[[1]][2:18]) > 0) %>% mutate(pred = (p*sd_points)+mn_points) %>% filter(year == 2018, wk == 10)

points2018 <- points2018 %>% mutate_at(vars(2:18), funs(.*sd_points+mn_points))
salary2018 <- salary2018 %>% mutate_at(vars(2:18), funs(.*sd_salary+mn_salary))

qplot(points2018$pred, points2018$`17`)
qplot(salary2018$`17`, points2018$pred)
qplot(salary2018$`17`, points2018$`17`)


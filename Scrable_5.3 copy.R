setwd("~/Desktop/R/Kaggle/2 Scrabble/scrabble-player-rating")
rm(list=ls())

### Load libraries ###

library(ggplot2)
library(dplyr)
library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyr)
library(corrplot)


### Load Data ###

train = read.csv("train.csv")
attach(train)
test = read.csv("test.csv")
games = read.csv("games.csv")
turns = read.csv("turns.csv")

### Clean train ###
games = subset(games,select = -c(winner))

#Combine rows with same game ID
cleaned_train = train
clean_train2 = cleaned_train %>% 
  group_by(game_id) %>% 
  summarise(nickname = list(nickname), 
            score = list(as.character(score)),
            rating = list(as.character(rating))) %>% 
  data.frame()

# Seperate columns for score nickname and ratings
dat = separate(clean_train2, nickname, c("nickname1", "nickname2"), sep = ",")
dat = separate(dat, score, c("score1", "score2"), sep = ",")
dat = separate(dat, rating, c("rating1", "rating2"), sep = ",")


# Get rid of list formatting
dat$nickname1 <- gsub("c(\"","",as.character(dat$nickname1), fixed = TRUE)
dat$nickname1 <- gsub("\"","",as.character(dat$nickname1))
dat$nickname1 <- gsub(" ","",as.character(dat$nickname1))


dat$nickname2 <- gsub(")","",as.character(dat$nickname2), fixed = TRUE)
dat$nickname2 <- gsub("\"","",as.character(dat$nickname2))
dat$nickname2 <- gsub(" ","",as.character(dat$nickname2))

dat$score1 <- gsub("c","",as.character(dat$score1))
dat$score1 <- gsub("\"","",as.character(dat$score1))
dat$score1 <- gsub("(","",as.character(dat$score1), fixed = TRUE)

dat$score2 <- gsub("c","",as.character(dat$score2))
dat$score2 <- gsub("\"","",as.character(dat$score2))
dat$score2 <- gsub(")","",as.character(dat$score2), fixed = TRUE)

dat$rating1 <- gsub("c","",as.character(dat$rating1))
dat$rating1 <- gsub("\"","",as.character(dat$rating1))
dat$rating1 <- gsub("(","",as.character(dat$rating1), fixed = TRUE)

dat$rating2  <- gsub("c","",as.character(dat$rating2))
dat$rating2 <- gsub("\"","",as.character(dat$rating2))
dat$rating2 <- gsub(")","",as.character(dat$rating2), fixed = TRUE)


# Renaming
dat = dat %>% 
  rename(
    Player.Nickname = nickname1,
    Opponent.Nickname = nickname2 , 
    Player.Score = score1,
    Opponent.Score= score2 ,
    Player.Rating = rating1 ,
    Opponent.Rating = rating2
  )





# Making numbers into numeric again 
dat$Player.Score = as.numeric(dat$Player.Score)
dat$Opponent.Score = as.numeric(dat$Opponent.Score)
dat$Player.Rating = as.numeric(dat$Player.Rating)
dat$Opponent.Rating = as.numeric(dat$Opponent.Rating)

# Difference in scores




# Relationship between nickname (player) and rating
# Is there a better way to do this perhaps using dplyr 



#plot(rating ~ difference_score,  data= dat)

#trying to get bots as index 
swap_ind = ifelse(dat$Player.Nickname=="STEEBot"|dat$Player.Nickname =="BetterBot" 
       | dat$Player.Nickname=="HastyBot", NA, 1)
first_number = ifelse(is.na(swap_ind),1, 0)
dat = cbind(dat,
            first_number)
swap_ind = which(is.na(swap_ind))



dat_copy = data.frame(dat)
dat_copy[swap_ind,]$Player.Rating = dat[swap_ind,]$Opponent.Rating
dat_copy[swap_ind,]$Opponent.Rating = dat[swap_ind,]$Player.Rating
dat_copy[swap_ind,]$Player.Nickname = dat[swap_ind,]$Opponent.Nickname
dat_copy[swap_ind,]$Opponent.Nickname= dat[swap_ind,]$Player.Nickname
dat_copy[swap_ind,]$Player.Score= dat[swap_ind,]$Opponent.Score
dat_copy[swap_ind,]$Opponent.Score= dat[swap_ind,]$Player.Score




dat = dat_copy
dat$difference_score = dat$Player.Score - dat$Opponent.Score 
#Get winner 
winner = ifelse(dat$difference_score>0 , 1,0)
winner = ifelse(dat$difference_score<0 , winner-1,winner)
dat = cbind(dat,winner)
any(is.na(dat$Opponent.Rating))


#NEW



#NEW END
occurences = as.data.frame(table(dat$Player.Nickname))
dat = dat[!(dat$Player.Nickname=="BB-8"| dat$Player.Nickname=="pandorable" |
              dat$Player.Nickname=="Goldenlamb"  | dat$Player.Nickname=="stevy"),]


#FORGETTIGN ABOUT DRAWS FOR NOW 

unique_nicknames = unique(dat$Player.Nickname)
name_df = data.frame(nickname = unique_nicknames)


#Turns
turns = turns[!(turns$nickname=="STEEBot"|turns$nickname =="BetterBot" 
                | turns$nickname=="HastyBot"),]
average_score = aggregate( turns$points ~ turns$game_id, FUN = mean )
colnames(average_score) = c("game_id", "points_per_turn")
dat = left_join(dat, average_score)
# 
# linear
plot(dat$Player.Rating~dat$points_per_turn)





dat = left_join(dat, games)
set.seed(20)



#ind =sample(1:nrow(dat), floor(0.7*nrow(dat)))
#train1 = dat[ind,]
#val1 = dat[-ind,]

unique_nicknames = unique(dat$Player.Nickname)
ind =sample((unique_nicknames), floor(0.7*length(unique_nicknames)))




dat = dat %>% arrange(Player.Nickname,created_at)
dat_behind = dat[1:nrow(dat)-1,]
dat_behind = rbind(dat[1,],dat_behind)
dat_behind = dat_behind %>% 
  rename(
    game_id1 = game_id ,
    Player.Nickname1 =  Player.Nickname  , 
    Opponent.Nickname1 = Opponent.Nickname,
    Player.Score1 = Player.Score ,
    Opponent.Score1 = Opponent.Score,
    Player.Rating1 = Player.Rating ,
    Opponent.Rating1 =  Opponent.Rating   , 
     first_number1 = first_number   ,
    difference_score1 = difference_score,
    winner1 = winner,
    points_per_turn1 = points_per_turn,
    first1 = first,
    time_control_name1  =   time_control_name,
    game_end_reason1 = game_end_reason,
    created_at1  =created_at,
    lexicon1  =lexicon,
    initial_time_seconds1 =initial_time_seconds,
    increment_seconds1   = increment_seconds,
    rating_mode1 =rating_mode,
    max_overtime_minutes1 =max_overtime_minutes,
    game_duration_seconds1 =game_duration_seconds
  )



dat_behind2 = dat[1:(nrow(dat)-2),]
dat_behind2 = rbind(dat[1:2,],dat_behind2)
dat_behind2 = dat_behind2 %>% 
  rename(
    game_id2 = game_id ,
    Player.Nickname2 =  Player.Nickname  , 
    Opponent.Nickname2 = Opponent.Nickname,
    Player.Score2 = Player.Score ,
    Opponent.Score2 = Opponent.Score,
    Player.Rating2 = Player.Rating ,
    Opponent.Rating2 =  Opponent.Rating   , 
    first_number2 = first_number   ,
    difference_score2 = difference_score,
    winner2 = winner,
    points_per_turn2 = points_per_turn,
    first2 = first,
    time_control_name2  =   time_control_name,
    game_end_reason2 = game_end_reason,
    created_at2  =created_at,
    lexicon2  =lexicon,
    initial_time_seconds2 =initial_time_seconds,
    increment_seconds2   = increment_seconds,
    rating_mode2 =rating_mode,
    max_overtime_minutes2 =max_overtime_minutes,
    game_duration_seconds2 =game_duration_seconds
  )


dat = cbind(dat, dat_behind)
dat = cbind(dat, dat_behind2)

dat_original = dat
dat1 = dat
dat1 = subset(dat1, select = -c(rating_mode, lexicon,time_control_name,Opponent.Nickname,
                               game_id, first, game_end_reason, created_at
                               ))
dat1 = subset(dat1, select = -c(rating_mode1, lexicon1,time_control_name1,
                                Opponent.Nickname1,
                                game_id1, first1, game_end_reason1, Player.Nickname1,
                                created_at1, Player.Rating1))

dat1 = subset(dat1, select = -c(rating_mode2, lexicon2,time_control_name2,Opponent.Nickname2,
                                game_id2, first2, game_end_reason2, Player.Nickname2,
                                created_at2, Player.Rating2))

dat2 = model.matrix( ~ rating_mode + lexicon+time_control_name + Opponent.Nickname 
                     + game_end_reason 
                     + rating_mode1 + lexicon1 +time_control_name1 + Opponent.Nickname1 
                     + game_end_reason1
                     + rating_mode2 + lexicon2 +time_control_name2 + Opponent.Nickname2
                     + game_end_reason2
                     - 1, dat)       # added game end reason

dat = cbind(dat1, dat2)



train1 = dat[dat[,1] %in% ind,]
val1 = dat[!(dat[,1] %in% ind),]

dat =  subset(dat, select = -c(Player.Nickname ))
train1 =  subset(train1, select = -c(Player.Nickname))
val1 =  subset(val1, select = -c(Player.Nickname))

y_train <- as.integer(train1$Player.Rating) - 1
y_test <- as.integer(val1$Player.Rating) - 1
X_train <- train1 %>% select(-Player.Rating)
X_test <- val1 %>% select(-Player.Rating)

y_dat = as.integer(dat$Player.Rating) - 1
x_dat = dat %>% select(-Player.Rating)
xgb_dat<- xgb.DMatrix(data = as.matrix(x_dat), label = y_dat)

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params_default <- list(
  booster = "gbtree",
 # eta = 0.1, #made this higher to speed up 
 # max_depth = 5,
 # gamma = 2,
 # subsample = 1, #was 0.9
 # colsample_bytree = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse"
  #lambda = 250
)

xgbcv <- xgb.cv( params = xgb_params_default, data = xgb_train, nrounds = 200, nfold = 5,
                 showsd = T, stratified = T, print.every.n = 10,
                 early.stop.round = 20, maximize = F)


xgb_params <- list(
  booster = "gbtree",
   eta = 0.1, #made this higher to speed up 
   max_depth = 6,
   gamma = 1,
   subsample = 1, #was 0.9
   colsample_bytree = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  lambda = 100
)




xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 90,
  verbose =1
)

xgb_model
xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
xgb_preds_full <- predict(xgb_model, as.matrix(x_dat), reshape = TRUE)
xgb_preds_full <- as.data.frame(xgb_preds_full)
dat_pred = cbind(dat_original,xgb_preds_full)


sqrt(mean((unlist(val1$Player.Rating-xgb_preds))^2)) 
#current is 149.64
#139???
#134

# 111???
xgbcv <- xgb.cv( params = xgb_params, data = xgb_train, 
                 nrounds = 100, nfold = 5, showsd = T, stratified = T, 
                 print.every.n = 10, early.stop.round = 20, maximize = F)
#328 9s the best



xgb1 <- xgb.train (params =  xgb_params, 
                   data = xgb_train, 
                   nrounds = 90,
                   watchlist = list(val=xgb_test,train=xgb_train), print.every.n = 10, 
                   early.stop.round = 10, maximize = F
                  )

xgb_preds_full2 <- predict(xgb1, as.matrix(X_test), reshape = TRUE)
xgb_preds_full2 <- as.data.frame(xgb_preds_full2)
dat_pred2 = cbind(dat_original,xgb_preds_full2)

sqrt(mean((unlist(val1$Player.Rating-xgb_preds_full2))^2))

mat <- xgb.importance (feature_names = colnames(X_train),model = xgb_model)
xgb.plot.importance (importance_matrix = mat[1:70]) 


dat_player = dat_pred[dat_pred$Player.Nickname==unique_nicknames[3],]
ggplot(dat_player, aes(x = created_at)) +
  geom_point(aes(x = created_at, y=Player.Rating, color = factor(winner)))

test <- chisq.test(X_train$lexicon1NSWL20, y_train)
print(test)


# messing around with graphs
do_graphs = FALSE
if (do_graphs == TRUE){
quartz()
for (i in ind){

  dat_player = dat_pred[dat_pred$Player.Nickname==i,]
  title_p = i
  p <- ggplot(dat_player, aes(x = created_at)) +
    geom_point(aes(y=Player.Rating, color = "actual"))+
    geom_point(aes(y=xgb_preds_full, color = "prediction"))+
    geom_hline(yintercept = ave(dat_player$Player.Rating))+
    geom_hline(yintercept = ave(dat_player$xgb_preds_full))+
   labs(title = title_p)+
    ylim(1000, 2500) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  print(p)
  Sys.sleep(2)
}
}


# time series 


### Formatting test data ###
head(test)
clean_test = test %>% 
  group_by(game_id) %>% 
  summarise(nickname = list(nickname), 
            score = list(as.character(score)),
            rating = list(as.character(rating))) %>% 
  data.frame()


dat_test = separate(clean_test, nickname, c("nickname1", "nickname2"), sep = ",")


dat_test = separate(dat_test, score, c("score1", "score2"), sep = ",")
dat_test = separate(dat_test, rating, c("rating1", "rating2"), sep = ",")


dat_test$nickname1 <- gsub("c(\"","",as.character(dat_test$nickname1), fixed = TRUE)
dat_test$nickname1 <- gsub("\"","",as.character(dat_test$nickname1))
dat_test$nickname1 <- gsub(" ","",as.character(dat_test$nickname1))


dat_test$nickname2 <- gsub(")","",as.character(dat_test$nickname2), fixed = TRUE)
dat_test$nickname2 <- gsub("\"","",as.character(dat_test$nickname2))
dat_test$nickname2 <- gsub(" ","",as.character(dat_test$nickname2))

dat_test$score1 <- gsub("c","",as.character(dat_test$score1))
dat_test$score1 <- gsub("\"","",as.character(dat_test$score1))
dat_test$score1 <- gsub("(","",as.character(dat_test$score1), fixed = TRUE)

dat_test$score2 <- gsub("c","",as.character(dat_test$score2))
dat_test$score2 <- gsub("\"","",as.character(dat_test$score2))
dat_test$score2 <- gsub(")","",as.character(dat_test$score2), fixed = TRUE)

dat_test$rating1 <- gsub("c","",as.character(dat_test$rating1))
dat_test$rating1 <- gsub("\"","",as.character(dat_test$rating1))
dat_test$rating1 <- gsub("(","",as.character(dat_test$rating1), fixed = TRUE)

dat_test$rating2  <- gsub("c","",as.character(dat_test$rating2))
dat_test$rating2 <- gsub("\"","",as.character(dat_test$rating2))
dat_test$rating2 <- gsub(")","",as.character(dat_test$rating2), fixed = TRUE)

dat_test = dat_test %>% 
  rename(
    Player.Nickname = nickname1,
    Opponent.Nickname = nickname2 , 
    Player.Score = score1,
    Opponent.Score= score2 ,
    Player.Rating = rating1 ,
    Opponent.Rating = rating2
  )

dat_test$Player.Score = as.numeric(dat_test$Player.Score)
dat_test$Opponent.Score = as.numeric(dat_test$Opponent.Score)
dat_test$Player.Rating = as.numeric(dat_test$Player.Rating)
dat_test$Opponent.Rating = as.numeric(dat_test$Opponent.Rating)
#ind_right = which(is.na(dat_test$Player.Rating))
#ind_wrong = which(is.na(dat_test$Opponent.Rating))

dat_test$Player.Score = as.character(dat_test$Player.Score)
dat_test$Opponent.Score = as.character(dat_test$Opponent.Score)
dat_test$Player.Rating = as.character(dat_test$Player.Rating)
dat_test$Opponent.Rating = as.character(dat_test$Opponent.Rating)
swap_ind_test = ifelse(dat_test$Player.Nickname=="STEEBot"|dat_test$Player.Nickname =="BetterBot" 
                       | dat_test$Player.Nickname=="HastyBot", 0, 1)
first_number = ifelse(swap_ind_test==1,0, 1)
swap_ind_test = ifelse(dat_test$Player.Nickname=="STEEBot"|dat_test$Player.Nickname =="BetterBot" 
                  | dat_test$Player.Nickname=="HastyBot", NA, 1)

swap_ind_test = which(is.na(swap_ind_test))



dat_test = cbind(dat_test,
            first_number)

dat_test_copy = data.frame(dat_test)

dat_test_copy[swap_ind_test,]$Player.Rating = dat_test[swap_ind_test,]$Opponent.Rating
dat_test_copy[swap_ind_test,]$Opponent.Rating = dat_test[swap_ind_test,]$Player.Rating
dat_test_copy[swap_ind_test,]$Player.Nickname = dat_test[swap_ind_test,]$Opponent.Nickname
dat_test_copy[swap_ind_test,]$Opponent.Nickname= dat_test[swap_ind_test,]$Player.Nickname
dat_test_copy[swap_ind_test,]$Player.Score= dat_test[swap_ind_test,]$Opponent.Score
dat_test_copy[swap_ind_test,]$Opponent.Score= dat_test[swap_ind_test,]$Player.Score

dat_test = dat_test_copy
dat_test$Player.Score = as.numeric(dat_test$Player.Score)
dat_test$Opponent.Score = as.numeric(dat_test$Opponent.Score)
dat_test$Player.Rating = as.numeric(dat_test$Player.Rating)
dat_test$Opponent.Rating = as.numeric(dat_test$Opponent.Rating)

head(dat_test)

dat_test$difference_score = dat_test$Player.Score - dat_test$Opponent.Score 


winner = ifelse(dat_test$difference_score>0 , 1,0)
winner = ifelse(dat_test$difference_score<0 , winner-1,winner)
dat_test = cbind(dat_test,winner)







#END CLEANING
#Plots
#NOT ADDING UP WITH NAMES #somehow got swapped with training datas
#change to full join to diagnoise problem
#final model


head(dat_test)
winner = ifelse(dat_test$difference_score>0 , 1,0)
winner = ifelse(dat_test$difference_score<0 , winner-1,winner)

dat_test = left_join(dat_test, average_score)
dat_test = left_join(dat_test, games)




#dat_test = subset(dat_test, select = -c(game_id, Player.Nickname, Opponent.Nickname, first,
                             # time_control_name, game_end_reason, created_at, lexicon,
                             # rating_mode))
#y_dat_test <- as.integer(dat_test$Player.Rating) - 1
#dat_test <- dat_test%>% select(-Player.Rating)


dat_test = dat_test %>% arrange(Player.Nickname,created_at)
dat_test_keep = dat_test
dat_behind_test = dat_test[1:nrow(dat_test)-1,]
dat_behind_test = rbind(dat_test[1,],dat_behind_test)
dat_behind_test = dat_behind_test %>% 
  rename(
    game_id1 = game_id ,
    Player.Nickname1 =  Player.Nickname  , 
    Opponent.Nickname1 = Opponent.Nickname,
    Player.Score1 = Player.Score ,
    Opponent.Score1 = Opponent.Score,
    Player.Rating1 = Player.Rating ,
    Opponent.Rating1 =  Opponent.Rating   , 
    first_number1 = first_number   ,
    difference_score1 = difference_score,
    winner1 = winner,
    points_per_turn1 = points_per_turn,
    first1 = first,
    time_control_name1  =   time_control_name,
    game_end_reason1 = game_end_reason,
    created_at1  =created_at,
    lexicon1  =lexicon,
    initial_time_seconds1 =initial_time_seconds,
    increment_seconds1   = increment_seconds,
    rating_mode1 =rating_mode,
    max_overtime_minutes1 =max_overtime_minutes,
    game_duration_seconds1 =game_duration_seconds
  )


dat_test = cbind(dat_test, dat_behind_test)

dat_test1 = subset(dat_test, select = -c(rating_mode, lexicon,time_control_name,Opponent.Nickname,
                                         game_id, first, game_end_reason,
                                         created_at, 
                                         rating_mode1, lexicon1,time_control_name1,Opponent.Nickname1,
                                         game_id1, first1, game_end_reason1, Player.Nickname1,
                                         created_at1, Player.Rating1))


dat_test2 = model.matrix( ~ rating_mode + lexicon+time_control_name + Opponent.Nickname 
                          + game_end_reason 
                          + rating_mode1 + lexicon1 +time_control_name1 + Opponent.Nickname1 
                          + game_end_reason1
                          - 1, dat_test)       # added game end reason

                    


dat_test = cbind(dat_test1, dat_test2)
dat_test = dat_test %>% select(-c(Player.Rating, Player.Nickname))
lexiconNSWL20 = data.frame(lexiconNSWL20 = 0 )
dat_test = cbind(dat_test,lexiconNSWL20 )
dat_test <- dat_test %>% relocate(lexiconNSWL20, .before = lexiconNWL20)
lexicon1NSWL20 = data.frame(lexicon1NSWL20 = 0 )
dat_test = cbind(dat_test,lexicon1NSWL20 )
dat_test <- dat_test %>% relocate(lexicon1NSWL20, .before = lexicon1NWL20)
# this does not working
xgb_params <- list(
  booster = "gbtree",
  eta = 0.1, #made this higher to speed up 
  max_depth = 5,
  gamma = 1,
  subsample = 1, #was 0.9
  colsample_bytree = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  lambda = 100
)





xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_dat,
  nrounds = 100,
  verbose = 1
)





xgb_preds2 <- predict(xgb_model, as.matrix(dat_test), reshape = TRUE)
xgb_preds2 <- as.data.frame(xgb_preds2)
output = data.frame(game_id = dat_test_keep$game_id, rating = xgb_preds2$xgb_preds2)

write.csv(output, file = 'boast_5.3.prev.game.6.csv', row.names = F)

#to do 
# add points per turn for second part

View(test)
View(turns)
View(dat_test)
#plot(dat_test$Player.Score~dat_test$points_per_turn)
#plot(dat$Player.Score~dat$points_per_turn)
# so the link is fine



#IDEAS
# win ratio
# win ratio to that point
# ave score per turn 
# winner is from last game

#IDEAS TO UNMESS THIS
# maybe it has to do with the lexicon?


ggplot(data = dat_original) + 
  geom_point(mapping = aes(x = Player.Rating, y = Player.Score), position = "jitter") + 
  facet_grid(rating_mode~ winner) +
  coord_polar()

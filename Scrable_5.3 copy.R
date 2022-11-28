setwd("~/Desktop/Kaggle/2 Scrabble/scrabble-player-rating")
rm(list=ls())
train = read.csv("train.csv")
test = read.csv("test.csv")
games = read.csv("games.csv")
attach(train)
library(ggplot2)
library(dplyr)library(xgboost)
library(caTools)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(tidyr)
library(corrplot)
# Clean train
#Combine rows with same game ID
games = subset(games,select = -c(winner))
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


# renaming
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


#Plots



dat = left_join(dat, games)
set.seed(20)



#ind =sample(1:nrow(dat), floor(0.7*nrow(dat)))
#train1 = dat[ind,]
#val1 = dat[-ind,]

unique_nicknames = unique(dat$Player.Nickname)
ind =sample((unique_nicknames), floor(0.7*length(unique_nicknames)))


#Tree based

#154

#stevy is a big problem 

#145




dat_original = dat
dat1 = dat
dat1 = subset(dat, select = -c(rating_mode, lexicon,time_control_name,Opponent.Nickname,
                               game_id, first, game_end_reason,
                               created_at))


dat2 = model.matrix( ~ rating_mode + lexicon+time_control_name + Opponent.Nickname 
                     + game_end_reason- 1, dat)       # added game end reason

dat = cbind(dat1, dat2)



train1 = dat[dat[,1] %in% ind,]
val1 = dat[!(dat[,1] %in% ind),]

dat =  subset(dat, select = -c(Player.Nickname))
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
xgb_params <- list(
  booster = "gbtree",
  eta = 0.1, #made this higher to speed up 
  max_depth = 4,
  gamma = 2,
  subsample = 1, #was 0.9
  colsample_bytree = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  lambda = 250
)



xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  verbose =1
)

xgb_model
xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
xgb_preds_full <- predict(xgb_model, as.matrix(x_dat), reshape = TRUE)
xgb_preds_full <- as.data.frame(xgb_preds_full)
dat_pred = cbind(dat_original,xgb_preds_full)

sqrt(mean((unlist(val1$Player.Rating-xgb_preds))^2))
#current is 150

xgbcv <- xgb.cv( params = xgb_params, data = xgb_train, nrounds = 500, nfold = 5,
                 showsd = T, stratified = T, print.every.n = 10,
                 early.stop.round = 20, maximize = F)
#328 9s the best


xgb1 <- xgb.train (params =  xgb_params, 
                   data = xgb_train, 
                   nrounds = 45,
                   watchlist = list(val=xgb_test,train=xgb_train), print.every.n = 10, 
                   early.stop.round = 10, maximize = F
                  )

mat <- xgb.importance (feature_names = colnames(X_train),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 



# messing around with graphs

quartz()



for (i in 1:length(unique_nicknames)){

  dat_player = dat_pred[dat_pred$Player.Nickname==unique_nicknames[i],]
  title_p = dat_player$Player.Nickname[i]
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



# time series 






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
first_number = ifelse(swap_ind_test==1,1, 0)
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
###

winner = ifelse(dat_test$difference_score>0 , 1,0)
winner = ifelse(dat_test$difference_score<0 , winner-1,winner)
dat_test = cbind(dat_test,winner)
#d_ave_opp_rating  = tapply(draw_df$Opponent.Rating, draw_df$Player.Nickname, mean, simplify = FALSE)
#d_ave_opp_rating  = data.frame(draw_opponent_rating = d_ave_opp_rating, names = dimnames(d_ave_opp_rating))
#d_ave_player_rating = tapply(draw_df$Player.Rating, draw_df$Player.Nickname, mean, simplify = FALSE)
#df3 = cbind(d_ave_opp_rating ,d_ave_player_rating)


#dat_ave = cbind(w_ave_opp_rating,w_ave_player_rating,l_ave_opp_rating, l_ave_player_rating)
# need to drop some columns

colnames(df1)[2] = "Names"
colnames(df2)[2] = "Names"
#colnames(df3)[2] = "Names"
colnames(ave_player_rating)[2] = "Names"

join1 = full_join(df1,df2)
#join2 = full_join(join1, df3)
join3 = as.data.frame(full_join(join1, ave_player_rating))



#trail for difference in rating as base

join3[,1] = unlist(as.character(join3[,1]))
join3[,2] = unlist(as.character(join3[,2]))
join3[,3] = unlist(as.character(join3[,3]))
join3[,4] = unlist(as.character(join3[,4]))
join3[,5] = unlist(as.character(join3[,5]))
join3[,6] = unlist(as.character(join3[,6]))
join3[,7] = unlist(as.character(join3[,7]))
join3[,8] = unlist(as.character(join3[,8]))

join3[,1] = as.numeric(join3[,1])
join3[,2] = as.character(join3[,2])
join3[,3] = as.numeric(join3[,3])
join3[,4] = as.numeric(join3[,4])
join3[,5] = as.numeric(join3[,5])
join3[,6] = as.numeric(join3[,6])
join3[,7] = as.numeric(join3[,7])
join3[,8] = as.numeric(join3[,8])

join3 = subset(join3, select = -c(ave_player_rating, w_ave_player_rating, l_ave_player_rating) )





#END CLEANING
#Plots
#NOT ADDING UP WITH NAMES #somehow got swapped with training datas
#change to full join to diagnoise problem
#final model


head(dat_test)
dat_test = left_join(dat_test, games)
winner = ifelse(dat_test$difference_score>0 , 1,0)
winner = ifelse(dat_test$difference_score<0 , winner-1,winner)


dat_test_keep = dat_test
#dat_test = subset(dat_test, select = -c(game_id, Player.Nickname, Opponent.Nickname, first,
                             # time_control_name, game_end_reason, created_at, lexicon,
                             # rating_mode))
#y_dat_test <- as.integer(dat_test$Player.Rating) - 1
#dat_test <- dat_test%>% select(-Player.Rating)


dat_test1 = subset(dat_test, select = -c(rating_mode, lexicon,time_control_name,Opponent.Nickname,
                               game_id, first, game_end_reason,
                               created_at))


dat_test2 = model.matrix( ~ rating_mode + lexicon+time_control_name + Opponent.Nickname 
                     + game_end_reason- 1, dat_test)       # added game end reason

dat_test = cbind(dat_test1, dat_test2)
dat_test = dat_test %>% select(-c(Player.Rating, Player.Nickname))
lexiconNSWL20 = data.frame(lexiconNSWL20 = 0 )
dat_test = cbind(dat_test,lexiconNSWL20 )
dat_test <- dat_test %>% relocate(lexiconNSWL20, .before = lexiconNWL20)
# this does not working
xgb_params <- list(
  booster = "gbtree",
  eta = 0.05, #made this higher to speed up 
  max_depth = 4,
  gamma = 2,
  subsample = 1, #was 0.9
  colsample_bytree = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse",
  lambda = 250
)



xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  verbose = 1
)




xgb_preds <- predict(xgb_model, as.matrix(dat_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
output = data.frame(game_id = dat_test_keep$game_id, rating = xgb_preds$xgb_preds)

write.csv(output, file = 'boast_5.3.csv', row.names = F)


#IDEAS
#win ratio
# win ratio to that point
# score per turn 


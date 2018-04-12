#install.packages("leaps")
#install.packages("glmnet")
#install.packages("stringr")
#install.packages("stringi")
library(leaps) # for regsubsets
library(glmnet) # for LASSO, ridge regression, mtcars data
library(stringr)
library(stringi)
set.seed(1)
#delete missing value 
#1729
setwd("~/Desktop/BA Project")
match_data = read.csv("Match_v3.csv")
match_data = match_data[,-1] #delete id
match_data = match_data[,-1] #
match_data = match_data[,-1] #

match_data = na.omit(match_data)
################################ find red card number of each team ################################# 
match_data["redCard_Home"] <- 0
match_data["redCard_Away"] <- 0
match_data["redCard_Total"] <- 0
nrows = 1375
for (i in 1:nrows){
  match_data$redCard_Total[i] = str_count(match_data$card[i],"<rcards>")
  tmp_total_redcards = str_count(match_data$card[i],"<rcards>")
  tmp_home = 0
  tmp_away = 0
  tmp_str = match_data$card[i]
  if(tmp_total_redcards > 0) {
    for( j in 1:tmp_total_redcards){
      t1 = regmatches(tmp_str, regexpr('<rcards>.+</team>', tmp_str))  #t1 = get string starting from <rcards> to  the last "</team>"
      t2 = gsub("^.*?<team>","",t1)  # cut string from t1 , get the string after the first "<team>" 
      team_num = gsub("</team>.*","",t2)  # cut the team number ,from the start unti the first "</team>"
      t3 = gsub("^.*?</team>","</team>",t2)   #get string from t2, start point = </team>
      tmp_str = regmatches(t3, regexpr('<rcards>.+</team>', t3)) # get new string that start from the next <rcards>
      if(match_data$home_team_api_id[i] == team_num){
        tmp_home = tmp_home + 1
      }
      if(match_data$away_team_api_id[i] == team_num){
        tmp_away = tmp_away + 1
      }
    }
    match_data$redCard_Home[i] = tmp_home
    match_data$redCard_Away[i] = tmp_away
  }
}
#######################################shotontarget########################################################

match_data["shotOn_Home"] <- 0
match_data["shotOn_Away"] <- 0
for (i in 1:nrows){
  #match_data$redCard_Total[i] = str_count(match_data$card[i],"<rcards>")
  tmp_total_shotOn = str_count(match_data$shoton[i],"<team>")
  temp_home = 0
  temp_away = 0
  temp_str = match_data$shoton[i]
  if(tmp_total_shotOn > 0) {
    for( j in 1:tmp_total_shotOn){
      tt1 = regmatches(temp_str, regexpr('<team>.+</team>', temp_str))  #t1 = get string starting from <rcards> to  the last "</team>"
      tt2 = gsub("^.*?<team>","",tt1)  # cut string from t1 , get the string after the first "<team>" 
      temp_num = gsub("</team>.*","",tt2)  # cut the team number ,from the start unti the first "</team>"
      tt3 = gsub("^.*?</team>","</team>",tt2)   #get string from t2, start point = </team>
      temp_str = regmatches(tt3, regexpr('<team>.+</team>', tt3)) # get new string that start from the next <rcards>
      if(match_data$home_team_api_id[i] == temp_num){
        temp_home = temp_home + 1
      }
      if(match_data$away_team_api_id[i] == temp_num){
        temp_away = temp_away + 1
      }
      if(temp_num == ""){
      }
    }
    match_data$shotOn_Home[i] = temp_home
    match_data$shotOn_Away[i] = temp_away
  }
}

######################################shotoff########################################################
match_data["shotOff_Home"] <- 0
match_data["shotOff_Away"] <- 0
for (i in 1:nrows){
  #match_data$redCard_Total[i] = str_count(match_data$card[i],"<rcards>")
  tmp_total_shotOff = str_count(match_data$shotoff[i],"<team>")
  temp_home = 0
  temp_away = 0
  temp_str = match_data$shotoff[i]
  if(tmp_total_shotOff > 0) {
    for( j in 1:tmp_total_shotOff){
      tt1 = regmatches(temp_str, regexpr('<team>.+</team>', temp_str))  #t1 = get string starting from <rcards> to  the last "</team>"
      tt2 = gsub("^.*?<team>","",tt1)  # cut string from t1 , get the string after the first "<team>" 
      temp_num = gsub("</team>.*","",tt2)  # cut the team number ,from the start unti the first "</team>"
      tt3 = gsub("^.*?</team>","</team>",tt2)   #get string from t2, start point = </team>
      temp_str = regmatches(tt3, regexpr('<team>.+</team>', tt3)) # get new string that start from the next <rcards>
      if(match_data$home_team_api_id[i] == temp_num){
        temp_home = temp_home + 1
      }
      if(match_data$away_team_api_id[i] == temp_num){
        temp_away = temp_away + 1
      }
      if(temp_num == ""){
      }
    }
    match_data$shotOff_Home[i] = temp_home
    match_data$shotOff_Away[i] = temp_away
  }
}


######################################corner########################################################

match_data["corner_Home"] <- 0
match_data["corner_Away"] <- 0
for (i in 1:nrows){
  tmp_total_corner = str_count(match_data$corner[i],"<team>")
  temp_home = 0
  temp_away = 0
  temp_str = match_data$corner[i]
  if(tmp_total_corner > 0) {
    for( j in 1:tmp_total_corner){
      tt1 = regmatches(temp_str, regexpr('<team>.+</team>', temp_str))  #t1 = get string starting from <rcards> to  the last "</team>"
      tt2 = gsub("^.*?<team>","",tt1)  # cut string from t1 , get the string after the first "<team>" 
      temp_num = gsub("</team>.*","",tt2)  # cut the team number ,from the start unti the first "</team>"
      tt3 = gsub("^.*?</team>","</team>",tt2)   #get string from t2, start point = </team>
      temp_str = regmatches(tt3, regexpr('<team>.+</team>', tt3)) # get new string that start from the next <rcards>
      if(match_data$home_team_api_id[i] == temp_num){
        temp_home = temp_home + 1
      }
      if(match_data$away_team_api_id[i] == temp_num){
        temp_away = temp_away + 1
      }
      if(temp_num == ""){
      }
    }
    match_data$corner_Home[i] = temp_home
    match_data$corner_Away[i] = temp_away
  }
}


######################################cross########################################################
match_data["cross_Home"] <- 0
match_data["cross_Away"] <- 0
for (i in 1:nrows){
  tmp_total_cross = str_count(match_data$cross[i],"<value><stats>")
  tmp_home = 0
  tmp_away = 0
  tmp_str = match_data$cross[i]
  if(tmp_total_cross > 0) {
    for( j in 1:tmp_total_cross){
      t1 = regmatches(tmp_str, regexpr('<value>.+</value>', tmp_str)) 
      focus_str = gsub("</value><value><stats>.*","",t1)
      if( str_count(focus_str,"<crosses>") > 0 ){
        t2 = gsub("^.*?<team>","",focus_str)
        team_num = gsub("</team>.*","",t2)
        if(match_data$home_team_api_id[i] == team_num){
          tmp_home = tmp_home + 1
        }
        if(match_data$away_team_api_id[i] == team_num){
          tmp_away = tmp_away + 1
        }
      }
      tmp_str = gsub("^.*?</value><value><stats>","<value><stats>",t1)
    }
    match_data$cross_Home[i] = tmp_home
    match_data$cross_Away[i] = tmp_away
  }
}

###### delete used columns #####
drops = c("goal","shoton","shotoff","foulcommit","card","cross","corner","possession","redCard_Total")
match_data_2 = match_data[ , !(names(match_data) %in% drops)]

match_data_2$result <- ifelse(match_data_2$home_team_goal > match_data_2$away_team_goal,1,ifelse(match_data_2$home_team_goal == match_data_2$away_team_goal, 0.5,0))
match_data_2$goal_dif = match_data_2$home_team_goal - match_data_2$away_team_goal
match_data_3 = match_data_2[,149:169]

######
head(match_data_3)
library(MASS)
attach(match_data_3)


train = sample(1:nrow(match_data_3),0.75*nrow(match_data_3))
test = -train


##########################################linear regression##########################################

##### actual data
tmp_home_model = lm(match_data_3[train,]$goal_dif~.-result, match_data_3[train,])
summary(tmp_home_model)
lm.pred = predict(tmp_home_model,match_data_3[test,])
errorlm = mean((lm.pred - match_data_3[test,]$goal_dif)^2)
errorlm #2.427582

#### avg data
numb = c(1,2,3,4,5)
error_matrix_linear = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  tmp_home_model = lm(te[train,]$goal_dif~.-result, te[train,])
  lm.pred = predict(tmp_home_model,te[test,])
  errorlm = mean((lm.pred - te[test,]$goal_dif)^2)
  error_matrix_linear[t] = errorlm
  t = t +1
}
error_matrix_linear

################################### forward subset selection##################################
regfit.fwd=regsubsets(match_data_3[train,]$goal_dif~.-result, match_data_3[train,], nvmax=20, method="forward")
best.model.fwd = which.max(summary(regfit.fwd)$adjr2)
best.model.fwd #16
coef(regfit.fwd,best.model.fwd)
summary(regfit.fwd)

predict.regsubsets=function(regfit.full,newdata,t){
  form=as.formula(regfit.full$call[[2]])
  mat=model.matrix(goal_dif~.,newdata) 
  coefi=coef(regfit.full,id=t) 
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi
  return(pred)
}

pred2=predict.regsubsets(regfit.fwd, match_data_3[test,], best.model.fwd)
actual2 = match_data_3[test,]$goal_dif
errorlm2=mean((actual2 - pred2)^2) 
errorlm2 #2.430666


###########################################PLS#########################################

library(pls)
pls.fit = plsr(match_data_3[train,]$goal_dif~.-result, data = match_data_3[train,], scale=TRUE, validation ="CV", type = 'class',segments = 10)
summary(pls.fit)
biplot(pls.fit)
validationplot(pls.fit,val.type="MSEP") #ncomp = 3
#MSEP = mean squared error of prediction
#here we see that best number of components = 3
## evaluate test set MSE
pls.pred = predict(pls.fit,match_data_3[test,-21],ncomp=3,type = 'response')

pls.pred
mean((pls.pred - match_data_3[test,21])^2) #2.422217


##########################################PCR############################################

pcr.fit = pcr(match_data_3[train,]$goal_dif~.-result, data = match_data_3[train,], scale=TRUE, validation ="CV", type = 'class',segments = 10)

#gives # of comps considered, CV scores, and % variance explained
summary(pcr.fit)

# plot cross-validation scores
validationplot(pcr.fit,val.type="MSEP") #MSEP = mean squared error of prediction
#here we see that best number of components = 9
#in red -- bias-corrected cross-validation error

#obtain predictions using # comps = 9
pcr.pred = predict(pcr.fit, match_data_3[test,-21], ncomp=9)

# calculate prediction errors (mean squared error)
mean((pcr.pred - match_data_3[test,21])^2) #2.505872



############################################lasso############################################################
library('leaps')
library('glmnet')

###### actual
grid=10^(-3:3)
lasso_cv.out = cv.glmnet(data.matrix(match_data_3[train,-20:-21]),as.factor(match_data_3[train,]$result),alpha=1,lambda=grid,family="multinomial",nfolds=5)
lasso_bestlam=lasso_cv.out$lambda.min
lasso_bestlam #0.01
lasso.mod=glmnet(data.matrix(match_data_3[train,-20:-21]), as.factor(match_data_3[train,]$result), alpha=1, lambda=lasso_bestlam, family="multinomial")
coef(lasso.mod,lasso_bestlam)
lasso_pred_test = predict(lasso.mod, data.matrix(match_data_3[test,-20:-21]) ,type="class")
lasso_error_rate_test = 1 - (sum(lasso_pred_test == match_data_3[test,]$result)/length(match_data_3[test,]$result))
lasso_error_rate_test #0.3808
lasso_pred_test2 = predict(lasso.mod, data.matrix(match_data_3[test,-20:-21]) ,type="response")
lasso_pred_test2 #each result prediction probability

#find the best previous games number for redcards prediction
numb = c(3,4,5)
error_matrix_redcard = matrix(, nrow = length(numb), ncol = 1)
t = 1
tmp_errorr = 1
best_red_game = 0
c = 0
for (n in numb){
  tmp_data = match_data_2
  for (j in 1:38){
    for (i in 1:nrow(match_data_2)){
      HomeTeamID = match_data_2$home_team_api_id[i]
      matchID = match_data_2$match_api_id[i]
      tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
      k = nrow(tmp_data2)
      if(k-n > 0){
        tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
        tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
        tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
        tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
      }else{
        tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
        tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
        tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
        tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
      }
      if (k>j){
        tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-j):(k-1)])
      }else{
        tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
      }
      AwayTeamID = match_data_2$away_team_api_id[i]
      tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
      k = nrow(tmp_data3)
      if(k-n > 0){
        tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
        tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
        tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
        tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
      }else{
        tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
        tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
        tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
        tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
      }
      if (k>j){
        tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-j):(k-1)])
      }else{
        tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
      }
    }
    te = tmp_data[,149:169]
    grid=10^(-3:3)
    lasso_cv.out = cv.glmnet(data.matrix(te[train,-20:-21]),as.factor(te[train,]$result),alpha=1,lambda=grid,family="multinomial",nfolds=5)
    lasso_bestlam=lasso_cv.out$lambda.min
    lasso.mod=glmnet(data.matrix(te[train,-20:-21]), as.factor(te[train,]$result), alpha=1, lambda=lasso_bestlam, family="multinomial")
    lasso_pred_test = predict(lasso.mod, data.matrix(te[test,-20:-21]) ,type="class")
    lasso_error_rate_test = 1 - (sum(lasso_pred_test == te[test,]$result)/length(te[test,]$result))
    if (tmp_errorr > lasso_error_rate_test){
      tmp_errorr = lasso_error_rate_test
      best_red_game = j
      c = n
    }
    t = t + 1
  }
}
###this part takes long time to run, more than 5 mins sorry:(
tmp_errorr #0.4331395
best_red_game #20
c #4


######## avg data
numb = c(1,2,3,4,5)
error_matrix_lasso = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  grid=10^(-3:3)
  lasso_cv.out = cv.glmnet(data.matrix(te[train,-20:-21]),as.factor(te[train,]$result),alpha=1,lambda=grid,family="multinomial",nfolds=5)
  lasso_bestlam=lasso_cv.out$lambda.min
  lasso.mod=glmnet(data.matrix(te[train,-20:-21]), as.factor(te[train,]$result), alpha=1, lambda=lasso_bestlam, family="multinomial")
  lasso_pred_test = predict(lasso.mod, data.matrix(te[test,-20:-21]) ,type="class")
  lasso_error_rate_test = 1 - (sum(lasso_pred_test == te[test,]$result)/length(te[test,]$result))
  error_matrix_lasso[t] = lasso_error_rate_test #0.3808
  t = t + 1
}
error_matrix_lasso   #error = 0.4331
plot(error_matrix_lasso,type="o", col="blue", xlab = "Number of Previous Games", ylab = "Error Rate", ylim = c(0.30,0.5))
title(main="Lasso Error Rate Test", col.main="red", font.main=4)
min(error_matrix_lasso)


##########################################LDA##########################################
####### actual
lda.fitsoccer = lda(match_data_3[train,]$result~.-goal_dif,match_data_3[train,])
lda.fitsoccer
#lda.fit
lda.predsoccer=predict(lda.fitsoccer, match_data_3[train,])
lda.class = lda.predsoccer$class
lda_error_rate = 1 - (sum (lda.class == match_data[train,20])/length(match_data[train,20]))
lda_error_rate #0.4316198

lda.pred_test=predict(lda.fitsoccer, match_data_3[-train,])
lda.class_test = lda.pred_test$class 
lda_error_rate_test = 1 - (sum (lda.class_test == match_data[-train,20])/length(match_data[-train,20]))
lda_error_rate_test  #0.4069767

lda.fitsoccer
coef(lda.fitsoccer)

#### avg data
numb = c(1,2,3,4,5)
error_matrix_lda = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  lda.fitsoccer = lda(te[train,]$result~.-goal_dif,te[train,])
  lda.predsoccer=predict(lda.fitsoccer, te[train,])
  lda.class = lda.predsoccer$class
  lda_error_rate = 1 - (sum (lda.class == match_data[train,20])/length(match_data[train,20]))
  lda.pred_test=predict(lda.fitsoccer, te[-train,])
  lda.class_test = lda.pred_test$class 
  lda_error_rate_test = 1 - (sum (lda.class_test == match_data[-train,20])/length(match_data[-train,20]))
  error_matrix_lda[t] = lda_error_rate_test
  t = t +1
}
error_matrix_lda
lda.pred_test$posterior
plot(error_matrix_lda,type="o", col="blue", xlab = "Number of Previous Games", ylab = "Error Rate", ylim = c(0.2,0.5))
title(main="LDA Error Rate Test", col.main="red", font.main=4)


#test for Chealsea vs Southam
che_vs_south_data = read.csv("che_vs_south2.csv")
che_vs_south_data["goal_dif"] = -100
lda.fitsoccer_chelsea = lda(te$result~.-goal_dif,te)
lda.pred_test_chelsea = predict(lda.fitsoccer_chelsea, che_vs_south_data)
lda.class_test_chelsea = lda.pred_test_chelsea$class 
lda.pred_test_chelsea$posterior #0:0.04580398 0.5:0.3491286 1:0.6050674

#test for Westham vs Sunderland
west_vs_sun_data = read.csv("sunder_vs_west2.csv")
west_vs_sun_data$goal_dif = 100
lda.fitsoccer_west = lda(te$result~.-goal_dif,te)
lda.pred_test_west = predict(lda.fitsoccer_west, west_vs_sun_data)
lda.class_test_west = lda.pred_test_west$class 
lda.pred_test_west$posterior #0:0.2567236 0.5:0.3693605 1:0.3739158



####Mutinomial Logistic

library("nnet")
modeltrain <- multinom(match_data_3[train,]$result~.-goal_dif, data = match_data_3[train,])
summary(modeltrain)
predicted = predict(modeltrain, data.matrix(match_data_3[test,-20]),type = "class")
lgm_error = 1-sum(predicted == match_data_3[test,]$result)/length(match_data_3[test,]$result)
lgm_error #0.38372  

numb = c(1,2,3,4,5)
error_matrix_multi = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  modeltrain <- multinom(te[train,]$result~.-goal_dif, data = te[train,])
  predicted = predict(modeltrain, data.matrix(te[test,-20]),type = "class")
  lgm_error = 1-sum(predicted == te[test,]$result)/length(te[test,]$result)
  error_matrix_multi[t] = lgm_error #0.3808
  t = t + 1
}
error_matrix_multi   # best n = 4, error = 0.4534884  
plot(error_matrix_multi,type="o", col="blue", xlab = "Number of Previous Games", ylab = "Error Rate", ylim = c(0.30,0.5))
title(main="Mutinomial Logistic Error Rate Test", col.main="red", font.main=4)

min(error_matrix_multi) #0.4534884

########################################################  KNN ##################################################

library(class)

####### actual
k.vec = 1:100
folds = 5
err.mat.knn = matrix(0, ncol = length(k.vec), nrow = folds)
for (i in 1:folds) {
  cv.ind = sample(1:nrow(match_data_3[train,]), .8*nrow(match_data[train,]))
  train.cv = match_data_3[train,][cv.ind, ]
  test.cv = match_data_3[train,][-cv.ind, ]
  for (j in 1:length(k.vec)) {
    knn.pred = knn(train.cv[,-20:-21], test.cv[,-20:-21], train.cv[,20], prob = TRUE, k=k.vec[j])
    err.mat.knn[i,j] = sum(knn.pred != test.cv[,20])/length(knn.pred) 
  }
}
k.opt = k.vec[which.min(apply(err.mat.knn, 2, mean))]
knn.pred = knn(match_data_3[train,-20:-21], match_data_3[test,-20:-21], match_data_3[train,20], prob = TRUE, k=k.opt)
error2=1-sum(knn.pred==match_data_3[train,20])/length(match_data_3[train,20])
error2  #0.6032978
k.opt  #68
knn.pred

###avg 
numb = c(1,2,3,4,5)
error_matrix_knn = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  k.vec = 1:100
  folds = 5
  err.mat.knn = matrix(0, ncol = length(k.vec), nrow = folds)
  for (i in 1:folds) {
    cv.ind = sample(1:nrow(te[train,]), .8*nrow(match_data[train,]))
    train.cv = te[train,][cv.ind, ]
    test.cv = te[train,][-cv.ind, ]
    for (j in 1:length(k.vec)) {
      knn.pred = knn(train.cv[,-20:-21], test.cv[,-20:-21], train.cv[,20], prob = TRUE, k=k.vec[j])
      err.mat.knn[i,j] = sum(knn.pred != test.cv[,20])/length(knn.pred) 
    }
  }
  k.opt = k.vec[which.min(apply(err.mat.knn, 2, mean))]
  knn.pred = knn(te[train,-20:-21], te[test,-20:-21], te[train,20], prob = TRUE, k=k.opt)
  error2=1-sum(knn.pred==te[train,20])/length(te[train,20])
  error_matrix_knn[t] = error2
  t = t + 1
}
error_matrix_knn   # best n = 4, error = 0.6236663  
plot(error_matrix_knn,type="o", col="blue", xlab = "Number of Previous Games", ylab = "Error Rate", ylim = c(0.30,0.5))
title(main="KNN Error Rate Test", col.main="red", font.main=4)


min(error_matrix_knn) #0.6236663






##################################################Decision Tree ##################################################
#install.packages("tree")
library(tree)

tree.match = tree(match_data_3[train,]$result~.-goal_dif, match_data_3[train,])
summary(tree.match)
plot(tree.match)
text(tree.match,pretty=0)

#install.packages("rpart")
library("rpart")
tree.matchrpart = rpart(match_data_3[train,]$result~.-goal_dif, match_data_3[train,],method="class")
printcp(tree.matchrpart)
plotcp(tree.matchrpart)
summary(tree.matchrpart)
plot(tree.matchrpart,uniform = TRUE)
text(tree.matchrpart,use.n = TRUE)
#post(tree.matchrpart)

tree_cp = prune(tree.matchrpart,cp=tree.matchrpart$cptable[which.min(tree.matchrpart$cptable[,"xerror"]),"CP"])
which.min(tree.matchrpart$cptable[,"xerror"]) #4
plot(tree_cp,uniform = TRUE)
text(tree_cp)

tree.pred = predict(tree_cp,match_data_3[-train,],type = "class")
error.tree = 1-sum(tree.pred == match_data_3[test,]$result)/length(match_data_3[test,]$result)
error.tree #0.4651163


### AVG 


numb = c(1,2,3,4,5)
error_matrix_tree = matrix(, nrow = length(numb), ncol = 1)
t = 1
for (n in numb){
  tmp_data = match_data_2
  for (i in 1:nrow(match_data_2)){
    HomeTeamID = match_data_2$home_team_api_id[i]
    matchID = match_data_2$match_api_id[i]
    tmp_data2 = match_data_2[match_data_2$home_team_api_id == HomeTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data2)
    if(k-n > 0){
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[(k-n):(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[(k-n):(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[(k-n):(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Home[i]  = mean(tmp_data2$shotOn_Home[1:(k-1)])
      tmp_data$shotOff_Home[i]  = mean(tmp_data2$shotOff_Home[1:(k-1)])
      tmp_data$corner_Home[i]  = mean(tmp_data2$corner_Home[1:(k-1)])
      tmp_data$cross_Home[i]  = mean(tmp_data2$cross_Home[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Home[i]  = mean(tmp_data2$redCard_Home[1:(k-1)])
    }
    AwayTeamID = match_data_2$away_team_api_id[i]
    tmp_data3 = match_data_2[match_data_2$away_team_api_id == AwayTeamID & match_data_2$match_api_id <= matchID,]
    k = nrow(tmp_data3)
    if(k-n > 0){
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[(k-n):(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[(k-n):(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[(k-n):(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[(k-n):(k-1)])
    }else{
      tmp_data$shotOn_Away[i]  = mean(tmp_data3$shotOn_Away[1:(k-1)])
      tmp_data$shotOff_Away[i]  = mean(tmp_data3$shotOff_Away[1:(k-1)])
      tmp_data$corner_Away[i]  = mean(tmp_data3$corner_Away[1:(k-1)])
      tmp_data$cross_Away[i]  = mean(tmp_data3$cross_Away[1:(k-1)])
    }
    if (k>20){
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[(k-20):(k-1)])
    }else{
      tmp_data$redCard_Away[i]  = mean(tmp_data3$redCard_Away[1:(k-1)])
    }
  }
  te = tmp_data[,149:169]
  tree.matchrpart = rpart(te[train,]$result~.-goal_dif, te[train,],method="class")
  tree_cp = prune(tree.matchrpart,cp=tree.matchrpart$cptable[which.min(tree.matchrpart$cptable[,"xerror"]),"CP"])
  #which.min(tree.matchrpart$cptable[,"xerror"])
  tree.pred=predict(tree_cp,te[-train,],type = "class")
  error.tree=1-sum(tree.pred == te[test,]$result)/length(te[test,]$result)
  error_matrix_tree[t] = error.tree #0.3808
  t = t + 1
}
error_matrix_tree   # best n = 2, error = 0.4622093   
plot(error_matrix_tree,type="o", col="blue", xlab = "Number of Previous Games", ylab = "Error Rate", ylim = c(0.30,0.5))
title(main="Mutinomial Logistic Error Rate Test", col.main="red", font.main=4)

min(error_matrix_tree) #0.4622093




##############Error Rate Comparison################


  
counts = c(min(error_matrix_lda),min(error_matrix_lasso),min(error_matrix_multi),min(error_matrix_knn),min(error_matrix_tree))
barplot(counts, main="Error Comparison", names.arg=c("LDA", "Lasso", "Mutinomial", "KNN", "Decision Tree"), col = "dark blue", xlab = 'Methods', ylab = "Error Rate")

#####################Poisson Distribution Method
match_2015_6=read.csv("match_2015_6.csv")
attach(match_2015_6)
avg_home=sum(home_team_goal)/nrow(match_2015_6)
avg_home #1.637602
avg_away=sum(away_team_goal)/nrow(match_2015_6)
avg_away #1.182561

for (i in 1:nrow(match_2015_6)){
  HomeTeamID=match_2015_6$home_team_api_id[i]
  AwayTeamID=match_2015_6$away_team_api_id[i]
  tmp_data4=match_2015_6[home_team_api_id==HomeTeamID,]
  match_2015_6$AttStrHome[i]=sum(tmp_data4$home_team_goal)/nrow(tmp_data4)/avg_home
  tmp_data5=match_2015_6[away_team_api_id==AwayTeamID,]
  match_2015_6$DefStrAway[i]=sum(tmp_data5$home_team_goal)/nrow(tmp_data5)/avg_home
  tmp_data6=match_2015_6[away_team_api_id==AwayTeamID,]
  match_2015_6$AttStrAway[i]=sum(tmp_data6$away_team_goal)/nrow(tmp_data6)/avg_away
  tmp_data7=match_2015_6[home_team_api_id==HomeTeamID,]
  match_2015_6$DefStrHome[i]=sum(tmp_data7$away_team_goal)/nrow(tmp_data7)/avg_away
  match_2015_6$AvgScoreHome[i]=match_2015_6$AttStrHome[i]*match_2015_6$DefStrAway[i]*avg_home
  match_2015_6$AvgScoreAway[i]=match_2015_6$AttStrAway[i]*match_2015_6$DefStrHome[i]*avg_away
}

for (i in 0:6){
  Colname=paste("HomeScore_",i)
  match_2015_6[,Colname]=dpois(i,lambda=match_2015_6$AvgScoreHome)
}
for (i in 0:6){
  Colname=paste("AwayScore_",i)
  match_2015_6[,Colname]=dpois(i,lambda=match_2015_6$AvgScoreAway)
}

match_2015_6$Draw=rep(0,nrow(match_2015_6))
for (i in 0:6){
  match_2015_6$Draw=match_2015_6[,12+i]*match_2015_6[,19+i]+match_2015_6$Draw
}
match_2015_6$Win=rep(0,nrow(match_2015_6))
for (i in 1:6){
  for (j in 0:6){
    if (i>j){
      match_2015_6$Win=match_2015_6[,12+i]*match_2015_6[,19+j]+match_2015_6$Win
    }
    else {
      match_2015_6$Win=match_2015_6$Win+0
    }
  }
}
match_2015_6$Loss=1-match_2015_6$Draw-match_2015_6$Win


for (i in 0:6){
  for (j in 0:6){
    Colname1=paste(i,j,sep=":")
    Colname2=paste("HomeScore_",i)
    Colname3=paste("AwayScore_",j)
    match_2015_6[,Colname1]=match_2015_6[,Colname2]*match_2015_6[,Colname3]
  }
}

n=length(match_2015_6[1,29:77])
for (j in 1:6){
  for (i in 1:nrow(match_2015_6)){
    Colname1=paste("MaxProb_",j)
    Colname2=paste("PredScore_",j)
    match_2015_6[i,Colname2]=colnames(sort(match_2015_6[i,29:77])[n+1-j])
    match_2015_6[i,Colname1]=sort(match_2015_6[i,29:77])[n+1-j]
  }
}
library(xlsx)
write.csv(match_2015_6,file = "match_2015_06.csv") ####write a csv file out to the work directory which contains the predictions data



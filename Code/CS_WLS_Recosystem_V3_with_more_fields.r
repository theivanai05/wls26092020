
##==> 1)( Create a table with More fields for MF. 
##==> 2)( Start working on GB Data First

#1) u_q_t_MS vs u_d_a_M

### Start of Tag Recommendation based on Pulse  Scores ###
smp_size = floor(0.8 * nrow(pulsescore_Master_GB))
train_ind = sample(1:nrow(pulsescore_Master_GB),size = smp_size)
train = pulsescore_Master_GB[train_ind,]
test = pulsescore_Master_GB[-train_ind,]
      
train_data = data_memory(train$masked_user_id, train$qtag, train$pulsescore,index1 = TRUE)
test_data = data_memory(test$masked_user_id, test$qtag, test$pulsescore,index1 = TRUE)


recommender = Reco()
recommender$train (train_data,opts=c(dim=10,costp_12 = 0.1,costq_12 = 0.1, 
                               lrate = 0.1, niter = 100, nthread = 6, verbose = F))


#show(recommender)

test$prediction = recommender$predict(test_data,out_memory())
#test$prediction

#Rounding the Predicted values so that MAE will be accurate  ### Rectification on the Predicted values as Input is not as precise ## For Slides 
names <- c('prediction')
test[,(names) := round(.SD,4), .SDcols=names]

# compute prediction MAE
test$MAE = abs(test$pulsescore - test$prediction)
mean(test$MAE, na.rm=TRUE) # show the MAE

# we can use the test framework in CFdemolib.r to derive a confusion matrix (assuming any given "like" threshold)
preds = t(test[,c("prediction","pulsescore")])
preds = unlist(preds)
cat("avg MAE =",avgMAE(preds))

#colnames(test)
#[1] "userid" "qtag" "pulsescore" "prediction" "MAE"    

#showCM(preds, like=4)
#showCM(preds, like=3)

#----------------------------------------Creating the Data for the GB Users Alone ----------------------------
rm(smp_size,train_ind,train_GB,test_GB,names,train_data,test_data)

### Start of Tag Recommendation based on Pulse  Scores ###
smp_size = floor(0.8 * nrow(new_Assess_woTime_dt))
train_ind = sample(1:nrow(new_Assess_woTime_dt),size = smp_size)
train_GB = new_Assess_woTime_dt[train_ind,]
test_GB = new_Assess_woTime_dt[-train_ind,]

#Factorizing the Fields 
names <- unlist(colnames(new_Assess_woTime_dt))
train_GB[,names] <- lapply(train_GB[,..names] , factor)
test_GB[,names] <- lapply(test_GB[,..names] , factor)

train_data = data_memory(user_index = train_GB$masked_user_id,item_index = train_GB$question_tags,rating = train_GB$score,index1 = TRUE)
test_data = data_memory(user_index = test_GB$masked_user_id,item_index = test_GB$question_tags,rating = test_GB$score,index1 = TRUE)

recommender_demo = Reco()
recommender_demo$train (train_data,opts=c(dim=10,costp_12 = 0.1,costq_12 = 0.1, 
                                     lrate = 0.1, niter = 100, nthread = 6, verbose = F))

test_GB$prediction = recommender_demo$predict(test_data,out_memory())

#Rounding the Predicted values so that MAE will be accurate  ### Rectification on the Predicted values as Input is not as precise ## For Slides 
names <- c('prediction')
test_UI[,(names) := round(.SD,4), .SDcols=names]

# compute prediction MAE
test_UI$MAE = abs(test_UI$pulsescore - test_UI$prediction)
mean(test_UI$MAE, na.rm=TRUE) # show the MAE

# we can use the test framework in CFdemolib.r to derive a confusion matrix (assuming any given "like" threshold)
preds = t(test_UI[,c("prediction","pulsescore")])
preds = unlist(preds)
print("For Demo Users PS Prediction Error")
cat("avg MAE =",avgMAE(preds))



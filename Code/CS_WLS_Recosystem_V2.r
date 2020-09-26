
### Start of Tag Recommendation based on Pulse  Scores ###
smp_size = floor(0.8 * nrow(pulsescore_Master))
train_ind = sample(1:nrow(pulsescore_Master),size = smp_size)
train = pulsescore_Master[train_ind,]
test = pulsescore_Master[-train_ind,]
      
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

#----------------------------------------Creating the Data for the Test Users Alone ----------------------------

Demo_Users_dt = data.table(c("97d0a65c","b1459d23","c80bffb2","c930cc66","f10f490e","f810564e"))
names(Demo_Users_dt)[names(Demo_Users_dt) == "V1"] <- "masked_user_id"

train_UI = pulsescore_Master[!Demo_Users_dt, on=c("masked_user_id")]  
test_UI = pulsescore_Master[Demo_Users_dt, on=c("masked_user_id")]  

#Factorizing the Fields 
names <- c('masked_user_id' ,'qtag')
train_UI[,names] <- lapply(train_UI[,..names] , factor)
test_UI[,names] <- lapply(test_UI[,..names] , factor)

train_data = data_memory(train_UI$masked_user_id, train_UI$qtag, train_UI$pulsescore,index1 = TRUE)
test_data = data_memory(test_UI$masked_user_id, test_UI$qtag, test_UI$pulsescore,index1 = TRUE)

recommender_demo = Reco()
recommender_demo$train (train_data,opts=c(dim=10,costp_12 = 0.1,costq_12 = 0.1, 
                                     lrate = 0.1, niter = 100, nthread = 6, verbose = F))


test_UI$prediction = recommender_demo$predict(test_data,out_memory())
#test$prediction

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

## **!! ** 3 of the Test Users do not have any Pulse Score 
#==> 

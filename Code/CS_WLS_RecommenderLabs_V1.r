
#install.packages("recommenderlab")
  #Current development version: Download package from AppVeyor or install from GitHub (needs devtools).
#install_github("mhahsler/recommenderlab")
  #Usage
  #Load the package and prepare a dataset (included in the package).

#library("recommenderlab")
#library("devtools")

# ==> Converting pulsescore_Master_GB into a real rating matrix ## !! to remove later on swhere applicable 
  pulsescore_Master_GB[,c("X")] <- NULL
  temp <- as(pulsescore_Master_GB,"realRatingMatrix")

## THe Matrix is a Very Very Sparse Matrix  to go and use it as part of UBCF or IBCF; Using Random and Popular might be the best bet  

  temp_matrix = as(temp,"matrix")


train <- temp[1:100]
rec <- Recommender(train, method = "UBCF")
rec

rec_i <- Recommender(train, method = "IBCF")
rec_i

rec_svd <- Recommender(train, method = "SVD")
rec_svd

rec_libmf <- Recommender(train, method = "LIBMF")
rec_libmf

rec_rnd <- Recommender(train, method = "RANDOM")
rec_rnd

rec_pop <- Recommender(train, method = "POPULAR")
rec_pop

#Create top-N recommendations for new users (users 101 and 102)

pre <- predict(rec, temp[201:202], n = 10)
pre
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre, "list")

#Create top-N recommendations for new users (users 101 and 102)

pre_i <- predict(rec_i, temp[201:202], n = 10)
pre_i
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre_i, "list")

#Create top-N recommendations for new users (users 101 and 102)

pre_svd <- predict(rec_svd, temp[201:202], n = 10)
pre_svd
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre_svd, "list")

#Create top-N recommendations for new users (users 101 and 102)

pre_libmf <- predict(rec_libmf, temp[201:202], n = 10)
pre_libmf
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre_libmf, "list")

#Create top-N recommendations for new users (users 101 and 102)

pre_rnd <- predict(rec_rnd, temp[201:202], n = 10)
pre_rnd


pre_rnd_ratings <- predict(rec_rnd, temp[201:202], type = "ratings")
pre_rnd_ratings
as(pre_rnd_ratings, "matrix")[,1:235]
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre_rnd, "list")


## Predictions for the popular Tags 
pre_pop<- predict(rec_pop, temp[201:202], n = 10)
pre_pop

pre_pop_ratings <- predict(rec_pop, temp[201:202], type = "ratings")
pre_pop_ratings
as(pre_pop_ratings, "matrix")[,1:235]
#Recommendations as ‘topNList’ with n = 10 for 2 users. 
as(pre_pop, "list")



Recommended_Recolabs_Tag = predict(rec_rnd, temp[], n = 10)
Recommended_Recolabs_Tag = as(Recommended_Recolabs_Tag,"list")
#Reordering the predicted Tags Output
Recommended_Recolabs_Tag = gather(bind_rows(Recommended_Recolabs_Tag))
#Changes to field names 
names(Recommended_Recolabs_Tag)[names(Recommended_Recolabs_Tag) == "key"] <- "masked_user_id"
names(Recommended_Recolabs_Tag)[names(Recommended_Recolabs_Tag) == "value"] <- "question_tags"

#rm(Recommended_Recolabs_Tag,Recommended_Recolabs_Tags,reco_dt,reco_dt_long)

#Changing directory for file movement 
setwd("~/Documents/NUS_EBAC")

# unlink("Recommended_Recolabs_Tags.RDS") # deleted the file 
saveRDS(Recommended_Recolabs_Tag,file = "Recommended_Recolabs_Tag.RDS")
#Recommended_Recolabs_Tag["22f08364"]




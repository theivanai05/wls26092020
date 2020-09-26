## Recommender Lab Evaluations : 



scheme <- evaluationScheme(data = temp, method = "split", train = 0.9, goodRating=0.00, given = 1)

# try User-Based CF (UBCF)
recommender <- Recommender(getData(scheme,"train"), "IBCF", parameter = list(method = "cosine")) 
# normalize = "center" is default
preds <- predict(recommender, getData(scheme, "known"), type="ratings") 
preds
p=as(preds, "matrix") [1:2,]
results <- calcPredictionAccuracy(preds, getData(scheme,"unknown"))
results
temp_matrix = as(temp,"matrix")
Temp_Orig_User = temp_matrix [c("043209ff","054bd7b6"),]




scheme <- evaluationScheme(temp[1:100], method="cross", k=4, given=3) 
#, given=3, goodRating=5)
evaluationScheme(data, method="split", train=0.8, k=10, given=3)
scheme

results <- evaluate(scheme, method="POPULAR", type = "topNList", n=c(1,3,5,10,15,20))

### real valued recommender
data(Jester5k)
## create 90/10 split (known/unknown) for the first 500 users in Jester5k
e <- evaluationScheme(temp, method="split", train=0.9, goodRating=0.12, given = 1)
e
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="ratings")
p
## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))
## evaluate topNLists instead (you need to specify given and goodRating!)
p <- predict(r, getData(e, "known"), type="topNList")
p
#6 dissimilarity
calcPredictionAccuracy(p, getData(e, "unknown"), given=1, goodRating=0.12)
## evaluate a binary recommender
data(MSWeb)
MSWeb10 <- sample(MSWeb[rowCounts(MSWeb) >10,], 50)
e <- evaluationScheme(MSWeb10, method="split", train=0.9,
                      k=1, given=3)
e
## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")
## create predictions for the test data using known ratings (see given above)
p <- predict(r, getData(e, "known"), type="topNList", n=10)
p
calcPredictionAccuracy(p, getData(e, "unknown"), given=3)
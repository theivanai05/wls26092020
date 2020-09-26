# For Cross Validation ==> 

scheme <- evaluationScheme(temp, method="cross", k=4, given=1,goodRating=0)
scheme

results <- evaluate(scheme, method="POPULAR", type = "topNList",n = c(1,3,5,10,15,20))

results

getConfusionMatrix(results)[[1]]

avg(results)

plot(results, annotate=TRUE)
plot(results, "prec/rec", annotate=TRUE)

## --- For Split Data 

scheme_Split <- evaluationScheme(data = temp, method = "split", train = 0.9, goodRating=0.00, given = 1)
results_Split <- evaluate(scheme_Split, method="POPULAR", type = "topNList",n = c(1,3,5,10,15,20))

results_Split

getConfusionMatrix(results_Split)[[1]]

avg(results_Split)

plot(results_Split, annotate=TRUE)
plot(results_Split, "prec/rec", annotate=TRUE)


temp_matrix = as(temp,"matrix")
Temp_Orig_User = temp_matrix [c("043209ff","054bd7b6"),]



scheme <- evaluationScheme(data = temp, method = "split", train = 0.9, goodRating=0.12, given = 1)

# try User-Based CF (UBCF)
recommender <- Recommender(getData(scheme,"train"), "IBCF", parameter = list(method = "cosine")) 
# normalize = "center" is default
preds <- predict(recommender, getData(scheme, "known"), type="ratings") 
preds
p=as(preds, "matrix")
results <- calcPredictionAccuracy(preds, getData(scheme,"unknown"))
results

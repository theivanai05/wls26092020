## 5.8. Comparing recommender algorithms
# Comparing top-N recommendations against many Recommendor Systems 

set.seed(2020)
scheme <- evaluationScheme(temp[1:1000], method="split", train = .9, k=1, given=1, goodRating=0)
scheme

algorithms <- list("random items" = list(name="RANDOM", param=NULL),
                   "popular items" = list(name="POPULAR", param=NULL),
                   "user-based CF" = list(name="UBCF", param=list(nn=50)),
                   "item-based CF" = list(name="IBCF", param=list(k=50)),
                   "SVD approximation" = list(name="SVD", param=list(k = 50)),
                   "LIBMF - Matrix Factorization" =list(name="LIBMF", param=NULL))

## run algorithms
results_ALLALGO <- evaluate(scheme, algorithms, type = "topNList",n=c(1, 3, 5, 10, 15, 20))
results_ALLALGO 
names(results_ALLALGO )

plot(results_ALLALGO , annotate=c(1,3), legend="bottomright")
plot(results_ALLALGO , "prec/rec", annotate=3, legend="topleft")

## run algorithms
results_ALLALGO_RATINGS <- evaluate(scheme, algorithms, type = "ratings")

results_ALLALGO_RATINGS 
plot(results_ALLALGO_RATINGS , ylim = c(0,.2))



## using the predict function after the recommendor Evaluator above 

e <- evaluationScheme(temp, method="split", train=0.9, goodRating=0, given = 1)

## create a user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")


## There moight not be any Known ratings ... Let me check out the recommendor values 
p_ratings <- predict(r, getData(e, "known"), type="ratings")
p_top_n<- predict(r, getData(e, "known"), type="topNList", n=10)

calcPredictionAccuracy(p_ratings, getData(e, "unknown"), given=1, goodRating=0.0)
calcPredictionAccuracy(p_top_n, getData(e, "unknown"), given=1)



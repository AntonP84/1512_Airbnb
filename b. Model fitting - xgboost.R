# 10_02_01.csv
# xgboost
# public data       NDCG@5 = 0.87981
# private data      NDCG@5 = 0.88451


# loading and processing train data ---------------------------------------

ver <- '10_02_01'

load(paste0('data/train_', '10_02', '.RData'))

var.id     <- 'id'
var.target <- 'country_destination'

train[, var.target] <- as.factor(train[, var.target])

dict.target <- levels(train[, var.target])
train[, var.target] <- as.integer(train[, var.target] ) - 1

vars <- names(train)
vars <- setdiff(vars, c(var.target, var.id))



# fitting the model -------------------------------------------------------

require(xgboost)

num_class <- length(unique(train[, var.target]))

xgtrain = xgb.DMatrix(data=as.matrix(train[ , vars])
                      , label=train[ , var.target]
                      , missing=NA
)
rm(train)

# best result from the grid search
param   <- list(
   eta                 = 0.3 # 0.3
   , max_depth         = 6 # 6
   , min_child_weight  = 20 # 1
   , subsample         = 0.95 # 1
   , colsample_bytree  = 1 # 1
   , num_parallel_tree = 1 # 1
   , gamma             = 0 # 0
   , max_delta_step    = 0 # 0
)

xgb.fit <- xgb.train(
   data         = xgtrain
   , objective        = "multi:softprob"
   , eval_metric      = "merror"
   , num_class        = num_class
   , seed             = 101
   , nthread          = 4
   , verbose          = 1
   , params           = param
   , nrounds           = 21
)
 
xgb.save(xgb.fit, paste0('data/models/', ver, '_XGB.model'))

importance_matrix <- xgb.importance(vars, model = xgb.fit)
save(importance_matrix, file=paste0('data/models/', ver, '_XGB_matrix.model'))

rm(xgtrain)
gc()



# making predictions ------------------------------------------------------

load(paste0('data/test_', '10_02', '.RData'))

pred <- predict(xgb.fit
                , data.matrix(test[, vars]) 
                , missing=NA
                )
rm(xgb.fit)
gc()

pred = matrix(pred, num_class, length(pred)/num_class)
pred = t(pred)


k <- 5 # кол-во результатов на выходе
submission <- data.frame()
submission <- apply(pred, 1, function(x) order(x, decreasing = T)[1:k]) # из каждой строки берем k наиболее частых dim(k, nrow)
submission <- dict.target[submission] # расшифровываем и переводим в вектор dim(k*nrow)

submission <- data.frame(
          id = unlist(lapply(test[, var.id], function(x) rep(x, k)))
          , country = submission
)

write.csv(submission, paste0('data/submissions/res_', ver, '.csv'), quote = F, row.names = F)






#### Grid Search for Max Depth ####

best_cv_mean = Inf
best_cv_mean_index = 0
for (iter in 1:100) {
    print(iter)
    param <- list(colsample_bytree = .63,
                  subsample = .84,
                  eta = .01,
                  eval_metric = 'error',
                  objective = 'binary:logistic',
                  max_depth = sample(6:20, 1),
                  gamma = .119, 
                  min_child_weight = 1
    )
    set.seed(1812)
    # Cross validation parameters
    res = xgb.cv(param,
                 dtrain,
                 nrounds=2000, # started at 750. Had to add more in order to lower ETA
                 early_stopping_rounds=100,
                 nfold = 4,
                 print_every_n = 120)
    #early.stop.round = 10)
    
    best_nrounds = res$best_iteration
    cv_mean <- res$evaluation_log$test_error_mean[best_nrounds]
    
    if (cv_mean < best_cv_mean) {
        best_cv_mean = cv_mean
        best_cv_mean_index = best_nrounds
        best_param = param
    }
}

## Confirmed the sweet spot was 10-11 for max depth








### Grid Search--- Seeinf if Training + Validation = better Test score
best_cv_mean = Inf
best_cv_mean_index = 0
for (iter in 1:100) {
    param <- list(colsample_bytree = runif(1, .4, .9),
                  subsample = runif(1, .5, .9),
                  eta = .01,
                  eval_metric = 'error',
                  objective = 'binary:logistic',
                  max_depth = sample(9:11, 1),
                  gamma = runif(1, 0.0, .4), 
                  min_child_weight = 1
    )
    set.seed(1812)
    # Cross validation parameters
    res = xgb.cv(param,
                 dtrain2,
                 nrounds=2000, # started at 750. Had to add more in order to lower ETA
                 early_stopping_rounds=100,
                 nfold = 4,
                 print_every_n = 100)
    #early.stop.round = 10)
    
    best_nrounds = res$best_iteration
    cv_mean <- res$evaluation_log$test_error_mean[best_nrounds]
    
    if (cv_mean < best_cv_mean) {
        best_cv_mean = cv_mean
        best_cv_mean_index = best_nrounds
        best_param = param
    }
    print(best_cv_mean)
}

best_param
### Best achieved is .1257
# So rerun again, and save model this time. 
param <- list(colsample_bytree = .48,
              subsample = .86,
              eta = .01,
              eval_metric = 'error',
              objective = 'binary:logistic',
              max_depth = 9,
              gamma = .07, 
              min_child_weight = 1
)
set.seed(1812)
# Cross validation parameters
res = xgb.cv(param,
             dtrain2,
             nrounds=2000, # started at 750. Had to add more in order to lower ETA
             early_stopping_rounds=100,
             nfold = 4,
             print_every_n = 40)
res$best_iteration # this tells us how far to go in the final model building

# Now we want to retrain with all the training data (not doing cross validation)
# CV was used to pick parameters
# We use best_iteration/.8 because we have more data this time, this will go a few more rounds and help improve accurary
xgb <- xgb.train(param, dtrain2, res$best_iteration/.9) 

# Test Accuracy
final <- predict(xgb,dtest)
final <- ifelse(final >.5,1,0)
acc1 <- table(final, test3[,"over_50k"])
sum(diag(acc1))/sum(acc1)


## Accuracy - 87.45%


























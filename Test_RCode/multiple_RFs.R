

### Model #1
rf1 <- h2o.randomForest(training_frame = trainh2o, validation_frame = validh2o,
                        x=1:12, y=13, 
                        model_id = "rf_v1", 
                        ntrees = 200, #  random forest model. The default is 50.
                        stopping_rounds = 3, # Stop fitting new trees when the 2-tree
                        max_depth = 20,  # This is default, and better than 15, or 30 (I tried)
                        score_each_iteration = T,
                        seed = 1812) 
rf1@model$validation_metrics # 13.8% Error rate
one <- h2o.predict(rf1, testh2o)


### Model #2
rf2 <- h2o.randomForest(training_frame = trainh2o, 
                        validation_frame = validh2o,
                        x=1:12,
                        y=13, 
                        model_id = "rf_v2", 
                        ntrees = 200, #  random forest model. The default is 50.
                        stopping_rounds = 3, # Stop fitting new trees when the 2-tree
                        #stopping_metric = "misclassification",
                        max_depth = 20,  # This is default, and better than 15, or 30 (I tried)
                        score_each_iteration = T,
                        seed = 1813) 
rf2@model$validation_metrics
two <- h2o.predict(rf2,testh2o)


### Model 3
rf3 <- h2o.randomForest(training_frame = trainh2o, 
                        validation_frame = validh2o,
                        x=1:12,
                        y=13, 
                        model_id = "rf_v3", 
                        ntrees = 200, #  random forest model. The default is 50.
                        stopping_rounds = 3, # Stop fitting new trees when the 2-tree
                        #stopping_metric = "misclassification",
                        max_depth = 20,  # This is default, and better than 15, or 30 (I tried)
                        score_each_iteration = T,
                        seed = 1814) 
rf3@model$validation_metrics
three <- h2o.predict(rf3,testh2o)


final <- as.numeric(one$predict) + as.numeric(two$predict) + as.numeric(three$predict)
final <- ifelse(final > 1, 1,0)


predrf = as.vector(final)
actual = as.vector(testh2o$over_50k)
tab = table (actual, predrf)
print(tab)
(error <- (tab[1]+tab[4])/dim(testh2o)[1])
### .144 misclassification rate - BOO. 








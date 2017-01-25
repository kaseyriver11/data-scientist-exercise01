

### Let's get started! - Load in all neccessary packages 
library(DBI)        # to connect to the database
library(tidyr)      # to reshape data
library(h2o)        # for random forest/neural network attempts
library(xgboost)    # for xgboost
library(caret)      # for train() - which creates the standard logistic regression
library(xgboost)    # for xgboost models
library(e1071)      # required for train() to work


### Let's begin by connecting to the database and taking a look at the tables 
# Establish a connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname="exercise01.sqlite")
# Vector of the different table names - 1 records table, 8 reference tables
(tables <- dbListTables(connection))

# Read the records table - and take a quick look
records_db <- dbGetQuery(connection,'select * from records' )
head(records_db)

# Read reference tables - and take a quick look
# Note that the only ordinal variable could be education level - but it is not in order
# This for-loop prints out each reference table
for(i in 1:length(tables)){
    if(tables[i] != "records"){
    print(dbGetQuery(connection, paste0('select * from ', tables[i])))}
}
# if you just want to look at one of them
(gender <- dbGetQuery(connection,'select * from sexes' ))
### --- NOTE: Relationship was not listed in the data dictionary --- ###
(relationship <- dbGetQuery(connection,'select * from relationships' ))



##### ----- Let us start to complete the tasks assigned ----- #####

##### ----- 2: Write a SQL query that creates a consolidated dataset from the normalized tables
# Make one complete table - keeping all columns for now ----- #####
df <- dbGetQuery(connection, 
                'select rc.*, cnt.name as Country,edu.name as Education_Level, 
                 ms.name as Marital_Status, job.name as Job, race.name as Race,
                 rel.name as Relationship, sx.name as Sex, wc.name as Work_Class
                 from records rc
                 left join Countries cnt on rc.country_id = cnt.id
                 left join education_levels edu on rc.education_level_id = edu.id
                 left join marital_statuses ms on rc.marital_status_id = ms.id
                 left join occupations job on rc.occupation_id = job.id
                 left join races race on rc.race_id = race.id
                 left join relationships rel on rc.relationship_id = rel.id
                 left join sexes sx on rc.sex_id = sx.id
                 left join workclasses wc on rc.workclass_id = wc.id')



##### ----- 3: Export the flattened table to a CSV file ----- #####
# If we are in the working directory - we don't need to specify the file path
# If you are not in the correct working directory - use setwd() 
write.csv(df, "records.csv", row.names = FALSE)



##### ----- 4: Import the exported CSV ----- #####
df <- read.csv("records.csv", sep = ",")
# We don't need duplicate columns. Trim the dataset to only columns we will use
records <- df[,c(1,2,5,11:13, 15:23)]
# Put the column "over_50k" at the end, it is the target variables. Remove country_id variable
records <- subset(records, select=c(1:6,8:15,7))
# Save the IDs for future call back. We cannot use them in a model, so we remove them
ID <- records$id 
records <- records[,2:15]
# One last bit of info, education number and education level appear to be the same variable
table(records$education_num, records$Education_Level)
# We will exclude education level going forward. Education number is a cleaner to read, ordinal variable
records <- records[,c(1:6,8:14)]


##### ----- 5: Exploratory Analysis ----- #####
# ----- Target Variable ----- #
table(records$over_50k)
# ~ 76% are 0. We have a good split though. Lots of responces for both 0 and 1


# ----- Continuous Variables (4 of these) ----- #
### --- AGE --- ###
summary(records$age)
table(records$age)
hist(records$age)
table(records$age, records$over_50k)
# Histogram looks fairly normal - minimum age of 17 might have been required for census
# It is highly unlikely that there were 55 people who were 90 years old. There were only
# 17 people between the ages of 85-89

### --- Education Number: individual's current education level --- ###
summary(records$education_num)
table(records$education_num)
hist(records$education_num)
edunum <- as.data.frame.matrix(table(records$education_num, records$over_50k))
edunum$percent <- edunum$'0'/(edunum$'0' + edunum$'1'); edunum

### --- CAPITAL_GAIN & CAPITAL_LOSS --- ###
# capital_gain: represents post-social insurance income, in the form of capital gains.
table(records$capital_gain)
# The maximum outside of 99999 is 41310. This variable may need to be transformed
summary(records$capital_gain)
hist(records$capital_gain[records$capital_gain > 0], breaks = 100)
(gain <- as.data.frame.matrix(table(records$capital_gain, records$over_50k)))
gain[gain$'0' == 0 & gain$'1' > 25,] # This people all make 50k+
gain[gain$'0' > 25 & gain$'1' == 0,] # None of these people make 50k+

# Are you telling me that every single person who had a capital gain of 5013 (117) 
# made less than 50k,  but every person with a gain of 5178(146) made above 50k?? THIS IS FISHY
# Also everyone with 99999 made 50k+
# Also 410 people with 7688 and 364 people with 7298

# Histogram - after a log transformation
hist(log(records$capital_gain[records$capital_gain > 0]), breaks = 100)
length(which(records$capital_gain > 0)) # only 4035 records > 0 

# capital_loss: a cont. var. ... in the form of capital losses
table(records$capital_loss) # This is a more reasonible table
summary(records$capital_loss)
hist(records$capital_loss)
hist(records$capital_loss[records$capital_loss > 0], breaks = 100)
length(which(records$capital_loss > 0)) # only 2282 records > 0 
(loss <- as.data.frame.matrix(table(records$capital_loss, records$over_50k)))
# Again more fishy thing. People with a loss of 1848, 1977, 1887, all made 50k+
loss[loss$'0' == 0 & loss$'1' > 15,]  # This people all make 50k+
loss[loss$'0' > 15 & loss$'1' == 0,]  # None of these people make 50k+


### --- Hours per week an indoividual work --- ###
summary(records$hours_week) 
hist(records$hours_week) 
table(records$hours_week) 
# There are a lot of people claiming to work > 60 hours in a week
length(which(records$hours_week > 60)) 
(hours <- as.data.frame.matrix(table(records$hours_week, records$over_50k)))
hours$hours <- as.numeric(rownames(hours))
hours$percent <- hours$'0'/(hours$'0' + hours$'1')
# Finally a table that makes more sense, although most people with 60+ hours 
# don't make 50k+
hours[hours$hours>60,]


### - Recap on Continuous 
sapply(records[,c(1:5)], mean) 
sapply(records[,c(1:5)], sd) 

# Remove things are are no longer using!
rm(edunum, gain, hours, loss, relationship, records_db, gender)


# ----- Categorical Variables (8 of these) ----- #
makeNice <- function(column, dataframe){
    tab10 <- as.data.frame.matrix(table(dataframe[,column], dataframe$over_50k))
    tab10$percentUnder50 <- tab10$'0'/(tab10$'0' + tab10$'1')
    tab10$OverallPercent <- round((tab10$'0' + tab10$'1')/dim(records)[1],3)
    tab10 <- tab10[order(-tab10$OverallPercent),]
    tab10
}

makeNice('Country', records)
# Country - 90% USA, Mexico 2%, 2% missing, Top 5 countires make up 95% 
# makeNice('Education_Level', records) - We are using education number instead
# Education_Level - 1/3 highschool grads, 38% come college or bachelors, only 6% PHD or Masters
makeNice('Marital_Status', records)
# Marital_Status - almost 1/2 are married, 1/3 never married, 14% divorced
# married people are much more likely to make 50k+
makeNice('Job', records)
# Job - 6 jobs with 10%+, 6% missing, Specialty highest at 13% (also highest % making 50k+)
makeNice('Race', records)
# Race - 86% white, 10% black. Asians highest percent making 50k+
makeNice('Relationship', records)
# Relationship - 40% husband, 26% not in family, 16% own-child, 10% unmarried, 5% wife
# Other - 97% make less than 50k
makeNice('Sex', records)
# Sex - 2/3 Male, 1/3 female
makeNice('Work_Class', records)
# Work_Class - ~ 70% private, 6% missing, 13% local/fed/state government
# Those who don't work or are without pay don't make 50k+ (only 2)



##### ----- 6: Split data into testing, training, and validation ----- #####

# First - check is each column is the correct class
lapply(records, class)

# Convert over_50k to factor
records$over_50k <- as.factor(records$over_50k)
# Did it work?
lapply(records, class)

# Split the data. Create 48,842 random numbers between 0 and 1.
# Use this to create a 60,20,20 split
set.seed(1812)
split <- runif(48842,0,1) 
train <- records[split < .6,]
validation <- records[.6 <= split & split < .8,]
test <- records[split >= .8,]
# Did we get all records, and was it a successful split? - TRUE means yes!
dim(train)[1] + dim(test)[1] + dim(validation)[1] == 48842
# We expect the percent to be around 76% for each dataset
paste0("Training:", table(train$over_50k)[1]/ (table(train$over_50k)[1] + table(train$over_50k)[2]))
paste0("Testing:", table(test$over_50k)[1]/ (table(test$over_50k)[1] + table(test$over_50k)[2]))
paste0("Validation:", table(validation$over_50k)[1]/ 
           (table(validation$over_50k)[1] + table(validation$over_50k)[2]))


##### ----- 7: Develope a model ----- #####
### --- Let's start with the basics --- ###
### --- we expect seperation warnings --- ###
# Looking at the tables above, we had major issues with perfect seperation in several variables
# Now that we are only considering 60% of the data, we expect bigger issues
logreg <- glm(over_50k ~ ., data = train, family = "binomial")
pred <- predict(logreg, newdata=validation)
pred <- ifelse(pred > .5, 1, 0)
accuracy <- table(pred, validation[,"over_50k"])
(glm_accuracy <- sum(diag(accuracy))/sum(accuracy))
# About 84.5 Accuracy 

# It was pretty obvious we would run into problems with perfect seperation. 
# We will try to make some 'acceptible' corrections based on the tables previously explored

# To fix Work-Class: Combine Never-worked and Without-pay into category: Other_Work_Class
# This logically makes sense - we are combining to categories of people who shouldn't be making money
records2 <- records
records2$Work_Class <- ifelse(records2$Work_Class %in% c("Never-worked", "Without-pay"), 
                             "?", as.character(records2$Work_Class))
table(records2$Work_Class, records2$over_50k) # ISSUE (hopefully) Resolved
records2$Work_Class <- as.factor(records2$Work_Class)

# To fix Country - we need to be smart about combining values
# If total count < 100 & Percent 0 is > 90% - we will oomebine into category: Other_Countries
# This logically makes sense - we are looking at poor countries with few records
t <- as.data.frame.matrix(table(records$Country, records$over_50k))
t$percent <- t$'0'/(t$'0' + t$'1')
t <- t[order(t$percent),]
t <- subset(t, t$'0' + t$'1' < 100 & t$percent >= .9)
t$Country <- rownames(t)
records2$Country <- ifelse(records2$Country %in% t$Country, 
                           "Other_Countries", as.character(records2$Country))
table(records2$Country, records2$over_50k) # Issue Potentially Fixed
records2$Country <- as.factor(records2$Country)

# I had originally fixed the categorical variable education level. But when it was removed, I decided
# to do the same for eduction number
# This logically makes sence - preschool and 1-4 are both 4th grade and below (numbers 1 and 2)
records2$education_num <- ifelse(records2$education_num %in%c(1,2), 
                           2, records2$education_num)
table(records2$education_num, records2$over_50k) # Issue Potentially Fixed

### We do not expect this to fix all of our problems, but rather limit the
### amount of times that it occurs. 
### Note that perfect seperation is happening for our continuous variables as well
### The problem is that they are so random, we can't bin them to fix the issue
## Also - note that perfect seperation isn't the end of the world
## If John is not working, we can be pretty confident he isn't making 50k+


# We have updated values, reset our train, validation, and testing datasets
# We need a new train, validation, and testing dataset because we have binned columns
train2 <- records2[split < .6,]
validation2 <- records2[.6 <= split & split < .8,]
test2 <- records2[split >= .8,]


# Let's restart with the basics
# logreg <- glm(over_50k ~ ., data = train, family = "binomial")
mod <- train(over_50k ~ .,  data=train2, method="glm", family="binomial")
pred <- predict(mod, newdata=validation2)
accuracy <- table(pred, validation2[,"over_50k"])
sum(diag(accuracy))/sum(accuracy)
# Need to beat 85.2% Classification
# Grab predictions for test data
final_glm <- predict(mod, newdata=test2)
accuracy <- table(final_glm, test2[,"over_50k"])
sum(diag(accuracy))/sum(accuracy)
# 84.7% accuracy

# Just quickly note that removing any of the predictive variables, does not lower the AIC
mod2 <- glm(over_50k ~ ., data=train2, family="binomial")
backmod <- step(mod2) # default in step is backwards selection






## Create an H2O cloud 
h2o.init(nthreads=-1)
#h2o.removeAll()
# Use the original train, validation, and test datasets
# We don't need to bin any variables for these methods
trainh2o <- h2o.assign(as.h2o(train), "train.hex")
validh2o <- h2o.assign(as.h2o(validation), "valid.hex")   ## R valid, H2O valid.hex
testh2o <- h2o.assign(as.h2o(test), "test.hex") 



##### ----- RANDOM FOREST ----- #####
rf1 <- h2o.randomForest(training_frame = trainh2o, 
                        validation_frame = validh2o,
                        x=1:12,
                        y=13, 
                        model_id = "rf_v1", 
                        ntrees = 200, #  random forest model. The default is 50.
                        stopping_rounds = 3, # Stop fitting new trees when the 2-tree
                        #stopping_metric = "misclassification",
                        max_depth = 20,  # This is default, and better than 15, or 30 (I tried)
                        score_each_iteration = T,
                        seed = 1812) 

rf1@model$validation_metrics # 15.2% Error rate

final_rf1 <- as.vector(predict(rf1, testh2o)$predict)
(rf_accuracy <- rf1@model$validation_metrics@metrics$max_criteria_and_metric_scores$value[4])

##### ----- Xgboost Turn ----- #####

# Note that h2os version of xgoost was fairly slow and is thus commented out

# # We will try using h2o first
# gbm <- h2o.gbm(x = 1:12, y = 13, training_frame = trainh2o)
# h2o.auc(h2o.performance(gbm, newdata = validh2o)) #.924075
# ## Also note that the misclassification error was ~13.1%
# 
# ## Use training and validation, but Cross-Validate this time
# gbm2 <- h2o.gbm(x = 1:12, y = 13, training_frame = h2o.rbind(trainh2o, validh2o), 
#                nfolds = 4, seed = 1812)
# gbm2@model$cross_validation_metrics_summary
# h2o.auc(h2o.performance(gbm2, xval = TRUE)) #.9241104 
# ## Also not the error was ~14.5%
# 
# ## Lets try our hand at a more tuned model
# gbm3 <- h2o.gbm(
#     x = 1:12, 
#     y = 13, 
#     training_frame = trainh2o, 
#     validation_frame = validh2o,
#     ntrees = 1000, 
#     learn_rate = .01,
#     stopping_rounds = 5, 
#     stopping_tolerance = 1e-4, 
#     stopping_metric = "AUC", 
#     sample_rate = .7,
#     col_sample_rate = .5,
#     seed = 1812,
#     score_tree_interval = 10
# )
# h2o.auc(h2o.performance(gbm3, valid = TRUE)) #.9259219
# ## Also Error rate was 13.1%



## Let's try using the package designed for specifically xgboost ##
records3 <- records 
lapply(records3, class) 
# xgboost package only excepts numbers - convert all factors and intergers to numeric
for(i in 1:dim(records3)[2]){
    records3[,i] <- as.numeric(records[,i])
}
# Over_50k was turn into a variable containing 1 and 2. Change this to 0 and 1. 
records3$over_50k <- records3$over_50k - 1
# Resplit the data - We need new dataframes for train, validation, and testing because
# we changed the data to be all numerical
train3 <- records3[split < .6,]
validation3 <- records3[.6 < split & split < .8,]
test3 <- records3[split >= .8,]

tv3 <- rbind(train3, validation3)

set.seed = 1812

# Create a data matrix that xgboost can work with
dtrain <- xgb.DMatrix(as.matrix(train3[,1:12]), label=train3$over_50k)
dvalid <- xgb.DMatrix(as.matrix(validation3[,1:12]), label=validation3$over_50k)
dtest  <- xgb.DMatrix(as.matrix(test3[,1:12]), label=test3$over_50k)
dtrain2 = xgb.DMatrix(as.matrix(tv3[,1:12]), label=tv3$over_50k)  

### After many grid searches - we have arrived at the following params ###
# List of all paramaters we will use
xgb_params = list(
    colsample_bytree = 0.4,
    subsample = 0.7,
    eta = 0.01,
    eval_metric = 'error',
    objective = "binary:logistic",
    max_depth = 12,
    alpha = 1,
    gamma = 2,
    min_child_weight = 1
)

# Create the model!
res = xgb.cv(xgb_params,
            dtrain,
            nrounds=2000,
            nfold=4,
            early_stopping_rounds=100,
            print_every_n = 10)
# This will take about 1-2 minutes to run.
# Error rate: 12.37% --- 87.62% accuracy 

### Grid Search Time - After 100 iterations
best_cv_mean = Inf
best_cv_mean_index = 0
for (iter in 1:100) {
    param <- list(colsample_bytree = runif(1, .4, .7),
                  subsample = runif(1, .6, .9),
                  eta = .01,
                  eval_metric = 'error',
                  objective = 'binary:logistic',
                  max_depth = sample(6:12, 1),
                  gamma = runif(1, 0.0, 0.2), 
                  min_child_weight = 1
    )
    set.seed(1812)
    # Cross validation parameters
    res = xgb.cv(param,
                 dtrain,
                 nrounds=2000, # started at 750. Had to add more in order to lower ETA
                 early_stopping_rounds=100,
                 nfold = 4,
                 print_every_n = 40)
                 #early.stop.round = 10)
    
    best_nrounds = res$best_iteration
    cv_mean <- res$evaluation_log$test_error_mean[best_nrounds]
    
    if (cv_mean < best_cv_mean) {
        best_cv_mean = cv_mean
        best_cv_mean_index = best_nrounds
        best_param = param
    }
}
##### This was found to be the most accurate
# best_cv - .1227
# best_nrounds - 847
# best params:
# colsample_bytree = .59
# subsample = .87
# eta = .01
# eval_metric = 'error',
# objective = 'binary:logistic',
# max_depth = 6
# gamma = .09
# min_child_weight = 1

# So rerun again, and save model this time. 
param <- list(colsample_bytree = .593,
              subsample = .874,
              eta = .01,
              eval_metric = 'error',
              objective = 'binary:logistic',
              max_depth = 6,
              gamma = .091, 
              min_child_weight = 1
)
set.seed(1812)
# Cross validation parameters
res = xgb.cv(param,
             dtrain,
             nrounds=2000, # started at 750. Had to add more in order to lower ETA
             early_stopping_rounds=100,
             nfold = 4,
             print_every_n = 40)
xgb_accuracy <- 1 - res$evaluation_log$test_error_mean[res$best_iteratio]
res$best_iteration # this tells us how far to go in the final model building
#### NICE: .1223 for error

# Now we want to retrain with all the training data (not doing cross validation)
# CV was used to pick parameters
# We use best_iteration/.8 because we have more data this time, this will go a few more rounds and help improve accurary
xgbModel <- xgb.train(param, dtrain, res$best_iteration/.85) 

# Validation Accuracy
final <- predict(xgbModel,dvalid)
final <- ifelse(final >.5,1,0)
acc1 <- table(final, validation3[,"over_50k"])
sum(diag(acc1))/sum(acc1)
# Test Accuracy
final_xgb <- predict(xgbModel,dtest)
final_xgb <- ifelse(final_xgb >.5,1,0)
acc1 <- table(final_xgb, test3[,"over_50k"])
sum(diag(acc1))/sum(acc1)

### What happens if we add more data to the model
# xgbModel <- xgb.train(param, dtrain2, res$best_iteration) 
# # Test Accuracy
# final_xgb <- predict(xgbModel,dtest)
# final_xgb <- ifelse(final_xgb >.5,1,0)
# acc1 <- table(final_xgb, test3[,"over_50k"])
# sum(diag(acc1))/sum(acc1)




### Let's see if ensembling improves our accuracy any!



#### Neural Net Time ####
# My experience with neural networks has always been it takes the combination of several models to achieve a good error rate

# dnn_model_1<-h2o.deeplearning(x=1:12, y=13,
#                               model_id = "Dnn1",
#                               training_frame=trainh2o, validation_frame = validh2o,
#                               epochs=50, 
#                               stopping_rounds=2,
#                               overwrite_with_best_model=T,
#                               activation="Rectifier",
#                               distribution="AUTO",
#                               keep_cross_validation_predictions = TRUE, # We like to see how we did!
#                               hidden=c(150,150))
# # Error: ~16%. - If we combined several of these models we could probably improve this slightly
# dnn_model_1@model$validation_metrics

length(final_rf1); length(final_xgb); length(final_glm)

final_ens <- as.numeric(final_rf1) + final_xgb + as.numeric(final_glm) - 1 
# we subtract one because glm is 1's and 2's
ens <- ifelse(final_ens > 1,1,0)
final_acc <- table(ens, test3[,"over_50k"])
sum(diag(acc1))/sum(acc1)


a <- cbind(test, final_xgb)
a$correct <- ifelse(a$over_50k == a$final_xgb,1,0)



### Where did we do a good job of predicting 50k+
makeTable <- function(column, dataframe){
    tab10 <- as.data.frame.matrix(table(dataframe[,column], dataframe$correct))
    tab10$Accuracy <- round(tab10$'1'/(tab10$'1' + tab10$'0'),3)
    tab10$Percent0 <- 
    tab10 <- tab10[order(-tab10$Accuracy),]
    tab10
}
for(i in 1:12){
    col <- colnames(records)[i]
    print(makeTable(col, a))
}






### Important Chart's
imp <- as.data.frame(h2o.varimp(rf1))
plot(imp$variable,imp$scaled_importance)



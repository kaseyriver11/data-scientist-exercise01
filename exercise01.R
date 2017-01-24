

### Let's get started!
library(DBI)        # to connect to the database
library(tidyr)      # to reshape data
library(h2o)        # for random forest
library(xgboost)    # for xgboost
library(caret)

# Establish a connection to the SQLite database
connection <- dbConnect(RSQLite::SQLite(), dbname="exercise01.sqlite")
# Vector of the different table name - 1 records table, and 8 reference tables
(tables <- dbListTables(connection))

# Read the records - and take a quick look
records_db <- dbGetQuery(connection,'select * from records' )
head(records_db)

# Read refernece tables - and take a quick look
# Note that the only possible ordinal variable is education - but it is not in order
for(i in 1:length(tables)){
    if(tables[i] != "records"){
    print(dbGetQuery(connection, paste0('select * from ', tables[i])))}
}
# Or if you just want to look at one of them
(gender <- dbGetQuery(connection,'select * from sexes' ))


### 2: Write a SQL query that creates a consolidated dataset from the normalized tables
# Make one complete table - keeping all columns for now
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

### 3 - Export the flattened table to a CSV file. 
# If we are in the working directory - we don't need to specify the file path
write.csv(df, "records.csv", row.names = FALSE)

### 4 - Import the exported CSV
df <- read.csv("records.csv", sep = ",")

# We don't need duplicate columns. Trim the dataset to only columns we will use
records <- df[,c(1,2,11:23)]
# Put the column "over_50k" at the end, it is the target variables. Remove country_id variable
records <- subset(records, select=c(1:5,8:15,7))
# Save for future reference - it may be needed!
ID <- records$id 
records <- records[,2:14]

### 5: Exploratory Analysis

# ----- Target Variable ----- #
table(records$over_50k)
# ~ 76% are 0. We have a good split though. Lots of responces for both 0 and 1


# ----- Continuous Variables (4 of these) ----- #
summary(records$age)
hist(records$age)
# Histogram looks fairly normal - minimum age of 17 might have been required for census

# capital_gain: a cont. var. representing post-social insurance income, in the form of capital gains.
table(records$capital_gain)
# The maximum outside of 99,999 is 41,310. This variable may need to be transformed
summary(records$capital_gain)
hist(records$capital_gain[records$capital_gain > 0], breaks = 100)
# Histogram - after a log transformation
hist(log(records$capital_gain[records$capital_gain > 0]), breaks = 100)
length(which(records$capital_gain > 0)) # only 4035 records > 0 

# capital_loss: a cont. var. ... in the form of capital losses
table(records$capital_loss)
# This is a more reasonible table
summary(records$capital_loss)
hist(records$capital_loss)
hist(records$capital_loss[records$capital_loss > 0], breaks = 100)
length(which(records$capital_loss > 0)) # only 2282 records > 0 

#hours_week: Hours per week an individual worked
summary(records$hours_week)
hist(records$hours_week)
table(records$hours_week)
# There are a lot of people claiming to work > 60 hours in a week
length(which(records$hours_week > 60))

### - Recap on Continuous 
sapply(records[,c(2:5)], mean)
sapply(records[,c(2:5)], sd)



# ----- Categorical Variables (8 of these) ----- #
# View all table frequencies - sorted
for(i in 5:12){
    current_table <- as.data.frame(table(records[,i]))
    colnames(current_table)[1] <- colnames(records)[i]
    current_table <- current_table[order(-current_table$Freq),]
    current_table$Percent <- round(current_table$Freq / 48842,3)
    print(current_table)
}
# If you just want to view one table:
a <- as.data.frame(table(records[,c("Country")]))
a$Percent <- round(a$Freq/48842,3)
(a <- a[order(-a$Freq),])

# Note from each variables
# Country - 90% USA, Mexico 2%, 2% missing, Top 5 countires make up 96% 
# Education_Level - 1/3 highschool grads, 38% come college or bachelors, only 6% PHD or Masters
# Marital_Status - almost 1/2 are married, 1/3 never married, 14% divorced
# Job - 6 jobs with 10%+, 6% missing, Specialty highest at 13%
# Race - 86% white, 10% black
# Relationship - 40% husband, 26% not in family, 16% own-child, 10% unmarried, 5% wife
# Sex - 2/3 Male, 1/3 female
# Work_Class - ~ 70% private, 6% missing, 13% local/fed/state government


### 6: Split data into testing, training, and validation

# First - check is each column is the correct class
lapply(records, class)

# Convert over_50k to factor
records$over_50k <- as.factor(records$over_50k)
# Did it work?
lapply(records, class)

# Split the data. Create 48,842 random numbers between 0 and 1.
# Use this to create a 60,20,20 split
split <- runif(48842,0,1) 
train <- records[split < .6,]
validation <- records[.6 < split & split < .8,]
test <- records[split >= .8,]
# Did we get all records, and was it a successful split? - TRUE means yes!
dim(train)[1] + dim(test)[1] + dim(validation)[1] == 48842
# We expect the percent to be around 76% for each dataset
paste0("Training:", table(train$over_50k)[1]/ (table(train$over_50k)[1] + table(train$over_50k)[2]))
paste0("Testing:", table(test$over_50k)[1]/ (table(test$over_50k)[1] + table(test$over_50k)[2]))
paste0("Validation:", table(validation$over_50k)[1]/ 
           (table(validation$over_50k)[1] + table(validation$over_50k)[2]))


### 7: Develope a model
# Let's start with the basics - we expect seperation warnings!
logreg <- glm(over_50k ~ ., data = train, family = "binomial")

# It was pretty obvious we would run into problems with perfect seperation. 
# Where does it likely (because we will use records and not train) occur?
table(train$Work_Class, train$over_50k) # ISSUE with Never-worked, and potentially Without-pay
table(records$Country, records$over_50k) # ISSUE with multiple countries
table(records$Education_Level, records$over_50k)# ISSUE with Preschool
table(records$Marital_Status, records$over_50k) # We are oK!
table(records$Job, records$over_50k) # POTENTIAL ISSUE - let's gamble for now
table(records$Race, records$over_50k) # We are ok!

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

# To fix Education_Level: Combine Preschool and 1st-4th to category: 4th_and_below
# This logically makes sence - preschool and 1-4 are both 4th grade and below
records2$Education_Level <- ifelse(records2$Education_Level %in%c("5th-6th", "1st-4th", "Preschool"), 
                           "4th_and_below", as.character(records2$Education_Level))
table(records2$Education_Level, records2$over_50k) # Issue Potentially Fixed
records2$Education_Level <- as.factor(records2$Education_Level)

##### We do not expect this to fix all of our problems, but rather limit the
##### amount of times that it occurs. 
## Also - note that perfect seperation isn't the end of the world
## If John is not working, we can be pretty confident he isn't making 50k+


# We have updated values, reset our train, validation, and testing datasets
# We need a new train, validation, and testing dataset because we have binned columns
train2 <- records2[split < .6,]
validation2 <- records2[.6 < split & split < .8,]
test2 <- records2[split >= .9,]


# Let's restart with the basics
# logreg <- glm(over_50k ~ ., data = train, family = "binomial")
mod <- train(over_50k ~ .,  data=train2, method="glm", family="binomial")
pred <- predict(mod, newdata=validation2)
accuracy <- table(pred, validation2[,"over_50k"])
sum(diag(accuracy))/sum(accuracy)
# Need to beat 84.8% Classification

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

rf1@model$validation_metrics # 13.8% Error rate

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
                        seed = 1813) 

rf1@model$validation_metrics




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
records3$over_50k <- records3$over_50k - 1
# Resplit the data - We need new dataframes for train, validation, and testing because
# we changed the data to be all numerical
train3 <- records3[split < .6,]
validation3 <- records3[.6 < split & split < .8,]
test3 <- records3[split >= .8,]

tv3 <- rbind(train3, validation3)

set.seed = 1812

# Create a data matrix that xgboost can work with
dtrain = xgb.DMatrix(as.matrix(train3[,1:12]), label=train3$over_50k)
dtrain2 = xgb.DMatrix(as.matrix(tv3[,1:12]), label=tv3$over_50k)  

# List of all paramaters we will use
xgb_params = list(
    colsample_bytree = 0.5,
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
            early_stopping_rounds=10,
            print.every.n = 10)
#,
            #feval=xg_eval_mae,
            #maximize=FALSE)
































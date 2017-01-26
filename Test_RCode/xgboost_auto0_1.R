

records4 <- records


(gain <- as.data.frame.matrix(table(records$capital_gain, records$over_50k)))
keep <- gain[gain$'0' == 0 & gain$'1' > 25,] # This people all make 50k+
records4$auto1 <- ifelse(records$capital_gain %in% rownames(keep),1,0)
keep <- gain[gain$'0' > 25 & gain$'1' == 0,]
records4$auto0 <- ifelse(records$capital_gain %in% rownames(keep),1,0)


(loss <- as.data.frame.matrix(table(records$capital_loss, records$over_50k)))
# Again more fishy thing. People with a loss of 1848, 1977, 1887, all made 50k+
keep <- loss[loss$'0' == 0 & loss$'1' > 15,]  # This people all make 50k+
records4$auto1 <- ifelse(records4$capital_loss %in% rownames(keep),1,records4$auto1)
loss[loss$'0' > 15 & loss$'1' == 0,] 
records4$auto0 <- ifelse(records$capital_loss %in% rownames(keep),1,records4$auto0)

records4$auto0 <- ifelse(records$age %in% c(17,18),1,records4$auto0)










lapply(records4, class) 
# xgboost package only excepts numbers - convert all factors and intergers to numeric
for(i in 1:dim(records4)[2]){
    records4[,i] <- as.numeric(records4[,i])
}
# Over_50k was turn into a variable containing 1 and 2. Change this to 0 and 1. 
records4$over_50k <- records4$over_50k - 1
# Resplit the data - We need new dataframes for train, validation, and testing because
# we changed the data to be all numerical
train4 <- records4[split < .6,]
validation4 <- records4[.6 < split & split < .8,]
test4 <- records4[split >= .8,]

# Create a data matrix that xgboost can work with
dtrain4 <- xgb.DMatrix(as.matrix(train4[,1:12,14:15]), label=train4$over_50k)
dvalid4 <- xgb.DMatrix(as.matrix(validation4[,1:12,14:15]), label=validation4$over_50k)
dtest4 <- xgb.DMatrix(as.matrix(test4[,1:12,14:15]), label=test4$over_50k)


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
             dtrain4,
             nrounds=15000,
             nfold=4,
             early_stopping_rounds=100,
             print.every.n = 40)











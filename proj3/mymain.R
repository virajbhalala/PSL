set.seed(0544)
myPackages = c("glmnet", "doMC", "xgboost", "gbm")
missingPackages = setdiff(myPackages, rownames(installed.packages())) 
if (length(missingPackages) > 0) { 
    install.packages(missingPackages)
}

# Load libraries
library(xgboost, quietly = TRUE)
library(doMC, quietly = TRUE)
library(glmnet, quietly = TRUE)
library(gbm, quietly = TRUE)

normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
train = read.csv("train-1.csv")
test = read.csv("test-1.csv")
rownames(train) = train$id
rownames(test) = test$id

###train###
train$loan_status = ifelse(train$loan_status %in% c("Default", "Charged Off"), 1, 0)
train$term = ifelse(train$term == "36 months", 36, 60)
# 
train$emp_length = as.character(train$emp_length)
train$emp_length[train$emp_length == "< 1 year"] = "0 years"
train$emp_length[train$emp_length == "10+ years"] = "10 years"
train$emp_length[is.na(train$emp_length)] = "0 years"
train$emp_length = as.numeric(matrix(unlist(strsplit(train$emp_length, " ")), ncol = 2, byrow = TRUE)[, 1])

train$home_ownership[train$home_ownership == "ANY" | train$home_ownership == "NONE"] = "OTHER"
train$annual_inc = log(train$annual_inc + 1)
train$dti[is.na(train$dti)] = 0
earliest_cr_line = as.numeric(matrix(unlist(strsplit(as.character(train$earliest_cr_line), "-")), ncol = 2, byrow = TRUE)[ ,2])
train$earliest_cr_line = earliest_cr_line
train$fico = (train$fico_range_low + train$fico_range_high) / 2
colnames(train)[colSums(is.na(train)) > 0]
naCols = colnames(train)[colSums(is.na(train)) > 0]
train[naCols][is.na(train[naCols])] <- 0
train = subset(train, select = -c(X, id, emp_title, title, fico_range_low, fico_range_high, zip_code, grade))

train$revol_bal = log(train$revol_bal + 1)
normCols = c("term","int_rate", "installment", "dti",  "earliest_cr_line", "open_acc", "revol_util", "total_acc", "mort_acc", "fico", "pub_rec_bankruptcies" )
train[normCols] <- apply(train[normCols], 2, normFunc)


train.X = model.matrix(~ ., subset(train, select = -c(loan_status)))
train.Y = train$loan_status


###test###

test$term = ifelse(test$term == "36 months", 36, 60)

test$emp_length = as.character(test$emp_length)
test$emp_length[test$emp_length == "< 1 year"] = "0 years"
test$emp_length[test$emp_length == "10+ years"] = "10 years"
test$emp_length[is.na(test$emp_length)] = "0 years"
test$emp_length = as.numeric(matrix(unlist(strsplit(test$emp_length, " ")), ncol = 2, byrow = TRUE)[, 1])

test$home_ownership[test$home_ownership == "ANY" | test$home_ownership == "NONE"] = "OTHER"
test$annual_inc = log(test$annual_inc + 1)
test$dti[is.na(test$dti)] = 0
earliest_cr_line = as.numeric(matrix(unlist(strsplit(as.character(test$earliest_cr_line), "-")), ncol = 2, byrow = TRUE)[ ,2])
test$earliest_cr_line = earliest_cr_line
test$fico = (test$fico_range_low + test$fico_range_high) / 2
colnames(test)[colSums(is.na(test)) > 0]
naCols = colnames(test)[colSums(is.na(test)) > 0]
test[naCols][is.na(test[naCols])] <- 0
test = subset(test, select = -c(X, id, emp_title, title, fico_range_low, fico_range_high, zip_code, grade))

test$revol_bal = log(test$revol_bal + 1)
normCols = c("term","int_rate", "installment", "dti",  "earliest_cr_line", "open_acc", "revol_util", "total_acc", "mort_acc", "fico", "pub_rec_bankruptcies" )
test[normCols] <- apply(test[normCols], 2, normFunc)
test.X = model.matrix(~ ., test)






start.time = Sys.time()
model1 = xgboost(data = train.X ,label = train.Y,nrounds = 100 ,objective = "binary:logistic",eval_metric = "logloss",max_depth = 4,print_every_n = 1,verbose = 1)
model1.predictions = predict(model1, test.X)
write.csv(data.frame(id = rownames(test.X),
                     prob = model1.predictions),
                      "mysubmission1.txt",
                      row.names = FALSE,
                      quote = FALSE)



print(paste0("Run time: ", as.numeric(difftime(Sys.time(), start.time, units = 'min'))))



# Note: This uses a two step process.
# Step 1 performs cross-validation to find the number of iterations needed to get the minimum loss.
# Step 2 creates the final model using the nround identified in Step 1, and makes the prediction. 
# 
# Also note that I have skipped a few pre-modeling steps:
#                                   Data Exploration, Handling Outliers, Handling/Imputing Null predictors

# Load the required libraries.
library(xgboost)
library(caret)      # for confusionMatrix

set.seed(100)

#Set the parameters for cross-validation and xgboost.
#Note: This is a multi-class classification problem, and the evaluation metric is "mlogloss".
#      The same parameters are used by Step 1 and Step 2.
#      You can try different values for nthread, max_depth, eta, gamma, etc., and see if you get lower prediction error.

param       = list("objective" = "multi:softmax", # multi class classification
                   "num_class"= 24 ,  		# Number of classes in the dependent variable.
                   "eval_metric" = "mlogloss",  	 # evaluation metric 
                   "nthread" = 8,   			 # number of threads to be used 
                   "max_depth" = 16,    		 # maximum depth of tree 
                   "eta" = 0.3,    			 # step size shrinkage 
                   "gamma" = 0,    			 # minimum loss reduction 
                   "subsample" = 0.7,    		 # part of data instances to grow tree 
                   "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
                   "min_child_weight" = 12  		 # minimum sum of instance weight needed in a child 
)

#OHE
ohe_feats = c('sexo', 'nomprov', 'pais_residencia','ind_empleado','tiprel_1mes','ult_fec_cli_1t','indresi','indext','conyuemp','canal_entrada','indfall','segmento')
dummies <- dummyVars(~ sexo +  nomprov + pais_residencia  +ind_empleado + tiprel_1mes+ult_fec_cli_1t +indresi +indext + conyuemp+ canal_entrada+indfall +segmento , data = df2)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df2))
df_all_combined <- cbind(df2[,-c(which(colnames(df2) %in% ohe_feats))],df_all_ohe)
df_all_combined <- sapply( df_all_combined, as.numeric )

#Identify the Predictors and the dependent variable, aka label.
predictors = colnames(df_all_combined[-10])

#Alas, xgboost works only if the numeric labels start from 0. Hence, subtract 1 from the label.
label = as.numeric(df_all_combined[,10])-1
print(table (label))

#########################################################################################################
# Step 1: Run a Cross-Validation to identify the round with the minimum loss or error.
#         Note: xgboost expects the data in the form of a numeric matrix.

x<- sample(nrow(df),10000)

df_cv <- df_all_combined[x,]

set.seed(100)

cv.nround = 200;  # Number of rounds. This can be set to a lower or higher value, if you wish, example: 150 or 250 or 300  
bst.cv = xgb.cv(
  param=param,
  data = as.matrix(df_cv[,predictors]),
  label = label,
  nfold = 3,
  nrounds=cv.nround,
  prediction=T)

#Find where the minimum logloss occurred
min.loss.idx = which.min(bst.cv$dt[, test.mlogloss.mean]) 
cat ("Minimum logloss occurred in round : ", min.loss.idx, "\n")

# Minimum logloss
print(bst.cv$dt[min.loss.idx,])

##############################################################################################################################
# Step 2: Train the xgboost model using min.loss.idx found above.
#         Note, we have to stop at the round where we get the minumum error.
set.seed(100)

#temp
#min.loss.idx = 199

bst = xgboost(
  param=param,
  data =as.matrix(df_training2[,predictors]),
  label = label,
  nrounds=min.loss.idx)

# Make prediction on the testing data.
pred = predict(bst, as.matrix(df_test2[,predictors]))
actual <- df_test2[,55]-1
results <- data.frame(pred, actual)

sample_sub <- read.csv("sampleSubmission.csv")

test_kaggle2 <- sapply( test_kaggle, as.numeric )
pred = predict(bst, as.matrix(test_kaggle2[,predictors]))
results <- data.frame( test_kaggle$Id, pred+1)
colnames(results) <- c("Id", "Cover_Type")
write.csv(results,"submission_1.csv", row.names = FALSE)
#accuracy
nrow(results[results$actual==results$pred,])/nrow(results)

#Compute the accuracy of predictions.
confusionMatrix( results$actual,results$pred)


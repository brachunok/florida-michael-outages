# Improvement in model quality by group

library('randomForest')
library('Metrics')

setwd("~/Documents/equity/code/")
load("../data/all_data_clean.Rdata")
df<- merged
df$res1Binned <- as.factor(df$res1Binned)
# FOLDS 
set.seed(2)
FOLDS= 8
sample_rows <- sample(c(1:FOLDS),size = nrow(df),replace=T)

# make an output dataframe to store errors
cv_outputs <- expand.grid(c(1:FOLDS),c("exposure","subj","inclu","trans"))
cv_outputs$ISaccuracy <- NA
cv_outputs$OOSaccuracy  <- NA

# First make a baseline prediction of engineering resilience using storm exposure ------------------
vars_1 <- c("storm_exposure","storm_dist")

for( k in 1:FOLDS){
  
  x_train <- df[which(sample_rows!=k),which(names(df)%in%vars_1)]
  x_test  <- df[which(sample_rows==k),which(names(df)%in%vars_1)]

  y_train <- df$res1Binned[which(sample_rows!=k)]  
  y_test  <- df$res1Binned[which(sample_rows==k)]

  rf <-randomForest(x=x_train,y=y_train)
  
  # get cm
  cm <- rf$confusion[1:3,1:3]
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class
  
  cv_outputs$ISaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="exposure")] <- sum(diag) / n 
  
  #cv_outputs$Rsq[which(cv_outputs$Var1==k&cv_outputs$Var2=="exposure")] <- 
  #  cor(y_test,predict(rf,newdata = x_test))^2
  pred <- predict(rf,newdata=x_test,type="response")

  cm_oos <- table(y_test,pred)
  n = sum(cm_oos) # number of instances
  nc = nrow(cm_oos) # number of classes
  diag = diag(cm_oos) # number of correctly classified instances per class 
  rowsums = apply(cm_oos, 1, sum) # number of instances per class
  colsums = apply(cm_oos, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class

  cv_outputs$OOSaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="exposure")] <- sum(diag)/n  
}

# then make a prediction based on static 'subjectivities' 

vars_2 <- c(vars_1,"fracWhite","fracBlack","fracCitizens","fracMovedFromOutOfState1y","fracDifferentCountySameState1yr",
            "fracBatchelor",
            "fracSSI","climate_base")

for(k in 1:FOLDS){
  
  x_train <- df[which(sample_rows!=k),which(names(df)%in%vars_2)]
  x_test  <- df[which(sample_rows==k),which(names(df)%in%vars_2)]
  
  y_train <- df$res1Binned[which(sample_rows!=k)]  
  y_test  <- df$res1Binned[which(sample_rows==k)]
  
  rf <-randomForest(x=x_train,y=y_train)
  
  # get cm
  cm <- rf$confusion[1:3,1:3]
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class
  
  cv_outputs$ISaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="subj")] <- sum(diag) / n 
  
  #cv_outputs$Rsq[which(cv_outputs$Var1==k&cv_outputs$Var2=="exposure")] <- 
  #  cor(y_test,predict(rf,newdata = x_test))^2
  pred <- predict(rf,newdata=x_test,type="response")
  
  cm_oos <- table(y_test,pred)
  n = sum(cm_oos) # number of instances
  nc = nrow(cm_oos) # number of classes
  diag = diag(cm_oos) # number of correctly classified instances per class 
  rowsums = apply(cm_oos, 1, sum) # number of instances per class
  colsums = apply(cm_oos, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class
  
  cv_outputs$OOSaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="subj")] <- sum(diag)/n  
  
}

# then make a prediction based on subjectivities, inclusion and scale, 
vars_3 <- c(vars_2,"fracWhite_I","fracBlack_I","fracCitizens_I","fracMovedFromOutOfState1y_I",
            "fracDifferentCountySameState1yr_I","fracBatchelor_I","fracSSI_I","climate_base")

for(k in 1:FOLDS){
  
  x_train <- df[which(sample_rows!=k),which(names(df)%in%vars_3)]
  x_test  <- df[which(sample_rows==k),which(names(df)%in%vars_3)]
  
  y_train <- df$res1Binned[which(sample_rows!=k)]  
  y_test  <- df$res1Binned[which(sample_rows==k)]
  
  rf <-randomForest(x=x_train,y=y_train)
  
  
  # get cm
  cm <- rf$confusion[1:3,1:3]
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class
  
  cv_outputs$ISaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="inclu")] <- sum(diag) / n 
  
  #cv_outputs$Rsq[which(cv_outputs$Var1==k&cv_outputs$Var2=="exposure")] <- 
  #  cor(y_test,predict(rf,newdata = x_test))^2
  pred <- predict(rf,newdata=x_test,type="response")
  
  cm_oos <- table(y_test,pred)
  n = sum(cm_oos) # number of instances
  nc = nrow(cm_oos) # number of classes
  diag = diag(cm_oos) # number of correctly classified instances per class 
  rowsums = apply(cm_oos, 1, sum) # number of instances per class
  colsums = apply(cm_oos, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted class
  
  cv_outputs$OOSaccuracy[which(cv_outputs$Var1==k&cv_outputs$Var2=="inclu")] <- sum(diag)/n  
  
}


# finally all of these AND transformat
boxplot(ISaccuracy~Var2,data=cv_outputs)
boxplot(OOSaccuracy~Var2,data=cv_outputs)


# in all cases, none of it is particularly helpful in making predictions 

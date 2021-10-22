# read in the indicator data, select what we want, and build our predictive mode

options(scipen=9999)
library('tidycensus')
library('tidyverse')
library('SpatialML')
library('housingData')
library("ggplot2")
theme_set(theme_bw())
library("sf")
library('maps')
library(hurricaneexposure)
library(hurricaneexposuredata)
library("rnaturalearth")
library("rnaturalearthdata")
library('naturalhearthhires')
library('stringr')
library("ggspatial")
library('glmnet')
library('Metrics')
library('dplyr')
options(java.parameters = "-Xmx10g")
library('bartMachine')


load("../data/all_data_clean.Rdata")
df<- merged

x <- df[,-c(1,2)]

# FOR NOW remove ALL THE I variables
x <- x[,-grep("_I",names(x))]
x <- x[,-c(56,59,60,63)]
x <- x[,which(colnames(x)!="frac_commute_under30")]
y <- df$res1

# do a feature selection with VSURF
library('VSURF')

vs1 <- VSURF(x=x,y=y)
#x <- x[,vs1$varselect.thres]
vsurf_imp_vars <- names(x)[vs1$varselect.thres]
vsurf_very_important <- names(x)[vs1$varselect.pred]

# do a cross validation for folds, report results and RMSE and see which is the best 
#
# test: NULL, LM, GLM, RF, BART, lasso?, 
FOLDS <- 5
model_list <- c("null","lm","GLM","lasso","RF","BART")

cv_outputs <- expand.grid(c(1:FOLDS),model_list)
cv_outputs$RMSE <- NA
cv_outputs$Rsq  <- NA

set_bart_machine_num_cores(10)
set.seed(1)
sample_rows <- sample(c(1:FOLDS),size = nrow(x),replace=T)

for(i in 1:FOLDS){
  print(paste0("FOLD IS: ",i))
  # sample training/test
  x_train <-  x[which(sample_rows!=i),]
  x_test  <- x[which(sample_rows==i),]
  
  y_train <- y[which(sample_rows!=i)]
  y_test  <- y[which(sample_rows==i)]
  
  training <- cbind(y_train,x_train)
  testing  <- cbind(y_test,x_test)
  
  # build Null
  model_null <- lm(y_train~1,data=x_train)
  
  # build LM
  model_lm <- lm(y_train~.,data=training)
  
  #TODO: i'm here, trying to find the NAs in my new county data and go from there
  
  # step-trim it 
  #model_lm <- step(model_lm,direction="forward")
  
  # build GLM
  model_glm <- glm(y_train~.,data=training)
  
  # build RF
  model_rf <- randomForest(y_train~.,data=training)
  
  # build BART
  model_bart <- bartMachine(X = x_train,y=y_train,serialize = T)
  
  # build LASSO
  model_lasso <- glmnet(x=as.matrix(x_train),y=y_train,family="gaussian",alpha=1)
  
  # evaluate test 
  cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="null")] <- 
    rmse(y_test,predict(model_null,newdata = x_test))
  
  cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="null")] <- 
    cor(y_test,predict(model_null,newdata = x_test))^2
  
  cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="lm")] <- 
    rmse(y_test,predict(model_lm,newdata = x_test))
  
  cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="lm")] <- 
    cor(y_test,predict(model_lm,newdata = x_test))^2
  
  cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="GLM")] <- 
    rmse(y_test,predict(model_glm,newdata = x_test))
  
  cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="GLM")] <- 
    cor(y_test,predict(model_glm,newdata = x_test))^2
  
  #cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="lasso")] <- 
  #  rmse(y_test,predict(model_lasso,newx = as.matrix(x_test)))
  
  # pull out best lambda
  
  #cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="lasso")] <- 
  #  cor(y_test,predict(model_lasso,newx = as.matrix(x_test)))^2
  
  cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="RF")] <- 
    rmse(y_test,predict(model_rf,newdata = x_test))
  
  cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="RF")] <- 
    cor(y_test,predict(model_rf,newdata = x_test))^2
  
  cv_outputs$RMSE[which(cv_outputs$Var1==i&cv_outputs$Var2=="BART")] <- 
    rmse(y_test,predict(model_bart,new_data = x_test))
  
  cv_outputs$Rsq[which(cv_outputs$Var1==i&cv_outputs$Var2=="BART")] <- 
    cor(y_test,predict(model_bart,new_data = x_test))^2
  
}

par(mfrow=c(1,2))
boxplot(RMSE~Var2,data=cv_outputs,main="Out of sample RMSE for each fold for each model")
boxplot(Rsq~Var2,data=cv_outputs, main="out of sample Rsquared for each model")

# so it's either RF or BART 
# tune and doublecheck
set_bart_machine_num_cores(10)
bm1 <- bartMachineCV(X=x,y=y)
vs2 <- var_selection_by_permute_cv(bm1,alpha=0.1)
vs3 <- investigate_var_importance(bm1)

# ^^ these change a bit when the model is re-run but we end up typically with the following
#bm_imp_vars <- c("fracCarTruckVanAloneCommute","fracDifferentCountySameState1yr",
#                 "fracWithCellOnly", "incomeDeficitFemale"  , "incomeDeficitTotal" ,
#                 "storm_dist"  ,"storm_exposure" ,"mdc1yr_less_than_10k","mdc1yr_35k_to_50k"   )

bm_imp_vars <- unique(names(vs3$avg_var_props[1:20]))
bm_very_important <- vs2$important_vars_cv
# get rid of income deficit female because we already have one

bm_imp_vars <- bm_imp_vars[which(bm_imp_vars!="incomeDeficitFemale")]

# i'm going to remove incomeDeficitFemale because we already have income deficit total
variable_pool <- unique(c(bm_imp_vars,vsurf_imp_vars))

# number of partials to plot
PARTIALS <-40
NROW <- 8
NCOL <- 5

par(mfrow=c(NROW,NCOL))
for(i in 1:PARTIALS){
  pd_plot(bm1,j=variable_pool[i])
}





#result is variable selection and a trimmed dataset
df <- df[,which(colnames(df)%in%variable_pool)]

df <- cbind(df,merged[,c(1,2)],merged$res1)

df <- cbind(df,merged$storm_exposure,merged$storm_dist)
save(df,file = "../data/reduced_data_clean_FINAL.Rdata")

# check for improved model quality

# model is pretty good 
######################Super Learner################################
###metal denotes the training dataset containing all variables##### 
###chem denotes chemicals we are interested from the training set.#  
###y denotes the continuous outcome,that is GGT in this study, from the training set.# 
###covs denotes the covariates we want to adjust in the model, from the training set.#



###Install related packages.
install.packages("gam", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("ipred", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("SuperLearner", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("car", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("missForest", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("itertools", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("iterators", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("glmnet", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("e1071", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("caret", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )
install.packages("xgboost", Sys.getenv("R_LIBS_USER"), repos = "http://cran.case.edu" )

options( java.parameters = "-Xmx15g" )
library(caret)
library(e1071)
library(glmnet)
library(bartMachine)
library(SuperLearner)
library(gam)
library(ipred)
library(rJava)
library(bartMachineJARs)
library(car)
library(missForest)
library(itertools)
library(iterators)
library(xgboost)



fit=lm(LBXSGTSI~riagendr+ridageyr+index_race2+index_race3+index_race4+index_race5
+bmxbmi+index_smk1+index_smk2+index_edu2+index_edu3+URXUCR, data=sc_metal[sampling1,])


fit=lm(y~covs)
residual_y=resid(fit)

residual_chem=data.frame(id=1:nrow(chem))
for (i in 1:ncol(chem)){
	fit_temp=lm(chem[,i]~covs)
	residual_chem[,i]=resid(fit_temp)
}

### generate Library and run Super Learner
SL.library <- c("SL.bartMachine","SL.caret", "SL.randomForest","SL.glm",  "SL.gam",  "SL.glm.interaction",  "SL.stepAIC" ,"SL.glmnet","SL.xgboost",  
"SL.ipredbagg",  "SL.nnls","SL.ridge","SL.svm")

###residual_chem_test denotes the residuals of chemicals adjusting for 
###covariates from the testing set. 
test_predict <- SampleSplitSuperLearner(Y = residual_y, X =residual_chem, newX =residual_chem_test, SL.library = SL.library, verbose = FALSE, method = "method.NNLS")

ERSL_SL_train=predict(test_predict,newdata=residual_chem)$pred
write.csv(ERSL_SL_train,"SL_train.csv",row.names = FALSE)

ERSCV_SL_test<-test_predict$SL.predict
write.csv(ERSCV_SL_test,"SL_test.csv",row.names = FALSE)





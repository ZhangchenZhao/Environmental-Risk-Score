###########Bayesian Additive Regression Tree##################
#metal denotes the training dataset containing all variables## 
###chem denotes chemicals we are interested from the training set.#  
###y denotes the continuous outcome,that is GGT in this study, from the training set.# 
###covs denotes the covariates we want to adjust in the model, from the training set.#


fit=lm(LBXSGTSI~riagendr+ridageyr+index_race2+index_race3+index_race4+index_race5
+bmxbmi+index_smk1+index_smk2+index_edu2+index_edu3+URXUCR, data=sc_metal[sampling1,])


fit=lm(y~covs)
residual_y=resid(fit)

residual_chem=data.frame(id=1:nrow(chem))
for (i in 1:ncol(chem)){
	fit_temp=lm(chem[,i]~covs)
	residual_chem[,i]=resid(fit_temp)
}


###residual_chem_test means the residuals of chemicals adjusting for 
###covariates from the testing set. 
library(BayesTree)
barfit=bart(residual_chem,residual_y,residual_chem_test,ndpost=200)


ERS_BART_test=barfit$yhat.test.mean
ERS_BART_train=barfit$yhat.train.mean
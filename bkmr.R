##########Bayesian kernel machine regression #################
#metal_all denotes the whole dataset containing all variables#
#metal denotes the training dataset containing all variables## 
###chem denotes chemicals we are interested from the training set.#
###chem_test denotes the chemicals from the testing set.######  
###y denotes the continuous outcome,that is GGT in this study, from the training set.# 
###covs denotes the covariates we want to adjust in the model, from the training set.#


library(bkmr)
set.seed(111)
fitkm=kmbayes(y, chem, covs, iter = 5000, verbose = FALSE, varsel = TRUE)


beta=ExtractEsts(fitkm)$beta[,1]
beta
###htrain is the ERS we constructed in the training set.
htrain=ExtractEsts(fitkm)$h[,1]
write.csv(htrain,file="htrain.csv")

#########htest is the ERS we constructed in the testing set.
htest=ComputePostmeanHnew(fitkm,Znew=chem_test)$postmean
write.csv(htest,file="htest.csv") 







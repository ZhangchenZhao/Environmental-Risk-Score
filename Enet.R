###################Adaptive Elastic Net#######################
#metal denotes the training dataset containing all variables## 
###chem denotes chemicals we are interested from the training set.#  
###y denotes the continuous outcome,that is GGT in this study, from the training set.# 
###covs denotes the covariates we want to adjust in the model, from the training set.#


##############################################################
#############elastic net with main effect only################
##############################################################
library(gcdnet)
chemcov=cbind(chem,covs)

x=data.matrix(chemcov)

###lambda2 is chosen based on the minimum cv error
###hence lambda2 can be a very large list
lambda2.list=seq(0.01,0.09,by=0.01)         
###lambda2.list=10^-(seq(1,13,by=1))  

num.lambda2=length(lambda2.list)
min.cv=c()
weight=c(rep(1,ncol(chem)),rep(0,ncol(covs)))
for(i in 1:num.lambda2){
	cv.enet=cv.gcdnet(x,y,foldid=foldid,lambda2=lambda2.list[i],pf=weight,method="ls")
	min.cv[i]=min(cv.enet$cvm) 
}
lambda2.opt=lambda2.list[which.min(min.cv)]
lambda2.opt
lambda1.list=cv.enet$lambda

cv.enet=cv.gcdnet(x,y,foldid=foldid,lambda2=lambda2.opt,pf=weight,method="ls")
plot(cv.enet)
lambda1.opt=lambda1.list[which.min(cv.enet$cvm)]

fit.enet=gcdnet(x,y,lambda=lambda1.opt,lambda2=lambda2.opt,pf=weight,method="ls")
beta=coef(fit.enet)
coef(fit.enet)
beta.enet=beta[2:(ncol(chem)+1)]


##############################################################
#######adaptive elastic net with main effect only#############
##############################################################
##gamma
num.obs=nrow(chem)
num.x=ncol(chem)
v.star=log(num.x)/log(num.obs)
gamma=ceiling(2*v.star/(1-v.star))+1
gamma 

##beta.enet.star
library(matrixStats)
col.x=colnames(chem)
x.sd=colSds(as.matrix(chem))*((num.obs-1)/num.obs)^0.5#sample sd for each predictor 

beta.enet.star=beta.enet*x.sd
###set the weight of covariates as zero
###in order to force the covariates in the model without shrinkage
weight=(abs(beta.enet.star)+1/num.obs)^(-gamma)
weight[(num.x+1):ncol(covs)+num.x]=0

lambda2.list=seq(0.01,0.1,by=0.01)
##lambda2.list=10^-(seq(1,6,by=1))

num.lambda2=length(lambda2.list)
min.cv=c()
for(i in 1:num.lambda2){
	cv.enet=cv.gcdnet(x,y,foldid=foldid,lambda2=lambda2.list[i],pf=weight,method="ls")
	min.cv[i]=min(cv.enet$cvm) 
}
lambda2.opt=lambda2.list[which.min(min.cv)]
lambda2.opt
lambda1.list=cv.enet$lambda
cv.enet=cv.gcdnet(x,y,foldid=foldid,lambda2=lambda2.opt,pf=weight,method="ls")
plot(cv.enet)  
lambda1.opt=lambda1.list[which.min(cv.enet$cvm)]
fit.enet=gcdnet(x,y,lambda=lambda1.opt,lambda2=lambda2.opt,pf=weight,method="ls")
coef(fit.enet)

ERS_AENET_M_train=as.matrix(chem)%*%coef(fit.enet)[2:ncol(chem)+1]

###Note: this is the ERS we constructed in the training set.
###Suppose chem_test denotes the testing set, then we can construct ERS by

ERS_AENET_M_test=as.matrix(chem_test)%*%coef(fit.enet)[2:ncol(chem)+1]

##############################################################
##################generate the interactions###################
##############################################################
inters=chem
flag=1
for (i in 1:(ncol(chem)-1)){ 
	for (j in (i+1):ncol(chem)){ 
		inters[,flag]=chem[,i]*chem[,j]
		flag=flag+1
	}
}

###ERS_AENET_I can also be done similary with the code above###
###The user just need to replace the chem with inters.#########


y=y
X=X
mu_0=rep(0,14)
Omega_0=1e2*diag(14)
v_0=1
sigma2_0=4^2
nIter=10000

## model
BayesLinReg(y, X, mu_0, Omega_0, v_0, sigma2_0, nIter)
beta_val<- BayesLinReg(y, X, mu_0, Omega_0, v_0, sigma2_0, nIter)$betaSample
## A


for (i  in 1:ncol(X)) {
  print(paste0("The 95% lower CI for beta_",i-1," is ", round(quantile(beta_val[,i],c(.05)),3), " and Upper CI is ",round(quantile(beta_val[,i],c(.95)),3)))
}

mean(beta_val[,2])
paste0("The 95% lower CI for beta_",i-1," is ", round(quantile(beta_val[,2],c(.05)),3), " and Upper CI is ",round(quantile(beta_val[,2],c(.95)),3))

# 

## B
sigma2_val<- BayesLinReg(y, X, mu_0, Omega_0, v_0, sigma2_0, nIter)$sigma2Sample

#mean
mean(sqrt(sigma2_val))

#median
median(sqrt(sigma2_val))


## C

X_new<-as.matrix(data.frame(intercept=1,crim=seq(min(X[,2]),max(X[,2]),by=.1),zn=40,indus=1.5,
                     chas=0,nox=.5,rm=6,age=30,dis=5,rad=3,tax=300,ptratio=17,black=390,
                     lstat=4))

lower<-c()
upper<-c()
for (i in 1:nrow(X_new)) {
  res<- beta_val%*%X_new[i,]
  lower[i]<-quantile(res,c(.05))
  upper[i]<-quantile(res,c(.95))
}

plt_data<- data.frame(x=seq(min(X[,2]),max(X[,2]),by=.1),lower=lower,upper=upper)

plt <- ggplot(plt_data,aes(x=x)) +
  geom_ribbon(aes(ymin = lower, ymax = upper)
              , alpha = 0.8,fill = "#EDC948")+
  labs(x = 'x_1',title ='. Compute 95% equal tail posterior probability intervals'
       ,color = "Line Legend") +
  scale_color_manual(values = c("#14213D","#59A14F")
                     , labels = c("2","3"))+
  theme(legend.position="bottom")
plt


## D
beta_val<- BayesLinReg(y, X, mu_0, Omega_0, v_0, sigma2_0, nIter)$betaSample

y_pred<-c()

for (i in 1:10000) {
  y_pred_val<-(beta_val[i,]%*%XNewHouse) + rnorm(1,mean = 0,sd=sqrt(sigma2_val[i]))
  y_pred[i]<-y_pred_val
}

hist(y_pred)
mean(y_pred>=20)

## E



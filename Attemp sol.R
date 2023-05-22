library(ggplot2)
#### Question A ####
# In this question we have alpha = 2326 and beta = 7
# We use the rgamma function to generate the 10000 draws by spesifiny n ) 10000 and 
# And shape = alpha and Scale = Beta.
rand_var<-data.frame(x=rgamma(10000,2326,7))
# We plote the our histogram:
ggplot() +
  geom_histogram(data=rand_var,aes(x = x,y=..density..),linetype=1,
                 fill='#14213D')+
  geom_density(data=rand_var,aes(x=x), color='#FCA311', size=1,
               fill='#FCA311',alpha=.5)+
  labs(title = "Posterior predictive distribution",
       subtitle = " of the demanded quantity in the next month",
       x = "Demanded",
       y = "Density") + theme_classic()

# Given the info we have from the alpha and beta values we can plug this into the function pgamma
# which gives the pgamma gives the distribution function and then we find the complement probability 
q6= 350
# the complement probability
1-pgamma(q6,2326,7)

# Given from the example help file 
utility_func <- function(a,Q6){
  util = rep(0,length(Q6))
  util[Q6<=a] = 15*Q6[Q6<=a]-(a-Q6[Q6<=a])
  util[Q6>a] = 15*a-0.1*(Q6[Q6>a]-a)^2
  return(util)
}

# We define the inital value we have 
initVal=0
# We define the value of Q6 by taking the mean of the 10000 gnerated values 
Q6=mean(rgamma(10000,2326,7))
# We use function optim to find the optimal value of Q6
OptimRes <- optim(initVal,utility_func,Q6,gr=NULL,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

##############################################################################

# Q2

source("ExamData.R")
rm(list = ls())
source("ExamData.R")
                  
library(rstan)   #install package
options(mc.cores = parallel::detectCores()) #tell Stan to use multiple cores
#setwd('c:/users/greg/desktop/stan_examples/')  #setwd(<wd must contain .stan file>)
setwd('d:/dropbox/teaching/stan_examples/')  #setwd(<wd must contain .stan file>)

library(viridis)
#######################################################
## Setup your data ####################################
#######################################################
P  <- 12 #number of groups
p  <- 10 #number of individuals within group
n  <- sample(30:100,p*P,replace=TRUE) #data points per location
pi <- c(0,cumsum(n)) #vector index for dataset stacked as single column
Pi <- pi[seq(1,length(pi),p)]
N  <- sum(n)
y  <- numeric(N) #allocate space for y
x  <- numeric(N) #allocate space for x

beta1_u    <- 0.2 #slope for synthetic data
beta0_u    <- 1.0
beta1_g_sd <- 2.5
beta0_g_sd <- 2.0
beta1_g    <- rnorm(P,mean=beta1_u,sd=beta1_g_sd)
beta0_g    <- rnorm(P,mean=beta0_u,sd=beta0_g_sd)
beta1_i_sd <- 0.5
beta0_i_sd <- 0.2
beta1_i    <- rnorm(p*P,mean=rep(beta1_g,each=p),sd=beta1_i_sd)
beta0_i    <- rnorm(p*P,mean=rep(beta0_g,each=p),sd=beta0_i_sd)

sigma_y    <- 0.25 #standard deviation for the measurement error
sigma_x    <- 0.5
k <- 1

for(i in 1:P){
	for(j in 1:p){
		xtmp <- rnorm(n[k],sd=sigma_x)
		y[(pi[k]+1):pi[k+1]] <- beta0_i[k] + beta1_i[k]*xtmp + rnorm(n[k],sd=sigma_y) 
		x[(pi[k]+1):pi[k+1]] <- xtmp
		k <- k + 1
	}
}

#############################
## Make a plot ##############
#############################

cols <- viridis(p*P)

par(mfrow=c(1,1)) 
#plot(x,y)
k <- 1
plot(-999,xlim=range(x),ylim=range(y))
for(i in 1:P){
	for(j in 1:p){
		points(x[(pi[k]+1):pi[k+1]], y[(pi[k]+1):pi[k+1]],col=adjustcolor(cols[k],alpha.f=0.4),pch=19)
		abline(lm(y[(pi[k]+1):pi[k+1]] ~ x[(pi[k]+1):pi[k+1]]),col=adjustcolor(cols[k],alpha.f=0.4))
	    k <- k + 1	
	}
}



#######################################################
## Fit Stan model #####################################
#######################################################
mod <- stan_model('d:/dropbox/teaching/ml_essg/hierarchical_regression.stan')  #pre-compile

data <- list(P=P,p=p,N=N,pi=pi,n=n,y=y,x=x)


opt <- optimizing(mod, data=data, algorithm='Newton')  #fit model
opt$par[grep("beta1_g",names(opt$par))]

plot(

mcmc <- sampling(mod, data=data, iter=2000, warmup=1000, open_progress=TRUE)  #fit model
post <- extract(mcmc)

names(post)
post$beta1_u


#######################################################
## Analyze Stan output ################################
#######################################################
post <- extract(fit)   #extract samples
par(mfrow=c(3,3))
for(i in 1:p){
	hist(post$beta0[,i],main='',xlab=i)
		abline(v=beta0t[i],lwd=2); abline(v=quantile(post$beta0[,i],c(0.025,0.975)),lty=2)}
hist(post$beta1,main=''); 
	abline(v=-beta1t,lwd=2); abline(v=quantile(post$beta1,c(0.025,0.975)),lty=2)
hist(post$beta0mean,main=''); 
	abline(v=0,lwd=2); abline(v=quantile(post$beta0mean,c(0.025,0.975)),lty=2)
hist(post$beta0_sd,main='')
	abline(v=beta0sdt,lwd=2); abline(v=quantile(post$beta0_sd,c(0.025,0.975)),lty=2)	


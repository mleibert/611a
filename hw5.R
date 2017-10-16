x<-c(169.14353,135.73850,102.46566,80.91151,148.45425,144.68948,
	106.56257,104.83559,94.81216,109.47048,95.94150,123.84673,
	87.18401,104.73420,111.94364,119.69467,151.77627,81.80692,116.58660,
	98.28933)
plot(sort(x),1:length(x))


##Incorporate everything into 1 code and run n iterations
## set the initial guesses for the distribution parameters
mu_1 <- 100
mu_2 <- 120
sd_1<-sqrt(20)
sd_2<-sqrt(25)
## as well as the latent variable parameters
tau_1 <- 0.3
tau_2 <- 1-tau_1 
for( i in 1:12 ) {
  ## Given the observed data, as well as the distribution parameters,
  ## what are the latent variables?
  T_1 <- tau_1 * dnorm( x, mean=mu_1 ,sd=sd_1 )
  T_2 <- tau_2 * dnorm( x, mean=mu_2 ,sd=sd_2 )
  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters
  mu_1 <- sum( P_1 * x ) / sum(P_1)
  mu_2 <- sum( P_2 * x ) / sum(P_2)
  print( c(mu_1, mu_2, mean(P_1)) )}


x<-c(4.1,2.2,3.9,7.1,6.2)
y<-c(0,1,0,1,1)
NR<-function(x,y,theta)
 {eps<-1e-6
  d<-1
  thetahat<-theta
  while(d>eps)
    {eta<-theta*x
     U<-sum(x*y)-sum(x*exp(eta)/(1+exp(eta)))
     I<-sum(x^2*exp(eta)/(1+exp(eta))^2)
     thetanew<-theta+(U/I)
     d<-abs(thetanew-theta)
     theta<-thetanew
     thetahat<-c(thetahat,theta)}
return(thetahat)}


rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

ps<-read.table("patientsatisfaction.txt")
head(ps)
colnames(ps)<-c("Y","X1","X2","X3" )

pairs(ps)
cor(ps[,-1])

lm(ps[,1]~ps[,2]+ps[,3]+ps[,4]+ps[,5])

#resids
ps.n<-nrow(ps)
ps.Y<-as.matrix(ps[,1])
ps.X<-as.matrix( data.frame(1,ps[,2:ncol(ps)]) )
ps.H<-ps.X %*% solve(t(ps.X)%*%ps.X) %*% t(ps.X)
ps.ee<-(diag(ps.n)-ps.H) %*% ps.Y
as.vector(ps.ee)
ps.p<-dim(ps.X)[2]


boxplot(ps.ee)

 (anova(  lm(ps[,1]~ps[,2]+ps[,3]+ps[,4] ) ))

ps.J<-matrix(1,ps.n,ps.n)
ps.MSR<-(t(ps.Y)%*%(ps.H-(1/ps.n)*ps.J)%*%ps.Y)/(ps.p-1)
ps.MSE<-t(ps.ee)%*%(ps.ee)/(ps.n-ps.p)

#t(ps.b) %*% t(ps.X) %*%ps.Y - (1/ps.n)*t(ps.Y)%*%ps.J%*%ps.Y


#ps.b<-matrix(as.numeric(lm(ps[,1]~ps[,2]+ps[,3]+ps[,4]+ps[,5]
#	)[[1]][1:5]),5,1)


ps.MSR/ps.MSE
ps.alpha<-.05
qf(1-ps.alpha,ps.p-1,ps.n-ps.p)

1-pf(  1-ps.alpha,3,ps.n-ps.p)

if( ps.MSR/ps.MSE > qf(1-ps.alpha,ps.p-1,ps.n-ps.p) ){print("conclude Ha")
	} else {print("conclude H0")}
#0.4878


#c
ps.b<- solve(t(ps.X)%*%ps.X)%*%(t(ps.X)%*%ps.Y)
ps.b
ps.sb<-as.numeric(diag(sqrt((as.numeric(ps.MSE)*solve(t(ps.X)%*%ps.X)))))
ps.sb
ps.g<-4

ps.B<-qt(  1-((ps.alpha)/(2*ps.g)) ,ps.n-ps.p)

ps.b[-1]-ps.sb[-1]*ps.B
ps.b[-1]+ps.sb[-1]*ps.B

summary ( (  lm(ps[,1]~ps[,2]+ps[,3]+ps[,4] ) ) )


(t(ps.Y)%*%(ps.H-(1/ps.n)*ps.J)%*%ps.Y)/
t(ps.Y)%*%(diag(ps.n)-(1/ps.n)*ps.J )%*%ps.Y

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))

summary(lm(ps$Y~ps$X1+ps$X2+ps$X3))


#7.5b
#SSR(X3|X1,X2)
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))
#F*
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][3]/(
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))




qf(1-.025,1,(ps.n-4))

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][3]/(
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))<qf(1-.025,1,(ps.n-4))
#Conclude H0

anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))
anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[5]][3]
#Conclude H0






#SSR(X2,X3|X1)
sum(anova(lm(ps$Y~ps$X1+ps$X2+ps$X3) )[[2]][2:3])

(sum(anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][2:3])/2)/(
	anova(lm(ps$Y~ps$X1+ps$X2+ps$X3))[[2]][4]/(ps.n-4))



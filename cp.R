rm(list=ls())
options(stringsAsFactors = FALSE) 
options(scipen=999)

 
setwd("G:/math/651")

cp<-read.table("CommercialProperties.txt")
head(cp)
colnames(cp)<-c("Y","X1","X2","X3" ,"X4")

pairs(cp)
cor(cp[,-1])

lm(cp[,1]~cp[,2]+cp[,3]+cp[,4]+cp[,5])

#resids
cp.n<-nrow(cp)
cp.Y<-as.matrix(cp[,1])
cp.X<-as.matrix( data.frame(1,cp[,2:ncol(cp)]) )
cp.H<-cp.X %*% solve(t(cp.X)%*%cp.X) %*% t(cp.X)
cp.ee<-(diag(cp.n)-cp.H) %*% cp.Y
as.vector(cp.ee)
cp.p<-dim(cp.X)[2]


boxplot(cp.ee)

 (anova(  lm(cp[,1]~cp[,2]+cp[,3]+cp[,4] ) ))

cp.J<-matrix(1,cp.n,cp.n)
cp.MSR<-(t(cp.Y)%*%(cp.H-(1/cp.n)*cp.J)%*%cp.Y)/(cp.p-1)
cp.MSE<-t(cp.ee)%*%(cp.ee)/(cp.n-cp.p)

#t(cp.b) %*% t(cp.X) %*%cp.Y - (1/cp.n)*t(cp.Y)%*%cp.J%*%cp.Y


#cp.b<-matrix(as.numeric(lm(cp[,1]~cp[,2]+cp[,3]+cp[,4]+cp[,5]
#	)[[1]][1:5]),5,1)


cp.MSR/cp.MSE
cp.alpha<-.05
qf(1-cp.alpha,cp.p-1,cp.n-cp.p)

1-pf(  1-cp.alpha,3,cp.n-cp.p)

if( cp.MSR/cp.MSE > qf(1-cp.alpha,cp.p-1,cp.n-cp.p) ){print("conclude Ha")
	} else {print("conclude H0")}
#0.4878


#c
cp.b<- solve(t(cp.X)%*%cp.X)%*%(t(cp.X)%*%cp.Y)
cp.b
cp.sb<-as.numeric(diag(sqrt((as.numeric(cp.MSE)*solve(t(cp.X)%*%cp.X)))))
cp.sb
cp.g<-4

cp.B<-qt(  1-((cp.alpha)/(2*cp.g)) ,cp.n-cp.p)

cp.b[-1]-cp.sb[-1]*cp.B
cp.b[-1]+cp.sb[-1]*cp.B

summary ( (  lm(cp[,1]~cp[,2]+cp[,3]+cp[,4] ) ) )


(t(cp.Y)%*%(cp.H-(1/cp.n)*cp.J)%*%cp.Y)/
t(cp.Y)%*%(diag(cp.n)-(1/cp.n)*cp.J )%*%cp.Y


###### HW 6


anova(	lm(cp$Y ~ cp$X1+cp$X4)	)







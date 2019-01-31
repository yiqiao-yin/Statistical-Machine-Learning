#############################
#Problem 2      #############
#code from Prof' Liu ########
############################
Credit<-read.csv("Credit.csv", header=T)
Credit<-data.frame(Credit[,-1])
colnorm<-apply(Credit[,c(1:6,11)],2,var)
Credit[,c(1:6,11)]<- as.matrix(Credit[,c(1:6,11)]) %*% diag(1/sqrt(colnorm))

library(leaps)
### Best subsect selection ###
regfit.full=regsubsets(Balance~., Credit, nvmax=20, method="exhaustive")
regfull.summary<-summary(regfit.full)
regfull.summary$rss
pdf("best.pdf",height=3,width=6)
par(oma=c(1,1,1,1),pty='s', mar=c(2,2,1,1),mgp=c(1.5,0.25,0), 
    lwd=0.5,tck=-0.01, cex.axis=0.5, cex.lab=0.9, cex.main=0.9,pty='s')
plot(c(1:11), regfull.summary$rss,,type='p',pch=16, col="red", xlab="", ylab=""
     ,axes=F)
lines(c(1:11),regfull.summary$rss,col=2)
axis(1,lwd=0.5)
axis(2,lwd=0.5)
box(lwd=par()$lwd)
mtext(1,text="Subset Size k",cex=0.6,line = 1)
mtext(2,text="Residual Sum-of-Squares",line = 1,cex=0.6)

### Forward stepwise selection ###
regfit.fwd=regsubsets(Balance~., data=Credit, nvmax=20, method="forward")
regfwd.summary<-summary(regfit.fwd)
points(c(1:11)+0.05, regfwd.summary$rss , pch=18, col="blue")
lines(c(1:11)+0.05,regfwd.summary$rss,col="blue")

### Backward stepwise selection ###
regfit.bwd=regsubsets(Balance~., data=Credit, nvmax=20, method="backward")
regbwd.summary<-summary(regfit.bwd)
points(c(1:11)+0.1, regbwd.summary$rss, pch=8, col="green")
lines(c(1:11)+0.1,regbwd.summary$rss,col="green")
legend(x="topright",legend=c("Best subsect selection", "Forward stepwise selection", "Backward stepwise selection"),col=c(2,"blue","green"),
       pch=c(16,18,8),cex=0.5,border = NA,box.lty=0)
dev.off()

which.min(regfull.summary$bic)
which.min(regfull.summary$cp)
which.min(regfwd.summary$bic)
which.min(regfwd.summary$cp)
which.min(regbwd.summary$bic)




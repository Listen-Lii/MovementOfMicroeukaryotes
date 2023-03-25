# Fig S5 sloan model

library(Hmisc)
library(minpack.lm)
library(stats4)

#- run 10 times
sp<-read.csv("Final data/parkv4.moss.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv4.soil.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv4.sediment.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv4.tree.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv4.water.csv",head=T,stringsAsFactors=F,row.names=1)

sp<-read.csv("Final data/parkv9.moss.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv9.soil.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv9.sediment.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv9.tree.csv",head=T,stringsAsFactors=F,row.names=1)
sp<-read.csv("Final data/parkv9.water.csv",head=T,stringsAsFactors=F,row.names=1)
#-


sp = as.data.frame(t(sp))
N <- mean(apply(sp, 1, sum))
p.m<- apply(sp, 2, mean)
p.m<- p.m[p.m != 0]
p <- p.m/N
sp.bi <- 1*(sp>0)
freq<- apply(sp.bi, 2, mean)
freq<- freq[freq != 0]
C <- merge(p, freq, by=0)
C <- C[order(C[,2]),]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]
p <- C.0[,2]
freq<- C.0[,3]
names(p) <- C.0[,1]
names(freq) <- C.0[,1]
d = 1/N
m.fit<- nlsLM(freq ~ pbeta(d, N*m*p, N*m*(1 -p), lower.tail=FALSE),start=list(m=0.1))
m.ci <- confint(m.fit, 'm', level=0.95)
freq.pred<- pbeta(d, N*coef(m.fit)*p, N*coef(m.fit)*(1 -p), lower.tail=FALSE)
Rsqr<- 1 - (sum((freq - freq.pred)^2))/(sum((freq - mean(freq))^2))

m.fit
N
Rsqr

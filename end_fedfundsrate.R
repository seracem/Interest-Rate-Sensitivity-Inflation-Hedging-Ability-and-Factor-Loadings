library(fredr)
library(lmtest)
library(BVAR)
library(randomForest)
library(sandwich)
library(dplyr)
library(PerformanceAnalytics)
library(R.matlab)
setwd("G:/Desktop2223/Seminar/Seminar")
# stocks <- read.csv("ccm.csv")
# stocks$dates <- as.Date(paste(stocks$year,"-",stocks$month,"-","01",sep = ""))
t <- sort(unique(stocks$dates))
stocks <- arrange(stocks,stocks$dates)
fredr_set_key("key")
data <- fred_md

cpi <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date(as.Date(t[1]))-1,
  observation_end = as.Date(as.Date(t[length(t)]))
)
cpi <- subset(cpi,(cpi$date)>=as.Date(t[1]))
cpi <- subset(cpi,(cpi$date)<=as.Date(t[length(t)]))

fama <- read.csv("F-F_Research_Data_Factors.CSV")
mom <- read.csv("F-F_Momentum_Factor.csv")
data2 <- subset(data,rownames(data)>=as.Date(t[1]))
data <- subset(data2,rownames(data2)<=as.Date(t[length(t)]))


cpii <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date(as.Date(t[1])-1),
  observation_end = as.Date(as.Date(t[length(t)]))
)

cpiii <- cpii$value
summary(cpiii)

inflation <- log(cpiii[-1])-log(cpiii[-length(cpiii)])



cp <- cpi$value
summary(cp)

interest <- cp
# interest <- log(cp[-1])-log(cp[-length(cp)])

data <- data
data$interest <- interest #data ready



data <- as.matrix(data)
numbervar <- c()
pred <- c()
for(i in 49:(length(cp))){
  #i <- 49
  x <- data[(i-48):(i-1),][,-120]
  x <- x[ , colSums(is.na(x)) == 0]
  numbervar[i] <- dim(x)[2]
  y <- data[(i-47):(i),][,120]
  d <- data.frame(x,y)
  fm <- randomForest(y~.,data = d,na.action = na.omit)
  x <- data.frame(t(data[i,]))[,-120]
  colnames(x) <- colnames(data)[-120]
  x[which((is.na(x)))] <- 0
  #x <- x[ , colSums(is.na(x)) == 1]
  
  #pred <- predict(fm,newdata = x)
  pred[i] <- predict(fm,newdata = x)
}

wango <- na.omit(cbind(pred,interest,inflation))
colnames(wango) <- c("prediction","interest","inflation")
plot(wango[,1],wango[,2],xlab = "Prediction", ylab = "Inflaiton")
sum((wango[,1]-wango[,2])**2)

wango <- data.frame(wango)
plot(wango$interest[-length(wango$interest)],wango$interest[-1],xlab = "lagged interest",ylab = "interest")
dd <- data.frame(wango$interest[-length(wango$interest)],wango$interest[-1],wango$pred[-1])
colnames(dd) <- c("laginterest","interest","predictions")
pred <- na.omit(pred)
#plot 1 
plot(interest,type = "l")
#table 1 not final
summary(interest)
#pred
plot(pred,type = "l")
#table 2 not final pred
summary(pred)
act_interest <- interest[49:length(interest)]

plot(interest[-1],interest[-length(interest)])
sum((interest[-1]-interest[-length(interest)])**2)
sum((wango[,1]-wango[,2])**2)
library(dplyr)

tt <- rownames(data[50:dim(data)[1],])
rownames(dd) <- tt



###### original ang et al interest betas with lagged interest
window <- 12*5
win <- (window+1)
q1 <- c()
q2 <- c()
q3 <- c()
q4 <- c()
q5 <- c()
me <- c()
dates <- rownames(dd)
for(k in 450:length(dates)){
  tryCatch({
    
    ind <- which(stocks$dates>=as.Date(dates[k-window]))
    d <- stocks[ind,]
    ind2 <- which(d$dates<as.Date(dates[k]))
    d <- d[ind2,]
    ind3 <- which(stocks$dates==dates[k])
    d2 <- stocks[ind3,]
    #p <- dd$predictions[(k-59):(k-1)]
    tickers <- unique(d$ticker)
    tickers2 <- unique(d2$ticker)
    ticks <- tickers
    betas <-  rep(NA,length(ticks))
    retu <-  rep(NA,length(ticks))
    retu2 <-  rep(NA,length(ticks))
    
    me <- rep(NA,length(ticks))
    p <- dd$interest[(k-window):(k-1)]
    for(j in 1:length(ticks)){
      tryCatch({
        
        sub <- subset(d,d$ticker==ticks[j])
        sub2 <- subset(d2,d2$ticker==ticks[j])
        y <- sub$ret
        #x <- pred
        t <- length(unique(d$dates))
        if(dim(sub)[1]<t){
          next
        }else{
          g <- lm(sub$ret~p)
          rin <- coeftest(g, vcov = NeweyWest)
          betas[j] <- rin[2]
          retu[j] <- sub2$ret
          me[j] <- ifelse(is.numeric(sub2$me_lag),sub2$me_lag,NA)
          retu2[j] <- ifelse(dim(sub2)[1]<1,-1,sub2$ret)
        }
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      
    }
    qu <- data.frame(betas,retu2,me)
    qa <- arrange(qu,desc(betas))
    qa <- na.omit(qa)
    quan <- quantile(qa$betas,seq(0,1,0.2))
    s1 <- subset(qa,qa$betas>=quan[1]&qa$betas<=quan[2])
    s2 <- subset(qa,qa$betas>=quan[2]&qa$betas<=quan[3])
    s3 <- subset(qa,qa$betas>=quan[3]&qa$betas<=quan[4])
    s4 <- subset(qa,qa$betas>=quan[4]&qa$betas<=quan[5])
    s5 <- subset(qa,qa$betas>=quan[5]&qa$betas<=quan[6])
    q1[k] <- (s1$retu2%*%(s1$me/sum(s1$me)))
    q2[k] <- (s2$retu2%*%(s2$me/sum(s2$me)))
    q3[k] <- (s3$retu2%*%(s3$me/sum(s3$me)))
    q4[k] <- (s4$retu2%*%(s4$me/sum(s4$me)))
    q5[k] <- (s5$retu2%*%(s5$me/sum(s5$me)))
    print(colMeans(data.frame(q1,q2,q3,q4,q5),na.rm = TRUE))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

library(stargazer)






matplot(data.frame(cumprod(1+na.omit(q1)),cumprod(1+na.omit(q2)),cumprod(1+na.omit(q3)),cumprod(1+na.omit(q4)),cumprod(1+na.omit(q5))))


fama <- read.csv("F-F_Research_Data_Factors.CSV")
mom <- read.csv("F-F_Momentum_Factor.csv")
d1 <- fama[482:1134,]
d2 <- d2 <- mom[476:1128,]
ddnew <- data.frame(cbind(dd,d1,d2))
ddnew$mkt <- as.numeric(ddnew$Mkt.RF)/100
ddnew$smb <- as.numeric(ddnew$SMB)/100
ddnew$hml <- as.numeric(ddnew$HML)/100
ddnew$mom <- as.numeric(ddnew$Mom)/100
ddnew$rf <- as.numeric(ddnew$RF)/100
ddnew$q1 <- q1
ddnew$q2 <- q2
ddnew$q3 <- q3
ddnew$q4 <- q4
ddnew$q5 <- q5


t <- rownames(ddnew)
t <- as.Date(t)
cpii <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date(as.Date(t[1])),
  observation_end = as.Date(as.Date(t[length(t)]))+31
)

# cpii <- fredr(
#   series_id = "CPIAUCSL",
#   observation_start = as.Date(as.Date(t[1]))-1,
#   observation_end = as.Date(as.Date(t[length(t)]))
# )

cpiii <- cpii$value
summary(cpiii)

inflation <- log(cpiii[-1])-log(cpiii[-length(cpiii)])
ddnew$inflation <- inflation

summary(lm(q5~interest,data = ddnew))
summary(lm(q4~interest,data = ddnew))
summary(lm(q3~interest,data = ddnew))
summary(lm(q2~interest,data = ddnew))
summary(lm(q1~interest,data = ddnew))


summary(lm(q5~inflation,data = ddnew))
summary(lm(q4~inflation,data = ddnew))
summary(lm(q3~inflation,data = ddnew))
summary(lm(q2~inflation,data = ddnew))
summary(lm(q1~inflation,data = ddnew))

g <- (lm(q5-rf~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)




da <- data.frame(q1,q2,q3,q4,q5)
stargazer(da)
apply(da,FUN = skewness,MARGIN = -1)
apply(da,FUN = kurtosis,MARGIN = -1)

s15 <- (lm(q5~interest,data = ddnew))
s14 <- (lm(q4~interest,data = ddnew))
s13 <- (lm(q3~interest,data = ddnew))
s12 <- (lm(q2~interest,data = ddnew))
s11 <- (lm(q1~interest,data = ddnew))

stargazer(s11, s12, s13, s14,s15, title="Regression of Quintile Portfolios",
          keep.stat="n")

s5 <- (lm(q5~inflation,data = ddnew))
s4 <- (lm(q4~inflation,data = ddnew))
s3 <- (lm(q3~inflation,data = ddnew))
s2 <- (lm(q2~inflation,data = ddnew))
s1 <- (lm(q1~inflation,data = ddnew))


stargazer(s1, s2, s3, s4,s5, title="Regression of Quintile Portfolios",
          keep.stat="n")


g <- (lm(q5-rf~mkt+smb+hml+mom,data = ddnew))
g5 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = ddnew))
g4 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = ddnew))
g3 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = ddnew))
g2 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = ddnew))
g1 <- coeftest(g, vcov = NeweyWest)


stargazer(g1, g2, g3, g4,g5, title="Regression of Quintile Portfolios",
          keep.stat="n")







ddnew1 <- na.omit(ddnew)
# tq1 <- as.xts(ts(c(ddnew1$q5),frequency = 12))
# rff <- as.xts(ts(c(ddnew$rf),frequency = 12))
# SharpeRatio.annualized(R=tq1,Rf=rff)
# 
# matplot(cumprod(1+data.frame(na.omit(q1),na.omit(q5))),type = "l")
# summary(lm(na.omit(q11)~na.omit(q55)))
# (sd(na.omit(q11)))*sqrt(12)
# matplot(cumprod(1+data.frame(na.omit(q1),na.omit(q5))),type = "l")
# 
# ddnew11 <- ddnew1[1:300,]
# g <- (lm(ddnew1$q55~ddnew1$interest))
# coeftest(g, vcov = NeweyWest)
# 

# 
# coeftest(g, vcov = NeweyWest)
# g <- (lm(q1-rf~mkt+smb+hml+mom,data = ddnew1))
# coeftest(g, vcov = NeweyWest)
# g <- (lm(interest~mkt,data = ddnew1))
# coeftest(g, vcov = NeweyWest)
# g <- (lm((q22)~mkt+smb+hml+mom,data = ddnew1))
# coeftest(g, vcov = NeweyWest)
# 
# df <- ddnew1$q5
# View(c(mean(df),mean(df-ddnew1$rf),median(df),max(df),min(df),sd(df)*sqrt(12),skewness(df),kurtosis(df),length(df)))
# g <- (lm((ddnew1$q11)~ddnew1$interest))
# summary(g)
# coeftest(g, vcov = NeweyWest)
# 
# 
# g <- (lm((q5-rf)~mkt+smb+hml+mom,data = ddnew1))
# coeftest(g, vcov = NeweyWest)
# summary(g)
# lambo <- coeftest(g, vcov = NeweyWest)
# data.frame(lambo[,1],lambo[,3])
# 
# test <- as.xts(ts(dd,start = as.Date(rownames(dd[1,])),frequency = 12))
# matplot((dd[,2:3]),type = "l",ylab = "interest",xlab = "Observations")

y <- quantile(ddnew1$interest,0.5)
lowinterest <- which(ddnew1$interest<y)
lowinterestsub <- ddnew1[lowinterest,]
dd1 <- lowinterestsub



da <- data.frame(dd1$q1,dd1$q2,dd1$q3,dd1$q4,dd1$q5)
stargazer(da)
apply(da,FUN = skewness,MARGIN = -1)
apply(da,FUN = kurtosis,MARGIN = -1)

s15 <- (lm(q5~interest,data = dd1))
s14 <- (lm(q4~interest,data = dd1))
s13 <- (lm(q3~interest,data = dd1))
s12 <- (lm(q2~interest,data = dd1))
s11 <- (lm(q1~interest,data = dd1))

stargazer(s11, s12, s13, s14,s15, title="Regression of Quintile Portfolios",
          keep.stat="n")

s5 <- (lm(q5~inflation,data = dd1))
s4 <- (lm(q4~inflation,data = dd1))
s3 <- (lm(q3~inflation,data = dd1))
s2 <- (lm(q2~inflation,data = dd1))
s1 <- (lm(q1~inflation,data = dd1))


stargazer(s1, s2, s3, s4,s5, title="Regression of Quintile Portfolios",
          keep.stat="n")


g <- (lm(q5-rf~mkt+smb+hml+mom,data = dd1))
g5 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = dd1))
g4 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = dd1))
g3 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = dd1))
g2 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = dd1))
g1 <- coeftest(g, vcov = NeweyWest)


stargazer(g1, g2, g3, g4,g5, title="Regression of Quintile Portfolios",
          keep.stat="n")





summary(lm(q5~interest,data = dd1))
summary(lm(q4~interest,data = dd1))
summary(lm(q3~interest,data = dd1))
summary(lm(q2~interest,data = dd1))
summary(lm(q1~interest,data = dd1))


summary(lm(q5~inflation,data = dd1))
summary(lm(q4~inflation,data = dd1))
summary(lm(q3~inflation,data = dd1))
summary(lm(q2~inflation,data = dd1))
summary(lm(q1~inflation,data = dd1))

g <- (lm(q5-rf~mkt+smb+hml+mom,data = dd1))
coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = dd1))
coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = dd1))
coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = dd1))
coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = dd1))
coeftest(g, vcov = NeweyWest)



x <- quantile(ddnew1$interest,0.5)
highinterest <- which(ddnew1$interest>x)
highinterestsub <- ddnew1[highinterest,]
dd2 <- highinterestsub
dd1 <- dd2

da <- data.frame(dd1$q1,dd1$q2,dd1$q3,dd1$q4,dd1$q5)
stargazer(da)
apply(da,FUN = skewness,MARGIN = -1)
apply(da,FUN = kurtosis,MARGIN = -1)

s15 <- (lm(q5~interest,data = dd1))
s14 <- (lm(q4~interest,data = dd1))
s13 <- (lm(q3~interest,data = dd1))
s12 <- (lm(q2~interest,data = dd1))
s11 <- (lm(q1~interest,data = dd1))

stargazer(s11, s12, s13, s14,s15, title="Regression of Quintile Portfolios",
          keep.stat="n")

s5 <- (lm(q5~inflation,data = dd1))
s4 <- (lm(q4~inflation,data = dd1))
s3 <- (lm(q3~inflation,data = dd1))
s2 <- (lm(q2~inflation,data = dd1))
s1 <- (lm(q1~inflation,data = dd1))


stargazer(s1, s2, s3, s4,s5, title="Regression of Quintile Portfolios",
          keep.stat="n")


g <- (lm(q5-rf~mkt+smb+hml+mom,data = dd1))
g5 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = dd1))
g4 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = dd1))
g3 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = dd1))
g2 <- coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = dd1))
g1 <- coeftest(g, vcov = NeweyWest)


stargazer(g1, g2, g3, g4,g5, title="Regression of Quintile Portfolios",
          keep.stat="n")



summary(lm(q5~interest,data = dd2))
summary(lm(q4~interest,data = dd2))
summary(lm(q3~interest,data = dd2))
summary(lm(q2~interest,data = dd2))
summary(lm(q1~interest,data = dd2))


summary(lm(q5~inflation,data = dd2))
summary(lm(q4~inflation,data = dd2))
summary(lm(q3~inflation,data = dd2))
summary(lm(q2~inflation,data = dd2))
summary(lm(q1~inflation,data = dd2))

g <- (lm(q5-rf~mkt+smb+hml+mom,data = dd2))
coeftest(g, vcov = NeweyWest)

g <- (lm(q4-rf~mkt+smb+hml+mom,data = dd2))
coeftest(g, vcov = NeweyWest)

g <- (lm(q3-rf~mkt+smb+hml+mom,data = dd2))
coeftest(g, vcov = NeweyWest)

g <- (lm(q2-rf~mkt+smb+hml+mom,data = dd2))
coeftest(g, vcov = NeweyWest)

g <- (lm(q1-rf~mkt+smb+hml+mom,data = dd2))
coeftest(g, vcov = NeweyWest)












g <- (lm((q5-rf)~mkt+smb+hml+mom,data = ddnew))
coeftest(g, vcov = NeweyWest)
summary(g)

#Subset according to interest
#plot(dd$interest)
#abline(h=quantile(interest,0.85))
#abline(h=quantile(interest,0.15))


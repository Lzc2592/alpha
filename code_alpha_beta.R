#    source("code_alpha_beta.R")
#===========================================================================
cat("\014")  # To clear console
source("/home/vishwas/Research/Alpha_Trading/Alpha Strategy/Lib.R")
require(lubridate)
library(readxl)
library(dplyr)
rm(list=ls())  # Clear all objects
#=====================================================================

#path <- 'Data/India/India_stocks_250_and_Nifty50(Modified).csv'
path <- "Data/France/France_stocks_250_and_CAC40(Modified).csv"
df <- read.csv(file.choose(),header = TRUE)
filename <- gsub(".*\\/(.*)\\..*", "\\1", c(path))
print(filename)

df$Date <- as.Date(as.character(df$Date))

n <- nrow(df)
p <- ncol(df)

## defining train and test months - we can define it as quarterly, weekly etc.
# first and last day of each month
date.start.month <- seq(as.Date("2006-01-01"),length=131,by="months")
date.end.month <- seq(as.Date("2006-02-01"),length=131,by="months")-1

tst.dt.start.mnth <- seq(as.Date("2006-02-01"),length=131,by="months")
tst.dt.end.mnth <- seq(as.Date("2006-03-01"),length=131,by="months")-1

## calculate log-return

df_rt <- data.frame(matrix(NA,nrow=n,ncol=p))
colnames(df_rt) <- colnames(df)
df_rt$Date <- df$Date

# close to close return of each stock
for(j in 2:p) # leave first column: date
{
  df_rt[2:n,j] <- diff(log(df[,j]))
}

T1<-length(date.start.month)

out_sample_rt<-data.frame()

for(t in 1:T1){
  sub_df_train <- subset(df_rt, Date >= date.start.month[t] 
                         & Date <= date.end.month[t])
  
  sub_df_test <- subset(df_rt, Date >= tst.dt.start.mnth[t] 
                        & Date <= tst.dt.end.mnth[t])
  
  ### model training
  
  m<-s<-r<-beta<-alpha <-  rep(NA,p)
  names(m)<-names(s)<-names(r)<-names(beta)<-names(alpha) <- colnames(df_rt)
  
  # find mean of each stock
  m[2:p] <- apply(sub_df_train[,2:p],2,mean,na.rm=TRUE)
  # find standard deviation of each stock
  s[2:p] <- apply(sub_df_train[,2:p],2,sd,na.rm=TRUE)
  
  for(j in 3:p) # first two columns left
  {
    # correlation among two stocks
    r[j]<-cor(sub_df_train[,c(2,j)], use="pairwise", method="pearson")[1,2]
    # find slope: beta
    beta[j] <- r[j]*s[j]/s[2]
    # find intercept : alpha (y = alpha + beta*x)
    alpha[j] <- m[j]-beta[j]*m[2] # m[2] mean of index
  } 
  stat <- na.omit(data.frame(cbind(m,s,r,beta,alpha)))
  
  stat_alpha_pos <- subset(stat[with(stat,order(alpha)),],alpha>0)
  
  stock_select_nm<-rownames(stat_alpha_pos)
  no_of_stocks<-length(stock_select_nm)
  
  omega<-rep(1/no_of_stocks,no_of_stocks)
  names(omega)<-stock_select_nm
  
  sub_df_test_final_stock<-as.matrix(sub_df_test[,stock_select_nm])
  
  port_rt<-sub_df_test_final_stock%*%omega
  test_sample_rt <- data.frame(sub_df_test[,1:2],port_rt)
  out_sample_rt <-rbind(out_sample_rt,test_sample_rt)
  cat("t = ",t,"\n")
}

out_sample_port_value<-data.frame(matrix(NA,nrow=nrow(out_sample_rt)
                              ,ncol=ncol(out_sample_rt)))
colnames(out_sample_port_value)<-colnames(out_sample_rt)

out_sample_port_value[1,2]<-100*exp(out_sample_rt[1,2])
out_sample_port_value[1,3]<-100*exp(out_sample_rt[1,3])

for(i in 2:nrow(out_sample_rt)){
  out_sample_port_value[i,2]<-out_sample_port_value[i-1,2]*exp(out_sample_rt[i,2])
  out_sample_port_value[i,3]<-out_sample_port_value[i-1,3]*exp(out_sample_rt[i,3])
  
}

windows()
plot(ts(out_sample_port_value$CAC40),ylim=c(0,200))
lines(ts(out_sample_port_value$port_rt),col="red")
# hist(alpha)
# hist(beta)
#windows()
#plot(alpha,beta, col='blue')
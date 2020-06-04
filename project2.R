library(readr)
library(tseries)
setwd("C:/Users/riazu/Google Drive/Forecasting Time Series Data/04 Projects/02 Project 2/02 Hang Seng")
data <- read_csv("res.csv")
View(data)

#creating the AICc table inclusive of 0 ARCH and GARCH

n<-81
d<-1
N<-n-d

log_lik_0<-(-.5)*N*(1+log(2*pi*mean(data$residuals^2)))

aic_df<- read_csv("arch_aic_temp.csv")
View(aic_df)
aic_df$loglik[1]<-log_lik_0

vals<-c(2,3,4,5,6,7,8,9,10,11)
for (i in vals){aic_df$loglik[i]<-logLik(garch(data$residuals, order=c(aic_df$P[i], aic_df$Q[i]), trace=F))}

vals2<-c(1,2,3,4,5,6,7,8,9,10,11)
for (i in vals2){aic_df$AICc[i]<- ((-2)*(aic_df$loglik[i]))+(2*(1+aic_df$Q[i])*(N/(N-aic_df$Q[i]-2)))}

aic_df[nrow(aic_df) + 1,] = c("GARCH","-", 0, 0)

View(aic_df)

garchmodel<-garch(data$residuals, c(1,1), trace=F)
q<-2
aic_df$loglik[12]<-logLik(garchmodel)
aic_df$AICc[12]<-(-2)*(logLik(garchmodel))+(2*(1+q)*(N/(N-q-2)))

View(aic_df)
write_csv(aic_df,"arch_aic_final.csv")

#unconditional variance

coefficients<-data.frame(coef(garchmodel))
omega<-coefficients[1,1]
alpha<-coefficients[2,1]
beta<-coefficients[3,1]

uncond_var<-omega/(1-alpha-beta)
uncond_var

1-(alpha+beta)

ht<-garchmodel$fit[,1]^2
cond_var<-data.frame(ht)
write_csv(cond_var,"cond_var.csv")
View(cond_var)

#forecast interval

res<- -0.042680217
convar<- 0.000217864073376619

ht_1<-omega+(alpha*(res^2))+(beta*convar)
ft_1<-1.96*sqrt(ht_1)
ft_1

forecast<-10.0689
forecast-ft_1
forecast+ft_1

#5th percentile

forecast-(1.645*sqrt(ht_1))

plot(ht,type="l")

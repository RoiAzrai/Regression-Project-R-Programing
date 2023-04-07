library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)
library(gmodels)
library(lattice)
library(hrbrthemes)
library(ggExtra)
library(rms)
library(strucchange)
library(leaps)
library(alr3)
library(HH)
library(car)
library(lmtest)
library(lubridate)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(reshape)
# ----------------------- PART B ------------

dataset<-read.csv(file.choose(),header = T)
dataset$X <- NULL
dataset$X.1 <- NULL
dataset$X.2 <- NULL

# ------------------------- Question 2.1 ---------------------------------------

# ---- Correlation With Y = adr -------

datasetCorNum<-dataset
datasetCorNum<-datasetCorNum[,sapply(datasetCorNum, is.numeric)] # Only numbers
cor(datasetCorNum)
coefficients(dataset)

corr_data <- round(cor(datasetCorNum),4)
melted_datasetCorNum <- melt(corr_data)
ggplot(data = melted_datasetCorNum, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


get_lower_tri<-function(corr_data){
  corr_data[upper.tri(corr_data)] <- NA
  return(corr_data)
}

get_upper_tri <- function(corr_data){
  corr_data[lower.tri(corr_data)]<- NA
  return(corr_data)
}

upper_tri <- get_upper_tri(corr_data)

melted_datasetCorNum <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_datasetCorNum, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+ coord_fixed()

reorder_cormat <- function(corr_data){
  
  dd <- as.dist((1-corr_data)/2)
  hc <- hclust(dd)
  corr_data <-corr_data[hc$order, hc$order]
}


corr_data <- reorder_cormat(corr_data)
upper_tri <- get_upper_tri(corr_data)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "darkblue", high = "red", mid = "yellow", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1.3,
                               title.position = "top", title.hjust = 0.5)) 
#---

# ---- Continuous -----

# - Full Model
mod_full <- lm(dataset$adr ~ 
                 dataset$lead_time + 
                 dataset$arrival_date_month +
                 dataset$arrival_date_day_of_month + 
                 dataset$stays_in_week_nights +
                 dataset$adults +
                 dataset$meal +
                 dataset$country +
                 dataset$reserved_room_type +
                 dataset$total_of_special_requests , data = dataset  )
summary(mod_full)



# X1 - Lead Time - OK.
mod_X1<-lm(dataset$adr ~ dataset$lead_time, data = dataset)
summary(mod_X1)
coefficients(mod_X1)

plot( x = dataset$lead_time, y =dataset$adr, col='red', pch=1,
      xlab = "Lead Time", ylab = "ADR")+
  abline(mod_X1 , col = "blue")

# X3 - Arrival Date Day of Month - Off the Model.
mod_X3<-lm(dataset$adr ~ dataset$arrival_date_day_of_month, data = dataset)
summary(mod_X3)
coefficients(mod_X3)

plot( x = dataset$arrival_date_day_of_month, y =dataset$adr, col='red', pch=1,
      xlab = "Arrival Date Day of Month", ylab = "ADR")+
  abline(mod_X3 , col = "blue")


# X4 - Stays In Week Nights - Off the Model.

# --- Before removing exceptions:
bpAWN<-boxplot(dataset$stays_in_week_nights, main='Stays in Week Nights') 
bpAWN$out
length(bpAWN$out)
# --- After removing exceptions:
bpAWNX<-subset(dataset,dataset$stays_in_week_nights<20) # we use this.
bpAWNnew<-boxplot(bpAWNX$stays_in_week_nights, main='Stays in Week Nights - new ')

mod_X4<-lm(bpAWNX$adr ~ bpAWNX$stays_in_week_nights, data = bpAWNX)
summary(mod_X4)
coefficients(mod_X4)

plot( x = bpAWNX$stays_in_week_nights, y =bpAWNX$adr, col='red', pch=1,
      xlab = "Stays In Week Nights", ylab = "ADR")+
  abline(mod_X4 , col = "blue")

# X5 - Number of adults - OK.
mod_X5<-lm(dataset$adr ~ dataset$adults, data = dataset)
summary(mod_X5)
coefficients(mod_X5)

plot( x = dataset$adults, y =dataset$adr, col='red', pch=1,
      xlab = "Number of adults", ylab = "ADR")+
  abline(mod_X5 , col = "blue")

# X9 - Total of Special Requests - Off the Model.
mod_X9<-lm(dataset$adr ~ dataset$total_of_special_requests, data = dataset)
summary(mod_X9)
coefficients(mod_X9)

plot( x = dataset$total_of_special_requests, y =dataset$adr, col='red', pch=1,
      xlab = "Total of Special Requests", ylab = "ADR")+
  abline(mod_X9 , col = "blue")

# ---- Categorical ----

# X2 - Arrival Date Month - OK.
box <- ggplot(data=dataset, aes(x=arrival_date_month, y=adr))
box + geom_boxplot(aes(fill=arrival_date_month)) +
  xlab("Arrival Date Month") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

mod_X2<-lm(dataset$adr ~ dataset$arrival_date_month , data = dataset)
summary(mod_X2)
coefficients(mod_X2)

# X6 - Meal - Off the Model.
box <- ggplot(data=dataset, aes(x=meal, y=adr))
box + geom_boxplot(aes(fill=meal)) +
  xlab("Meals") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

mod_X6<-lm(dataset$adr ~ dataset$meal, data = dataset)
summary(mod_X6)
coefficients(mod_X6)

# X7 - Country 
box <- ggplot(data=dataset, aes(x=country, y=adr))
box + geom_boxplot(aes(fill=country)) +
  xlab("Countries") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

mod_X7<-lm(dataset$adr ~ dataset$country, data = dataset)
summary(mod_X7)
coefficients(mod_X7)

# X8 - Reserved Room Type 
box <- ggplot(data=dataset, aes(x=reserved_room_type, y=adr))
box + geom_boxplot(aes(fill=reserved_room_type)) +
  xlab("Room Types") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

mod_X8<-lm(dataset$adr ~ dataset$reserved_room_type, data = dataset)
summary(mod_X8)
coefficients(mod_X8)

# ------------------------- Question 2.2 ---------------------------------------

datasetNew <- dataset

# X6 - Meals : FB and HB
datasetNew$meal[datasetNew$meal == "FB"] <- "FB/HB"
datasetNew$meal[datasetNew$meal == "HB"] <- "FB/HB"
# X7 - Countries : GBR - United kingdom and IRL - Ireland
datasetNew$country[datasetNew$country == "GBR"] <- "GBR/IRL"
datasetNew$country[datasetNew$country == "IRL"] <- "GBR/IRL"


# New Box plots - Meals.
box <- ggplot(data=datasetNew, aes(x=meal, y=adr))
box + geom_boxplot(aes(fill=meal)) +
  xlab("Meals") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

# FB/HB 
FBHBTable <- sqldf('SELECT *
                       FROM datasetNew
                       WHERE meal = "FB/HB"
                      ')
distFBHBTable <- sqldf('SELECT meal, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM FBHBTable')
skewness(FBHBTable$adr)
summary(FBHBTable)
mod_X6<-lm(datasetNew$adr ~ datasetNew$meal, data = datasetNew)
summary(mod_X6)
coefficients(mod_X6)

# New Box plots - Countries.
box <- ggplot(data=datasetNew, aes(x=country, y=adr))
box + geom_boxplot(aes(fill=country)) +
  xlab("Countries") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic")) + 
  geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

# GBR/IRL
GBRIRLTable <- sqldf('SELECT *
                       FROM datasetNew
                       WHERE country = "GBR/IRL"
                      ')
distIRLTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM GBRIRLTable')
skewness(GBRIRLTable$adr)
summary(GBRIRLTable)

mod_X7<-lm(datasetNew$adr ~ datasetNew$country, data = datasetNew)
summary(mod_X7)
coefficients(mod_X7)

# ------------------------- Question 2.3 ---------------------------------------

# Dummy Variables:

datasetNew$country <- factor(datasetNew$country) # X7
datasetNew$country <- relevel(datasetNew$country,ref = c(1)) # X2

datasetNew$reserved_room_type <- factor(datasetNew$reserved_room_type) # X8
datasetNew$reserved_room_type <- relevel(datasetNew$reserved_room_type,ref = c(1)) # X2

datasetNew$arrival_date_month <- factor(datasetNew$arrival_date_month) # X2
datasetNew$arrival_date_month <- relevel(datasetNew$arrival_date_month,ref = c(1)) # X2

datasetNew$meal <- factor(datasetNew$meal) # X6
datasetNew$meal <- relevel(datasetNew$meal,ref = c(1)) # X6

# ------------------------- Question 2.4 ---------------------------------------

# Interactions :

# X5 - Number of Adults & X2 - Months.
mod_X5_X2 <- lm(adr ~ adults*arrival_date_month, data = datasetNew)
plot_model(mod_X5_X2, type = "pred", terms = c("adults", "arrival_date_month"))
summary(mod_X5_X2)

# X5 - Number of Adults & X2 - Months.
mod_X5_X2 <- lm(adr ~ adults + 
                      arrival_date_month + 
                      adults:arrival_date_month, data = datasetNew)
plot_model(mod_X5_X2, type = "pred", terms = c("adults", "arrival_date_month"))
summary(mod_X5_X2)

# X5 - Number of Adults & X6 - Meal.
mod_X5_X6 <- lm(adr ~ adults * meal, data = datasetNew)
plot_model(mod_X5_X6, type = "int", terms = c("adults", "meal"))
summary(mod_X5_X6)

# X5 - Number of Adults & X7 - Countries.
mod_X5_X7 <- lm(adr ~ adults * country, data = datasetNew)
plot_model(mod_X5_X7, type = "int", terms = c("adults", "country"))
summary(mod_X5_X7)

# X5 - Number of Adults & X8 Room Types
mod_X5_X8 <- lm(adr ~ adults * reserved_room_type, data = datasetNew)
plot_model(mod_X5_X8, type = "int", terms = c("adults", "reserved_room_type"))
summary(mod_X5_X8)

# ----------------------------

# X1 - Lead Time & X2 - Months.
mod_X1_X2 <- lm(adr ~ lead_time * arrival_date_month, data = datasetNew)
plot_model(mod_X1_X2, type = "int", terms = c("lead_time", "arrival_date_month"))
summary(mod_X1_X2)

# X1 - Lead Time & X6 - Meal.
mod_X1_X6 <- lm(adr ~ lead_time * meal, data = datasetNew)
plot_model(mod_X1_X6, type = "int", terms = c("lead_time", "meal"))
summary(mod_X1_X6)

# X1 - Lead Time & X7 - Countries.
mod_X1_X7 <- lm(adr ~ lead_time * country, data = datasetNew)
plot_model(mod_X1_X7, type = "int", terms = c("lead_time", "country"))
summary(mod_X1_X7)

# X1 - Lead Time & X8 - Room Types.
mod_X1_X8 <- lm(adr ~ lead_time * reserved_room_type, data = datasetNew)
plot_model(mod_X1_X8, type = "int", terms = c("lead_time", "reserved_room_type"))
summary(mod_X1_X8)

# ------------------------- Question 3.1 ---------------------------------------

datasetNewX <- datasetNew

# Full Model
FMnew<- lm(datasetNewX$adr ~ 
                      datasetNewX$lead_time + 
                      datasetNewX$adults + 
                      factor(datasetNewX$arrival_date_month) +
                      factor(datasetNewX$country) +
                      factor(datasetNewX$reserved_room_type) +
                      factor(datasetNewX$meal) + 
                      datasetNewX$lead_time * datasetNewX$reserved_room_type,  
                      data = datasetNewX)

FAIC<-extractAIC(FMnew)
FBIC<-extractAIC(FMnew,k = log(nrow(datasetNewX)))
summary(FMnew)
FRadj<-summary(FMnew)$adj.r.squared
cat("Full Model - AIC:",FAIC[2])%>%
  cat("\nFull Model - BIC:",FBIC[2])%>%
  cat("\nFull Model - Radj:",FRadj)

# Empty Model
Emp <- lm (adr ~ 1,data=datasetNewX)
EAIC<-extractAIC(Emp)[2]
EBIC<-extractAIC(Emp,k = log(nrow(datasetNewX)))[2]
ERadj<-summary(Emp)$adj.r.squared
summary(Emp)
cat("Empty Model - AIC:",EAIC)%>%
  cat("\nEmpty Model - BIC:",EBIC)

# AIC Forward Regression 
fwd.Model.AIC <-  step(Emp, direction='forward',scope=formula(FMnew))
fwd.A.AIC<-extractAIC(fwd.Model.AIC)[2]
fwd.A.BIC<-extractAIC(fwd.Model.AIC,k = log(nrow(datasetNewX)))[2]
fwd.A.Radj<-summary(fwd.Model.AIC)$adj.r.squared
summary(fwd.Model.AIC)
cat("fwd.Model.AIC - AIC:",fwd.A.AIC)%>%
  cat("\nfwd.Model.AIC - BIC:",fwd.A.BIC)%>%
  cat("\nfwd.Model.AIC - Radj:",fwd.A.Radj)

# BIC Forward Regression
fwd.Model.BIC <-  step(Emp, direction='forward',scope=formula(FMnew), k = log(nrow(datasetNewX)))
fwd.B.AIC<-extractAIC(fwd.Model.BIC)[2]
fwd.B.BIC<-extractAIC(fwd.Model.BIC,k = log(nrow(datasetNewX)))[2]
fwd.B.Radj<-summary(fwd.Model.BIC)$adj.r.squared
summary(fwd.Model.BIC)
cat("fwd.Model.BIC - AIC:",fwd.B.AIC)%>%
  cat("\nfwd.Model.BIC - BIC:",fwd.B.BIC)%>%
  cat("\nfwd.Model.BIC - Radj:",fwd.B.Radj)

# AIC Backward Regression 
bw.Model.AIC <-  step(FMnew, direction='backward',scope=~1)
bw.A.AIC<-extractAIC(bw.Model.AIC)[2]
bw.A.BIC<-extractAIC(bw.Model.AIC,k = log(nrow(datasetNewX)))[2]
bw.A.Radj<-summary(bw.Model.AIC)$adj.r.squared
summary(bw.Model.AIC)
cat("bw.Model.AIC - AIC:",bw.A.AIC)%>%
  cat("\nbw.Model.AIC - BIC:",bw.A.BIC)%>%
  cat("\nbw.Model.AIC - Radj:",bw.A.Radj)

# BIC Backward Regression
bw.Model.BIC <-  step(FMnew, direction='backward',scope=~1, k = log(nrow(datasetNewX)))
bw.B.AIC<-extractAIC(bw.Model.BIC)[2]
bw.B.BIC<-extractAIC(bw.Model.BIC,k = log(nrow(datasetNewX)))[2]
bw.B.Radj<-summary(bw.Model.BIC)$adj.r.squared
summary(bw.Model.BIC)
cat("bw.Model.BIC - AIC:",bw.B.AIC)%>%
  cat("\nbw.Model.BIC - BIC:",bw.B.BIC)%>%
  cat("\nbw.Model.BIC - Radj:",bw.B.Radj)

# AIC Steps Regression
SW.AIC <- step(Emp,direction = "both",scope = formula(FMnew))
Sw.A.AIC<-extractAIC(SW.AIC)[2]
Sw.A.BIC<-extractAIC(SW.AIC,k = log(nrow(datasetNewX)))[2]
Sw.A.Radj<-summary(SW.AIC)$adj.r.squared
summary(SW.AIC)
cat("SW.AIC - AIC:",Sw.A.AIC)%>%
  cat("\nSW.AIC - BIC:",Sw.A.BIC)%>%
  cat("\nSW.AIC - Radj:",Sw.A.Radj)

# BIC Steps Regression
SW.BIC <- step(Emp,direction = "both",scope = formula(FMnew), k = log(nrow(datasetNewX)))
Sw.B.AIC<-extractAIC(SW.BIC)[2]
Sw.B.BIC<-extractAIC(SW.BIC,k = log(nrow(datasetNewX)))[2]
Sw.B.Radj<-summary(SW.BIC)$adj.r.squared
summary(SW.BIC)
cat("SW.BIC - AIC:",Sw.B.AIC)%>%
  cat("\nSW.BIC - BIC:",Sw.B.BIC)%>%
  cat("\nSW.BIC - Radj:",Sw.B.Radj)

# ------------------------- Question 3.2 ---------------------------------------

FM<- lm(datasetNewX$adr ~ 
          datasetNewX$lead_time + 
          datasetNewX$adults + 
          factor(datasetNewX$arrival_date_month) +
          factor(datasetNewX$country) +
          factor(datasetNewX$reserved_room_type) +
          factor(datasetNewX$meal),  
          data = datasetNewX)
summary(FM)

# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM) # Predicted values.
datasetNewX$residuals<-residuals(FM) # Residuals.
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM, type = "Chow")

# Variance check :

# Number of Adults
plot(datasetNewX$adults, datasetNewX$adr,ylab = "ADR",xlab = "Number of Adults")
lm(adr~adults, datasetNewX)
abline(a =  21.67 ,b = 26.33, col="blue")
Vad0<-var(datasetNewX$adults[datasetNewX$adults < 1.5])%>%print()
Vad100<-var(datasetNewX$adults[datasetNewX$adults < 2.5 & datasetNewX$adults > 1.5])%>%print()
Vad150<-var(datasetNewX$adults[datasetNewX$adults < 3.5 & datasetNewX$adults > 2.5])%>%print()
var(datasetNewX$adults)

# Lead Time
plot(datasetNewX$lead_time, datasetNewX$adr,ylab = "ADR",xlab = "Lead Time")
lm(adr~lead_time, datasetNewX)
abline(a =  89.4991,b = -0.2011, col="blue")
Vlt50<-var(datasetNewX$adr[datasetNewX$lead_time < 50])%>%print()
Vlt100<-var(datasetNewX$adr[datasetNewX$lead_time < 100 & datasetNewX$lead_time > 50])%>%print()
Vlt150<-var(datasetNewX$adr[datasetNewX$lead_time < 150 & datasetNewX$lead_time > 100])%>%print()
Vlt200<-var(datasetNewX$adr[datasetNewX$lead_time < 200 & datasetNewX$lead_time > 150])%>%print()

# Goldfeld Quandt - Test :
gqtest(FM,alternative = "two.sided")

# ------------------------- Question 4.0 ---------------------------------------

datasetNewX <- datasetNew

# Box Cox
BC <- boxcox(lm(datasetNewX$adr ~ 
                  datasetNewX$lead_time + 
                  datasetNewX$adults + 
                  factor(datasetNewX$arrival_date_month) +
                  factor(datasetNewX$country) +
                  factor(datasetNewX$reserved_room_type) +
                  factor(datasetNewX$meal),  
                data = datasetNewX))

lambda_BC <- BC$x[which.max(BC$y)]%>%print() 

# ----- Log( Y ) -----

datasetNewX$adr_Log <- log(datasetNewX$adr) # New Column of Log(y = adr).

# Log(y) Model:
FM_Log<- lm((datasetNewX$adr_Log) ~ 
              datasetNewX$lead_time + 
              datasetNewX$adults + 
              factor(datasetNewX$arrival_date_month) +
              factor(datasetNewX$country) +
              factor(datasetNewX$reserved_room_type) +
              factor(datasetNewX$meal),  
            data = datasetNewX)

summary(FM_Log)

# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM_Log) 
datasetNewX$residuals<-residuals(FM_Log)
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM_Log)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM_Log, type = "Chow")

# Goldfeld Quandt - Test :
gqtest(FM_Log,alternative = "two.sided")

# ----- Sqrt( Y ) -----

datasetNewX <- datasetNew
datasetNewX$adr_Sqrt <- (datasetNewX$adr)^0.5

# Sqrt( Y ) Model:
FM_Sqrt<- lm((datasetNewX$adr_Sqrt) ~ 
               datasetNewX$lead_time + 
               datasetNewX$adults + 
               factor(datasetNewX$arrival_date_month) +
               factor(datasetNewX$country) +
               factor(datasetNewX$reserved_room_type) +
               factor(datasetNewX$meal),  
             data = datasetNewX)

summary(FM_Sqrt)

# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM_Sqrt) 
datasetNewX$residuals<-residuals(FM_Sqrt)
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM_Sqrt)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM_Sqrt, type = "Chow")

# Goldfeld Quandt - Test :
gqtest(FM_Sqrt,alternative = "two.sided")

# ----- Squared - ( Y )^2 -----

datasetNewX <- datasetNew
datasetNewX$adr_Squared <- (datasetNewX$adr)^2

# ( Y )^2 Model:
FM_Squared<- lm((datasetNewX$adr_Squared) ~ 
               datasetNewX$lead_time + 
               datasetNewX$adults + 
               factor(datasetNewX$arrival_date_month) +
               factor(datasetNewX$country) +
               factor(datasetNewX$reserved_room_type) +
               factor(datasetNewX$meal),  
             data = datasetNewX)

summary(FM_Squared)

# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM_Squared) 
datasetNewX$residuals<-residuals(FM_Squared)
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM_Squared)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM_Squared, type = "Chow")

# Goldfeld Quandt - Test :
gqtest(FM_Squared,alternative = "two.sided")

# ----- NegSqrt - ( Y )^-0.5 -----

datasetNewX <- datasetNew
datasetNewX$adr_NegSqrt <- (datasetNewX$adr)^-0.5

# ( Y )^-0.5 Model:
FM_NegSqrt<- lm((datasetNewX$adr_NegSqrt) ~ 
                  datasetNewX$lead_time + 
                  datasetNewX$adults + 
                  factor(datasetNewX$arrival_date_month) +
                  factor(datasetNewX$country) +
                  factor(datasetNewX$reserved_room_type) +
                  factor(datasetNewX$meal),  
                data = datasetNewX)

summary(FM_NegSqrt)

# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM_NegSqrt) 
datasetNewX$residuals<-residuals(FM_NegSqrt)
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM_NegSqrt)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM_NegSqrt, type = "Chow")

# Goldfeld Quandt - Test :
gqtest(FM_NegSqrt,alternative = "two.sided")

# ----- Log( Y ) , (lead_time)^2 (adults)^2 -----

datasetNewX <- datasetNew
datasetNewX$adr_Log <- log(datasetNewX$adr) # New Column of Log(y = adr).

datasetNewX<-subset(datasetNewX,datasetNewX$lead_time> 0 )

datasetNewX$lead_timeS <- log(datasetNewX$lead_time)
datasetNewX$adultsS <- log(datasetNewX$adults)

datasetNewX$lead_timeS <- (datasetNewX$lead_time)^2
datasetNewX$adultsS <- (datasetNewX$adults)^2


FM_Log<- lm((datasetNewX$adr_Log) ~ 
              datasetNewX$lead_timeS + 
              datasetNewX$adultsS + 
              factor(datasetNewX$arrival_date_month) +
              factor(datasetNewX$country) +
              factor(datasetNewX$reserved_room_type) +
              factor(datasetNewX$meal),  
            data = datasetNewX)

summary(FM_Log)


# Linearity and Variance equality check in plots:
datasetNewX$fitted<-fitted(FM_Log) 
datasetNewX$residuals<-residuals(FM_Log)
S.e_res<-sqrt(var(datasetNewX$residuals))
datasetNewX$stan_residuals<-(residuals(FM_Log)/S.e_res)

plot(datasetNewX$fitted, datasetNewX$stan_residuals, xlab = "Predicted Value",
     ylab = "Normalized Error")
abline(h = 0,col="blue")

# Normality tests and plots
qqnorm(datasetNewX$stan_residuals)
abline(a=0, b=1,col="blue")
hist(datasetNewX$stan_residuals,prob=TRUE, main="Histogram of Normalized Eror",
     ylab = '',
     xlab = "Normalized Eror",
     col = "pink")
lines(density(datasetNewX$stan_residuals),col="purple",lwd=4)

# KS Test - Normality :
ks.test(x = datasetNewX$stan_residuals, y = "pnorm", 
        alternative = "two.sided", exact = NULL)

# Shapiro Wilk - Normality Test :
shapiro.test(datasetNewX$stan_residuals)

# Chow - Linearity Test :
sctest(FM_Log, type = "Chow")

# Goldfeld Quandt - Test :
gqtest(FM_Log,alternative = "two.sided")


# -------- End , Love Team 10 ----------



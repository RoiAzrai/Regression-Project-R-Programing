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

# ----------------------- PART A ------------
dataset<-read.csv(file.choose(),header = T)
dataset$X <- NULL
dataset$X.1 <- NULL
dataset$X.2 <- NULL

# -------------- Question 4 ----------------------------------------------

plot(dataset)



# Color Correlation Function:

library(reshape2)
library(ggplot2)

datasetCorNum<-dataset
datasetCorNum$adr<-NULL
datasetCorNum<-datasetCorNum[,sapply(datasetCorNum, is.numeric)] # Only numbers
plot(datasetCorNum)
cor(datasetCorNum)


#---
corr_data <- round(cor(datasetCorNum),3)
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
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "green", mid = "white", 
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



# X1 - Lead Time & X4 - Nights in week
plot(x=dataset$lead_time,y=dataset$stays_in_week_nights,xlab="Nights in week",ylab="Lead Time")

# -------------- Question 5 ----------------------------------------------

# Y
summary(dataset$adr)
sd(dataset$adr,na.rm = TRUE)
skewness(dataset$adr,na.rm=TRUE)

# X1 - Lead Time
summary(dataset$lead_time)
sd(dataset$lead_time,na.rm = TRUE)
skewness(dataset$lead_time,na.rm=TRUE)

# X2 - Arrival Date Month - TEXT
mytable <- table(dataset$arrival_date_month)
slices <-  table(dataset$arrival_date_month)
lbls <- paste(names(mytable), "\n")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="Arrival Date Month")

box <- ggplot(data=dataset, aes(x=arrival_date_month, y=adr))
box + geom_boxplot(aes(fill=arrival_date_month)) +
  xlab("Arrival Date Month") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic"))

# X3 - Arrival Day of Month
summary(dataset$arrival_date_day_of_month)
sd(dataset$arrival_date_day_of_month,na.rm = TRUE)
skewness(dataset$arrival_date_day_of_month,na.rm=TRUE)

# X4 - Stays in Week Nights
summary(dataset$stays_in_week_nights)
sd(dataset$stays_in_week_nights,na.rm = TRUE)
skewness(dataset$stays_in_week_nights,na.rm=TRUE)

# X5 - Number of Adults
summary(dataset$adults)
sd(dataset$adults,na.rm = TRUE)
skewness(dataset$adults,na.rm=TRUE)

# X6 - Meal - TEXT
mytable <- table(dataset$meal)
slices <-  table(dataset$meal)
lbls <- paste(names(mytable), "\n")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="Meals")

box <- ggplot(data=dataset, aes(x=meal, y=adr))
box + geom_boxplot(aes(fill=meal)) +
  xlab("Meals") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic"))

# X7 - Country - TEXT
mytable <- table(dataset$country)
slices <-  table(dataset$country)
lbls <- paste(names(mytable), "\n")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="Countries")

box <- ggplot(data=dataset, aes(x=country, y=adr))
box + geom_boxplot(aes(fill=country)) +
  xlab("Countries") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic"))

# X8 - Reserved Room Type - TEXT
mytable <- table(dataset$reserved_room_type)
slices <-  table(dataset$reserved_room_type)
lbls <- paste(names(mytable), "\n")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls,
    main="Room Types")

box <- ggplot(data=dataset, aes(x=reserved_room_type, y=adr))
box + geom_boxplot(aes(fill=reserved_room_type)) +
  xlab("Room Types") + ylab("ADR") + ggtitle("") +
  stat_summary(fun=mean, geom="point", shape=5, size=2) +
  theme(axis.text.x = element_text(angle = 90, face = "italic"))

# X9 - Total of Special Requests
summary(dataset$total_of_special_requests)
sd(dataset$total_of_special_requests,na.rm = TRUE)
skewness(dataset$total_of_special_requests,na.rm=TRUE)

#mytable <- table(dataset$country)
#slices <-  table(dataset$country)
#lbls <- paste(names(mytable), ":\n", mytable, sep="")
#pct <- round(slices/sum(slices)*100)
#lbls <- paste(lbls,"-", pct) # add percents to labels
#lbls <- paste(lbls,"%",sep="") # ad % to labels
#pie(slices,labels = lbls,
#    main="Pie Chart of Countries :")

# -------------- Question 5 BY EACH Categorical vs adr ----------------------------------------------

# by Months :-----------------------------------------------

# Jan 1
JanuaryTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "January"
                      ')
distJanuaryTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM JanuaryTable')
skewness(JanuaryTable$adr)
summary(JanuaryTable)

# Feb 2
FebruaryTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "February"
                      ')
distFebruaryTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM FebruaryTable')
skewness(FebruaryTable$adr)
summary(FebruaryTable)

# March 3
MarchTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "March"
                      ')
distMarchTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM MarchTable')
skewness(MarchTable$adr)
summary(MarchTable)

# July 7
JulyTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "July"
                      ')
distJulyTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM JulyTable')
skewness(JulyTable$adr)
summary(JulyTable)

# August 8
AugustTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "August"
                      ')
distAugustTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM AugustTable')
skewness(AugustTable$adr)
summary(AugustTable)

# September 9
SeptemberTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "September"
                      ')
distSeptemberTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM SeptemberTable')
skewness(SeptemberTable$adr)
summary(SeptemberTable)

# October 10
OctoberTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "October"
                      ')
distOctoberTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM OctoberTable')
skewness(OctoberTable$adr)
summary(OctoberTable)

# November 11
NovemberTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "November"
                      ')
distNovemberTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM NovemberTable')
skewness(NovemberTable$adr)
summary(NovemberTable)

# December 12
DecemberTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE arrival_date_month = "December"
                      ')
distDecemberTable <- sqldf('SELECT arrival_date_month, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM DecemberTable')
skewness(DecemberTable$adr)
summary(DecemberTable)

# by Meals Type : -----------------------------------------------

# BB 
BBTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE meal = "BB"
                      ')
distBBTable <- sqldf('SELECT meal, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM BBTable')
skewness(BBTable$adr)
summary(BBTable)

# HB 
HBTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE meal = "HB"
                      ')
distHBTable <- sqldf('SELECT meal, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM HBTable')
skewness(HBTable$adr)
summary(HBTable)

# FB 
FBTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE meal = "FB"
                      ')
distFBTable <- sqldf('SELECT meal, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM FBTable')
skewness(FBTable$adr)
summary(FBTable)

# by Countries -----------------------------------------------

# CN 
CNTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE country = "CN"
                      ')
distCNTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM CNTable')
skewness(CNTable$adr)
summary(CNTable)

# ESP 
ESPTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE country = "ESP"
                      ')
distESPTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM ESPTable')
skewness(ESPTable$adr)
summary(ESPTable)

# GBR 
GBRTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE country = "GBR"
                      ')
distGBRTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM GBRTable')
skewness(GBRTable$adr)
summary(GBRTable)

# IRL 
IRLTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE country = "IRL"
                      ')
distIRLTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM IRLTable')
skewness(IRLTable$adr)
summary(IRLTable)

# PRT 
PRTTable <- sqldf('SELECT *
                       FROM dataset
                       WHERE country = "PRT"
                      ')
distPRTTable <- sqldf('SELECT country, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM PRTTable')
skewness(PRTTable$adr)
summary(PRTTable)

# by Room Types -----------------------------------------------

# A 
A_Table <- sqldf('SELECT *
                       FROM dataset
                       WHERE reserved_room_type = "A"
                      ')
distA_Table <- sqldf('SELECT reserved_room_type, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM A_Table')
skewness(A_Table$adr)
summary(A_Table)

# D 
D_Table <- sqldf('SELECT *
                       FROM dataset
                       WHERE reserved_room_type = "D"
                      ')
distD_Table <- sqldf('SELECT reserved_room_type, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM D_Table')
skewness(D_Table$adr)
summary(D_Table)

# E 
E_Table <- sqldf('SELECT *
                       FROM dataset
                       WHERE reserved_room_type = "E"
                      ')
distE_Table <- sqldf('SELECT reserved_room_type, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM E_Table')
skewness(E_Table$adr)
summary(E_Table)

# F 
F_Table <- sqldf('SELECT *
                       FROM dataset
                       WHERE reserved_room_type = "F"
                      ')
distF_Table <- sqldf('SELECT reserved_room_type, 
                                  avg ([adr]) as Avg,
                                  median([adr]) as Median,
                                  STDEV([adr]) as Sd
                          FROM F_Table')
skewness(F_Table$adr)
summary(F_Table)

# -------------- Question 6 ----------------------------------------------

# Box Plots for Exception analysis.

# Y
bpADR<-boxplot(dataset$adr, main='ADR')
bpADR$out
length(bpADR$out)

# X1 - None
bpLT<-boxplot(dataset$lead_time, main='Lead Time') 

# X3 - None
bpADDM<-boxplot(dataset$arrival_date_day_of_month, main='Arrival Day of Month') 

# X4
bpAWN<-boxplot(dataset$stays_in_week_nights, main='Stays in Week Nights') 
bpAWN$out
length(bpAWN$out)
# X4 new
bpAWNX<-subset(dataset,dataset$stays_in_week_nights<20)
bpAWNnew<-boxplot(bpAWNX$stays_in_week_nights, main='Stays in Week Nights - new ')

# X5
bpAD<-boxplot(dataset$adults, main='Number of Adults') 
bpAD$out
length(bpAD$out)

# X9
bpTSR<-boxplot(dataset$total_of_special_requests, main='Total of Special Requests') 
bpTSR$out
length(bpTSR$out)

# -------------- Question 7 ----------------------------------------------

# Cumulative distribution functions & Density functions.

# Y
hist(dataset$adr,prob=TRUE, main='',ylab = 'Density',xlab = 'ADR', col = "pink")
lines(density(dataset$adr),col="purple",lwd=4)
plot.ecdf(dataset$adr,main = "",col = "purple",ylab = 'Probability', xlab = 'ADR')

# X1 - Lead Time
hist(dataset$lead_time,prob=TRUE, main='',ylab = 'Density',xlab = 'Lead Time', col = "pink")
lines(density(dataset$lead_time),col="purple",lwd=4)
plot.ecdf(dataset$lead_time,main = "",col = "purple",ylab = 'Probability', xlab = 'Lead Time')

# X4 - Stays in Week Nights - without irregulars (bpAWNX)
hist(bpAWNX$stays_in_week_nights,prob=TRUE, main='',ylab = 'Density',xlab = 'Stays in Week Nights', col = "pink")
lines(density(bpAWNX$stays_in_week_nights),col="purple",lwd=4)
plot.ecdf(bpAWNX$stays_in_week_nights,main = "",col = "purple",ylab = 'Probability', xlab = 'Stays in Week Nights')

# X9 - Total of Special Requests
hist(dataset$total_of_special_requests,prob=TRUE, main='',ylab = 'Density',xlab = 'Total of Special Requests', col = "pink")
lines(density(dataset$total_of_special_requests),col="purple",lwd=4)
plot.ecdf(dataset$total_of_special_requests,main = "",col = "purple",ylab = 'Probability', xlab = 'Total of Special Requests')


# X3 - Arrival Day of Month
hist(dataset$arrival_date_day_of_month,prob=TRUE, main='',ylab = 'Density',xlab = 'Arrival Day of Month', col = "pink")
lines(density(dataset$arrival_date_day_of_month),col="purple",lwd=4)
plot.ecdf(dataset$arrival_date_day_of_month,main = "",col = "purple",ylab = 'Probability', xlab = 'Arrival Day of Month')


# X5 - Number of Adults
hist(dataset$adults,prob=TRUE, main='',ylab = 'Density',xlab = 'Number of Adults', col = "pink")
lines(density(dataset$adults),col="purple",lwd=4)
plot.ecdf(dataset$adults,main = "",col = "purple",ylab = 'Probability', xlab = 'Number of Adults')


# -------------- Question 8 ----------------------------------------------

library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)


# ADR and Room Type - Y & X8 :
Y_X5_gg <- ggplot(dataset, aes(x = reserved_room_type, y = adr))+
  xlab("Room Type")+ylab("ADR")+
  geom_boxplot()
Y_X5_gg+geom_jitter(shape=16, position=position_jitter(0.2), col = "blue")

# Lead Time and Arrival day of months - X1 &  X4 :
plot( x = dataset$arrival_date_day_of_month, y =dataset$lead_time, col='red', pch=1,
      xlab = "Arrival day of months", ylab = "Lead Time")+
  abline(lm(dataset$lead_time ~ dataset$arrival_date_day_of_month , col = "red"))

# Months and Arrival day of months - X2 &  X4 :
ggplot(dataset, aes(x = arrival_date_month , y = arrival_date_day_of_month))+
  geom_violin( col = "pink")+
  geom_point( col = "blue")+xlab("Months")+ylab("Arrival day of Months")

# Adults groups and AVG(Lead Time) - X5 & AVG(X1) :
A<-sqldf('SELECT adults, AVG(lead_time) FROM dataset GROUP BY  adults')
data <- data.frame(
  name=A$adults,
  value=A$`AVG(lead_time)`,3)
# Specific color for each bar? Use a well known palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set3") 
barplot(height=data$value, names=data$name, col=coul,horiz=T,xlab="Avarage Leat Time", 
        ylab="Number of Adults", ) 


# Countries and Room Types - X7 & X8 :
library(fmsb)
T_A<-sqldf('SELECT country, Count(reserved_room_type) FROM dataset WHERE reserved_room_type = "A"  GROUP BY  country')
T_D<-sqldf('SELECT country, Count(reserved_room_type) FROM dataset WHERE reserved_room_type = "D"  GROUP BY  country')
T_E<-sqldf('SELECT country, Count(reserved_room_type) FROM dataset WHERE reserved_room_type = "E"  GROUP BY  country')
T_F<-sqldf('SELECT country, Count(reserved_room_type) FROM dataset WHERE reserved_room_type = "F"  GROUP BY  country')

M<-matrix(nrow=5,ncol=4)
M<-cbind(c(1,4,6,2,66),c(1,0,3,0,12),c(0,2,5,1,12),c(1,0,2,0,7))
M.t<-t(M)
data <- as.data.frame(M.t)
colnames(data) <- unique(dataset$country)
rownames(data) <- unique(dataset$reserved_room_type)
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(12,1) , rep(0,5) , data)
# Color vector
colors_border=c(rgb(0.3,0.5,0.5,0.4), rgb(0.9,0.2,0.5,0.4) , rgb(0.8,0.5,0.1,0.4) ,rgb(0.3,0.5,0.1,0.2))
colors_in=c( rgb(0.1,0.4,0.3,0.4), rgb(0.8,0.1,0.4,0.4) , rgb(0.7,0.4,0.1,0.4) ,rgb(0.2,0.4,0.1,0.2))

# plot with default options:
radarchart( data  , axistype=2 , pfcol=colors_in ,pcol=colors_border,
            #custom polygon
            plwd=3 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", caxislabels=seq(0,20,5), cglwd=2,
            #custom labels
            vlcex=1.2 )
# Add a legend
legend(x=1.3, y=1.2, legend = rownames(data[-c(1,2),]), bty = "n", pch=16 , col=colors_in , text.col = "grey", cex=1.3, pt.cex=3)

#-------------- 


# -------------- Question 9 ---------------------------------------------------------

# Y
cbind(Freq=table(cut(dataset$adr, breaks=seq(0,225,45))),
      relative=round(prop.table(table(cut(dataset$adr, breaks = seq(0,225,45)))),2))

# X4 - Stays in Week Nights - without irregulars (bpAWNX)
cbind(Freq=table(cut(bpAWNX$stays_in_week_nights, breaks=seq(0,10,2))),
      relative=round(prop.table(table(cut(bpAWNX$stays_in_week_nights, breaks = seq(0,10,2)))),2))

# Y and X7 - Countries
cbind(Freq=table(dataset$country, cut(dataset$adr, breaks = seq(0,225,25))))
cbind(relative=round(prop.table(table(dataset$country, cut(dataset$adr, breaks = seq(0,225,25)))),3))

# Y and X1 - Months
cbind(Freq=table(dataset$arrival_date_month, cut(dataset$adr, breaks = seq(0,225,25))))
cbind(relative=round(prop.table(table(dataset$arrival_date_month, cut(dataset$adr, breaks = seq(0,225,25)))),3))


# --------------------------------- Good Options we did not use ---------------------

# Countries and Meals : (Did not use)
library(fmsb)
TA1<-sqldf('SELECT country, Count(meal) FROM dataset WHERE meal = "BB"  GROUP BY  country')
TA2<-sqldf('SELECT country, Count(meal) FROM dataset WHERE meal = "HB"  GROUP BY  country')
TA3<-sqldf('SELECT country, Count(meal) FROM dataset WHERE meal = "FB"  GROUP BY  country')

M<-matrix(nrow=5,ncol=3)
M<-cbind(TA1$`Count(meal)`,c(1,3,7,0,32),c(0,2,0,0,2))
M.t<-t(M)
data <- as.data.frame(M.t)
colnames(data) <- unique(dataset$country)
rownames(data) <- unique(dataset$meal)
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(65,15) , rep(0,5) , data)
# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

# plot with default options:
radarchart( data  , axistype=2 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=3 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=1,
            #custom labels
            vlcex=1.2 )
# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=5 , col=colors_in , text.col = "grey", cex=1, pt.cex=2)

#--------------

#-------------- 
# Countries and Months : (Did not use)
library(fmsb)
TA1<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "January"  GROUP BY  country')
TA2<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "February"  GROUP BY  country')
TA3<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "March"  GROUP BY  country')
TA7<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "July"  GROUP BY  country')
TA8<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "August"  GROUP BY  country')
TA9<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "September"  GROUP BY  country')
TA10<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "October"  GROUP BY  country')
TA11<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "November"  GROUP BY  country')
TA12<-sqldf('SELECT country, Count(arrival_date_month) FROM dataset WHERE arrival_date_month = "December"  GROUP BY  country')

M<-matrix(nrow=5,ncol=9)
M<-cbind(c(0,0,1,0,10),c(0,2,0,0,12),c(2,0,0,0,39),c(1,1,0,0,8),c(0,3,1,0,4),c(0,0,0,0,3),c(0,0,3,2,6),c(0,0,0,0,8),c(0,0,11,1,7))
M.t<-t(M)
data <- as.data.frame(M.t)
colnames(data) <- unique(dataset$country)
rownames(data) <- unique(dataset$arrival_date_month)
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(rep(40,5) , rep(0,5) , data)
# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),rgb(0.2,0.3,0.1,0.9),rgb(0.3,0.9,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4),rgb(0.2,0.3,0.1,0.9),rgb(0.3,0.9,0.1,0.9) )

# plot with default options:
radarchart( data  , axistype=2 , 
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=5 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,10,5), cglwd=2,
            #custom labels
            vlcex=1.2 )
# Add a legend
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=5 , col=colors_in , text.col = "grey", cex=1, pt.cex=2)

#--------------

# Countries and Total of Special Requests : (Did not use)
QW<-sqldf('SELECT country, SUM(total_of_special_requests) FROM dataset GROUP BY  country')
data <- data.frame(
  name=QW$country,
  value=QW$`SUM(total_of_special_requests)`,5)
# Specific color for each bar? Use a well known palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot(height=data$value, names=data$name, col=coul )
# Change border color
barplot(height=data$value, names=data$name, border="#69b3a2", col="white" )

barplot(height=data$value, names=data$name, 
        col="#69b3a2",        xlab="Total of Special Requests", 
        ylab="Countries",
        horiz=T, las=1
)

# Months and AVG(Lead Time) : (Did not use)
A<-sqldf('SELECT arrival_date_month, AVG(lead_time) FROM dataset GROUP BY  arrival_date_month')
data <- data.frame(
  name=A$arrival_date_month,
  value=A$`AVG(lead_time)`,9)
# Specific color for each bar? Use a well known palette
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
barplot(height=data$value, names=data$name, col=coul )
# Change border color
barplot(height=data$value, names=data$name, border="#69b3a2", col="white" )

barplot(height=data$value, names=data$name, 
        col="#69b3a2",        xlab="Avarage Leat Time", 
        ylab="",
        horiz=T, las=1
)











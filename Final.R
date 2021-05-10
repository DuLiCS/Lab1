library(dplyr)
library(ggplot2)



setwd("Programme/Spare-Work/20210508free/code/")
covid_data <- read.csv("covid19-download.csv")
canda_data <- subset(covid_data, pruid == 1) 
Edmonton_climate <- read.csv("Edmonton_climate_daily_Alberta_P1D.csv")
Halifax_climate <- read.csv("Halifax_climate_daily_Nova Scotia_P1D.csv")
Montreal_climate <- read.csv("Montreal_climate_daily_Quebec_P1D.csv")
StJohn_climate <- read.csv("St.John's_climate_daily_Newfounland and Labrador_P1D.csv")
Toronto_climate <- read.csv("Toronto_climate_daily_Ontario_P1D.csv")
Vancouver_climate <- read.csv("Vancouver_climate_daily_British Columbia_P1D.csv")

Edmonton_data <- subset(covid_data, pruid == 48)
colnames(Edmonton_climate)[5] = 'date'
Edmonton_merge <- merge(Edmonton_data, Edmonton_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <- c()
total_precip <- c()

for (m in 1:5)
{temp_set <- subset(Edmonton_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])
}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - 19000)/600, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*600 + 19000, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Edmonton")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pEdmonton.png",width = 10,height = 10,limitsize = F)

a<-2000
b<-4000
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = total_precip, color = "total precip"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Edmonton")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "precipEdmonton.png",width = 10,height = 10,limitsize = F)




#Edmonton 48   Halifax 12 Montreal 24 StJohn's 10 Toronto 35 Vancouver 59
Halifax_data <- subset(covid_data, pruid == 12)
colnames(Halifax_climate)[5] = 'date'
Halifax_merge <- merge(Halifax_data, Halifax_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <-c()
total_precip <- c()

for (m in 1:5)
{temp_set <- subset(Halifax_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Total.Precip..mm.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Total.Precip..mm.))
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])
}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
a<-79.333
b<-406.6667
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Halifax")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pHalifax.png",width = 10,height = 10,limitsize = F)




Montreal_data <- subset(covid_data, pruid == 12)
colnames(Montreal_climate)[5] = 'date'
Montreal_merge <- merge(Montreal_data, Montreal_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <-c()
total_precip <- c()

for (m in 1:5)
{temp_set <- subset(Montreal_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])
}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
a<-60
b<-600
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Montreal")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pMontreal.png",width = 10,height = 10,limitsize = F)

a<-13.333
b<<-0
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = total_precip, color = "total precip"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Montreal")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "precipMontreal.png",width = 10,height = 10,limitsize = F)






StJohn_data <- subset(covid_data, pruid == 10)
colnames(StJohn_climate)[5] = 'date'
StJohn_merge <- merge(StJohn_data, StJohn_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <-c()
total_precip <- c()
for (m in 1:5)
{temp_set <- subset(StJohn_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])

}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
a<-26.36364
b<-131.8182
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "StJohn")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pStJohn.png",width = 10,height = 10,limitsize = F)

a<-1.611111
b<-0
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = total_precip, color = "total precip"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "StJohn")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "precipStJohn.png",width = 10,height = 10,limitsize = F)











Toronto_data <- subset(covid_data, pruid == 35)
colnames(Toronto_climate)[5] = 'date'
Toronto_merge <- merge(Toronto_data, Toronto_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <-c()
total_precip <- c()
for (m in 1:5)
{temp_set <- subset(Toronto_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])

}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
a<-3529.412
b<34705.88
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Toronto")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pToronto.png",width = 10,height = 10,limitsize = F)


a<-1060
b<12000
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = total_precip, color = "total precip"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Toronto")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "precipToronto.png",width = 10,height = 10,limitsize = F)







Vancouver_data <- subset(covid_data, pruid == 59)
colnames(Vancouver_climate)[5] = 'date'
Vancouver_merge <- merge(Vancouver_data, Vancouver_climate,by = 'date', all=F)
mean_temp <- c()
num_confirmed <-c()
total_precip <- c()
for (m in 1:5)
{temp_set <- subset(Vancouver_merge, Month == m) 
print(m)
early_month <- subset(temp_set, Day>=1 & Day<15)
early_month <- subset(early_month, !is.na(Mean.Temp...C.))
late_month <- subset(temp_set, Day>=15 & Day <=31)
late_month <- subset(late_month, !is.na(Mean.Temp...C.))
mean_temp[(length(mean_temp) + 1)] <- mean(early_month[,54])
mean_temp[(length(mean_temp) + 1)] <- mean(late_month[,54])
num_confirmed[(length(num_confirmed) + 1)] <- sum(early_month[,16])
num_confirmed[(length(num_confirmed) + 1)] <- sum(late_month[,16])
total_precip[(length(total_precip) + 1)] <- sum(early_month[,64])
total_precip[(length(total_precip) + 1)] <- sum(late_month[,64])

}
#date_stamp <- c("Early Jan","Late Jan","Early Feb","Late Feb","Early Mar","Late Mar","Early April","Late April","Early May","Late May")
date_stamp <- c(1,2,3,4,5,6,7,8,9,10)
dd <- data.frame(date_stamp,mean_temp,num_confirmed,total_precip)
dd <- subset(dd, !is.na(mean_temp))
summary(dd)
a<-1000
b<3000
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = mean_temp, color = "mean temperature"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Vancouver")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "pVancouver.png",width = 10,height = 10,limitsize = F)

a<-85.71429
b<3000
p <- ggplot(dd, aes(x = date_stamp))+
  geom_line(aes(y = total_precip, color = "total precip"))+
  geom_line(aes(y = (num_confirmed - b)/a, color = "num of confirmed cases"))+
  scale_y_continuous(sec.axis = sec_axis(~.*a + b, name = "num of confirmed cases"))+
  scale_color_manual(values = c("red","green"))+
  labs(title = "Vancouver")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12))
ggsave(p,filename = "precipVancouver.png",width = 10,height = 10,limitsize = F)


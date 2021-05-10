library(dplyr)
library(ggplot2)

setwd("Programme/Spare-Work/20210508free/code/")
covid_data <- read.csv("covid19-download.csv")
canda_data <- subset(covid_data, pruid == 1) 
arrange(canda_data, canda_data[,4])
p2 <- ggplot(data = canda_data, mapping = aes(x = date, y = numtoday, colour=numtoday)) + geom_point(shape=19,size=0.1) + labs(title= "Number of New Cases Since Last Update", x = "Date", y = "Number of New Confirmed Cases") + scale_x_discrete(breaks = c("2020-01-31", "2020-08-11", "2020-12-31","2021-05-08"),labels = c("2020-01-31", "2020-08-11", "2020-12-31","2021-05-08")) + scale_color_gradientn(colors = c("red","#008800","#002233","#002299")) +theme(text = element_text(),
                                                                                                                                                                                                                                                                                                                                                                                                                                            axis.text.x = element_text(angle=45, hjust=1)) 

ggsave(p2,filename = "numofnew.png",width = 10,height = 10,limitsize = F)

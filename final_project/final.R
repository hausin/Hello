library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ggpubr)

date16=array(0:0,c(31,12))
date15=array(0:0,c(31,12))
date14=array(0:0,c(31,12))
topval=array(0:0,c(5,12))
lowval=array(0:0,c(5,12))
#date[1,1]=1

MyData16 <- read.csv(file="/Users/ymc/Desktop/R_final_proj/cleanData2016.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
MyData15 <- read.csv(file="/Users/ymc/Desktop/R_final_proj/cleanData2015.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
MyData14 <- read.csv(file="/Users/ymc/Desktop/R_final_proj/cleanData2014.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

for (i in 1:nrow(MyData16))
  date16[as.numeric(substr(MyData16$Date[i],9,10)),as.numeric(substr(MyData16$Date[i],6,7))] = date16[as.numeric(substr(MyData16$Date[i],9,10)),as.numeric(substr(MyData16$Date[i],6,7))] + 1

for (i in 1:nrow(MyData15))
  date15[as.numeric(substr(MyData15$Date[i],9,10)),as.numeric(substr(MyData15$Date[i],6,7))] = date15[as.numeric(substr(MyData15$Date[i],9,10)),as.numeric(substr(MyData15$Date[i],6,7))] + 1

for (i in 1:nrow(MyData14))
  date14[as.numeric(substr(MyData14$Date[i],9,10)),as.numeric(substr(MyData14$Date[i],6,7))] = date14[as.numeric(substr(MyData14$Date[i],9,10)),as.numeric(substr(MyData14$Date[i],6,7))] + 1

acc_se16 <- MyData16$Accident_Severity
acc_se15 <- setNames(data.frame(table(MyData15$Accident_Severity)),c("Accident_Severity","Count"))
acc_se14 <- setNames(data.frame(table(MyData14$Accident_Severity)),c("Accident_Severity","Count"))
totalacc_se <- merge(MyData16,MyData15)

datetable16 <- setNames(data.frame(table(MyData16$Date)),c("Date","Count"))
datetable15 <- setNames(data.frame(table(MyData15$Date)),c("Date","Count"))
datetable14 <- setNames(data.frame(table(MyData14$Date)),c("Date","Count"))

for (i in 1:12)
  topval[,i]=order(date16[,i], decreasing = TRUE)[1:5]
for (i in 1:12)
  lowval[,i]=order(date16[,i], decreasing = FALSE)[1:5]

bp16 <- ggplot(MyData16, aes(x = Accident_Severity, fill = Accident_Severity)) +
  geom_bar() +   # this tells R you want a bar chart
  theme_light() +   # this adds a theme
  labs(x = "Accident Severity",   # this labels the x axis
       y = "Count",   # this labels the y axis
       title = "2016") # title of the plot
pie16 <- bp16 + coord_polar("y")
bp15 <- ggplot(MyData15, aes(x = Accident_Severity, fill = Accident_Severity)) +
  geom_bar() +   # this tells R you want a bar chart
  theme_light() +   # this adds a theme
  labs(x = "Accident Severity",   # this labels the x axis
       y = "Count",   # this labels the y axis
       title = "2015") # title of the plot
pie15 <- bp15 + coord_polar("y")
bp14 <- ggplot(MyData14, aes(x = Accident_Severity, fill = Accident_Severity)) +
  geom_bar() +   # this tells R you want a bar chart
  theme_light() +   # this adds a theme
  labs(x = "Accident Severity",   # this labels the x axis
       y = "Count",   # this labels the y axis
       title = "2014") # title of the plot
pie14 <- bp14 + coord_polar("y")
ggarrange(pie16, pie15, pie14, nrow = 2, ncol = 2)

ggplot(MyData16, aes(x = Date, fill = Date)) +
  geom_bar() +   # this tells R you want a bar chart
  theme_light() +   # this adds a theme
  labs(x = "Accident Severity",   # this labels the x axis
       y = "Count",   # this labels the y axis
       title = "") # title of the plot

newdt <- datetable16[1:31,]
jan16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1))+
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "January16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[1:31,]
jan15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1))+
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "January15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[1:31,]
jan14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1))+
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "January14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(jan16, jan15, jan14, nrow = 3)

newdt <- datetable16[32:60,]
feb16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Febuary16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[32:59,]
feb15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Febuary15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[32:59,]
feb14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "Febuary14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(feb16, feb15, feb14, nrow = 3)

newdt <- datetable16[61:91,]
mar16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "March16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[60:90,]
mar15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "March15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[60:90,]
mar14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "March14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(mar16, mar15, mar14, nrow = 3)

newdt <- datetable16[92:121,]
apr16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "April16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[91:120,]
apr15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "April15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[91:120,]
apr14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "April14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(apr16, apr15, apr14, nrow = 3)

newdt <- datetable16[122:152,]
may16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "May16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[121:151,]
may15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "May15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[121:151,]
may14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "May14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(may16, may15, may14, nrow = 3)

newdt <- datetable16[153:182,]
jun16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "June16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[152:181,]
jun15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "June15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[152:181,]
jun14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "June14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(jun16, jun15, jun14, nrow = 3)

newdt <- datetable16[183:213,]
jul16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "July16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[182:212,]
jul15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "July15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[182:212,]
jul14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "July14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(jul16, jul15, jul14, nrow = 3)

newdt <- datetable16[214:244,]
aug16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "August16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[213:243,]
aug15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "August15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[213:243,]
aug14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "August14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(aug16, aug15, aug14, nrow = 3)

newdt <- datetable16[245:274,]
sep16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "September16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[244:273,]
sep15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "September15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[244:273,]
sep14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "September14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(sep16, sep15, sep14, nrow = 3)

newdt <- datetable16[275:305,]
oct16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "October16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[274:304,]
oct15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "October15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[274:304,]
oct14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "October14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(oct16, oct15, oct14, nrow = 3)

newdt <- datetable16[306:335,]
nov16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "November16")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[305:334,]
nov15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "November15")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[305:334,]
nov14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "November14")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(nov16, nov15, nov14, nrow = 3)

newdt <- datetable16[336:366,]
dec16 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "December")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable15[335:365,]
dec15 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "December")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
newdt <- datetable14[335:365,]
dec14 <- ggplot(data=newdt, aes(x=Date, y=Count, group=1)) +
  geom_line(linetype="dashed", color = "blue")+
  geom_point(color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "December")+
  geom_hline(yintercept = mean(newdt$Count), color="gold2")
ggarrange(dec16, dec15, dec14, nrow = 3)

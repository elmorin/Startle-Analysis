bl <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Startle_BL_Trajectory_Data.csv", header=TRUE)

library(ggplot2)

quartz()

Bf13 <- ggplot(data = bl[bl$Subject=="Bf13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Bf13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Nc14 <- ggplot(data = bl[bl$Subject=="Nc14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Nc14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Zu14 <- ggplot(data = bl[bl$Subject=="Zu14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Zu14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
On14 <- ggplot(data = bl[bl$Subject=="On14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("On14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
St14 <- ggplot(data = bl[bl$Subject=="St14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("St14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Uu14 <- ggplot(data = bl[bl$Subject=="Uu14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Uu14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")

#control females
multiplot(Bf13, Nc14, Zu14, On14, St14, Uu14, cols=3)

Bo14 <- ggplot(data = bl[bl$Subject=="Bo14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Bo14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Fo14 <- ggplot(data = bl[bl$Subject=="Fo14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Fo14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Nt14 <- ggplot(data = bl[bl$Subject=="Nt14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Nt14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ym14 <- ggplot(data = bl[bl$Subject=="Ym14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ym14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Tm15 <- ggplot(data = bl[bl$Subject=="Tm15",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Tm15 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")

#control males
quartz()
multiplot(Bo14, Fo14, Nt14, Ym14, Tm15, cols=3)

Im13 <- ggplot(data = bl[bl$Subject=="Im13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Im13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Rv13 <- ggplot(data = bl[bl$Subject=="Rv13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Rv13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ul13 <- ggplot(data = bl[bl$Subject=="Ul13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ul13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ws14 <- ggplot(data = bl[bl$Subject=="Ws14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ws14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ct15 <- ggplot(data = bl[bl$Subject=="Ct15",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ct15 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ws15 <- ggplot(data = bl[bl$Subject=="Ws15",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ws15 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")

#maltreated females
quartz()
multiplot(Im13, Rv13, Ul13, Ws14, Ct15, Ws15, cols=3)

Lc14 <- ggplot(data = bl[bl$Subject=="Lc14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Lc14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Dw13 <- ggplot(data = bl[bl$Subject=="Dw13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Dw13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Pv13 <- ggplot(data = bl[bl$Subject=="Pv13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Pv13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Py13 <- ggplot(data = bl[bl$Subject=="Py13",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Py13 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Kr14 <- ggplot(data = bl[bl$Subject=="Kr14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Kr14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Nn14 <- ggplot(data = bl[bl$Subject=="Nn14",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Nn14 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Ta15 <- ggplot(data = bl[bl$Subject=="Ta15",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Ta15 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")
Df15 <- ggplot(data = bl[bl$Subject=="Df15",], 
  aes(x = Task, y = Amplitude, group=dB, colour = dB))+ 
  geom_line()+
  ggtitle("Df15 - Baseline Startle")+
  theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+
  labs(y = "Startle Amplitude (mV)", x="Baseline Session")

#maltreated males
quartz()
multiplot(Lc14, Dw13, Pv13, Py13, Kr14, Nn14, cols=3)
multiplot(Ta15, Df15, cols=3, rows=2)

all <- ggplot(bl, aes(Task, Amplitude, group = Subject, colour = Subject, linetype=Sex))+ geom_line()+ scale_color_manual(values=c("Black", "Black", "Red", "Red", "Red","Black", "Red", "Red", "Red", "Black","Red", "Black", "Black", "Red", "Red"))
#group means
groupbl.mean <- aggregate(Amplitude ~ Sex + Condition + Task, bl, mean)
groupbl.err <- aggregate(Amplitude ~ Sex + Condition + Task, bl, st.err)
groupbl.all <- cbind(groupbl.mean, groupbl.err[,4])

all <- ggplot(groupbl.all, aes(as.numeric(Task), Amplitude, colour = Condition, linetype=Sex))+ geom_line()+ scale_color_manual(values=c("Black", "Red"))+ ggtitle("ALL - Baseline Startle (grouped)")+theme(plot.title = element_text(color="Black", size=12, hjust = 0.5))+labs(y = "Startle Amplitude (mV)", x="Baseline Session")+ theme(legend.text=element_text(size=9),legend.title=element_text(size=12), axis.text=element_text(size=12),axis.title=element_text(size=12,))+geom_errorbar(aes(ymin=Amplitude-se, ymax=Amplitude+se), width=0.1)+scale_x_discrete(name ="Session",limits=c('First Baseline','Probe','Second Baseline'))

all

#plot just the first baseline
firstbl <- bl[bl$Task=="First Baseline",]
firstbl.mean <- aggregate(Amplitude ~ Sex + Condition, firstbl, mean)
firstbl.err <- aggregate(Amplitude ~ Sex + Condition, firstbl, st.err)
firstbl.all <- cbind(firstbl.mean, firstbl.err[,3])
names(firstbl.all)[4]<-"se"

firstbl.all.plot <- ggplot(firstbl.all, aes(Condition, Amplitude, colour = Sex, group=Sex))+ geom_point(aes(),na.rm=TRUE, position=position_dodge(width=0.9), size=3.5) +scale_color_manual(values=c("Magenta", "Blue")) + ggtitle("Pre-Training Baseline Startle")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Startle Amplitude (mV)")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=Amplitude-se, ymax=Amplitude+se), width=0.2, position=position_dodge(width=0.9))+facet_grid(.~Condition,scales="free_x")+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+scale_y_continuous(limits = c(0, 1.25),breaks=seq(0,1.2,by=0.2))+theme(strip.text.x = element_text(size = 16))
firstbl.all.plot

firstbl.aov <- aov(Amplitude ~ Condition*Sex, data=firstbl)
summary(firstbl.aov)



#______________________________
#after committee meeting
#for First Baseline prior to discrimination training
#remove first two trials, then take the average of the rest
#plot day 1 and day 2 separately

st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}


bl <- read.csv("/Users/ElyseLouise/Desktop/Data Analysis/Startle Data Analysis/NIDA_Startle_BL_Sessions_Data.csv", header=TRUE)

blr2 <- bl[bl$Trial!=1:2,]
blr2.mean.subj <- aggregate(Startle ~ Subject + Day+ Condition+Sex, blr2, mean)
blr2.err.subj <- aggregate(Startle ~ Subject+ Day+ Condition+ Sex, blr2, st.err)
blr2.mean.err.subj <- cbind(blr2.mean.subj, blr2.err.subj[,5])
names(blr2.mean.err.subj)[5]<-"se"




##Bar plots
blr2.mean.all <- aggregate(Startle ~ Day+ Condition+ Sex, blr2, mean)
blr2.err.all <- aggregate(Startle ~ Day+ Condition+ Sex, blr2, st.err)
blr2.mean.err.all <- cbind(blr2.mean.all, blr2.err.all[,4])
names(blr2.mean.err.all)[5]<-"se"
#Baseline ~ Condition + Sex
blr2.mean.all.plot <- ggplot(blr2.mean.err.all, aes(as.numeric(Day), Startle, colour = Sex, linetype = Condition))+geom_line(aes(linetype=Condition))+ geom_point(aes(),na.rm=TRUE, size=2)+scale_color_manual(values=c("Magenta", "Blue")) + ggtitle("Baseline Startle - by Condition & Sex")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Startle Amplitude (mV)")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=Startle-se, ymax=Startle+se), width=0.2)+facet_grid(.~Condition,scales="free_x")+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(strip.text.x = element_text(size = 16))+scale_x_discrete(name ="Session",limits=c("Day 1","Day 2"))
blr2.mean.all.plot
#Baseline Day 1
blr2d1 <- blr2[blr2$Day==1,]
blr2d1.mean.all <- aggregate(Startle ~ Condition+ Sex, blr2d1, mean)
blr2d1.err.all <- aggregate(Startle ~ Condition+ Sex, blr2d1, st.err)
blr2d1.mean.err.all <- cbind(blr2d1.mean.all, blr2d1.err.all[,3])
names(blr2d1.mean.err.all)[4]<-"se"
blr2d1.mean.all.plot <- ggplot(blr2d1.mean.err.all, aes(x=Condition,y=Startle,group=Sex))+ geom_bar(aes(fill=Sex), position = "dodge", stat="identity")+scale_color_manual(values=c("Magenta", "Blue")) + ggtitle("Baseline Startle - Day 1")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Startle Amplitude (mV)")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=Startle-se, ymax=Startle+se), width=0.2, position=position_dodge(0.9))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(strip.text.x = element_text(size = 16))+facet_grid(.~Condition,scales="free_x")+scale_fill_manual("Sex", values = c("Male" = "blue", "Female" = "magenta"))+ylim(0, 3)
blr2d1.mean.all.plot
#Baseline Day 2
blr2d2 <- blr2[blr2$Day==2,]
blr2d2.mean.all <- aggregate(Startle ~ Condition+ Sex, blr2d2, mean)
blr2d2.err.all <- aggregate(Startle ~ Condition+ Sex, blr2d2, st.err)
blr2d2.mean.err.all <- cbind(blr2d2.mean.all, blr2d2.err.all[,3])
names(blr2d2.mean.err.all)[4]<-"se"
blr2d2.mean.all.plot <- ggplot(blr2d2.mean.err.all, aes(x=Condition,y=Startle,group=Sex))+ geom_bar(aes(fill=Sex), position = "dodge", stat="identity")+scale_color_manual(values=c("Magenta", "Blue")) + ggtitle("Baseline Startle - Day 2")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Startle Amplitude (mV)")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=Startle-se, ymax=Startle+se), width=0.2, position=position_dodge(0.9))+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(strip.text.x = element_text(size = 16))+facet_grid(.~Condition,scales="free_x")+scale_fill_manual("Sex", values = c("Male" = "blue", "Female" = "magenta"))+ylim(0, 3)
blr2d2.mean.all.plot



## Correlation with 5HT1A BP in PFC
#-        Ligand 1: Fallypride (D2 R ligand)
#-        Ligand 2: MPPF (5HT1A R ligand)
#-        Ligand 3: M100 (5HT2A R ligand).



BP1 <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/PET_Expl_DVRNorm_1_5HT1A.csv")
BP2 <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/PET_Expl_DVRNorm_1_5HT2.csv", stringsAsFactors=FALSE)
BP2 <- BP2[1:25,]
BP2[BP2$Subj=="Uu14",] <- NA
BP2 <- na.omit(BP2)
BP2$PFC <- as.numeric(BP2$PFC)
BP3 <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/PET_Expl_DVRNorm_1_5HT3.csv")
BP3[BP3$Subj=="Df15",] <- NA
BP3 <- na.omit(BP3)
BP3$PFC <- as.numeric(BP3$PFC)

#This is actually D2
cor.test(BP1$BL_startle, BP1$PFC)
#This is actually 5HT1A
cor.test(BP2$BL_startle, BP2$PFC)
#This is actually 5HT2A
cor.test(BP3$BL_startle, BP3$PFC)

BL_5HT1Acorr <- plot(BP2$PFC,BP2$BL_startle)
abline(BL_5HT1Acorr)

BP2[BP2$Subj=="RLc14",] <- NA
BP2[BP2$Subj=="Ws15",] <- NA
BP2 <- na.omit(BP2)

##################################################################
######PRE BASELINE
##All dB levels graph

library(reshape)

alldb <- read.csv('/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_all_dB.csv', header=TRUE)

alldb_melt <- melt(alldb, id=c("Subject","Condition","Sex", "Day", "Trial"))
#remove first 2 trials
alldb_melt_1 <- alldb_melt[!(alldb_melt$Trial==1 & alldb_melt$variable=="X115dB" & alldb_melt$Day==1),]
alldb_melt_1 <- alldb_melt_1[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X95dB" & alldb_melt_1$Day==1),]
alldb_melt_1 <- alldb_melt[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X120dB" & alldb_melt_1$Day==2),]
alldb_melt_2 <- alldb_melt_1[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X10dB" & alldb_melt_1$Day==2),]

#average by dB level
#day 1
#alldb_melt_2 <- alldb_melt_2[alldb_melt_2$Day==1,]
#recode variable dB
library(plyr)
alldb_melt_2$variable <- revalue(alldb_melt_2$variable, c("X95dB"="95","X100dB"="100","X110dB"="110","X115dB"="115","X120dB"="120"))
alldb.mean.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, mean)
alldb.err.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, st.err)
alldb.mean.err.subj <- cbind(alldb.mean.subj, alldb.err.subj[,5])
names(alldb.mean.err.subj)[6]<-"se"
names(alldb.mean.err.subj)[5]<-"mean"

alldb.mean.err.subj_day1 <- alldb.mean.err.subj

alldb.mean <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, mean)
alldb.err <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, st.err)
alldb.mean.err <- cbind(alldb.mean, alldb.err[,4])
names(alldb.mean.err)[5]<-"se"
names(alldb.mean.err)[4]<-"mean"


#day 2
alldb_melt_2 <- alldb_melt_2[!(alldb_melt_2$Trial==1 & alldb_melt_2$variable=="X100dB"),]
#recode variable dB
library(plyr)
alldb_melt_2$variable <- revalue(alldb_melt_2$variable, c("X95dB"="95","X100dB"="100","X110dB"="110","X115dB"="115","X120dB"="120"))
alldb_melt_2 <- alldb_melt_2[alldb_melt_2$Day==2,]
alldb.mean.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, mean)
alldb.err.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, st.err)
alldb.mean.err.subj <- cbind(alldb.mean.subj, alldb.err.subj[,5])
names(alldb.mean.err.subj)[6]<-"se"
names(alldb.mean.err.subj)[5]<-"mean"

alldb.mean.err.subj_day2 <- alldb.mean.err.subj

alldb.mean <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, mean)
alldb.err <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, st.err)
alldb.mean.err <- cbind(alldb.mean, alldb.err[,4])
names(alldb.mean.err)[5]<-"se"
names(alldb.mean.err)[4]<-"mean"

alldb.mean.err_day2 <- alldb.mean.err

##activity

activity <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_activity.csv", header=TRUE)

activity <- activity[!(activity$Trial==1:2),]


activity.mean <- aggregate(Activity ~ Day+ Condition+variable,activity, mean)
activity.err <- aggregate(Activity ~ Day + Condition+variable, activity, st.err)
activity.mean.err <- cbind(activity.mean, activity.err[,4])
names(activity.mean.err)[5]<-"se"
names(activity.mean.err)[4]<-"mean"

activity_1 <- activity[activity$Day==1,]

activity.mean.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex,activity_1, mean)
activity.err.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex, activity_1, st.err)
activity.mean.err.subj <- cbind(activity.mean.subj, activity.err.subj[,5])
names(activity.mean.err.subj)[6]<-"se"
names(activity.mean.err.subj)[5]<-"mean"

activity.mean.err.subj_1 <- activity.mean.err.subj

activity_2 <- activity[activity$Day==2,]

activity.mean.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex,activity_2, mean)
activity.err.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex, activity_2, st.err)
activity.mean.err.subj <- cbind(activity.mean.subj, activity.err.subj[,5])
names(activity.mean.err.subj)[6]<-"se"
names(activity.mean.err.subj)[5]<-"mean"

activity.mean.err.subj_2 <- activity.mean.err.subj

#final datasets
#for barplots
activity.mean.err_1 <- activity.mean.err[activity.mean.err$Day==1,]
db_act_day1 <- rbind(activity.mean.err_1, alldb.mean.err_day1)

activity.mean.err_2 <- activity.mean.err[activity.mean.err$Day==2,]
db_act_day2 <- rbind(activity.mean.err_2, alldb.mean.err_day2)

#for geompoint
#db_act_subj_day1 <- rbind(activity.mean.err.subj_1, alldb.mean.err.subj_day1)
#db_act_subj_day2 <- rbind(activity.mean.err.subj_2, alldb.mean.err.subj_day2)
db_act_day1 <- alldb.mean.err_day1[alldb.mean.err_day1$variable!="Training_days",]
db_act_day2 <- alldb.mean.err_day2[alldb.mean.err_day2$variable!="Training_days",]

#to fix plot in slide
alldb.mean.err_1 <- alldb.mean.err[alldb.mean.err$Day==1,]
alldb.mean.err_2 <- alldb.mean.err[alldb.mean.err$Day==2,]
alldb.mean.err_1 <- alldb.mean.err_1[alldb.mean.err_1$variable!="Training_days",]
alldb.mean.err_2 <- alldb.mean.err_2[alldb.mean.err_2$variable!="Training_days",]

library(ggplot2)
#barplot
#day1
p1 <- ggplot(alldb.mean.err_1, aes(x=variable,y=mean,group=Condition))+ geom_bar(aes(fill=Condition), position = "dodge", stat="identity")+scale_color_manual(values=c("Black", "Red")) + ggtitle("Baseline Startle - Day 1")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Mean Startle Amplitude (mV)", x="dB Tone")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+theme(strip.text.x = element_text(size = 16))+scale_fill_manual("Condition", values = c("Control" = "black", "Maltreated" = "red"))+ylim(0, 4)
# now draw the points on the plot
p2 <- p1 + geom_point(data = db_act_subj_day1, aes(x=variable,y=mean,group=Condition,colour = factor(Sex)), position = position_dodge(width=0.9), stat="identity")+scale_color_manual("Sex", values = c("Female" = "magenta", "Male" = "blue"))
p1
View(db_act_day1)

#day2
#day1
p3 <- ggplot(alldb.mean.err_2, aes(x=variable,y=mean,group=Condition))+ geom_bar(aes(fill=Condition), position = "dodge", stat="identity")+scale_color_manual(values=c("Black", "Red")) + ggtitle("Baseline Startle - Day 2")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Mean Startle Amplitude (mV)", x="dB Tone")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+theme(strip.text.x = element_text(size = 16))+scale_fill_manual("Condition", values = c("Control" = "black", "Maltreated" = "red"))+ylim(0, 4)
# now draw the points on the plot
p4 <- p3 + geom_point(data = db_act_subj_day2, aes(x=variable,y=mean,group=Condition,colour = factor(Sex)), position = position_dodge(width=0.9), stat="identity")+scale_color_manual("Sex", values = c("Female" = "magenta", "Male" = "blue"))
p3



##################################################################
######POST BASELINE
##All dB levels graph

library(re)

alldb <- read.csv('/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Post_Baseline_all_dB.csv', header=TRUE)

alldb_melt <- melt(alldb, id=c("Subject","Condition","Sex", "Day", "Trial"))
#remove first 2 trials
alldb_melt_1 <- alldb_melt[!(alldb_melt$Trial==1 & alldb_melt$variable=="X115dB" & alldb_melt$Day==1),]
alldb_melt_1 <- alldb_melt_1[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X95dB" & alldb_melt_1$Day==1),]
alldb_melt_1 <- alldb_melt[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X120dB" & alldb_melt_1$Day==2),]
alldb_melt_2 <- alldb_melt_1[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X10dB" & alldb_melt_1$Day==2),]

#average by dB level
#day 1
alldb_melt_2 <- alldb_melt_2[alldb_melt_2$Day==1,]
#recode variable dB
library(plyr)
alldb_melt_2$variable <- revalue(alldb_melt_2$variable, c("X95dB"="95","X100dB"="100","X110dB"="110","X115dB"="115","X120dB"="120"))
alldb.mean.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, mean)
alldb.err.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, st.err)
alldb.mean.err.subj <- cbind(alldb.mean.subj, alldb.err.subj[,5])
names(alldb.mean.err.subj)[6]<-"se"
names(alldb.mean.err.subj)[5]<-"mean"

alldb.mean.err.subj_day1 <- alldb.mean.err.subj

alldb.mean <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, mean)
alldb.err <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, st.err)
alldb.mean.err <- cbind(alldb.mean, alldb.err[,4])
names(alldb.mean.err)[5]<-"se"
names(alldb.mean.err)[4]<-"mean"

alldb.mean.err_day1 <- alldb.mean.err

#day 2
alldb_melt_2 <- alldb_melt_1[!(alldb_melt_1$Trial==1 & alldb_melt_1$variable=="X100dB"),]
#recode variable dB
library(plyr)
alldb_melt_2$variable <- revalue(alldb_melt_2$variable, c("X95dB"="95","X100dB"="100","X110dB"="110","X115dB"="115","X120dB"="120"))
alldb_melt_2 <- alldb_melt_2[alldb_melt_2$Day==2,]
alldb.mean.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, mean)
alldb.err.subj <- aggregate(value ~ Subject +Condition+variable+Sex, alldb_melt_2, st.err)
alldb.mean.err.subj <- cbind(alldb.mean.subj, alldb.err.subj[,5])
names(alldb.mean.err.subj)[6]<-"se"
names(alldb.mean.err.subj)[5]<-"mean"

alldb.mean.err.subj_day2 <- alldb.mean.err.subj

alldb.mean <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, mean)
alldb.err <- aggregate(value ~ Day+ Condition+variable, alldb_melt_2, st.err)
alldb.mean.err <- cbind(alldb.mean, alldb.err[,4])
names(alldb.mean.err)[5]<-"se"
names(alldb.mean.err)[4]<-"mean"

alldb.mean.err_day2 <- alldb.mean.err

##activity

activity <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Post_Baseline_activity.csv", header=TRUE)

activity <- activity[!(activity$Trial==1:2),]


activity.mean <- aggregate(Activity ~ Day+ Condition+variable,activity, mean)
activity.err <- aggregate(Activity ~ Day + Condition+variable, activity, st.err)
activity.mean.err <- cbind(activity.mean, activity.err[,4])
names(activity.mean.err)[5]<-"se"
names(activity.mean.err)[4]<-"mean"

activity_1 <- activity[activity$Day==1,]

activity.mean.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex,activity_1, mean)
activity.err.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex, activity_1, st.err)
activity.mean.err.subj <- cbind(activity.mean.subj, activity.err.subj[,5])
names(activity.mean.err.subj)[6]<-"se"
names(activity.mean.err.subj)[5]<-"mean"

activity.mean.err.subj_1 <- activity.mean.err.subj

activity_2 <- activity[activity$Day==2,]

activity.mean.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex,activity_2, mean)
activity.err.subj <- aggregate(Activity ~ Subject + Condition+variable+Sex, activity_2, st.err)
activity.mean.err.subj <- cbind(activity.mean.subj, activity.err.subj[,5])
names(activity.mean.err.subj)[6]<-"se"
names(activity.mean.err.subj)[5]<-"mean"

activity.mean.err.subj_2 <- activity.mean.err.subj

#final datasets
#for barplots
activity.mean.err_1 <- activity.mean.err[activity.mean.err$Day==1,]
db_act_day1 <- rbind(activity.mean.err_1, alldb.mean.err_day1)

activity.mean.err_2 <- activity.mean.err[activity.mean.err$Day==2,]
db_act_day2 <- rbind(activity.mean.err_2, alldb.mean.err_day2)

#for geompoint
db_act_subj_day1 <- rbind(activity.mean.err.subj_1, alldb.mean.err.subj_day1)
db_act_subj_day2 <- rbind(activity.mean.err.subj_2, alldb.mean.err.subj_day2)

#barplot
#day1
p1 <- ggplot(db_act_day1, aes(x=variable,y=mean,group=Condition))+ geom_bar(aes(fill=Condition), position = "dodge", stat="identity")+scale_color_manual(values=c("Black", "Red")) + ggtitle("Post Baseline Startle - Day 1")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Mean Startle Amplitude (mV)", x="dB Tone")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+theme(strip.text.x = element_text(size = 16))+scale_fill_manual("Condition", values = c("Control" = "black", "Maltreated" = "red"))+ylim(0, 12)
# now draw the points on the plot
p2 <- p1 + geom_point(data = db_act_subj_day1, aes(x=variable,y=mean,group=Condition,colour = factor(Sex)), position = position_dodge(width=0.9), stat="identity")+scale_color_manual("Sex", values = c("Female" = "magenta", "Male" = "blue"))
p2

#day2
#day1
p3 <- ggplot(db_act_day2, aes(x=variable,y=mean,group=Condition))+ geom_bar(aes(fill=Condition), position = "dodge", stat="identity")+scale_color_manual(values=c("Black", "Red")) + ggtitle("Post Baseline Startle - Day 2")+theme(plot.title = element_text(color="Black", size=20, hjust = 0.5))+labs(y = "Mean Startle Amplitude (mV)", x="dB Tone")+ theme(legend.text=element_text(size=12),legend.title=element_text(size=16), axis.text=element_text(size=16),axis.title=element_text(size=16))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.2, position=position_dodge(0.9))+theme(strip.text.x = element_text(size = 16))+scale_fill_manual("Condition", values = c("Control" = "black", "Maltreated" = "red"))+ylim(0, 12)
# now draw the points on the plot
p4 <- p3 + geom_point(data = db_act_subj_day2, aes(x=variable,y=mean,group=Condition,colour = factor(Sex)), position = position_dodge(width=0.9), stat="identity")+scale_color_manual("Sex", values = c("Female" = "magenta", "Male" = "blue"))
p4



######
#Averaging over trials to create SPSS ready dataset (rmANOVA over different dB levels and activity)

db_all <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_all_dB.csv")
activity <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_activity.csv")

ac.mean <- aggregate(Activity ~ Subject + Condition+Day+Sex,activity, mean)
db.mean95 <- aggregate(X95dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean100 <- aggregate(X100dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean110 <- aggregate(X110dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean115 <- aggregate(X115dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean120 <- aggregate(X120dB ~ Subject + Condition+Day+Sex,db_all, mean)

activity_db_all_means <- cbind(ac.mean, db.mean95[,5], db.mean100[,5],db.mean110[,5],db.mean115[,5],db.mean120[,5])
names(activity_db_all_means)[6]<-"db95"
names(activity_db_all_means)[7]<-"db100"
names(activity_db_all_means)[8]<-"db110"
names(activity_db_all_means)[9]<-"db115"
names(activity_db_all_means)[10]<-"db120"

write_csv(activity_db_all_means, "/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/BL_Pre_SPSS_rm_anova.csv")

######
# Melinda's MLM method using 0dB as covariate
# taking average of all trials at each db level, removing first two for each day
# taking average of 0dB and repeating it as the covariate
# remove day 1 - first 115 dB & 95 dB
# remove day 2 - first 120 dB & 110 dB
#remove first two trials of 0 dB
library(reshape2)

db_all <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_all_dB.csv")
activity <- read.csv("/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/NIDA_Baseline_activity.csv")

db_all$X95dB[db_all$Day==1 & db_all$Trial == 1] <- NA
db_all$X115dB[db_all$Day==1 & db_all$Trial == 1] <- NA
db_all$X120dB[db_all$Day==2 & db_all$Trial == 1] <- NA
db_all$X110dB[db_all$Day==2 & db_all$Trial == 1] <- NA

activity$Activity[activity$Day==1 & activity$Trial==1] <- NA
activity$Activity[activity$Day==1 & activity$Trial==2] <- NA
activity$Activity[activity$Day==2 & activity$Trial==1] <- NA
activity$Activity[activity$Day==2 & activity$Trial==2] <- NA

ac.mean <- aggregate(Activity ~ Subject + Condition+Day+Sex,activity, mean)
db.mean95 <- aggregate(X95dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean100 <- aggregate(X100dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean110 <- aggregate(X110dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean115 <- aggregate(X115dB ~ Subject + Condition+Day+Sex,db_all, mean)
db.mean120 <- aggregate(X120dB ~ Subject + Condition+Day+Sex,db_all, mean)

activity_db_all_means <- cbind(ac.mean, db.mean95[,5], db.mean100[,5],db.mean110[,5],db.mean115[,5],db.mean120[,5])
names(activity_db_all_means)[6]<-"db95"
names(activity_db_all_means)[7]<-"db100"
names(activity_db_all_means)[8]<-"db110"
names(activity_db_all_means)[9]<-"db115"
names(activity_db_all_means)[10]<-"db120"

new <- melt(activity_db_all_means, id=c("Subject", "Condition", "Day", "Sex"))

write.csv(new, '/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/reshape.csv')

reshape <- read.csv('/Users/elmorin/Desktop/Data Analysis/Startle Data Analysis/reshape_forSPSS.csv', header=T)

library(tidyverse)
library(cowplot)


#id pulses
dat<-read.csv("data_clean/Clean_record_info.csv")
variables <- c("ET", "T", "Gs", "PWP",
               "ecosystemR","belowgroundR",
               "NPP", "GPP", "Anet")
dat <- dat %>%
  filter(varType %in% variables) %>%
  filter(Pulse.amount > 0) %>%
  mutate(varGroup = ifelse(varType %in% c("ET", "T", "Gs", "PWP"), "water", "carbon"))
pulses<-unique(cbind(dat$Study.ID,dat$Pulse.ID))
pulseID<-numeric()
for(i in 1:dim(dat)[1]){
  c1<-dat$Study.ID[i]==pulses[,1]
  c2<-dat$Pulse.ID[i]==pulses[,2]
  pulseID[i]<-which(c1&c2)
}

#put time on same units

day<-as.numeric(dat$Time.relative.to.pulse.unit=="day")
hr<-as.numeric(dat$Time.relative.to.pulse.unit=="hr")
min<-as.numeric(dat$Time.relative.to.pulse.unit=="min")

time<-dat$Time.relative.to.pulse*day*(24*60)+
  dat$Time.relative.to.pulse*hr*60+
  dat$Time.relative.to.pulse*min

#normalize y and x axes
pulse.norm<-numeric()
peak.time<-numeric()
norm.y<-numeric() #normalizing response data to a proportion
cent.x<-numeric() #centering time axis on peak
for(p in 1:max(pulseID)){
  print(p)
  #find peak
  sel<-which(pulseID==p)
  peak.val<-max(abs(dat$Mean[sel]),na.rm=T)
  record<-match( peak.val,dat$Mean)
  peak.time<-time[record]
  #normalize
  norm.y[sel]<-dat$Mean[sel]*100/peak.val
  cent.x[sel]<-time[sel]-peak.time
}

#below is commented out because I don't see any C/H20 variable
#carbon<-which(dat$type=="CARBON")
#water<-which(dat$type=="water")

# I am imagining the first half of the data is carbon-related (ie NPP)
# and the second half is water related (ie transpiration)
carbon<- which(dat$varGroup == "carbon")
water<-which(dat$varGroup == "water")
baseC<-col2rgb("forestgreen")
col.carbon<-rgb(red = baseC[1]/255,green = baseC[2]/255,blue = baseC[3]/255,alpha=0.1)
baseW<-col2rgb("purple4")
col.water<-rgb(red = baseW[1]/255,green = baseW[2]/255,blue = baseW[3]/255,alpha=0.1)

#I can't get the loess function to work here so I made a stupid mean line
shape.C<-numeric()
shape.C[1]<-mean(norm.y[carbon][cent.x[carbon]< -200],na.rm=T)
shape.C[2]<-mean(norm.y[carbon][cent.x[carbon]>-50&cent.x[carbon]<50],na.rm=T)
shape.C[3]<-mean(norm.y[carbon][cent.x[carbon]>200],na.rm=T)
shape.W<-numeric()
shape.W[1]<-mean(norm.y[water][cent.x[water]< -200],na.rm=T)
shape.W[2]<-mean(norm.y[water][cent.x[water]>-50&cent.x[water]<50],na.rm=T)
shape.W[3]<-mean(norm.y[water][cent.x[water]>200],na.rm=T)

#the plot
plot(y=norm.y,x=cent.x,col="white",bty="l",tck=0.02,las=1,ylab="Normalized pulse magnitude",
     xlab="Time relative to peak response (min)")
points(y=norm.y[carbon],x=cent.x[carbon],pch=19,col=col.carbon)
points(y=norm.y[water],x=cent.x[water],pch=19,col=col.water)
lines(y=shape.C,x=c(-20000,0,20000),col="forestgreen",lwd=3)
lines(y=shape.W,x=c(-20000,0,20000),col="purple4",lwd=3)

# Emma's version
dat$norm.y <- norm.y
dat$cent.x <- cent.x
p <- dat %>%
  ggplot(aes(y=norm.y,x=cent.x, color = varGroup)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "loess") +
  scale_color_manual(values = c("forestgreen", "purple4")) +
  xlim(c(-50000,50000)) +
  ylab("Normalized pulse magnitude") + xlab("Time relative to peak response (min)") +
  theme_bw() +
  theme(legend.title = element_blank())
p

ggsave2("drewplot2.png", plot = p, path = "./figures/", width = 7, height = 6)


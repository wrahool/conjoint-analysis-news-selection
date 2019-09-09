rm(list=ls())
setwd("C:/ASC/21/SelectiveProject/data/replication files")
library(foreign)
library("ggplot2")

#plot drawing functions
theme_bw1 <- function(base_size = 16, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = 13, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = 13 , colour = "black", hjust = 0 , vjust=.5 ), 
      axis.ticks =        element_line(colour = "grey50"),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "none"
    )
}
draw_plot <- function(dat, range, breakseq, scale){
  ggplot(dat ,aes(y=pe,x=var, colour=group)) + 
    facet_grid(.~subsetlabel)+ 
    coord_flip(ylim = range)+ 
    geom_hline(yintercept = 0,size=.5,colour="darkgrey",linetype="solid") +
    geom_pointrange(aes(ymin=lower,ymax=upper,width=.5),position="identity",size=.7)+
    scale_y_continuous(name="Effect on Choosing the Facebook Post",breaks=breakseq,
                       labels=scale)+
    scale_x_discrete(name="")+
    scale_fill_brewer(palette="Pastel1")+
    theme_bw1()
}

#Results in Main Texts
#Figure 1
#main effects
ForcedR<-read.csv("ForcedMainF1.csv")
RatedR<-read.csv("RatedMainF1.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("F1-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("F1-2.pdf",width=12,height=6)


#Figure 2
#interaction effects
ForcedR<-read.csv("ForcedInteractionF2.csv")
RatedR<-read.csv("RatedInteractionF2.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("F2-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("F2-2.pdf",width=12,height=6)

#Robustness 1: deduplication
#Figure A3
ForcedR<-read.csv("ForcedRobust1Dedup.csv")
RatedR<-read.csv("RatedRobust1Dedup.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("FA3-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("FA3-2.pdf",width=12,height=6)

#Figure A4
ForcedR<-read.csv("ForcedRobust1DedupInt.csv")
RatedR<-read.csv("RatedRobust1DedupInt.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("FA4-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("FA4-2.pdf",width=12,height=6)

#Robustness 2: valid responses
#Figure A5
ForcedR<-read.csv("ForcedRobust2Valid.csv")
RatedR<-read.csv("RatedRobust2Valid.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("FA5-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("FA5-2.pdf",width=12,height=6)

#Figure A6
ForcedR<-read.csv("ForcedRobust2ValidInt.csv")
RatedR<-read.csv("RatedRobust2ValidInt.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
#ggsave("FA6-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
#ggsave("FA6-2.pdf",width=12,height=6)

#Robustness 3: Democrats/Republicans 
#Main Effects
#Figure A7 - Democrats
ForcedR<-read.csv("ForcedRobust3Democrats.csv")
RatedR<-read.csv("RatedRobust3Democrats.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.75,.75),seq(-.75,.75,.15), c("","-0.60","","-0.30","","0","","0.30","","0.60","") )
ggsave("FA7-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
ggsave("FA7-2.pdf",width=12,height=6)

#Figure A8 - Republicans
ForcedR<-read.csv("ForcedRobust3Republicans.csv")
RatedR<-read.csv("RatedRobust3Republicans.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
ggsave("FA8-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
ggsave("FA8-2.pdf",width=12,height=6)

#Interaction effects with strong PID
#Figure A9 - Democrats
ForcedR<-read.csv("ForcedRobust3DemocratsInt.csv")
RatedR<-read.csv("RatedRobust3DemocratsInt.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
ggsave("FA9-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
ggsave("FA9-2.pdf",width=12,height=6)

#Figure A10 - Republicans
ForcedR<-read.csv("ForcedRobust3RepublicansInt.csv")
RatedR<-read.csv("RatedRobust3RepublicansInt.csv")
dat<-rbind(ForcedR,RatedR)
dat$var <- factor(dat$var, levels=rev(unique(dat$var)))
draw_plot(dat,c(-.5,.5),seq(-.5,.5,.1), c("-.5","-.4","-.3","-.2","-.1","0",".1",".2",".3",".4",".5") )
ggsave("FA10-1.pdf",width=12,height=6)
draw_plot(dat,c(-.15,.15),seq(-.15,.15,.03), c("-.15","-.12","-.09","-.06","-.03","0",".03",".06",".09",".12",".15") )
ggsave("FA10-2.pdf",width=12,height=6)

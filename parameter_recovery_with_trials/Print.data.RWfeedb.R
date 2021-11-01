#------------------------------------------------------------------------------#
# Print graph for parameter RW feedb

# Cerated: "Mon Oct 25 10:12:16 2021"
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)

betalim<-10

model<-"RWfeedb"
name<- paste("output_files/parameterRecovery", model, ".", "betalimit=",  betalim,  
             ".initialQ=", 0.25 , sep="")

# retrieve the file
parameterRecov<-read.csv(paste0(name, ".csv"))

plotalpha<-ggplot(parameterRecov, aes(x=SimAlpha, y=fitAlpha)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Alpha parameter")

plotbeta<-ggplot(parameterRecov, aes(x=simBeta, y=fitBeta)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Beta parameter")
  
g<-grid.arrange( plotalpha, plotbeta, ncol=2)

arrangeGrob( plotbeta,ncol=2)

# save
ggsave(file=paste0("figures/ParameterRecovery_", model, "betalimit=", betalim,  ".jpg"), g)
       
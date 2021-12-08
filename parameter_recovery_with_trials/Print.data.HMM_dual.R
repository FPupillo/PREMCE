#------------------------------------------------------------------------------#
# Print graph for parameter recovery for HMM

# Cerated: ""Thu Oct 28 12:47:39 2021"
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)

betalim<-10

model<-"HMM_dual"
Ntrial<-24
name<- paste("output_files/parameterRecovery", model, ".", "Ntrial=",  Ntrial,  
             ".initialQ=", 0.25 , sep="")

# retrieve the file
parameterRecov<-read.csv(paste0(name, ".csv"))



plotc<-ggplot(parameterRecov, aes(x=simC, y=fitC)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("c parameter")
  
plotgamma<-ggplot(parameterRecov, aes(x=simGamma, y=fitGamma)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("gamma parameter")

g<-grid.arrange(  plotc, plotgamma, ncol=2)

arrangeGrob(plotc, plotgamma,ncol=1)

# save
ggsave(file=paste0("figures/ParameterRecovery_", model, "betalimit=", betalim,  ".jpg"), g)
       
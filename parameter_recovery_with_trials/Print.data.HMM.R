#------------------------------------------------------------------------------#
# Print graph for parameter recovery for HMM

# Cerated: ""Thu Oct 28 12:47:39 2021"
#------------------------------------------------------------------------------#

rm(list=ls())

library(ggplot2)
library(gridExtra) # for plotting
library(ggpubr)

betalim<-10

model<-"HMM"
Ntrial<-24
name<- paste("output_files/parameterRecovery", model, ".", "Ntrial=",  Ntrial,  
             ".initialQ=", 0.25 , sep="")

# retrieve all the files in the folder "cluster"
# parameterRecov<-vector()
# files<-list.files("computational_model/cluster/output_files")
# for (n in 1: length(files)){
#   currFile<-read.csv(paste0("computational_model/cluster/output_files/",files[n]))
#   # append it
#   parameterRecov<-rbind(parameterRecov, currFile)
# }

# retrieve the file
parameterRecov<-read.csv(paste0(name, ".csv"))


plotbeta<-ggplot(parameterRecov, aes(x=simBeta, y=fitBeta)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("Beta parameter")

plotc<-ggplot(parameterRecov, aes(x=simC, y=fitC)) + 
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  stat_cor(method="pearson")+
  #stat_cor(method = "pearson", label.x = 3, label.y = 30)+
  ggtitle("c parameter")
  
g<-grid.arrange( plotbeta, plotc, ncol=2)

arrangeGrob(plotalpha, plotbeta,ncol=2)

# save
ggsave(file=paste0("figures/ParameterRecovery_", model, "betalimit=", betalim,  ".jpg"), g)
       
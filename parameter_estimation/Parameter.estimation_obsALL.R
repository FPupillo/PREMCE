# compute likelihood
rm(list=ls())
library(ggplot2)
library(dplyr)

setwd(dirname(getwd()))
setwd(dirname(getwd()))

# source the functions
source("helper_functions/selPart.R")
source("computational_model/softmax.R")
source("computational_model/likelihood_functions/lik_RescorlaWagner_obsALL.R")
source("computational_model/fitting_functions/fit_RescorlaWagner_obsALL.R")
source("computational_model/BICcompute.R")
source("helper_functions/getOpt.R")
source("helper_functions/isOpt.R")
source("helper_functions/perOpt.R")
source("helper_functions/cumAcc.R")
source("helper_functions/getCorrkey.R")
source("helper_functions/getCatresp.R")


DataAll<-read.csv("output_files/day2_accuracy.csv")
participants<-unique(DataAll$SubNum)

# set boundaries for the optimization algorithm
alphaBound<-c(0,1)
betaBound<-c(0,10)

# set the initial Q
initialQ<-0.25

# set the number of the starting points for the optimization function
startPoints<-2

# initialize matrix to store the parameters
Parameters<-matrix(NA, nrow = length(participants),ncol = 5) 
colnames(Parameters)<-c("PartNum", "alpha","beta", "BIC", "LL") #names of the columns

pb<-txtProgressBar(min=0, max=length(participants), style =3)

# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(2)
setwd(cd)

# columns to delete
columnsDel<-  c("myownaccuracy" ,"respcorrs","minNrPractice", "considerNrTrials","Instr_resp.keys", "Instr_resp.rt",   
 "taskGoresp.keys","taskGoresp.rt", "warmup_resp.keys","warmup_resp.corr" , "warmup_resp.rt", "myownaccuracyCORR"  , 
"trials.thisRepN","trials.thisTrialN","trials.thisN", "trials.thisIndex", "trials.ran" ,
"date"  ,  "expName", "psychopyVersion" ,"OS" , "frameRate" ,"task_loop.thisRepN"  , "task_loop.thisTrialN", "task_loop.thisN"  ,   
"task_loop.thisIndex" , "task_loop.ran"   ,     "list_loop.thisRepN" ,  "list_loop.thisTrialN", "list_loop.thisN"  ,    "list_loop.thisIndex" ,
 "list_loop.ran" , "conditions"  )       

# delete the double files (3)
numb3<-files[substr(files, 1,2)=="01"][1:2]

files<-files[!files %in% numb3]

#datafiles<-list.files("/data_files/", num, )
# loop by participant
for (j in 1: length(participants)){

  # retrieve the file
  DataSub<-read.csv(paste0("data_files/", files[j]))
  
  # delete unnecessary columnts
  DataSub<-select(DataSub, -columnsDel)
  
  # print a message showing on which participant the script is working on
  print(paste("Working on participant", j))
  
  # subset the data for the current participant
  #DataSub<-DataAll[DataAll$SubNum==participants[j],]
  
  # Delete the first list
 # DataSub<-DataSub[DataSub$listN!=0,]
  
  # create cued character
  #DataSub$cuedCharacter<-paste0("stimuli/", DataSub$character, ".jpg")
  # create object category
  #DataSub$obj_category<-DataSub$corrCat
  
  # Delete the first list
  DataSub<-DataSub[DataSub$switch_cond!=1,]
  
  # delete nas
  DataSub<-DataSub[!is.na(DataSub$obj_category),]
  
  DataSub$obj_category<-as.character(DataSub$obj_category)  
  DataSub$cuedCharacter<-as.character(DataSub$cuedCharacter)  
  
  DataSub$response<-DataSub$task_resp.keys
  # create cued character
  #DataSub$cuedCharacter<-paste0("stimuli/", DataSub$character, ".jpg")
  #DataSub$cuedCharacter<-as.character(DataSub$cuedCharacter)
  
  DataSub<-getCatresp(DataSub)
  
  for (n in 1:nrow(DataSub)){
    DataSub$respCat[n]<-substr(DataSub$respCat[n],9, nchar(DataSub$respCat[n])-4)
  }
  # estimate alpha and beta, calculate the time
  start_time<-Sys.time() # take the starting time
  est<-fit_RescorlaWagner_obsAll(DataSub, alphaBound,betaBound, initialQ)
  end_time<-Sys.time() # take the ending time
  print(end_time-start_time) # calculate how long it took to estimate the parameters for that participant
  
  # extract alpha and beta from estimation results
  alpha<-est$alpha[1]
  beta<-est$alpha[2]
  
  # feed the RWM with the alpha and beta obtained to get Qs and PE
  par<-lik_RescorlaWagner_obsALL(DataSub, alpha, beta, 3, initialQ)
  
  # now, get PE
  PE<-sum(par$Delta, na.rm=T)
  
  # add the data to a data long dataframe
  if (!exists("Datalong")){
    Datalong<-lik_RescorlaWagner_obsALL(DataSub, alpha, beta, 3, initialQ)
  } else{
    Datalong<-rbind(Datalong,lik_RescorlaWagner_obsALL(DataSub, alpha, beta, 3, initialQ)) 
  }
  
  # prepare results for saving
  Parameters[j, c(1:5)]<-c(DataSub$participant[1],
                           alpha, beta, est$BIC, est$logLikel)
  
  setTxtProgressBar(pb, j) 
  
}

# beep for when the parameter estimation has finished
#install.packages("beepr")
library(beepr)
beep(8)

# save parameters
write.csv(Parameters, "output_files/estimated_parameters_obsALL.csv", row.names = F)

# save data from reinforcement learning
write.csv (Datalong, "output_files/RLdata_obsALL.csv", row.names = F)


#get optimal choice
par$switch_cond<-par$listN

# convert response cat
par$respCat<-paste0("stimuli/", par$respCat, ".png")

par$corrCat<-paste0("stimuli/", par$corrCat, ".png")

par<-getCorrkey(par)

par$task_resp<-par$response

par$task_resp.corr<-par$accuracy

#par<-getCatresp(par)

par<-getOpt(par)

par<-isOpt(par)

par<-perOpt(par)

ggplot(par, aes(x = trialN , y =perOptimalChoice)) +
  geom_vline(xintercept = c(48, 96,144))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  ylim(0,1)

ggplot(par, aes(x = trialN , y =perOptimalChoiceByMurks)) +
  geom_vline(xintercept = c(48, 96,144))+
  
  #stat_summary(fun.y="mean",geom="line") +
  facet_wrap(.~character)+
  geom_line()+
  ylim(0,1)

# plot the p
# plot the estimated probabilities for each character
ggplot(par[par$trial_cond==1,], aes(x=trialNum))+

  geom_line(aes(y=P1), size = 1.5, color = "blue")+
  geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
  geom_line(aes(y=P3),size = 1.5, color = "brown")+
  geom_line(aes(y=P4), size = 1.5,color = "orange")+
  facet_wrap(.~character)
  
par$Delta<-unlist(par$Delta)

ggplot(par, aes(x=trialNum))+
  geom_line(aes(y=Delta), size = 1.5, color = "red")+
  facet_wrap(.~character)+
  geom_vline(xintercept = c(48, 96,144))
  
ggplot(par[par$trial_cond==1,], aes(x=trialN))+
  geom_line(aes(y=Delta), size = 1.5, color = "red")+
  facet_wrap(.~character)+
  geom_vline(xintercept = c(48, 96,144))

ggplot(par[par$trial_cond==1,], aes(x=trialN))+
  geom_line(aes(y=uncertainty2), size = 1.5, color = "green")+
  facet_wrap(.~character)+
  geom_vline(xintercept = c(48, 96,144))
  # geom_line(aes(y=Delta), color = "red")+

ggplot(par, aes(x=trialN))+
  geom_line(aes(y=uncertainty2), size = 1.5, color = "green")+
  facet_wrap(.~character)+
  geom_vline(xintercept = c(48, 96,144))

ggplot(par, aes(x = trial , y =cumAcc)) +
  geom_vline(xintercept = c(48, 96,144))+
  
  #stat_summary(fun.y="mean",geom="line") +
  geom_line()+
  ylim(0,1)




  #facet_wrap(.~character)

# Datalong$character<-as.factor(Datalong$character)
# levels(Datalong$character)<-c("m2", "m5")

# # plot the estimated probabilities for each flower
# ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
#   
#   geom_line(aes(y=P1), size = 1.5, color = "blue")+
#   geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
#   geom_line(aes(y=P3),size = 1.5, color = "brown")+
#   geom_line(aes(y=P4), size = 1.5,color = "orange")+
#   # geom_line(aes(y=Delta), color = "red")+
#   
#   
#   geom_vline(xintercept = c(18, 55,90, 126))+
#   #facet_grid(butterfly~SubNum)+
#   facet_wrap(character~., ncol = 3)
# 
# # now prediction error
# Datalong$PEobs<-NA
#   for (o in 1: nrow(Datalong)){
#     if (Datalong$corrCat[o]==1){
#       Datalong$PEobs[o]<-Datalong$Delta1[o]
#     }else if(Datalong$corrCat[o]==2){
#       Datalong$PEobs[o]<-Datalong$Delta2[o]
#     }else if(Datalong$corrCat[o]==3){
#       Datalong$PEobs[o]<-Datalong$Delta3[o]
#     }else if(Datalong$corrCat[o]==4){
#       Datalong$PEobs[o]<-Datalong$Delta4[o]
#     }
#   }
# 
# ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+
#    geom_line(aes(y=PEobs),size = 1.5, color = "darkred")+
#   
#   
#   geom_vline(xintercept = c(16, 50,86, 122))+
#   #facet_grid(butterfly~SubNum)+
#   facet_wrap(character~., ncol = 3)
# 
# 
# # by befAft
# # distribution of PE by condition
# Datalong$befAft<-as.factor(Datalong$befAft)
# PEobsdistr<-ggplot(Datalong, aes(x= PEobs, fill=befAft))
# print(
#   PEobsdistr+geom_histogram()+ facet_grid( ~ befAft)+ggtitle("PE observational")
# )
# 
# # now I want the uncertainty, calculated as 1/variance of the prob
# # probs<-c("P1", "P2", "P3", "P4")
# # for (n in 1:nrow(Datalong)){
# # Datalong$uncertainty[n]<-1/(var(unlist(Datalong[n,probs]))+1)
# # }
# 
# 
# 
# # # now calculated as the negative sum of probabilities
# # for (n in 1:nrow(Datalong)){
# #   Datalong$uncertainty2[n]<- -sum(  unlist(Datalong[n,probs]) *log(unlist(Datalong[n,probs])))
# # }
# 
# 
# # uncertainty  as the variance of the probabilities
# ggplot(Datalong[Datalong$SubNum==2,], aes(x=trialNum))+ ylim(c(0,1))+
#   
#   geom_line(aes(y=uncertainty), size = 1.5, color = "blue")
# 
# ggplot(Datalong[Datalong$SubNum==4,], aes(x=trialNum))+
#   geom_line(aes(y=uncertainty2),size = 1.5, color = "darkred")+
#   geom_vline(xintercept = c(16, 50,86, 122))
#   
# # plot the estimated probabilities for each flower
# ggplot(Datalong[Datalong$SubNum==7,], aes(x=trialNum))+
#   
#   geom_line(aes(y=uncertainty), size = 1.5, color = "blue")
# 
# # now for all
# ggplot(Datalong, aes(x=trialNum, y = uncertainty))+stat_summary(fun.y="mean",geom="line")
#   
# Uncdistr<-ggplot(Datalong, aes(y= uncertainty, x = befAft, fill=befAft))
# print(
#   Uncdistr+geom_boxplot()#+ facet_grid( ~ befAft)+ggtitle("Uncertainty observational")
# )
# 

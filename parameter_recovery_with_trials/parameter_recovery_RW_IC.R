#------------------------------------------------------------------------------#
# simulating trials switch points and character - RW_EC
# created: "Tue Oct 26 14:26:25 2021"
#------------------------------------------------------------------------------#

#setwd(dirname(getwd()))
setwd(dirname(getwd()))

#soruce the functions
source("helper_functions/taskSim.R")
source("simulation_functions/simulate_RescorlaWagner_IC.R")
source("helper_functions/update_RW.R")
source(("helper_functions/softmax.R"))
source(("helper_functions/chooseMultinom.R"))
source(("helper_functions/BICcompute.R"))
source("likelihood_functions/lik_RescorlaWagner_IC.R")
source("fitting_functions/fit_RescorlaWagner_IC.R")
source("helper_functions/getCat.R")
source("helper_functions/preSim.R")
source("helper_functions/getcorrCat.R")

#------------------------------------------------------------------------------#
# parameters
#------------------------------------------------------------------------------#
Pcong<-0.75
Ntrial<-24
Ncharacter<-1
switchN<-3
categN<-4
#------------------------------------------------------------------------------#
lengthparam<-10

sims<-10

alphaseq<-seq(0,1, length.out =lengthparam)

betaseq<-seq(1,10, length.out=lengthparam)

data<-matrix(NA, nrow=1,ncol = 5)

df<-data.frame(data)

names(df)<-c("simCAlpha", "fitAlpha","simBeta", "fitBeta","BIC")

model<-simulate_RescorlaWagner_IC

fit<-fit_RescorlaWagner_IC

modname<-as.character("RW_IC")

name<- paste("output_files/parameterRecovery", modname, ".Ntrial=",Ntrial, 
             ".initialQ=", 0.25 , sep="")

write.csv(df, paste0(name, ".csv"), row.names = F)

# initiate a counter
count<-1

# progress bar
pb<-txtProgressBar(min=0, max=(lengthparam)*sims, style =3)


for (sim in 1:sims){
for (a in 1:length(alphaseq)){
  for (b in 1:length(betaseq)){

    # generate the task
    Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
    
    # prepare data
    Data$cuedCharacter<-Data$character
    
    for (n in 1:nrow(Data)){
      Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
    }
    
      # simulate data with the RW obs
      sim<-model(Data=Data,alpha=alphaseq[a], beta =betaseq[b],
                 initialV = 0.25)

    
    # calculate percentage optimal choice
    # rename variables
    sim$task_resp.keys<-sim$response
    sim$character<-sim$cuedCharacter
    
    # create response category
    categ<-as.character(Data[1,1:4])
    
    sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
    
    # now fit the data
    est<-fit(data=sim, alphaBound = c(0,1), betaBound = c(0,10),
             initialV =0.25)
    
    estAlpha<-est$alphabetaPAR[1]
    
    estBeta<-est$alphabetaPAR[2]
    
    temp<-read.csv( paste0(name, ".csv"))
    
    #append the data
    temp[nrow(temp)+1, ]<-c(alphaseq[a], estAlpha,betaseq[b],estBeta,
                            est$BIC)
    
    #write it
    write.csv(temp, paste0(name, ".csv"), row.names = F)
    
    count <-count+1
    
    setTxtProgressBar(pb, count) 
    
  }
    }
}

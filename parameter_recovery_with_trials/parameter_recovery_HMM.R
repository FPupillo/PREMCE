#------------------------------------------------------------------------------#
# simulating trials switch points and character - HMM
# created: "Tue Oct 26 14:26:25 2021"
#------------------------------------------------------------------------------#

setwd(dirname(getwd()))
setwd(dirname(getwd()))

#soruce the functions
source("simulate_data/taskSim2.R")
source("computational_model/simulation_functions/simulate_HMM.R")
source(("computational_model/softmax.R"))
source(("computational_model/chooseMultinom.R"))
source(("computational_model/BICcompute.R"))
source("computational_model/likelihood_functions/lik_HMM.R")
source("computational_model/fitting_functions/fit_HMM.R")
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

cseq<-seq(0,1, length.out =lengthparam)

betaseq<-seq(1,10, length.out=lengthparam)

data<-matrix(NA, nrow=1,ncol = 5)

df<-data.frame(data)

names(df)<-c("simC", "fitC","simBeta", "fitBeta", "BIC")

model<-simulate_HMM

fit<-fit_HMM

modname<-as.character("HMM")

name<- paste("output_files/parameterRecovery", modname, ".Ntrial=",Ntrial, 
             ".initialQ=", 0.25 , sep="")

write.csv(df, paste0(name, ".csv"), row.names = F)

# initiate a counter
count<-1

# progress bar
pb<-txtProgressBar(min=0, max=(lengthparam^2)*sims, style =3)


for (sim in 1:sims){
for (c in 1:length(cseq)){
  for (b in 1:length(betaseq)){

    # generate the task
    Data<-taskSim(Pcong, Ntrial, Ncharacter, switchN, categN)
    
    # prepare data
    Data$cuedCharacter<-Data$character
    
    for (n in 1:nrow(Data)){
      Data[n,1:4]<-paste0("stimuli/",Data[n,1:4], ".png")
    }
    
    if (!is.null(formals(model)$k)){
      
      # simulate data with the RW obs
      sim<-model(Data=Data,alpha=alphaseq[a], beta =betaseq[b], 
                 k=kseq[k], eta = etaseq[eta],
                 initialQ = 0.25)
      
    } else if (!is.null(formals(model)$c)){
      
      
      # simulate data with the HMM
      sim<-model(Data=Data,c=cseq[c], gamma =0.1, beta =betaseq[b],
                 initialPs = 0.25)
      
    } else{ 
      
      # simulate data with the RW obs
      sim<-model(Data=Data,alpha=alphaseq[a], beta =betaseq[b],
                 initialQ = 0.25)
    }
    
    # calculate percentage optimal choice
    # rename variables
    sim$task_resp.keys<-sim$response
    sim$character<-sim$cuedCharacter
    
    # create response category
    categ<-as.character(Data[1,1:4])
    
    sim$respCat<-substr(categ[sim$response], 9, nchar(categ[sim$response])-4)
    
    # now fit the data
    est<-fit(data=sim, cBound = c(0,1),  betaBound = c(1,10),
             initialPs =0.25)
    
    estC<-est$alphabetaPAR[1]
    
    estBeta<-est$alphabetaPAR[2]
    
    temp<-read.csv( paste0(name, ".csv"))
    
    
    #append the data
    temp[nrow(temp)+1, ]<-c(cseq[c], estC, betaseq[b], estBeta,
                        est$BIC)
    
    
    #write it
    write.csv(temp, paste0(name, ".csv"), row.names = F)
    
    count <-count+1
    
    setTxtProgressBar(pb, count) 
    
  }
    }
}

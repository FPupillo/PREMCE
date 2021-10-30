perOpt<-function(data, accuracy=0){
  #------------------------------------------------------------------------------#
  # Function that calculates the percentage optimal choice
  # 
  # This function takes in the participants' response and calculate
  # the percentage of optimal choices over the length of the trials
  # Precisely, it calculates the percentage of optimal choice over the trials
  # for the category that IN THAT BLOCK is the optimal choice. 
  # 
  # 
  #     INPUT: data -  with participants' response 
  #            accuray - if 1, takes the accuracy instead of optimal response
  #     OUTPUT: data with a variable created that indicates the percentage 
  #             of optimal choice at trial t
  # 
  # created : "Sun Oct 10 10:52:48 2021"
  #------------------------------------------------------------------------------#

    
currlist<-data

# first, create trial number for ordering later on
currlist$trialN<-1:nrow(currlist) 

listAll<-vector()

# mow do it for listN
for (n in 1:length(unique(currlist$switch_cond))){

  # subset
  currlistSwitch<-currlist[currlist$switch_cond==unique(currlist$switch_cond)[n],]
  
  # cumulative accuracy and percentage choice per list
  currlistSwitch$perOptimalChoice<-NA
  for(n in 1:nrow(currlistSwitch)){
    
    currlistSwitch$perOptimalChoice[n]<-sum(currlistSwitch$isoptimal[1:n], na.rm=T)/n
    
  }
  
  # cumulative accuracy
  if (currlistSwitch$switch_cond[1]!=0){
  currlistSwitch$cumAcc<-cumAcc(currlistSwitch$task_resp.corr)
  } else {
    currlistSwitch$cumAcc<-cumAcc(currlistSwitch$pract_resp.corr)
    
  }
  
  characters<-unique(currlistSwitch$character)


  # create cumulative accuracy per character now
  currlistbychar<-vector()
  for (c in 1:length(characters)){
    
    # subset the character  
    currChar<- currlistSwitch[currlistSwitch$character==characters[c],]
    
    # create trialnum by char
    currChar$trialNbychar<-1:nrow(currChar)
    
    # calculateperOpt
    currChar$perOptimalChoiceByMurks<-NA
    currChar$cumAccbyMurks<-NA
    for (t in 1:nrow(  currChar)){
      
      currChar$perOptimalChoiceByMurks[t]<-sum(currChar$isoptimal[1:t], na.rm=T)/
        currChar$trialNbychar[t]
      
    }
    
    if (currlistSwitch$switch_cond[1]!=0){
      currChar$cumAccbyMurks<-cumAcc(currChar$task_resp.corr)
    } else { 
      currChar$cumAccbyMurks<-cumAcc(currChar$pract_resp.corr)
    }
    
    # attach to the previous list
    currlistbychar<-rbind(currlistbychar, currChar)
    
  }

# order it
  currlistbychar<-currlistbychar[order(currlistbychar$trialN),]

# delete trialN
  currlistbychar$trialN<-NULL

# bind it with the list all
listAll<-rbind(listAll, currlistbychar)

}

listAll$trialN<-1:nrow(listAll)

return(listAll)
  

}

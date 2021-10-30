lik_RescorlaWagner_LRchoice<-function (Data, beta, eta, k, print,  initialQ){
  # This function computes the likelihood of the participants'
  # choices conditional on a Rescorla Wagner model with a trial-level LR
  # as in Rouhani 2021. 
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   beta: a candidate beta parameter
  #   eta 
  #   K
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #           of choosing category 1), 9:"P2", 10: "P3"
  #   initialQ: value of the inital Q
  #
  # Output:
  #   Negative Log Likelihood
  # -------------
  
  
  
  # convert the resp character into numeric variable
  levels(Data$respCat)
  
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  Data$catNum<-as.numeric((Data$respCat))
  
  Data$corrCat<-as.numeric((Data$corrCat))
  
  
  # Initialize variables: Qs, the expected values
  Data$Q1<-NA; Data$Q2<-NA; Data$Q3<-NA ; Data$Q4<-NA 
  
  # Ps (probabilities for each category's choice)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA ; Data$P4<-NA
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # uncertainty as the 1/variance of the probability
  Data$uncertainty<-NA
  # uncertainty as the -log(sum prob * log prob)
  Data$uncertainty2<-NA
  # change point probability
  Data$CPP<-NA
  
  # index variables for Q, P, and Delta
  Qindex<-c("Q1", "Q2", "Q3", "Q4")
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # convert butterfly as numeric
  Data$character<-as.character(Data$character)
  for (t in 1:nrow(Data)){
    if(Data$character[t]=="m2"){
      Data$character[t]<-1
    }else{Data$character[t]<-2}
  }
  Data$character<-as.numeric(Data$character)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # assign information about the category displayed to a vector
  obs<-getobs(Data)
  
  # loop over trials
  for (t in 1: max(Data$trialNum)){
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Data$character[t]]==0){
      Q<-c(initialQ, initialQ, initialQ, initialQ) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
      Q<-Data[Data$character==Data$character[t],][count[Data$character[t]],Qindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    count[Data$character[t]]<-count[Data$character[t]]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(Q, beta)
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$catNum[t]) ){
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[Data$catNum[t]])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update the counter 
      count2<-count2+1
      
      # delta
      delta <- obs[t,]-Q # subtracting 1 to the objectcategory, the category shown at the end
      
      # take only the object category PE
      deltaChoice<-unlist(delta[Data$response[t]])
      
      act <- eta+k*deltaChoice
      
      Data$alpha[t] <- sigmoid(act)#
      # update the Q related to the response according to the rw model.
      Q[Data$response[t]] <- Q[Data$response[t]]+ Data$alpha[t]*deltaChoice
      
    } else{deltaChoice<-0
        delta<-c( NA)}
    
    # assign values to the dataset
    Data[t, Qindex]<-Q
    Data[t, Pindex]<-p
    Data[t, Deltaindex]<-delta
    
  
  # compute uncertainty as the 1/variance of the probability
  uncertainty<-1/(var(unlist(Data[t,Pindex]))+1)
  
  # uncertainty as the -log(sum prob * log prob)
  uncertainty2<- -sum(  unlist(Data[t,Pindex]) *log(unlist(Data[t,Pindex])))
  
  # change point probability
  # probability of that observations
  probobs<-Data[t, paste("P", Data$corrCat[t], sep="")]
  
  if (count[Data$character[t]]==1 |count[-Data$character[t]]==0 ){ # while one of the two character has not been shown
    
    Data$uncertainty[t]<-uncertainty
    Data$uncertainty2[t]<-uncertainty2
    Data$CPP[t]<-probobs
    # uncertainty as the -log(sum prob * log prob)
    
  } else{
    # retrieve uncertainty on the last trial of the other butterfly
    unctmin1<-Data[Data$character!=Data$character[t],][count[-Data$character[t]], "uncertainty"]
    
    unc2tmin1<-Data[Data$character!=Data$character[t],][count[-Data$character[t]], "uncertainty2"]
    
    # retrieve probability of the observation on the previous trial for the same butterfly
    probobstminus1<-Data[Data$character==Data$character[t],][count[Data$character[t]]-1,paste("P", Data$corrCat[t], sep="") ]
    
    Data$uncertainty[t]<-mean(uncertainty,unctmin1)
    Data$uncertainty2[t]<-mean(uncertainty2,unctmin1)
    Data$CPP[t]<-probobs*probobstminus1
    
    
  }
}
# we could take the probability only for the congruent trials, but for now we are taking all the probabilities

NegLL<--sum(log(Data$Prob), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,"Q3"= Data$Q3,"Q4"= Data$Q4,
               "Delta"= Data$Delta,"P1"=Data$P1,"P2"= Data$P2, "P3"=Data$P3 , "P4"=Data$P4))
} else if(print==3){
  return(Data)}
}


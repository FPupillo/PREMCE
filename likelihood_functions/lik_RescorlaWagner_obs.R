lik_RescorlaWagner_obs<-function (Data,alpha, beta,print,  initialQ){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model . In this particular 
  # version, only the category shown is updated
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha: a candidate alpha parameter 
  #   beta: a candidate beta parameter
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #           of choosing category 1), 9:"P2", 10: "P3"
  #   initialQ: value of the inital Q
  #
  # Output:
  #   Negative Log Likelihood
  # -------------
  

  # convert the object category into character variables
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$catNum<-as.numeric((Data$respCat))
  
  ##Data$corrCat<-as.numeric((Data$corrCat))
  
  
  # Initialize variables: Qs, the expected values
  Data$Q1<-NA; Data$Q2<-NA; Data$Q3<-NA ; Data$Q4<-NA 
  Data$Qup1<-NA; Data$Qup2<-NA; Data$Qup3<-NA ; Data$Qup4<-NA
  
  # Ps (probabilities for each category's choice)
  Data$P1<-NA; Data$P2<-NA; Data$P3<-NA ; Data$P4<-NA
  
  Data$x1<-NA; Data$x2<-NA; Data$x3<-NA ; Data$x4<-NA
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # get the response of the participant
  #Data$response<-NA
  
  # uncertainty as the 1/variance of the probability
  Data$uncertainty<-NA
  # uncertainty as the -log(sum prob * log prob)
  Data$uncertainty2<-NA
  # change point probability
  #Data$CPP<-NA
  
  # index variables for Q, P, and Delta
  Qindex<-c("Q1", "Q2", "Q3", "Q4")
  Qupindex<-c("Qup1", "Qup2", "Qup3", "Qup4") 
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  xindex<-c("x1", "x2", "x3", "x4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)

  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  count2<-1
  
  # loop over trials
  for (t in 1: nrow(Data) ){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      Q<-c(initialQ, initialQ, initialQ, initialQ) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
      Q<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qupindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    Data[t, Qindex]<-Q
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(Q, beta)
    

    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$response[t]) ) {
      
      # placeholder for the category selected by part
      respCounter<-which(categ == as.character(Data$respCat[t]))
      
      # That is the category to updated
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[respCounter])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
      
      # update the counter 
      count2<-count2+1
    }
    
    # get the observation as 1 if that chategory is present, and 0 if it is not
    if (Data$obj_category[t]==categ[1]){
      obs<-c(1,0,0,0)
    } else if (Data$obj_category[t]==categ[2]){
      obs<-c(0,1,0,0)
    } else if (Data$obj_category[t]==categ[3]){
      obs<-c(0,0,1,0)
    }else if (Data$obj_category[t]==categ[4]){
      obs<-c(0,0,0,1)
    }
        

    # prediction error
    delta <- 1 - Q[which(obs==1)]
        
        #Data$Delta[t]<-unlist(delta[which(obs==1)])
        
        # assign it to the dataset
        Data[t, Deltaindex]<-delta
        
        # update the q accordingly
        Q[which(obs==1)]<-Q[which(obs==1)]+ alpha *delta
  
  # assign values to the dataset
  Data[t, Qupindex]<-Q
  Data[t, Pindex]<-p
  
    
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


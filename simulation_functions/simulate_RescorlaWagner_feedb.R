simulate_RescorlaWagner_feedb<-function ( Data,alpha, beta, 
                                          k=NULL, eta =NULL, 
                                          initialV){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  #
  # Input
  #   Data: data containing the structure of the task
  
  #   alpha: alpha parameter 
  #   beta:  beta parameter
  #   initialV: value of the inital Q
  #
  # Output:
  #   dastaframe with $response and $object_cat
  # -------------
  
  # convert the object category into numeric variable
  categ<-levels(as.factor(Data$obj_category))
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$obj_category<-as.numeric(as.factor(Data$obj_category))
  
  for (n in 1:4){
    
    # Initialize variables: Qs, the expected values
    Data[[paste("V", n, sep="")]]<-NA
    
    # Ps (probabilities for each category's choice)
    Data[[paste("P", n, sep="")]]<-NA
    
  }
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA
  
  # Delta, prediction error
  Data$Delta<-NA
  
  # participants' response
  Data$response<-NA
  
  # participants' response categor
  Data$respCat<-NA
  
  # accuracy
  Data$accuracy<-NA
  
  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2", "V3", "V4")
  Pindex<-c("P1", "P2", "P3", "P4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  
  # loop over trials
  for (t in 1:nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialV,4) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Vindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    # map response onto the keys
    # which category was the response?
    # estract the order of the categories at trial t
    Data$respCat[t]<-as.character(categ[Data$response[t]])
    
    #which cat is the corr ans?
    corr_resp<-Data$obj_category[t]
    
    # get accuracy
    if (Data$respCat[t]==corr_resp){
      Data$accuracy[t]<-1
    }else{
      Data$accuracy[t]<-0
    }
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$accuracy[t]==1){
      r<-1 
    } else {
      r<-0
    }
    # prediction error
    delta <- r - V[Data$response[t]]
    
    # update Q
    V[Data$response[t]]<-V[Data$response[t]]+alpha*delta
    
    # assign it to the dataset
    Data$Delta[t]<-delta
    
    # assign values to the dataset
    Data[t, Vindex]<-V
    Data[t, Pindex]<-p
    
    # convert the choice into the order of the categories
    Data$response[t]<-which(unlist(Data[t,c("left_categ","centleft_categ" , 
                                            "centright_categ","right_categ") ])== 
                              paste0("stimuli/", Data$respCat[t],  ".png"))
    
  }
  
  return(Data)
  
}


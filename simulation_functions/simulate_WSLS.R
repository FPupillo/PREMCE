simulate_WSLS<-function ( Data, beta, initialV){
  #----------------------------------------------------------------------------#
  # This function computes choices
  # choices conditional on a win stay lose shift model
  #
  # Input
  #   Data: data containing the structure of the task
  #   beta : beta parameter
  #   initialV: initial expected values for the choices
  # Output:
  #   Dataframe with $response and $object_cat
  #----------------------------------------------------------------------------#
  
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
  
  # category of the response
  Data$respCat<-NA
  
  # now accuracy
  Data$accuracy<-NA

  # index variables for Q, P, and Delta
  Vindex<-c("V1", "V2", "V3", "V4")
  Pindex<-c("V1", "V2", "V3", "V4") 
  Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA

  # loop over trials
  for (t in 1: nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      V<-rep(initialQ, times = 4) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
      V<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Vindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(V, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- as.numeric(chooseMultinom(p))
    
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
    
    # get accuracy
    if (Data$accuracy[t]==1){
      V[Data$response[t]]<-1
      V[-Data$response[t]]<-0
      
    } else{
      V[Data$response[t]]<-0
      V[-Data$response[t]]<-1
    }

  # update all the Qs
  
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


simulate_RescorlaWagner_feedbALL<-function ( Data,alpha, beta,  initialQ){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model 
  #
  # Input
  #   Data: data containing the structure of the task

  #   alpha: alpha parameter 
  #   beta:  beta parameter
  #   initialQ: value of the inital Q
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
    Data[[paste("Q", n, sep="")]]<-NA
    
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
  
  # convert character as numeric
  #Data$character<-as.character(Data$cuedCharacter)
  #Data$cuedCharacter<-as.numeric(Data$cuedCharacter)
  
  # for (t in 1:nrow(Data)){
  #   if(Data$character[t]=="stimuli/m2.jpg"){
  #     Data$cuedCharacter[t]<-1
  #   }else{Data$cuedCharacter[t]<-2}
  # }
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA

  # loop over trials
  for (t in 1: 6 ){ #nrow(Data)){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      Q<-c(initialQ, initialQ, initialQ, initialQ) # if it is the first time that butterfly is shown, the Qs are at their initial value
    } else{
        Q<-Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],Qindex] # if it is not the first time that butterfly is shown, retrieve the Qs of the last trial of that butterfly
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(Q, beta)
    
    # make choice according to choice probabilities
    Data$response[t] <- chooseMultinom(p)
    
    # get accuracy
    if (Data$response[t]==Data$corr_ans[t]){
      Data$accuracy[t]<-1
    }else{
      Data$accuracy[t]<-0
    }
    
    # which category was the response?
    # estract the order of the categories at trial t
    Data$respCat[t]<-as.character(unlist(Data[t,c("left_categ","centleft_categ" , 
     "centright_categ","right_categ") ][Data$response[t]]))
    
    # substring
    
    Data$respCat[t]<-substr(Data$respCat[t], 9, nchar(Data$respCat[t])-4)
    
    # placeholder for the category selected by part
    respCounter<-which(categ == as.character(Data$respCat[t]))
    
    # now map the response into the current trial categories
    # respCounter<-which(as.character(unlist(Data[t, c("left_categ", "centleft_categ", 
    #                                                          "centright_categ", "right_categ")]) )
    #                        == paste0("stimuli/",categ[Data$response[t]], ".png"))
    
    
    
    # get the observation as 1 if that category is present, and 0 if it is not
    if (Data$accuracy[t]==1){
      if (respCounter==1){
        r<-c(1,0,0,0)
      } else if (respCounter==2){
        r<-c(0,1,0,0)
      } else if (respCounter==3){
        r<-c(0,0,1,0)
      }else if (respCounter==4){
        r<-c(0,0,0,1)
      }
    } else {
      r<-c(0,0,0,0)
    }
        # prediction error
        delta <- r - Q
        
        # assign it to the dataset
        Data[t, Deltaindex]<-delta
        
        # update all the Qs
        Q<-Q+ alpha *delta
        
  
  # assign values to the dataset
  Data[t, Qindex]<-Q
  Data[t, Pindex]<-p
  
  }
  
  return(Data)
  
  }


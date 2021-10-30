lik_HMM<-function (Data,c, gamma, beta,print, initialPs){
  # This function computes the likelihood of the participants'
  # choices conditional on the Rescorla Wagner model = only value of the choice
  # is updated in this model
  #
  # Input
  #   Data: a long dataset where each row represents a trial. 
  #   alpha_0: initial associability parameter 
  #   beta:  beta parameter
  #   c : probability that the reward indicated that one state is the true state
  #   gamma: 
  #   print: 1: return only the negative log-likelihood; 
  #          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
  #          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
  #           of choosing category 1), 9:"P2", 10: "P3"
  #   initialQ: value of the inital Q
  #
  # Output:
  #   Negative Log Likelihood
  # -------------
  
  # convert the object category into numeric variable
  categ<-c("Outdoor activity & sport item", "Kitchen & utensil",
           "Electronic device & accessory","Hand labour tool & accessory")
  
  murks<-levels(as.factor(Data$cuedCharacter))
  
  Data$cuedCharacter<-as.character(Data$cuedCharacter)
  
  #1  "Electronic device & accessory"  2  "Hand labour tool & accessory" 
  # 3 "Kitchen & utensil"     4  "Outdoor activity & sport item"
  #Data$obj_category<-as.numeric(as.factor(Data$obj_category))
  
  for (n in 1:4){
    
    
    # P probability of the state, given the observation (reward)
    Data[[paste("P_O_S", n, sep="")]]<-NA
    
  }
  
  # probability of the states, prior and posterior
  PS_pre<-rep(initialPs, 4)
  PS_post<-rep(initialPs, 4)
  
  # probability for the choice that participants' made on a trial
  Data$Prob<-NA

  
  # accuracy
  Data$accuracy<-NA

  # surprise
  Data$surprise<-NA
  
  # entropy
  Data$entropy<-NA
  
  # index variables for StatesPre, P, and Delta
  PSpreindex<-c("PS_pre1", "PS_pre2", "PS_pre3", "PS_pre4")
  PSpostindex<-c("PS_post1", "PS_post2", "PS_post3", "PS_post4") 
  
  #Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4")
  
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)
  
  # initialise choice probability and counter for the choiceprobability
  prob<-NA
  # Counter for indicating which character has to be updated
  count<-rep(0, 2)

  count2<-1
  
  # loop over trials
  for (t in 1: nrow(Data) ){
    
    # update the counter
    Murkcounter<-which(murks==Data$cuedCharacter[t])
    
    # The following loop retrieves the Q values of the butterfly that corresponds to the current trial (time t).
    if (count[Murkcounter]==0){
      
      PS_pre<-rep(0.25,4)
      #Ps<-rep(0.25,4)
      
    } else {
      PS_post<-unlist(Data[Data$cuedCharacter==Data$cuedCharacter[t],][count[Murkcounter],PSpostindex])
      
      #PS_pre[1]<-PS_post[1]*(1-gamma)+  gamma*  (PS_post[2]) + gamma*PS_post[3]+gamma*PS_post[4]
      
      
      PS_pre[1]<-PS_post[1]*(1-gamma)+PS_post[2]*(gamma/3)+PS_post[3]*(gamma/3)+PS_post[4]*(gamma/3)
      PS_pre[2]<-PS_post[2]*(1-gamma)+PS_post[3]*(gamma/3)+PS_post[4]*(gamma/3)+PS_post[1]*(gamma/3)
      PS_pre[3]<-PS_post[3]*(1-gamma)+PS_post[1]*(gamma/3)+PS_post[2]*(gamma/3)+PS_post[4]*(gamma/3)
      PS_pre[4]<-PS_post[4]*(1-gamma)+PS_post[1]*(gamma/3)+PS_post[2]*(gamma/3)+PS_post[3]*(gamma/3)
    }
    
    count[Murkcounter]<-count[Murkcounter]+1 # update the counter
    
    # update choice probabilities using the softmax distribution
    p<-softmax(PS_pre, beta)
    
    # compute Q, delta, and choice probability for actual choice, only if a choice is computed
    if (Data$response[t]!=0 & !is.na(Data$response[t]) ) {
      
      # which category was the response?
      # extract the order of the categories at trial t
      Data$respCat[t]<-as.character(unlist(Data[t,c("left_categ","centleft_categ" , 
                                                    "centright_categ","right_categ") ][Data$response[t]]))
      # substring
      Data$respCat[t]<-substr(Data$respCat[t], 9, nchar(Data$respCat[t])-4)
      
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
      
      # we now need to map the category into our
      # predefined order
      respCounter<-which(categ == Data$respCat[t])
      
      # That is the category to updated
      
      # probability only for the response made by participant
      prob[count2]<-unlist(p[respCounter])
      
      # assign it to the dataset
      Data$Prob[t]<- prob[count2]
    } else {
      Data$accuracy[t]<-0
      r<-0
    }
    
    
    if (r == 1) {
      P_O_S1 = 0.25 * ( ifelse(Data$response[t] == 1, c, (1-c)/3))
      P_O_S2 = 0.25 * ( ifelse(Data$response[t] == 2, c, (1-c)/3))
      P_O_S3 = 0.25 * ( ifelse(Data$response[t] == 3, c, (1-c)/3))
      P_O_S4 = 0.25 * ( ifelse(Data$response[t] == 4, c, (1-c)/3))
      
    } else if (r== 0) {
      P_O_S1 = 0.25 * ( ifelse(Data$response[t] == 1, (1-c)/3, c))
      P_O_S2 = 0.25 * ( ifelse(Data$response[t] == 2, (1-c)/3, c))
      P_O_S3 = 0.25* ( ifelse(Data$response[t] == 3, (1-c)/3, c))
      P_O_S4 = 0.25 * ( ifelse(Data$response[t] == 4, (1-c)/3, c))
    }
    
    # State belief update using Bayesian rule, after observing the outcome
    PS_post[1] = (P_O_S1 * PS_pre[1]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[2] = (P_O_S2 * PS_pre[2]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[3] = (P_O_S3 * PS_pre[3]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    PS_post[4] = (P_O_S4 * PS_pre[4]) / ( P_O_S1 * PS_pre[1] + P_O_S2 * PS_pre[2] + P_O_S3 * PS_pre[3] + P_O_S4 * PS_pre[4])
    
    # / compute entropy
    # if (PS_post[1] == 0 | PS_post[1] == 1){
    #  Data$entropy[t] = 0
    #} else {
    Data$entropy[t] = -((PS_post[1]*log(PS_post[1]) + PS_post[2]*log(PS_post[2]) + PS_post[3]*log(PS_post[3]) + PS_post[4]*log(PS_post[4]) ))
    #}
    
    # compute bayesian surprise
    # if (PS_post[1] == 0){
    #   Data$surprise[t] = PS_post[2] * log(PS_post[2]/PS_pre[2])
    # }else if ( PS_post[1] == 1)
    #   surprise[s,t] = Ps_post[1] * log(Ps_post[1]/Ps_pre[1]); 
    # else  
    Data$surprise[t] = PS_post[1] * log(PS_post[1]/PS_pre[1]) + PS_post[2] * log(PS_post[2]/PS_pre[2]) +
      PS_post[3] * log(PS_post[3]/PS_pre[3]) + PS_post[4] * log(PS_post[4]/PS_pre[4])  
    
    
    
    # assign other values to the dataset
    Data[t, PSpreindex]<-PS_pre
    Data[t, PSpostindex]<-PS_post
    
  
 
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


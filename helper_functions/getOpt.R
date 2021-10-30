getOpt<-function(data){
#------------------------------------------------------------------------------#
# function to get the optimal category and 
#------------------------------------------------------------------------------#
  data$optimalChoice<-NA
  for (n in 1:nrow(data)){
    
    # sel optimal choice for that block ("switch")
    # first, subset the list selecting only the curr block and the congruent trials
    subs<-data[data$switch_cond==data$switch_cond[n] & 
                 data$cuedCharacter == data$cuedCharacter[n] &
                 data$trial_cond==1,]
    
    cat<-c("left_categ", "centleft_categ", "centright_categ", "right_categ")
    
    # optimal choice for one murks (we are taking only the first
    # as all of them should be the same)
    data$optimalChoice[n]<-as.character(subs[1,cat[subs$corr_ans[1]]])
    
    
  }
  
  return(data)
}
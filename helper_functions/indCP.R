# calculate individual change point depending on participants' cumulative accuracy

indCP<-function(data){
  ################
  # function that calculates the idinvidual change point depending on 
  # participants' accuracy
  ###############
  
  # create the vector
  data$iCP<-as.vector(NA)
  for (n in 1:nrow(data)){
    
    if (data$cumAccBlock[n]<0.55){
      data$iCP[n]<-"afterCP"
    } else {
      data$iCP[n]<-"beforeCP"
    }
    
  }
  
  return(data)
}

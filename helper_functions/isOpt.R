isOpt<-function(data){
  #------------------------------------------------------------------------------#
  # function tthat calculate
  # whether response on a task was optimal choice or not
  #------------------------------------------------------------------------------#
  data$isoptimal<-NA
  for (n in 1:nrow(data)){
    
    data$isoptimal[n]<-ifelse(data$respCat[n]==data$optimalChoice[n],1,0)
    
  }
  
  return(data)
}
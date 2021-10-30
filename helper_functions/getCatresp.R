
getCatresp<-function(data){
#------------------------------------------------------------------------------#
# function to get the category of participants' response
#------------------------------------------------------------------------------#
  cat<-c("left_categ", "centleft_categ", "centright_categ", "right_categ")
  
  data$respCat<-NA
  
  for (n in 1:nrow(data)){
  
  if (data$switch_cond[n]==0){
    
    if (!is.na(data$pract_resp.keys[n])){
      
      # get response made by participant
      data$respCat[n]<-as.character(data[n,cat[data$pract_resp.keys[n]]])
      
    } 
    
    } else {
      
      if (!is.na(data$task_resp.keys[n])){
        
        data$respCat[n]<-as.character(data[n,cat[data$task_resp.keys[n]]])
        
      }
      
    }
    
  }
   
    return(data)
  
}
    
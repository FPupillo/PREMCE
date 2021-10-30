getCat<-function(key){
  # this function take the key pressed by participant
  # and returns the column where the category can be found
  
  if (key==1){
    name<-"left_categ"
  }else if (key == 2){
    name<- "centleft_categ"
  }else if (key == 3) {
      name <- "centright_categ"
    } else if (key==4){
      name <-"right_categ" 
    }
  
  return(name)
}

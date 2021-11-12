

update_RW<-function(r, V, alpha){
  #----------------------------------------------------------------------------#
  # This function returns the updated value q and delta according to 
  # Rescorla Wagner model
  #
  # Input
  #   r: reinforcement (feedback): 1 if correct, 0 if incorrect
  #   V: value
  #   alpha: alpha parameter (learning rate)
  #
  # Output:
  #   updated V and Delta (prediction error)
  #----------------------------------------------------------------------------#
  delta = r - V;
  Q = V + alpha * delta
  return(list("V"= Q, "delta"= delta))  
}

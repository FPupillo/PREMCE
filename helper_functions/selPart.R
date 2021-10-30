selPart<- function(part){
  # -------------------------------------------------------------------------------------------------#
  # This function extracts the names of the files that belongs to  part 1, 2, or 3
  #
  # Input: Part
  #   1: select files of part 1
  #   2: select files of part 2
  #   3: select files of part 3
  #
  # Output:
  #   A string file containing the names of the files referring to the part selected (1,2,3)
  # -------------------------------------------------------------------------------------------------#
  
  # initialize variables
  part1Files<-vector()
  part2Files<-vector()
  part3Files<-vector()
  
  for (i in 1: length (list.files())){
    
    check<-substr((list.files()[i]), 15,28)
    
    if(check=="ses-01_part-01"){
      part1Files<- c(part1Files,(list.files()[i]))
    } else if (check=="ses-02_part-01") {
      part2Files<- c(part2Files,(list.files()[i]))
    } else if (check=="ses-02_part-02"){
      part3Files<- c(part3Files,(list.files()[i]))
    }
  }
  
  if (part==1){return(part1Files)
  } else if (part==2) {return(part2Files)
  } else if (part==3){return(part3Files)}
  
}
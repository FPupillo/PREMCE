# screen data, deleting unuseful data
rm(list=ls())

cd<-getwd()

setwd("raw_data")

# list files
files<-list.files()

# loop through the files
for (f in files){
if (!grepl( "premceCat2",f)) { # if this file does not contain "premceCat2"
  file.remove(f)
}
}

# delete the files that start with "Participant"
for (f in files){
  if (grepl( "PARTICIPANT",f)) { # if this file does not contain "premceCat2"
    file.remove(f)
  }
}

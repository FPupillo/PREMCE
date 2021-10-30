# Script to change the names of the characters
# from the colours to the monsters

# nabvigate to the folder
cd<-getwd()
setwd("lists")

# get the name of all the .csv files, which are the lists
names<-list.files(pattern = "*.csv")
# delete the recognition files, which are the first eight
names<-names[9:length(names)]

# loop through the files
# replace the colours with the characters
# red = m1.bmp
# yellow = m4
# black = m5
# white = m2
for (files in names){
  currfile<- read.csv(files)
  # convert all the variables as characters
  currfile$first_character<-as.character(currfile$first_character)
  currfile$second_character<-as.character(currfile$second_character)
  currfile$cuedCharacter<- as.character(currfile$cuedCharacter)
  
  
  # loop through the characters variables
  for (col in 1: nrow(currfile)){

    
    if(currfile$first_character[col]=="stimuli/red_character.jpg"){
      currfile$first_character[col]<-"stimuli/m1.jpg"
    }else if (currfile$first_character[col]=="stimuli/yellow_character.jpg"){
      currfile$first_character[col]<-"stimuli/m4.jpg"
    } else if (currfile$first_character[col]=="stimuli/black_character.jpg"){
      currfile$first_character[col]<-"stimuli/m5.jpg"
    } else if (currfile$first_character[col]=="stimuli/white_character.jpg"){
      currfile$first_character[col]<-"stimuli/m2.jpg"
    }
      
    
    if(currfile$second_character[col]=="stimuli/red_character.jpg"){
      currfile$second_character[col]<-"stimuli/m1.jpg"
    }else if (currfile$second_character[col]=="stimuli/yellow_character.jpg"){
      currfile$second_character[col]<-"stimuli/m4.jpg"
    }else if (currfile$second_character[col]=="stimuli/black_character.jpg"){
      currfile$second_character[col]<-"stimuli/m5.jpg"
    } else if (currfile$second_character[col]=="stimuli/white_character.jpg"){
      currfile$second_character[col]<-"stimuli/m2.jpg"
    }
    
    # now the cued character
    if (currfile$cuedCharacter[col] == "stimuli/red_character.jpg"){
    currfile$cuedCharacter[col]<-"stimuli/m1.jpg"
    } else if (currfile$cuedCharacter[col]=="stimuli/yellow_character.jpg"){
      currfile$cuedCharacter[col]<-"stimuli/m4.jpg"
    } else if (currfile$cuedCharacter[col]=="stimuli/black_character.jpg"){
      currfile$cuedCharacter[col]<-"stimuli/m5.jpg"
    }else if (currfile$cuedCharacter[col]=="stimuli/white_character.jpg"){
      currfile$cuedCharacter[col]<-"stimuli/m2.jpg"
    }
    
  }
  
  # now the cued character
  
  # print
  write.table(currfile, files,
         col.names = T,row.names = F, quote=F, sep=",")
  
  
}

setwd(cd)

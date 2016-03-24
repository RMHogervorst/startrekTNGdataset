# STAR trek TNG script reader version 1
# 
# R. M Hogervorst 
# 2016-3-23
# 
# Working with a lot! of regular expressions here
# Scripts have a very particular structure with lots of indentation
# You will see that descriptions and what people say is indented 
# differently
# I have documented what every part does
# 
# #######################################################
# 
### Reading the text
sample <- readLines("data-raw/150.txt", n = -1, encoding = "UTF-8")
### setting bounderies for where the cast is described
startcast <- grep("  CAST   ", sample)+1
endcast<- grep(" - SETS", sample)-1

startsets<-grep(" {5,}SETS ", sample) +4    # this matches the sets part and
# skips the interior and exterior parts
endsets <- grep("TEASER          1.", sample) -2

### replace occuronce of space, tab, newline, 
# carriage return, vertical tab, form feed with a space
# in the part of the script[] between cast and sets
CHARNAMES <- paste(gsub("[\t\n\r\v\f]{5,}", " ", sample[startcast:endcast] ), collapse = " ")
CHARNAMES <- gsub(" {5,}", ",",CHARNAMES)    # replace 5 spaces or more {5,} with a comma
CHARNAMES <- gsub("^,", "", CHARNAMES) # replace comma at start of sentence
CHARNAMES <- gsub(",$", "", CHARNAMES)  # replace comma at end of sentance

### Identical procedure  (could probably build this into a function...)
SETNAMES <- paste(gsub("[\t\n\r\v\f]{5,}", " ", sample[startsets:endsets] ), collapse = " ")
SETNAMES <- gsub(" {5,}", ",",SETNAMES)    # replace 5 spaces or more {5,} with a comma
SETNAMES <- gsub("^,", "", SETNAMES) # replace comma at start of sentence
SETNAMES <- gsub(",$", "", SETNAMES)  # replace comma at end of sentance

### pull some other information out of the script
TITLE <- grep("\"*\"", sample[1:20], value = TRUE)  # find anything with quotation marks
TITLE <- gsub("^ *\"", "", TITLE)  # remove start line any  number of spaces and a "
TITLE <- gsub("\" ", "", TITLE)   # remove final " mark and space
PRODNUM <-grep("#", sample[1:20], value = TRUE)  # find a line with a pount symbol
PRODNUM <- gsub(" *", "", PRODNUM)    # remove any spaces




sample <- sample[-grep("STAR TREK: ", sample)]    # remove the headers within the 
# document, I'm not interested in the page number

# head(sample[50:30]) 
# sample2<-scan("data-raw/150.txt", character(0), sep = "\n", fileEncoding = "UTF-8")

# 
#library(stringr)
# sum(grepl("captain", sample))
# ?stringr

# regexp start line with number 1[spaces]caps
#grepl("^[0-7]{1,3}[:space:]{3,4}[A-Z]*", sample )
#grepl("^[0-7][:space:]", sample )
# this count the number of scenes ####
#grep("^[0-7]{1,3}[:space:]*[A-Z]*"   ,sample)  # matches starting with a number 
# spaces caps. which is all the scenes.
# list from [number <number]

# delete lines that are like this
#           STAR TREK: "Evolution" - 7/24/89 - TEASER          1.
#     spaces STAR TREK: anything data ending in number dot endline
# find scenes within acts
# grep("TEASER", sample)
# grep("END OF TEASER", sample)
# grep("   ACT ONE   ", sample)
# grep("   ACT TWO   ", sample, value = TRUE)
# grep("   ACT THREE   ", sample, value = TRUE)
# grep("   ACT FOUR   ", sample, value = TRUE)
# grep("   ACT FIVE   ", sample, value = TRUE)
# grep("   ACT SIX   ", sample, value = TRUE)
# grep("END OF ACT", sample, value = TRUE)
# grep("THE END", sample, value = TRUE)

### create a data frame with act details
### Select lines which start with 1-3 numbers, any number of spaces followed
### by any number of CAPS.
### 
### Take a substring of that to collect the number
### also take the line number
actdetails<- data.frame( 
        names = grep("^[0-9]{1,3}[:space:]*[A-Z]*"   ,sample, value = TRUE),
        scenenumber = substr(grep("^[0-9]{1,3}[:space:]*[A-Z]*" ,sample, value = TRUE),1,3),
        number = grep("^[0-9]{1,3}[:space:]*[A-Z]*"   ,sample),
stringsAsFactors = FALSE)   # seriously... a data frame standard makes characters into
# factors it took me a while to figure out what happened.

### Add a variable ACT, based on the linenumber of the line that says END of
### 
actdetails$act <- ifelse(actdetails$number < grep("END OF TEASER", sample), "TEASER",
                    ifelse( actdetails$number < grep("END OF ACT ONE", sample), "ONE", 
                    ifelse(actdetails$number < grep("END OF ACT TWO", sample), "TWO",
                    ifelse(actdetails$number < grep("END OF ACT THREE", sample), "THREE",
                    ifelse(actdetails$number < grep("END OF ACT FOUR", sample), "FOUR",
                    ifelse(actdetails$number < grep("END OF ACT FIVE", sample), "FIVE",
                    ifelse(actdetails$number < grep("END OF ACT SIX", sample), "SIX",
                    ifelse(actdetails$number < grep("END OF ACT SEVEN", sample), "SEVEN",
                    ifelse(actdetails$number < grep("END OF ACT EIGHT", sample), "EIGHT",
                           "OTHER" )))))))))
### This does not work for the final one, 
### So I created this ugly function to fix that.
### If the name in the vector actdetails$act contains a SEVEn assume that the empty ones
### are one more.
actdetails$act[is.na(actdetails$act)] <- ifelse("EIGHT" %in% unique(actdetails$act), "OTHER", 
                                                ifelse("SEVEN" %in% unique(actdetails$act), "EIGHT",
                                                       ifelse("SIX" %in% unique(actdetails$act), "SEVEN",
                                                              ifelse("FIVE" %in% unique(actdetails$act), "SIX",
                                                                     ifelse("FOUR" %in% unique(actdetails$act), "FIVE",
                                                                            ifelse("THREE" %in% unique(actdetails$act), "FOUR",
                                                                                   ifelse("TWO" %in% unique(actdetails$act), "THREE",
                                                                                          ifelse("ONE" %in% unique(actdetails$act), "TWO",
                                                                                                 "ERROR IN ACT PARSING"))))))))




# functions ####
# function select text from linenumber 1 to endnumber 1
# ends in dataframe?
# 
# select_text<-function(text = sample, start, end) {
#         text[start:end]
# }

# text <- vector(mode = "character", length = length(startnumbers))
# title <- vector(mode = "character", length = length(startnumbers))
# scenenumber <- vector(mode = "character", length = length(startnumbers))
# place <- vector(mode = "character", length = length(startnumbers))
# 
# for(i in startnumbers) {
#         text[i] <- sample[startnumbers[i]: endnumbers[i]]
#         title[i] <- text[1]
#         scenenumber[i] <- substr(grep("^[0-7]{1,3}[:space:]*[A-Z]*" ,text, value = TRUE),1,3) # scene number
#         place[i] <- ifelse(grepl("INT.",title), "INT",
#                         ifelse(grepl("EXT.",title), "EXT", 
#                                ifelse( grepl("CONTINUED:", title), "CONTINUED",  "OTHER")))
#         
#         
# }
#title <- select_text(sample, startnumbers[3], endnumbers[3]) [1]


# detecting scene description

# linecounter <- ifelse( is.na(descriptionvector[2]-descriptionvector[1]), NA,
#                ifelse(descriptionvector[2]-descriptionvector[1] == 1, 2 ,
#                ifelse(descriptionvector[3]-descriptionvector[2]== 1 ,3,
#                ifelse(descriptionvector[4]-descriptionvector[3]== 1, 4,
#                ifelse(descriptionvector[5]-descriptionvector[4]== 1, 5, 
#                ifelse(descriptionvector[6]-descriptionvector[5]== 1, 6,
#                ifelse(descriptionvector[7]-descriptionvector[6]== 1, 7,
#                ifelse(descriptionvector[8]-descriptionvector[7]== 1, 8,
#                ifelse(descriptionvector[9]-descriptionvector[8]== 1, 9, 
#                ifelse(descriptionvector[10]-descriptionvector[9]== 1, 10, 
#                ifelse(descriptionvector[11]-descriptionvector[10]== 1, 11, 
#                       99)      
#                                  ))))))))))
# freetext <- paste(gsub("[\r\n\t]", "", text[descriptionvector[1:linecounter]]), collapse = " ")

# if(length(descriptionvector)> linecounter) {
#    left<- descriptionvector[linecounter:length(descriptionvector)]
#    linecounter <- ifelse( is.na(left[2]-left[1]), NA,
#                   ifelse(left[2]-left[1] == 1, 2 ,
#                 ifelse(left[3]-left[2]== 1 ,3,
#                 ifelse(left[4]-left[3]== 1, 4,
#                 ifelse(left[5]-left[3]== 1, 5, 99)
#                 ))))
#}
# if(linecounter ==99)stop("more then 11 lines")

#freetext2 <- paste(gsub("[\r\n\t]", "", text[left[1:linecounter]]), collapse = " ")




#paste(gsub("[\r\n\t]", "", text[voicevec]))
# \t\t\t\t\t who says   voicevec
# \t\t\t what is said   speech vec
#  ""                   emptylinevec
#  aaneensluitende nummers in vector samennemen. 
## speechextraction ###
# length(voicevec) # loop through?
# length(speech_descr)  # hoeveel descr zijn er?
# overal waar whitespace volgt op speech
# for (i in voicevec) {
#         
# }
# i <- 1
# speechdescription <- FALSE
# voicevec[i]
# #voicevec[1] +1
# if (voicevec[i] +1 == speech_descr[i]) speechdescription <- TRUE  # extra marking
# # vind opeenvolgende nummer in speechvec
# speechvec[i]  # start
# endspeech[i] # einde
# subspeech <- text[speechvec[i]:endspeech[i]]
# i <- which(speechvec > endspeech[i])[1] # set counter to next part?
# # doesn't work because there are multiple speech vectors, but part end vectors.
# # Better to extract vectors before hand and execute everyting at once.
# startspeechvec <- vector(mode = "integer", length = length(endspeech))
# for (i in speechvec) {
#         if(speechvec[i+1]-speechvec[i] != 1){append(startspeechvec, i)} 
#         
# }

# per file
# extract scenes, 
# per scene:
#  number, location, etc
#  als location continues, use previous.
#  title
#  description texts
#  dialogues
#  

# per row  season, episode, act, total number of scenes, scene number, subnumber, 
# type (scene description, voice), who says (empty in description), text
# 
# 
# 
###  data frame method ####
### create a bunch of linenumbers based on the characteristics of line
###  
startnumbers<-grep("^[0-9]{1,3}[:space:]*",sample)  # starts with number thus start of scene
endnumbers<- startnumbers -1             # endnumbers are logically the line before a new start
endnumbers<-endnumbers[-1]              # except for the first one
descriptionvector<-grep("^\t[A-Za-z]", sample)  # descriptions start with one tab
voicevec<-grep("^\t{5}[A-Za-z]", sample)        # Picard :  start with 5 tabs
speech_descr <- grep("\t{3,}\\(.*\\)$", sample) # emphesis or way of speaking 3 tabs ()
#descriptionintext <- grep("^\t{4}[A-Za-z]", sample)  # NOT sure what
speechvec<-grep("^\t{3}[A-Za-z]", sample)       # the speech itself with 3
specialvec<-grep("*\t{6,}[A-Za-z]", sample)     # some effect descriptions
emptylinevec <- grep("^[:space:]{0,}$", sample) # empty lines
endspeech <- emptylinevec[which(emptylinevec %in% (speechvec + 1))] -1
enddescription <- emptylinevec[which(emptylinevec %in% (descriptionvector + 1))] -1
actline <- c( grep("TEASER", sample), grep("ACT", sample) )  

final_line <- grep("THE END", sample)

### Make a dataframe with all the lines of the script
# and classify the linenumbers based on previous vectors
scriptdataframe <- data.frame( linenumber = startnumbers[1]:final_line)
scriptdataframe$classifier <- ifelse(scriptdataframe$linenumber %in% startnumbers,
                                     "scenedetails",
                                     ifelse(scriptdataframe$linenumber %in% descriptionvector, 
                                            "description", 
                                            ifelse(scriptdataframe$linenumber %in% voicevec, 
                                                   "whosays", 
                                                   ifelse(scriptdataframe$linenumber %in% speech_descr,
                                                          "speech_description",
                                                          ifelse(scriptdataframe$linenumber %in% speechvec,
                                                                 "speech", 
                                                                 ifelse(scriptdataframe$linenumber %in% specialvec,
                                                                        "film-effects",
                                                                        ifelse(scriptdataframe$linenumber %in% emptylinevec,
                                                                               "empty line", ifelse(scriptdataframe$linenumber %in% actline, 
                                                                                                    "ACTLINE",
                                                                                                    ifelse(scriptdataframe$linenumber %in% final_line, 
                                                                                                           "THE END","NA")))))))))
### add the text to the data frame
scriptdataframe$text <-sample[startnumbers[1]:final_line]  # add the text of script
scriptdataframe$endline <- ifelse(scriptdataframe$linenumber %in% endspeech, "speechend",
                                  ifelse(scriptdataframe$linenumber %in% enddescription, "descriptionend",
                                         NA))
### extract from whosays +1 till speechend

# 
length(which(scriptdataframe$classifier == "whosays"))
length(which(scriptdataframe$endline == "speechend"))

scriptdataframe$partnumber <- NA
counter <-1
for (i in 2:length(scriptdataframe$linenumber)){
        
        if(scriptdataframe$classifier[i-1]== "empty line" &
           scriptdataframe$classifier[i] == "description" &
           is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i] <- counter     
        }else if(scriptdataframe$classifier[i-1]== "empty line" &
                 scriptdataframe$classifier[i] == "description" &
                 !is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter 
                counter <- counter + 1                                  #
        }else if(scriptdataframe$classifier[i-1]== "description" &
                 scriptdataframe$classifier[i] == "description" &
                 is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter
        }else if(scriptdataframe$classifier[i-1]== "description" &
                 scriptdataframe$classifier[i] == "description" &
                 !is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter 
                counter <- counter + 1                                  #
        }else if(scriptdataframe$classifier[i-1]== "empty line" &
                 scriptdataframe$classifier[i] == "whosays" &
                 !is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter
                counter <- counter + 1
        }else if(scriptdataframe$classifier[i-1]== "empty line" &
                 scriptdataframe$classifier[i] == "whosays" &
                 is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter
        }else if(scriptdataframe$classifier[i-1] == "whosays" &
                 scriptdataframe$classifier[i] == "speech" &
                 is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter  
        }else if(scriptdataframe$classifier[i-1] == "whosays" &
                 scriptdataframe$classifier[i] == "speech" &
                 !is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter 
                         counter <- counter + 1                         #
        }else if(scriptdataframe$classifier[i-1] == "speech" &
                 scriptdataframe$classifier[i] == "speech" &
                 is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter       
        }else if(scriptdataframe$classifier[i-1] == "speech" &
                 scriptdataframe$classifier[i] == "speech" &
                 !is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter 
                         counter <- counter + 1                         #
        }else if(scriptdataframe$classifier[i-1] == "whosays" &
                 scriptdataframe$classifier[i] == "speech_description" &
                 !is.na(scriptdataframe$endline[i])){
                                  scriptdataframe$partnumber[i]<- counter 
                                  counter <- counter + 1                         #
        }else if(scriptdataframe$classifier[i-1] == "whosays" &
                 scriptdataframe$classifier[i] == "speech_description" &
                 is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter  
        }else if(scriptdataframe$classifier[i-1] == "speech_description" &
                 scriptdataframe$classifier[i] == "speech" &
                 !is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter 
                         counter <- counter + 1
        }else if(scriptdataframe$classifier[i-1] == "speech_description" &
                 scriptdataframe$classifier[i] == "speech" &
                 is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter
        }else if(scriptdataframe$classifier[i]== "THE END") {
                scriptdataframe$partnumber[i]<- NA
        }
               
       }

# perhaps only some logic with description and is.na(endline) partnumber
# if bot description and endline, at to the counter below.
# if emptyline scenedetails keep goin with numbering
# 
# or empty line above, description add number 
# description and endline means nothing
# empty line 
# 
# or with counter 
# count <- 0
# for (val in x) {
#        if(val %% 2 == 0)  count = count+1
# }
# 
#extract description untill descriptionend.
# length(which(scriptdataframe$classifier == "description"))
# length(which(scriptdataframe$endline == "descriptionend"))

# something with the values
# TITLE, SETNAMES, CHARNAMES, PRODNUM

# add actdetails to data frame. actdetails$number 

suppressMessages(library(dplyr)) 
script <-  left_join(scriptdataframe, actdetails, by = c("linenumber"="number", "text"="names"))
###
k <-2
for (k in 2:nrow(script)) {
        if(!is.na(script$scenenumber[k-1]) &
           is.na(script$scenenumber[k])){
                script$scenenumber[k] <- script$scenenumber[k-1]
                script$act[k] <- script$act[k-1]
        }
        
}

gsub("[\r\n]", "", x)


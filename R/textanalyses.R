# STAR trek TNG script reader version 1
# 
# R. M Hogervorst 
# 2016-3-23
# 
# Working with a lot! of regular expressions here
# Scripts have a very particular structure with lots of indentation
# You will see that descriptions and what people say is indented 
# differently. So it can be extracted. 
# I have documented what every part does
# 
# #######################################################
# function(range) { everything}
#

star_trek_combine <- function(eprange = 102:106, dir = "data-raw"){
        ### first make a list of all files in directory
        file_list<-list.files(paste(dir, "/", sep = ""), full.names=TRUE)
        # then extract the numbers
        name_list<-gsub(".txt","", gsub(paste(dir, "/", sep = ""), "", file_list))
        ### compare the numbers in eprange to the filenames
        #Throw an error when none are in range
        if (sum(eprange %in% name_list) ==0){stop("none of the episodes in range can be found in directory: ", dir)} 
        ### select only the possible files
        range <- eprange[eprange %in% name_list]
        ## execute start_trek_episode function for each
        main <- list()
        print("working on episode number:..")
        for (e in range) {
                #sink("logs.txt",append = TRUE)
                print(e)
                main[[e]] <- tryCatch({star_trek_episode(e)},
                                      error=function(cond) {
                        message(paste("error occured in file ",e,".txt", sep = ""))
                        message(cond)
                        # Choose a return value in case of error
                        return(NULL)
                },
                warning=function(cond) {
                        message("a warning message:")
                        message(cond)
                        # Choose a return value in case of warning
                        return(NULL)
                })
                flush.console()
        }
        print("combining data frames into one data table....")
        flush.console()
        suppressMessages(library(data.table))  
        #sink()
        rbindlist(main)
}
        

# 
## main function to create dataframe from script
star_trek_episode <- function(number, dir = "data-raw", trace = FALSE){
        ### Reading the text
        sample <- readLines(paste(dir, "/",number,".txt", sep = ""), 
                            n = -1, encoding = "UTF-8", warn = FALSE)
### setting bounderies for where the cast is described
### 
startcast <- grep("  CAST   ", sample)+1
startsets<-grep(" {5,}SETS ", sample) +4    # this matches the sets part and
# skips the interior and exterior parts
endcast<- grep(" - SETS", sample)-1
endsets <- (grep("TEASER          [0-9]{,2}", sample) -2 )[[1]]
if(length(startcast) == 0){
        CHARNAMES <- NA
        SETNAMES <- NA
}else  if(length(startsets)==0 & length(endcast)== 0){
        CHARNAMES <- NA
        SETNAMES <- NA
}else if(length(startsets)> 0 & length(endcast)== 0){
        endcast<- startsets
        SETNAMES <- NA
        CHARNAMES <- paste(gsub("[\t\n\r\v\f]{5,}", " ", sample[startcast:endcast] ), collapse = " ")
        CHARNAMES <- gsub(" {5,}", ",",CHARNAMES)    # replace 5 spaces or more {5,} with a comma
        CHARNAMES <- gsub("^,", "", CHARNAMES) # replace comma at start of sentence
        CHARNAMES <- gsub(",$", "", CHARNAMES)  # replace comma at end of sentance
}else{
        CHARNAMES <- paste(gsub("[\t\n\r\v\f]{5,}", " ", sample[startcast:endcast] ), collapse = " ")
        CHARNAMES <- gsub(" {5,}", ",",CHARNAMES)    # replace 5 spaces or more {5,} with a comma
        CHARNAMES <- gsub("^,", "", CHARNAMES) # replace comma at start of sentence
        CHARNAMES <- gsub(",$", "", CHARNAMES)  # replace comma at end of sentance
        ###  (could probably build this into a function...)
        SETNAMES <- paste(gsub("[\t\n\r\v\f]{5,}", " ", sample[startsets:endsets] ), collapse = " ")
        SETNAMES <- gsub(" {5,}", ",",SETNAMES)    # replace 5 spaces or more {5,} with a comma
        SETNAMES <- gsub("^,", "", SETNAMES) # replace comma at start of sentence
        SETNAMES <- gsub(",$", "", SETNAMES)  # replace comma at end of sentance
} 

### pull some other information out of the script
TITLE <- grep("\"*\"", sample[1:20], value = TRUE)[1]  # find anything with quotation marks
TITLE <- gsub("^ *\"", "", TITLE)  # remove start line any  number of spaces and a "
TITLE <- gsub("\" ", "", TITLE)   # remove final " mark and space
PRODNUM <-grep("#", sample[1:20], value = TRUE)  # find a line with a pount symbol
PRODNUM <- gsub(" *", "", PRODNUM)    # remove any spaces

if(length(TITLE) ==0) TITLE <-NA
if(length(PRODNUM)== 0) PRODNUM <- NA



sample <- sample[-grep("STAR TREK: ", sample)]    # remove the headers within the 
# document, I'm not interested in the page number

### create a data frame with act details  ######
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
### FOUND out that first one doesn't have acts, but parts.... 
### This really needs to be simpler function
actdetails$act <- character(length = length(actdetails$names))
TEASEREND <-ifelse(length(grep("END OF TEASER", sample)) ==0, 
                   grep("                  PART ONE            ", sample),
                   grep("END OF TEASER", sample))
ACTONEEND <- ifelse(length(grep("END OF ACT ONE", sample)) ==0, 
                    grep("                  PART TWO            ", sample),
                    grep("END OF ACT ONE", sample))
ACTTWOEND <- ifelse(length(grep("END OF ACT TWO", sample)) ==0, 
                    grep("                  PART THREE            ", sample),
                    grep("END OF ACT TWO", sample))
ACTTHREEEND<- ifelse(length(grep("END OF ACT THREE", sample)) ==0, 
                     grep("                  PART FOUR            ", sample),
                     grep("END OF ACT THREE", sample))
ACTFOUREND <- ifelse(length(grep("END OF ACT FOUR", sample)) ==0, 
                  grep("                  PART FIVE            ", sample),
                  grep("END OF ACT FOUR", sample))

ACTFIVEEND <- ifelse(length(grep("END OF ACT FIVE", sample)) ==0, 
                  grep("                  PART SIX            ", sample),
                  grep("END OF ACT FIVE", sample))
ACTSIXEND <- ifelse(length(grep("END OF ACT SIX", sample)) ==0, 
                 grep("                  PART SEVEN            ", sample),
                 grep("END OF ACT SIX", sample))
ACTSEVENEND <- ifelse(length(grep("END OF ACT SEVEN", sample)) ==0, 
                   grep("                  PART EIGHT            ", sample),
                   grep("END OF ACT SEVEN", sample))
ACTEIGHTEND <- ifelse(length(grep("END OF ACT EIGHT", sample)) ==0, 
                grep("                  PART NINE            ", sample),
                grep("END OF ACT EIGHT", sample))

actdetails$act <- ifelse(actdetails$number < TEASEREND, "TEASER",
                    ifelse( actdetails$number < ACTONEEND, "ONE", 
                    ifelse(actdetails$number < ACTTWOEND, "TWO",
                    ifelse(actdetails$number < ACTTHREEEND, "THREE",
                    ifelse(actdetails$number < ACTFOUREND, "FOUR",
                    ifelse(actdetails$number < ACTFIVEEND, "FIVE",
                    ifelse(actdetails$number < ACTSIXEND, "SIX",
                    ifelse(actdetails$number < ACTSEVENEND, "SEVEN",
                    ifelse(actdetails$number < ACTEIGHTEND, "EIGHT",
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





###  data frame of scriptdataframe ####
### create a bunch of linenumbers based on the characteristics of line
###  
startnumbers<-grep("^[0-9]{1,3}[:space:]*",sample)  # starts with number thus start of scene
endnumbers<- startnumbers -1             # endnumbers are logically the line before a new start
endnumbers<-endnumbers[-1]              # except for the first one
descriptionvector<-c(grep("^\t[A-Za-z]", sample),grep("^\t\\([A-Za-z]", sample) )  # descriptions start with one tab
voicevec<-c(grep("^\t{5}[A-Za-z]", sample),grep("^\t{5}\"[A-Za-z]" , sample) )
### Picard :  start with 5 tabs .... They write Q like this "Q"...
speech_descr <- grep("\t{3,}\\(.*\\)$", sample) # emphesis or way of speaking 3 tabs ()
#descriptionintext <- grep("^\t{4}[A-Za-z]", sample)  # NOT sure what
speechvec<- c(grep("^\t{3}[A-Za-z]", sample), grep("^\t{3}\"[A-Za-z]", sample))      # the speech itself with 3
specialvec<-grep("*\t{6,}[A-Za-z]", sample)     # some effect descriptions
emptylinevec <- grep("^[:space:]{0,}$", sample) # empty lines
endspeech <- emptylinevec[which(emptylinevec %in% (speechvec + 1))] -1
enddescription <- emptylinevec[which(emptylinevec %in% (descriptionvector + 1))] -1
actline <- c( grep("TEASER", sample), grep("ACT", sample) )  

final_line <- grep("THE END", sample)
if(length(final_line)==0){
        final_line <- length(sample)
}

### KILLER ROBOT  #####

if(length(startnumbers)==0){
        episode <-data.frame( 
                episode = TITLE,
                productionnumber = PRODNUM,
                setnames = SETNAMES,
                characters = CHARNAMES,
                act = character(1),
                scenenumber = integer(1),
                scenedetails = character(1),
                partnumber = integer(1),
                type = character(1),
                who = character(1),
                text = "ERROR in PARSING",
                speechdescription = logical(1)
                ,stringsAsFactors = F
        )
        episode
}else{
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
## the following script loops through all rows and checks the row above and 
## itself. if it is empty it will copy the counter value.
## if the the line is not empty it will copy the counter and add 1 to
## the number. 
## This is a very bulky script that is slow as hell.
## There must be a vectorized operation using ifelse of some other
## computation, that would be much faster. But i don't know how.
## 
scriptdataframe$partnumber <- NA # create the vector / column first
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
                 is.na(scriptdataframe$endline[i])){
                         scriptdataframe$partnumber[i]<- counter       
        }else if(scriptdataframe$classifier[i-1]== "speech_description" &
                 scriptdataframe$classifier[i] == "whosays" &
                 is.na(scriptdataframe$endline[i]) ){
                scriptdataframe$partnumber[i]<- counter
        }else if(scriptdataframe$classifier[i-1]== "scenedetails" &
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
        }else if(scriptdataframe$classifier[i-1] == "NA" &
                 scriptdataframe$classifier[i] == "speech" &
                 is.na(scriptdataframe$endline[i])){
                scriptdataframe$partnumber[i]<- counter
        }else if(scriptdataframe$classifier[i-1] == "speech" &
                 scriptdataframe$classifier[i+1] == "speech" &
                 is.na(scriptdataframe$endline[i])){
                scriptdataframe$partnumber[i]<- counter  
        }else if(scriptdataframe$classifier[i-1] == "description" &
                          scriptdataframe$classifier[i+1] == "description" &
                          is.na(scriptdataframe$endline[i]) &
                 scriptdataframe$classifier[[i]] != "empty line"){
                scriptdataframe$partnumber[i]<- counter 
        }else if(scriptdataframe$classifier[i-1] == "empty line" &
                 scriptdataframe$classifier[i] == "speech"){
                scriptdataframe$partnumber[i]<- counter
        }
               
       }
### Join the two data frames together
suppressMessages(library(dplyr)) 
script <-  left_join(scriptdataframe, actdetails, by = c("linenumber"="number", "text"="names"))
script$scenenumber[grep(" THE END", script$text)]<-NA

rm(scriptdataframe, actdetails) # remove the parent data frames

### Another loop with the same logic as before. look above and at
### self, if self empty copy the value above.
k <-2
for (k in 2:nrow(script)) {
        if(!is.na(script$scenenumber[k-1]) &
           is.na(script$scenenumber[k])){
                script$scenenumber[k] <- script$scenenumber[k-1]
                script$act[k] <- script$act[k-1]
        }
        
}

### Create a data frame with all relevant information. 
#
# needs to look like this
# TITLE, PRODNUM, number, act, scenenumber, partnumber, type (discription or speech),
# Whosays (only in speech), text (combining multiple rows), speechdescription yes no, 
### since appending a dataframe is really slow, it's important to already shape its
### dimensions. 
df_length <- length(unique(script$partnumber)) - sum(is.na(unique(script$partnumber)))

episode <-data.frame( 
        episode = TITLE,
        productionnumber = PRODNUM,
        setnames = SETNAMES,
        characters = CHARNAMES,
        act = character(df_length),
        scenenumber = integer(df_length),
        scenedetails = character(df_length),
        partnumber = integer(df_length),
        type = character(df_length),
        who = character(df_length),
        text = character(df_length),
        speechdescription = logical(df_length)
        ,stringsAsFactors = F
) 

### extracting info for final data frame ####
## this will loop through the dataframe created above and 
## extract who says something if it is speech and if there are things
## like (gasps and so forth)
## it also fills act, scenenumber, partnumber, type.
for (i in 1:df_length) { # every i is a partnumber
        if (trace == TRUE) print(paste(number,"in part", i))
        extractedtext <- script %>% filter(partnumber == i)
        who <- gsub(" {1,}"," ", 
                    gsub("\t"," ", 
                         paste(extractedtext$text[extractedtext$classifier == "whosays"], 
                               collapse = " ")))
        if(who == "") who <- NA 
        episode$who[i]<-who
        replacementtext <- gsub(" {2,}"," ", gsub("\t"," ", paste(unlist(extractedtext$text[which(extractedtext$classifier == "description")]),collapse = " ")))
        if(length(replacementtext)==0 ){replacementtext <- "No text ERROR"} ## This does nothing
        episode$text[i]<- if(sum(extractedtext$classifier == "description") >=1){
                replacementtext
        }else if(sum(extractedtext$classifier == "speech") >=1 ){
                gsub(" {1,}"," ", gsub("\t"," ", paste(unlist(
                        extractedtext$text[which(extractedtext$classifier == "speech")]) ,collapse = ""))) 
        }
        episode$act[i]<- extractedtext$act[1]
        episode$scenenumber[i] <- extractedtext$scenenumber[1]
        episode$partnumber[i] <-i
        episode$type[i] <- if(sum(extractedtext$classifier == "description") >=1){
                "description"
        }else if(sum(extractedtext$classifier == "speech") >=1){
                "speech"
        }else {"error"}
        episode$speechdescription[i]<- if(sum(extractedtext$classifier == "speech_description") >=1){
        TRUE
                }else {FALSE}
}
episode
}
} 

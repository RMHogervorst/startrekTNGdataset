#scan("data-raw/150.txt", what = "character")


sample <- readLines("data-raw/150.txt", n = -1, encoding = "UTF-8")
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

#create a data frame with scene details
scenedetails<- data.frame( 
names = grep("^[0-7]{1,3}[:space:]*[A-Z]*"   ,sample, value = TRUE),
scenenumber = substr(grep("^[0-7]{1,3}[:space:]*[A-Z]*" ,sample, value = TRUE),1,3),
number = grep("^[0-7]{1,3}[:space:]*[A-Z]*"   ,sample)
)
scenedetails$act <- ifelse(scenedetails$number < grep("END OF TEASER", sample), "TEASER",
                    ifelse( scenedetails$number < grep("END OF ACT ONE", sample), "ONE", 
                    ifelse(scenedetails$number < grep("END OF ACT TWO", sample), "TWO",
                    ifelse(scenedetails$number < grep("END OF ACT THREE", sample), "THREE",
                    ifelse(scenedetails$number < grep("END OF ACT FOUR", sample), "FOUR",
                    ifelse(scenedetails$number < grep("END OF ACT FIVE", sample), "FIVE",
                    ifelse(scenedetails$number < grep("END OF ACT SIX", sample), "SIX",
                    ifelse(scenedetails$number < grep("END OF ACT SEVEN", sample), "SEVEN",
                    ifelse(scenedetails$number < grep("END OF ACT EIGHT", sample), "EIGHT",
                           "OTHER" )))))))))
scenedetails$number <-NULL      # remove number column because the numbers will change
sample <- sample[-grep("STAR TREK: ", sample)]    # remove the headers


startnumbers<-grep("^[0-7]{1,3}[:space:]*[A-Z]*"   ,sample)
endnumbers<- startnumbers -1
endnumbers<-endnumbers[-1]

# functions ####
# function select text from linenumber 1 to endnumber 1
# ends in dataframe?
select_text<-function(text = sample, start, end) {
        text[start:end]
}
i<- 5

for(i in startnumbers) {
        text <- sample[startnumbers[i]: endnumbers[i]]
        title <- text[1]
        scenenumber <- substr(grep("^[0-7]{1,3}[:space:]*[A-Z]*" ,text, value = TRUE),1,3) # scene number
        place <- ifelse(grepl("INT.",title), "INT",
                        ifelse(grepl("EXT.",title), "EXT", 
                               ifelse( grepl("CONTINUED:", title), "CONTINUED",  "OTHER")))
        
        
}
#title <- select_text(sample, startnumbers[3], endnumbers[3]) [1]


# detecting scene description
descriptionvector<-grep("^\t[A-Za-z]", text)
linecounter <- ifelse( is.na(descriptionvector[2]-descriptionvector[1]), NA,
               ifelse(descriptionvector[2]-descriptionvector[1] == 1, 2 ,
               ifelse(descriptionvector[3]-descriptionvector[2]== 1 ,3,
               ifelse(descriptionvector[4]-descriptionvector[3]== 1, 4,
               ifelse(descriptionvector[5]-descriptionvector[3]== 1, 5, 99)      
                                 ))))
freetext <- paste(gsub("[\r\n\t]", "", text[descriptionvector[1:linecounter]]), collapse = " ")

if(length(descriptionvector)> linecounter) {
   left<- descriptionvector[linecounter:length(descriptionvector)]
   linecounter <- ifelse( is.na(left[2]-left[1]), NA,
                  ifelse(left[2]-left[1] == 1, 2 ,
                ifelse(left[3]-left[2]== 1 ,3,
                ifelse(left[4]-left[3]== 1, 4,
                ifelse(left[5]-left[3]== 1, 5, 99)
                ))))
}
if(linecounter ==99)stop("more then 5 lines")

freetext2 <- paste(gsub("[\r\n\t]", "", text[left[1:linecounter]]), collapse = " ")

voicevec<-grep("^\t{5}[A-Za-z]", text)
descriptionintext <- grep("^\t{4}[A-Za-z]", text)
speechvec<-grep("^\t{3}[A-Za-z]", text)
specialvec<-grep("*\t{6,}[A-Za-z]", text)
emptylinevec <- grep("^[:space:]{0,}$", text)

# \t\t\t\t\t who says   voicevec
# \t\t\t what is said   speech vec
#  ""                   emptylinevec
#  aaneensluitende nummers in vector samennemen. 

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



grep("  CAST   ", sample)
#tot
grep(" - SETS", sample)


gsub("[\r\n]", "", x)

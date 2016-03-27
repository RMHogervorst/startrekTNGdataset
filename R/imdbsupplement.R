# imdb series information
# devtools::install_github("RMHogervorst/imdb)
library(imdb)
TNG_imdb<-imdbSeries("Star Trek The Next Generation", 1:7)
library(dplyr)
#library(magrittr)
TNG_imdb %>% 
        select(Title, Season, Episode, imdbRating) %>%
        anti_join( all_episodes_TNG, by = c("Title"="episode"))
# 29 cannot be joined. 
check_ep <- function(check, data = all_episodes_TNG){
data %>%
        group_by(episode) %>%
        filter(partnumber <2) %>%
        filter(grep(check, episode))
}
# there is a time's arrow and a times arrow part 2
check_ep("The Arsenal of Freedom") # 0
check_ep("The Arsenal Of Freedom") # 
# let's see if this actually makes sense

TNG_imdb$Title <- tolower(TNG_imdb$Title)
all_episodes_TNG$episode <- tolower(all_episodes_TNG$episode) 
TNG_imdb %>% 
        select(Title, Season, Episode, imdbRating) %>%
        anti_join( all_episodes_TNG, by = c("Title"="episode"))
# 25 left... 
check_ep("all good things")[[1]]
grepl("all good things", TNG_imdb)

all_episodes_TNG$episode <- gsub(" {2,}","",all_episodes_TNG$episode)
# solved 2 more. 23 to go.
# Q is without qoutation marks.
grep("\"q",all_episodes_TNG$episode, value = TRUE)
check_ep("Hide and")
check_ep("\"q")
check_TNG("hide and")
check_ep("q")
all_episodes_TNG$episode <- gsub("\"q","q",all_episodes_TNG$episode)
check_ep("déjà q") #none
check_ep("deja q") # one.
all_episodes_TNG$episode <- gsub("deja q","déjà q",all_episodes_TNG$episode)
check_ep("birthright")# parts are different
check_ep("part")
all_episodes_TNG$episode <- gsub("part i$","part 1",all_episodes_TNG$episode)
all_episodes_TNG$episode <- gsub("part ii$","part 2",all_episodes_TNG$episode)
check_ep("birthright")
all_episodes_TNG$episode <- gsub(",",":",all_episodes_TNG$episode)
# 13 left
check_ep("whisper")[[1,1]]
# TNG_imdb %>% filter(grep("whisper",Title))
check_TNG <- function(check){
        paste(grep(check, TNG_imdb$Title, value = TRUE)," number: ",
        grep(check, TNG_imdb$Title))
}
check_TNG("whisper")
all_episodes_TNG$episode <- gsub(" $","",all_episodes_TNG$episode) # replace space at end with nothing
# 12 left
check_ep("no one")
check_TNG("no one")
check_ep("has gone")
check_ep("where")
# where none have gone before
all_episodes_TNG$episode <- gsub("where none have gone before","where no one has gone before",all_episodes_TNG$episode) # replace space at end with nothing
check_ep("unification")
check_TNG("unification")
TNG_imdb$Title <- gsub("unification i$","unification part 1",TNG_imdb$Title )
TNG_imdb$Title <- gsub("unification ii$","unification part 2",TNG_imdb$Title )
# 9 left.
#       Title           Season          Episode imdbRating
# 1    birthright: part 1      6      16        7.5
# 2 elementary, dear data      2       3        8.2
# 3      future imperfect      4       8        7.8
# 4            hide and q      1       9        6.9
# 5                i borg      5      23        8.6
# 6         ménage à troi      3      24        6.6
# 7          skin of evil      1      22        6.8
# 8       the big goodbye      1      11        7.3
# 9  time's arrow: part 1      5      26        8.4
check_ep("birthright")
all_episodes_TNG$episode <- gsub("birthright$","birthright: part 1",all_episodes_TNG$episode)
check_ep("data")
check_TNG("data")
# elementary, dear data does not fit because i replaced the comma with a :
all_episodes_TNG$episode <- gsub("elementary: ","elementary, ",all_episodes_TNG$episode)
check_ep("*fect") # no idea
check_ep("hide") # q has qoutation mark before
all_episodes_TNG$episode <- gsub("hide and \"q","hide and q",all_episodes_TNG$episode)
check_TNG("borg")
check_ep("borg") # artefact from replacement. i: borg
all_episodes_TNG$episode <- gsub("i: borg","i borg",all_episodes_TNG$episode)
check_ep("troi")
all_episodes_TNG$episode <- gsub("menage a troi","ménage à troi",all_episodes_TNG$episode)
check_ep("evil") # no idea
check_ep("goodbye") # no idea
check_ep("the big") #the big good-bye
all_episodes_TNG$episode <- gsub("the big good-bye","the big goodbye",all_episodes_TNG$episode)
check_ep("arrow")
all_episodes_TNG$episode <- gsub("time's arrow$","time's arrow: part 1",all_episodes_TNG$episode)
# left
# Title Season Episode imdbRating
# 1     future imperfect      4       8        7.8
# 2         skin of evil      1      22        6.8
# 3 time's arrow: part 2      6       1        8.3
all_episodes_TNG$episode <- gsub("time's arrow: part 1: part 2$","time's arrow: part 2",all_episodes_TNG$episode)
check_ep("skin")
all_episodes_TNG %>% group_by(episode)%>%
        filter(grep(" of ", episode))

# episode
# 1
# f.k.a.'the shroud'"
# 2
# time's arrow: part 2
# 3
# rascals

check_TNG("arrow")
check_ep("arrow")[[1]] # it contains a space...
# solved.
check_TNG("shroud")
check_ep("shroud")
check_ep("evil")
## research shows that shroud and skin of evil are the same
##   "Loud as a Whisper  is the 132th episode. is this rascal?
##   
##   rascals zit niet in imdb dataset. future imperfect zit nietin mijn stuk.
all_episodes_TNG$episode <- gsub("f.k.a.'the shroud'\"","skin of evil",all_episodes_TNG$episode)
all_episodes_TNG$episode <- gsub(" {2,}","",all_episodes_TNG$episode)

## well lets just join them
TNG <- left_join(data.frame(all_episodes_TNG), TNG_imdb, by = c("episode"="Title"))

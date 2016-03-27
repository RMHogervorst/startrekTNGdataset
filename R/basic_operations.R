# basic operations on all episodes.
# Roel M Hogervorst
# 
# 2016-3-27
library(dplyr)
library(magrittr)
library(ggplot2)
all_episodes_TNG %>%
        filter(type == "speech") %>%
        #filter(act== "ONE") %>%
        group_by(episode, who) %>%
        summarise(peeps = n()) %>%
        arrange(desc(peeps)) %>%
        View

# head(grep("picard", TNG$who, value = TRUE))
teacomsumption <- TNG %>%
      filter(grepl("PICARD",who))%>%
      filter(grepl("[Tt]ea", text)) %>% 
        group_by(Season, Episode) %>%
        summarise(number_of_tea = n())

ggplot(data = teacomsumption, aes(number_of_tea, Episode)) + geom_point()
#teaconsumption <- apply(teacomsumption,2, as.integer)
sapply(teacomsumption, class)
teacomsumption$Season <- as.factor(as.integer(teacomsumption$Season))
teacomsumption$Episode <- as.integer(teacomsumption$Episode)

ggplot(data = teacomsumption, aes( Episode, number_of_tea)) + geom_point(aes(colour = Season)) +
        geom_smooth(aes(group = Season, colour = Season), method = "lm")

# What makes these weird high numbers
summary(teacomsumption$number)
teacomsumption[teacomsumption$number == 9 ,]
# season1, episode 23
hightea<- TNG[TNG$Episode ==23,][1,] 
hightea$episode # we'll always have paris
# Captain Picard encounters a woman from his past after her 
# scientist husband's experiments begin to unravel the fabric of time. 
#
# obviously time travel had to be involved.
hightea$imdbRating # 6.6

hightea_episode<- TNG[TNG$Episode ==23,]
grep("time", hightea_episode$text)
grep("tea", hightea_episode$text)

lowtea <- TNG %>% filter(Episode == 24)
grep("time", lowtea$text)
grep("tea", lowtea$text)

sum(teacomsumption$number_of_tea, na.rm= TRUE) # 216 consumptions of tea


# time score
# Tea score
# #picard score
# 
greytea <- TNG %>%
        filter(grepl("picard",who))%>%
        filter(grepl("gr[ea]y", text)) %>% 
        group_by(Season, Episode) %>%
        summarise(number_of_tea = n())
sum(greytea$number_of_tea, na.rm = TRUE) # 12 none in season 1, 4

earl<- TNG %>%
        filter(grepl("PICARD",who))%>%
        filter(grepl("earl", text)) %>% 
        group_by(Season, Episode) %>%
        summarise(number_of_tea = n())
sum(earl$number_of_tea, na.rm = TRUE)  #62 

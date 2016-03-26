Star Trek The Next Generation Dataset
================

TLDR
====

I'm a very nerdy person. I made a TNG dataset.

Short intro
===========

This repo contains the raw files, moviescripts from the sci-fi series Star Trek The Next Generation, R scripts to transform them and a dataset.

The dataset has 12 variables/columns and 110176 rows. variable names are here:

    [1] "episode"           "productionnumber"  "setnames"          "characters"       
    [5] "act"               "scenenumber"       "scenedetails"      "partnumber"       
    [9] "type"              "who"               "text"              "speechdescription"

Episode contains the name of the episode, productionnumber, setnames, and characters were scraped from the toppart of the moviescript. All scripts are divided up into partnumbers. A part can be a description or speech (as told by the TYPE variable). speech and descriptions over multiple lines is put together. ACT, SCENENUMBER, PARTNUMBER tell you what follows what and where in the episode this happened.

for example in the episode New Ground somewhere in the episode a certain grubby crewmember confirms something...

    all_episodes_TNG[65305,]

has episode New Ground, production number \#40275-210 a bunch of sets and the following people in the cast:

PICARD,HELENA ROZHENKO,RIKER,ALEXANDER,DATA,MS. LOWRY,BEVERLY,ENSIGN FELTON,TROI,DOCTOR JA'DAR,GEORDI,WORF,Non-Speaking,SUPERNUMERARIES,SEVERAL BOYS,SEVERAL FATHERS,A SKULL-FACED ALIEN,WAITER

*As you can see Non-Speaking is not really a castmember. but describes the next people* That happens when you scrape text.

       act scenenumber scenedetails partnumber   type   who   text speechdescription
    1: ONE         6A                       95 speech  WORF  Good.             FALSE

And as you can see, WORF says "Good." in act one, scene 6a, partnumber 95. There is no description how Worf says this.

### disclaimer

I haven't checked everything and I had some errors during the construction, so some scripts are not complete and some parts are perhaps wrongly classified as speech or description.

### Resources

I've dowloaded all the files from <http://www.st-minutiae.com/resources/scripts/>

And discovered that the scripts (mostly...) follow a convention of

-   one tab for descriptions
-   three tabs for what people say
-   five tabs for who says things
-   etc

### Licence

My code is under MIT licence I don't know what licence the moviescripts have My dataset is CC0 PUBLIC domain.

Use how you like, but please mention me and help me make the code better. *Also don't hold me responsable for the code (that is in essence the MIT licence)*

I'm very curious to see your analyses of TNG. Enjoy

Roel M. Hogervorst

2016-3-26

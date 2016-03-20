# loading the data    https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html
# preprocessing with textmining.
library(tm)
docs<-Corpus(DirSource("data-raw"))
summary(docs)
docs <- tm_map(docs, removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, removeWords, stopwords("english"))  
#docs <- tm_map(docs, removeWords, c("department", "email"))   
for (j in seq(docs))
{
        docs[[j]] <- gsub("star trek", "star-trek", docs[[j]])
        docs[[j]] <- gsub("number one", "number-one", docs[[j]])
        #docs[[j]] <- gsub("qualitative analysis", "QDA", docs[[j]])
        #docs[[j]] <- gsub("research methods", "research_methods", docs[[j]])
}
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   
docs <- tm_map(docs, stripWhitespace)   
docs <- tm_map(docs, PlainTextDocument)   
# Stage the Data
# 
# To proceed, create a document term matrix.
# This is what you will be using from this point on.
# 
dtm <- DocumentTermMatrix(docs)   
dtm   
tdm <- TermDocumentMatrix(docs)   
tdm   
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)
# focus on interesting stuff
#  Start by removing sparse terms:   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)  
freq[head(ord)]  
freq[tail(ord)] 
head(table(freq), 20)
tail(table(freq), 20) 
freq <- colSums(as.matrix(dtms))   
freq 
findFreqTerms(dtm, lowfreq=400) 
findAssocs(dtm, c("riker" , "captain"), corlimit=0.70) # specifying a correlation limit of 0.98   
# Hierarchal Clustering  #####
# 
# First calculate distance between words & then cluster them according to similarity.
# 
library(cluster)
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   
inspect(dtmss) 
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit   
plot(fit, hang=-1)  
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=8)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   
# k means clustering ####
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 10)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

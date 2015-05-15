library(XML)
library(RCurl)
library(tm)
library(wordcloud)

xml.url <- "http://www.minutoseguros.com.br/blog/feed/"
xml.doc <- getURL(xml.url)
xml.parse <- xmlParse(xml.doc)

xml.title <- xpathSApply(xml.parse, '//item/title', xmlValue)
xml.description <- xpathSApply(xml.parse, '//item/description', xmlValue)
xml.content <-xpathSApply(xml.parse, '//item/content:encoded', xmlValue)

blog <- data.frame(xml.title, xml.description, xml.content)

#Corpus from data data frame
corpus <- Corpus(VectorSource(blog$xml.content))

#All words lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

#Removing punctuation
corpus <- tm_map(corpus, removePunctuation)

#Removing words like 
bad_words <- c("relbookmark","minutoap","post","p8211","hrefhttpblogminutoazurewebsitesnetblog", "classyarpprelatedrss", "appeared","relnofollow", "postsp","relnofollow","div", "first", "pthe", "related", "targetblankseguros", "targetblankseguro", "classalignright", "pno", "yarpprelatednone")
corpus <- tm_map(corpus, removeWords, c(bad_words, stopwords("portuguese")))

#http://en.wikipedia.org/wiki/Document-term_matrix
dtm <- DocumentTermMatrix(corpus)

# Only 5% of the relevant words
sparse <- removeSparseTerms(dtm, 0.95)

#Most frequently terms
findFreqTerms(sparse, lowfreq = 8)

#Create new data frame
allFrenquencies <- as.data.frame(as.matrix(sparse))

#word of clouds
wordcloud(colnames(allFrenquencies), colSums(allFrenquencies))

#clustering the words
#Measure the euclidean distance between the words
distance <- dist(allFrenquencies, method="euclidean")
cluster <- hclust(distance, method="ward.D")

plot(cluster)
#After see the plot of the cluters, I choose to group 4 cluster
groups <- cutree(cluster, k = 4)

#4 cluster of words
clusterOfWords <- split(allFrenquencies, groups)

# Words in the cluster and the means of time the word appears in each cluster 

# Cluster #1
# surgiu   dia    primeira  anos    seguros   seguro 
#    2      3        3        5        5        6 
tail(sort(colMeans(clusterOfWords[[1]])))

# Cluster #2
# duelo     faz     lia seguros   valor   seguro 
#    3       3       3       3       3       9 
tail(sort(colMeans(clusterOfWords[[2]])))

# Cluster #3
#   seguro    importante  segurança    seguros        ser     minuto 
# 0.8571429      1.0         1.0       1.1428571  1.2857143  1.4285714 
tail(sort(colMeans(clusterOfWords[[3]])))

# Cluster #4
# acidentes       bem      pode     podem    ambiente  trabalho 
#    4             4         4         4         5         5 
tail(sort(colMeans(clusterOfWords[[4]])))

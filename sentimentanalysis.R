library(tm)
library(wordcloud)
library(syuzhet)
#read dataset
reviews <- read.csv(file.choose(), header = T)
str(reviews)

corpus <- iconv(reviews$review.text)
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

reviews_final <- corpus

tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]
#barplot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, las = 2, col = "blue")
#create wordcloud
w <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(w), freq = w, max.words = 50,random.order = T,
          min.freq = 5,colors = brewer.pal(25,"Dark2"),
          scale = c(3,0,3))

sentiment_data <- iconv(reviews$review.text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

#calculate review score
s$score <- s$positive - s$negative
s[1:10,]


#check overall sentiment 
review_score <- colSums(s[,])
print(review_score)

#plot product sentiment
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')



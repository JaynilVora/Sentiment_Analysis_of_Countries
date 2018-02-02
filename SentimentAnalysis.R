

library("twitteR")

require('ROAuth')
require('RCurl')
key <- "vZSxLplTfbaXI8gVyThJ4JqvR"
secret <- "c4LKqTMoia49wAup3lEAEGr3z0OfaMxgyZniPioOWjSTwZPTpu"
secrettk <- "3FYoesz1EOeB4CbwUHy5cHv38zH6B0RTsE1MpyWxlBFxQ"
mytoken <- "915707499172556801-TAAl4VV55IELsToq3M2rMD6yt0MZm8g"
setup_twitter_oauth(key, secret, mytoken, secrettk)
library("twitteR")
library("httr")



#Seacrh tweets with udemy mentioned
udemytweets = searchTwitter("#India", n=1000)
head(udemytweets)

#tweets form udemy
userTimeline("Udemy")

class(udemytweets)
list(udemytweets)
head(udemytweets)


#textmining
library("tm")

udemylist <- sapply(udemytweets, function(x) x$getText())
udemycorpus <- Corpus(VectorSource(udemylist))
udemycorpus <- tm_map(udemycorpus,function(x)removeWords(x,stopwords()))
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
udemycorpus<- tm_map(udemycorpus,toSpace,"[^[:graph:]]")
udemycorpus <- tm_map(udemycorpus, content_transformer(tolower))
udemycorpus <- tm_map(udemycorpus,removePunctuation)

?getTransformations

udemycorpus <- tm_map(udemycorpus, PlainTextDocument)

udemycorpus = tm_map(udemycorpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
# UTF-8-MAC b/c I'm on macOS

library("wordcloud")
?wordcloud

wordcloud(udemycorpus, min.freq = 4, scale = c(5,1), random.color = F, max.word = 50, random.order = F)


#Termdocumentmatrix - The term-document matrix then is a two-dimensional matrix whose rows a
#re the terms and columns are the documents, so each entry (i, j) represents the frequency of
#term i in document j.

udemytdm <- TermDocumentMatrix(udemycorpus)
udemytdm

findFreqTerms(udemytdm, lowfreq = 11)
?findFreqTerms

findAssocs(udemytdm, 'udemy', 0.40)


#dendogram
#udemy2tdm <-removeSparseTerms(udemytdm, sparse=0.9)
#udemy2tdmscale <- scale(udemy2tdm)
#udemydist <- dist(udemy2tdmscale, method = "euclidean")
#udemyfit <- hclust(udemydist)
#plot(udemyfit)
#cutree(udemyfit, k=6)
#rect.hclust(udemyfit, k=6, border="red")


#Sentimental analysis
pos = readLines("/Users/jaynilvora/Documents/Positive-Words.txt")
neg = readLines(file.choose())

sentences = c("I am not happy sad", "you are bad", "awesome experience")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    # sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


testsentiment = score.sentiment(sentences, pos, neg)
class(testsentiment)

testsentiment$score


# sentiment analysis of tweets
usatweets = searchTwitter("#usa", n=900, lang = "en")
indiatweets = searchTwitter("#india", n=900, lang = "en")
russiatweets = searchTwitter("#russia", n=900, lang = "en")
irantweets = searchTwitter("#iran", n=900, lang = "en")

usalist <- sapply(usatweets, function(x) x$getText())
indialist <- sapply(indiatweets, function(x) x$getText())
russialist <- sapply(russiatweets, function(x) x$getText())
iranlist <- sapply(irantweets, function(x) x$getText())

nd = c(length(usalist), length(indialist), length(russialist), length(iranlist))
country = c(usalist,indialist,russialist,iranlist)


scores = score.sentiment(country,pos,neg,.progress = 'text')
scores$country = factor(rep(c("USA", "India", "Russia", "Iran"), nd))
scores$very.pos = as.numeric(scores$score >= 2)
scores$very.neg = as.numeric(scores$score <= -2)

numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)

global_score = round(100*numpos / (numpos + numneg))
head(scores)

boxplot(score~country, data=scores)

library("lattice")
histogram(data=scores, ~score|country, main="Sentiment Analysis of 4 Countries", xlab="", sub="Sentiment Score")


















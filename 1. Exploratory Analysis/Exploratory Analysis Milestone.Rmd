# Exploratory Analysis Milestone
##### Candace Ng  
##### August 17, 2020  
```{r include=FALSE}
knitr::opts_chunk$set(warning=FALSE, message=FALSE, fig.width = 16, fig.align="center")
```
   
### Summary  
This report explains the preliminary steps taken towards exploratory analysis. Goals for the eventual app/algorithm will also be outlined. 
  
  
### Understanding the Data
We will examine the three en_US text files that contain data scraped from various blogs, news articles, and tweets. First, we read in the text files and examine how many lines, characters, and words are in each text file. It is also useful to analyze file sizes to ensure it is within CPU constraints. 
```{r echo=FALSE}
blogs <- readLines("./en_US/en_US.blogs.txt", encoding="UTF-8", skipNul=TRUE)
news <- readLines("./en_US/en_US.news.txt", encoding="UTF-8", skipNul=TRUE)
twitter <- readLines("./en_US/en_US.twitter.txt", encoding="UTF-8", skipNul=TRUE)

size_blogs <- file.info("./en_US/en_US.blogs.txt")$size/(1024^2)
size_news <- file.info("./en_US/en_US.news.txt")$size/(1024^2)
size_twitter <- file.info("./en_US/en_US.twitter.txt")$size/(1024^2)

len_blogs<-length(blogs)
len_news<-length(news)
len_twitter<-length(twitter)

nchar_blogs<-sum(nchar(blogs))
nchar_news<-sum(nchar(news))
nchar_twitter<-sum(nchar(twitter))

library(stringi)
nword_blogs<-stri_stats_latex(blogs)[4]
nword_news<-stri_stats_latex(news)[4]
nword_twitter<-stri_stats_latex(twitter)[4]
table<-data.frame("File Name"=c("Blogs","News","Twitter"),
                  "File Size(MB)"=c(size_blogs,size_news,size_twitter),
                  "Num of rows"=c(len_blogs,len_news,len_twitter),
                  "Num of character"=c(nchar_blogs,nchar_news,nchar_twitter),
                  "Num of words"=c(nword_blogs,nword_news,nword_twitter))
table
```
The blogs text file takes up the most space in memory, and has the most characters and words. 

### Cleaning the Data
First we convert the text files from UTF-8 encoding to ASCII for faster performance given our text files are very large. 
```{r}
set.seed(1023)

blogs1<-iconv(blogs,"UTF-8","ASCII",sub="")
news1<-iconv(news,"UTF-8","ASCII",sub="")
twitter1<-iconv(twitter,"UTF-8","ASCII",sub="")

sample_data<-c(sample(blogs1,length(blogs1)*0.01),
               sample(news1,length(news1)*0.01),
               sample(twitter1,length(twitter1)*0.01))
```
Now we construct a volatile corpus from the sample data. We use the text mining package tm to convert our corpora to lowercase, and to remove punctuation and stop words.
```{r}
library(tm)
library(NLP)

corpus<-VCorpus(VectorSource(sample_data))
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,PlainTextDocument)
corpus<-tm_map(corpus,removeWords,stopwords("english"))

corpusdf<-data.frame(text=unlist(sapply(corpus,'[',"content")),stringsAsFactors = FALSE)
```

### Visualizing the Data
We construct a bar chart of the most frequent words observed in our text corpus. Given that the corpus is quite large, we may need to remove sparse terms so that more memory is available. 
```{r fig.width = 10}
library(ggplot2)
library(gridExtra)

tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, 0.999)
freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
wordFreq <- data.frame(word = names(freq), freq = freq)

g <- ggplot (wordFreq[1:10,], aes(x = reorder(wordFreq[1:10,]$word, -wordFreq[1:10,]$fre), y = wordFreq[1:10,]$fre))
g <- g + geom_bar( stat = "Identity" , fill = I("salmon"))
g <- g + geom_text(aes(label = wordFreq[1:10,]$fre), vjust = -0.20, size = 3)
g <- g + xlab("")
g <- g + ylab("Frequency")
g <- g + ggtitle("Most Frequent Words")

print(g)
```
  
A word cloud can also be constructed using the wordcloud library. 
```{r}
library(wordcloud)
library(RColorBrewer)

wordcloud(words = wordFreq$word,
              freq = wordFreq$freq,
              min.freq = 1,
              max.words = 100,
              random.order = FALSE,
              rot.per = 0.35, 
              colors=brewer.pal(8, "GnBu"))
```

### Building N-grams
We will use the library RWeka to analyze the most frequent unigrams, bigrams, and trigrams that occur in our corpus. 
```{r}
library(RWeka)

unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = unigramTokenizer))
unigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(unigramMatrix, 0.999))), decreasing = TRUE)
unigramMatrixFreq <- data.frame(word = names(unigramMatrixFreq), freq = unigramMatrixFreq)

g1 <- ggplot(unigramMatrixFreq[1:20,], aes(x = reorder(word, -freq), y = freq))
g1 <- g1 + geom_bar(stat = "identity", fill = I("lightblue"))
g1 <- g1 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g1 <- g1 + xlab("")
g1 <- g1 + ylab("Frequency")
g1 <- g1 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g1 <- g1 + ggtitle("20 Most Common Unigrams")

bigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))
bigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(bigramMatrix, 0.999))), decreasing = TRUE)
bigramMatrixFreq <- data.frame(word = names(bigramMatrixFreq), freq = bigramMatrixFreq)

g2 <- ggplot(bigramMatrixFreq[1:20,], aes(x = reorder(word, -freq), y = freq))
g2 <- g2 + geom_bar(stat = "identity", fill = I("salmon"))
g2 <- g2 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g2 <- g2 + xlab("")
g2 <- g2 + ylab("Frequency")
g2 <- g2 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g2 <- g2 + ggtitle("20 Most Common Bigrams")

trigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = trigramTokenizer))
trigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(trigramMatrix, 0.9999))), decreasing = TRUE)
trigramMatrixFreq <- data.frame(word = names(trigramMatrixFreq), freq = trigramMatrixFreq)

g3 <- ggplot(trigramMatrixFreq[1:20,], aes(x = reorder(word, -freq), y = freq))
g3 <- g3 + geom_bar(stat = "identity", fill = I("seagreen2"))
g3 <- g3 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
g3 <- g3 + xlab("")
g3 <- g3 + ylab("Frequency")
g3 <- g3 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
               axis.text.x = element_text(hjust = 1.0, angle = 45),
               axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
g3 <- g3 + ggtitle("20 Most Common Trigrams")

grid.arrange(g1, g2, g3, ncol=3)
```

### Next Steps
After cleaning the data and analyzing some basic information about the corpus we constructed, the next step is to build a web application that uses our N-gram model to predict the next words in a sentence. 
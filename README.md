# Johns Hopkins Data Science Capstone

## Background

Around the world, people are spending an increasing amount of time on their mobile devices for email, social networking, banking and a whole range of other activities. SwiftKey builds a smart keyboard that makes it easier for people to type on their mobile devices. One cornerstone of their smart keyboard is predictive text models. When someone types:

"I went to the "

the keyboard presents three options for what the next word might be. For example, the three words might be gym, store, restaurant. The focus of this project is to understand and build a predictive text model like those used by SwiftKey.

This project analyzes a large corpus of text documents to discover the structure in the data and how words are put together. It will cover two main topics:

1. Cleaning and analyzing text data
2. Building and sampling from a predictive text model

### 1. Exploratory Analysis

3 text files containing collections of blogs, news, and tweets were read in and cleaned to create a corpus of text, which was used to construct N-gram models. The focus was on the frequency of words, unigrams, bigrams, and trigrams. A [word cloud](https://github.com/candaceng/data-science-capstone/blob/master/2.%20Web%20Application/wordcloud.png) and several [bar charts](https://github.com/candaceng/data-science-capstone/blob/master/2.%20Web%20Application/ngram-barcharts.png) were constructed in R to aid in visualizing this process. A detailed walkthrough of the data cleaning and exploratory analysis can be found [here](https://rpubs.com/candaceng/exploratory-analysis). 

### 2. Web Application

The [web application](https://candaceng.shinyapps.io/Text_Prediction_Using_N-grams/) was built using Shiny. The user interface presents a predictive model that makes the best guess of what the next word of an incomplete sentence will be. Users can also toggle between bar charts of the most frequently occurring N-grams. We currently support unigrams, bigrams, and trigrams. 

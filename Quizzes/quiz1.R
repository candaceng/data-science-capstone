#' Question 1 
#' get the size of blogs.txt file in MB
size <- file.info("./en_US/en_US.blogs.txt")
mb <- size$size/(1024^2)

#' Question 2
#' get number of lines in twitter.txt
twitter <- readLines(con <- file("./en_US/en_US.twitter.txt"), encoding="UTF-8", skipNul=TRUE)
length(twitter)

#' Question 3
#' get length of longest line in all text files
blogs <- file("./en_US/en_US.blogs.txt", "r")
blogs_lines <- readLines(blogs)
close(blogs)
summary(nchar(blogs_lines))
news <- file("./en_US/en_US.news.txt", "r")
news_lines <- readLines(news)
close(news)
summary(nchar(news_lines))
twitter <- file("./en_US/en_US.twitter.txt", "r")
twitter_lines <- readLines(twitter)
close(twitter)
summary(nchar(twitter_lines))

#' Question 4
#' number of lines with 'love' divided by lines with'hate'
love <- length(grep("love", twitter_lines))
hate <- length(grep("hate", twitter_lines))
love / hate

#' Question 5 
#' line that matches 'biostats'
grep("biostats", lines, value=T)

#' Question 6
#' find number of occurences of specific line of text
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter_lines))
twitter <- readLines("data/en_US/en_US.twitter.txt") # vector of lines
blogs <- readLines("data/en_US/en_US.blogs.txt")
news <-  readLines("data/en_US/en_US.news.txt")

# The en_US.twitter.txt lines of text?
length(twitter) # 2360148

# length of the longest line seen in any of the three en_US data sets?
nchar(twitter[which.max(lapply(twitter, nchar))])
nchar(blogs[which.max(lapply(blogs, nchar))])
nchar(news[which.max(lapply(news, nchar))])

# love/hate in twitter
length(which(lapply(twitter, grep, pattern="love") == 1)) / length(which(lapply(twitter, grep, pattern="hate") == 1)) # 4.1

# The tweet in the en_US twitter data set that matches the word "biostats"
twitter[which(lapply(twitter, grep, pattern="taiwan") == 1)] 

# Trying out Regex, tweets have the exact characters "". 
length(which(lapply(twitter, grep, pattern="^BLAH BLAH BLAH$") == 1)) # 3
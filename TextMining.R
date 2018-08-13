

myScriptsDir <- "D:/DataScienceCapstoneR/Scripts"
myRawDataDir <- "D:/DataScienceCapstoneR/RawData"
myRefDataDir <- "D:/DataScienceCapstoneR/RefData"
mySampleDataDir <- "D:/DataScienceCapstoneR/SampleData"
myGenDataDir <- "D://DataScienceCapstoneR/GenData"

setwd(mySampleDataDir)

#5. Creating a Document Corpus to explore the data further
#===========================================================

#  Here, using the Text-mining package(tm) of R, a document corpus is created and several cleansing functions are applied to clean up the data, like striping whitespaces, removing Punctuations,Numbers, stop-words and Profanity words.  Stemming has not been done, considering the possible effect this might have on word-prediction accuracy in the context of blogs & tweets if not news.
#Removing profane words using the two profane word collections jointly,with the lapply function didn't work properly and failed to remove many listed profane words. Hence, this was done separately for the two profane word collections.
#```{R createDocCorpus, echo=F}
library(tm)
library(quanteda)
library(data.table)

myNewCorpus <- Corpus(DirSource(getwd(),pattern = "*Sample.txt"))
# Cleaning the corpus for 1.Whitespaces 2. Punctuation 3. Numbers 4. Stopwords 5. Profanity words etc.
myNewCorpus <- tm_map(myNewCorpus,stripWhitespace)
myNewCorpus <- tm_map(myNewCorpus,removePunctuation)
myNewCorpus <- tm_map(myNewCorpus,removeNumbers)
myNewCorpus <- tm_map(myNewCorpus,tolower)
myNewCorpus <- tm_map(myNewCorpus,removeWords,stopwords("english"))

# Download RefData
swearWordURL <- "http://www.bannedwordlist.com/lists/swearWords.txt"
badWordURL <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"

#```
#```{R downloadProfanityWords, echo=FALSE}

setwd(myRefDataDir)
if(!file.exists("swearWords.txt")){
  download.file(swearWordURL,"swearWords.txt")
}

if(!file.exists("badWords.txt")){
  download.file(badWordURL,"badWords.txt")
}

swearWords.df <- data.frame(word=readLines("swearWords.txt"))
badWords.df <- data.frame(word=readLines("badWords.txt"))
#```

#```{R removingProfanityWords, echo=TRUE}
# Removing profane words separately using the two profane lists, as doing this in one shot using lapply function of plyr package didn't work properly and failed to remove many listed profane words.

swearWords.df <- swearWords.df[!duplicated(swearWords.df),]
badWords.df <- badWords.df[!duplicated(badWords.df),]

myNewCorpus <- tm_map(myNewCorpus,removeWords,swearWords.df)
myNewCorpus <- tm_map(myNewCorpus,removeWords,badWords.df)

rm(swearWordURL,
   badWordURL,
   swearWords.df,
   badWords.df)
gc()

#myNewCorpus_bkup <- myNewCorpus
# Will not stem the document to improve the accuracy of predictions

#Stemming the corpus
#library(SnowballC)
#myNewCorpus <- tm_map(myNewCorpus,stemDocument)

#```

# 5b. Additional cleansing

removeNumeric <- 
  content_transformer(function(x) 
    gsub('[0-9]+', '', x)
  )

removeNonASCII <- 
  content_transformer(function(x) 
    iconv(x, "latin1", "ASCII", sub="")
  )

modifyURL1 <- 
  content_transformer(function(x) 
    gsub("www",replacement = "www.",x)
  )
modifyURL2 <- 
  content_transformer(function(x) 
    gsub("com$",replacement = ".com",x)
  )

# Taking a Corpus backup here, to prevent re-running of code from the beginning when there is no need
# myNewCorpus <- myNewCorpus_bkup

#myNewCorpus <- tm_map(myNewCorpus, replIsolChar,charSet3)
#myNewCorpus <- tm_map(myNewCorpus, replNonAlphaNum)
myNewCorpus <- tm_map(myNewCorpus, removeNumeric)
myNewCorpus <- tm_map(myNewCorpus, removeNonASCII)
myNewCorpus <- tm_map(myNewCorpus, modifyURL1)
myNewCorpus <- tm_map(myNewCorpus, modifyURL2)

rm(removeNumeric,
   removeNonASCII,
   modifyURL1,
   modifyURL2
)
gc()

##  Data Exploration

#6. Creating Term Document Matrix
#===========================================================

#  The TDM matrix provides quick insights into the variety and frequency of words that are used in the documents.  

#```{R TermDocumentMatrix, echo=T}
mytdm <- TermDocumentMatrix(myNewCorpus)
mytdm_matr <- as.matrix(mytdm)
mytdm_sorted_matr <- sort(rowSums(mytdm_matr),decreasing=TRUE)
mytdm_df <- data.frame(word=names(mytdm_sorted_matr),freq=mytdm_sorted_matr)

head(mytdm_df)
#mytdm_bkup <- mytdm
#```

#7. Creating Wordclouds and histograms to understand the frequencies of words
#================================================================
#  A Word Cloud is a nice depiction of words of most occurance, in relation to other words in the Corpus.
#```{r WordCloud, echo=TRUE}
library(wordcloud)
library(RColorBrewer)
set.seed(07262018)

#dev.set(which = 3)  #set graphic device to 3(png)
#library(jpeg)
#jpeg(filename = "wordCloud.jpg",width = 480,height = 480)
myNewWordCloud <- wordcloud(words = mytdm_df$word,
                            freq = mytdm_df$freq,
                            min.freq = 100,
                            max.words = 25,
                            random.order = FALSE,
                            rot.per = 0.35,
                            colors = brewer.pal(8,"Dark2"))
#dev.off()

wordsLimit <- c(0,100) # this range has been set after a few iterations to determine which one is most useful
freqLimit <- c(0,1000) # this range has been set after a few iterations to determine which one is most useful

#jpeg(filename = "barPlot.jpg",width = 480,height = 480)
#dev.set(which = 3)  #set graphic device to 3(png)
myBarPlot <- barplot(height = mytdm_df$freq,width = 8,main = "Word Freq Chart",xlab = "Words", ylab = "Freq",xlim = wordsLimit, ylim = freqLimit,names.arg = mytdm_df$words,xpd = F, col = brewer.pal(3,"Dark2"))
#dev.off()

#jpeg("wordCloud.jpg")
#jpeg("barPlot.jpg")
#myBarPlot
#```
setwd(myGenDataDir)
rm(list=setdiff(ls(), "myNewCorpus"))
saveRDS(myNewCorpus,"./myNewCorpus.RData")
#readRDS()
#load.RData(paste0(myGenDataDir,"/myNewCorpus.RData"))

library(xml2)

library(tmcn)
source('pttTestFunction.R')

id = c(1:10)
URL = paste0("https://www.ptt.cc/bbs/NTUcourse/index", id, ".html")
filename = paste0(id, ".txt")
pttTestFunction(URL[1], filename[1])
mapply(pttTestFunction, 
       URL = URL, filename = filename)
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines)
docs <- Corpus(VectorSource(files))
#ç§»é™¤?¯?ƒ½??‰å?é?Œç?„ç¬¦???
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "?€?")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "?€?")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "???")
docs <- tm_map(docs, toSpace, "?˜¯")
docs <- tm_map(docs, toSpace, "??‹æ¿")
docs <- tm_map(docs, toSpace, "ä½œè€?")
docs <- tm_map(docs, toSpace, "?™¼ä¿¡ç??")
docs <- tm_map(docs, toSpace, "?‰¹è¸¢è¸¢å¯¦æ¥­???")
docs <- tm_map(docs, toSpace, "äº?")
docs <- tm_map(docs, toSpace, "?Ž¨")
docs <- tm_map(docs, toSpace, "?¯ä»?")
docs <- tm_map(docs, toSpace, "?œ¨")
docs <- tm_map(docs, toSpace, "ä¹?")
docs <- tm_map(docs, toSpace, "??€ä»?")
docs <- tm_map(docs, toSpace, "ä¹?")
docs <- tm_map(docs, toSpace, "?•¦")
docs <- tm_map(docs, toSpace, "?‘¢")
#ç§»é™¤æ¨™é?žç¬¦??? (punctuation)
#ç§»é™¤?•¸å­? (digits)?€ç©º?™½ (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
library(knitr)
kable(head(freqFrame), format = "markdown")

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=5,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)

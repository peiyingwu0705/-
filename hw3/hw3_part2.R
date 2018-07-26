
#TFIDF
##資料整理
##
library(tm)
library(tmcn)
library(Matrix)
library(wordcloud)
library(ggplot2)
library(varhandle)
library(scales)
library(dygraphs)
library(xts)
library(factoextra)


docs <- readLines("./DATA/01.txt")
docs <- readLines("./DATA/02.txt")
docs <- readLines("./DATA/03.txt")
docs <- readLines("./DATA/04.txt")
docs <- readLines("./DATA/05.txt")
docs <- readLines("./DATA/06.txt")

docs <- gsub("\\[[0-9]+\\]", "", docs)
##將txt檔匯入成corpus檔
docs.corpus <- Corpus(VectorSource(docs))
##將corpus檔斷詞
docs.seg <- tm_map(docs.corpus, segmentCN)
##斷詞後轉成tdm檔
docs.tdm <- TermDocumentMatrix(docs.seg, control = list())
inspect(docs.tdm)

#TFIDF Counting
##計算每個詞的term frequency
docs.tf <- apply(as.matrix(docs.tdm), 2, function(doc) {doc / sum(doc)})
##定義計算idf的function
idf.function <- function(word_doc) { log2( (length(word_doc)+1) / nnzero(word_doc) ) }
##計算idf
docs.idf <- apply(docs.tdm, 1, idf.function)
##tfidf = tf*idf
docs.tfidf <- docs.tf * docs.idf
head(docs.tfidf)

#Wordcloud

f <- sort(rowSums(docs.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f )
wordcloud(docs.df$word, docs.df$freq, scale=c(15,0.1),min.freq=20,max.words=50, colors=brewer.pal(10, "Paired"))


ggplot(data = docs.df,aes(x = word, y = freq , colour = word)) +
  geom_point() +
  xlab("word") +  ylab ("freq")

#PCA
docs.pca <- prcomp(docs.tfidf, scale = T)
fviz_eig(docs.pca)
fviz_pca_ind(docs.pca, geom.ind = c("point"), col.ind = "cos2")
fviz_pca_var(docs.pca, col.var = "contrib")
fviz_pca_biplot(docs.pca, geom.ind = "point")
#PCA results
docs.eig <- get_eig(docs.pca)
docs.var <- get_pca_var(docs.pca)
docs.ind <- get_pca_ind(docs.pca)
#K-means
#Choosing K
ind.coord2 <- docs.ind$coord[, 1:2]
wss <- c()
for (i in 1:10) { wss[i] <- kmeans(ind.coord2, i)$tot.withinss }
plot(wss, type = "b")
#Clustering
km <- kmeans(ind.coord2, 3)
plot(ind.coord2, col = km$cluster)
points(km$centers, col = 1:3, pch = 8, cex = 2)





####做不出來QQ
#Query of WordsC
query.tfidf <- function(q){
  q.position <- which(rownames(docs.tfidf) %in% q)
  q.tfidf <- docs.tfidf[q.position, ]
  return (q.tfidf)
}
query.tfidf(c("寶玉", "黛玉", "劉姥姥", "寶釵"))

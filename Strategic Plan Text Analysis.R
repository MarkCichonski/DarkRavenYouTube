#Setting Up
library(pdftools)
library(dplyr)
library(purrr)
library(tm)
library(wordcloud)
library(RColorBrewer)
directory<-"C:/Users/Mark Cichonski/Desktop/Cleanup/To Read/TA"
pdfs<-paste(directory,"/",list.files(directory,pattern = "*.pdf"),sep = "")
pdf_names<-list.files(directory,pattern = "*.pdf")
pdfs_text<-map(pdfs,pdftools::pdf_text)
strategies_df<-data_frame(document=pdf_names,text=pdfs_text)

#Create a data frame of words, line numbers and documents
library(magrittr)
library(tidytext)
library(tidyr)
strategies_words<-strategies_df %>%
  unnest %>%
  group_by(document)%>%
  mutate(linenumber=row_number())%>%
  ungroup()%>%
  unnest_tokens(word, text)%>%
  filter(word != "1")

head(strategies_words)

#Initial Overview
#List the documents and words
strategy_wc<-strategies_df%>%
  unnest %>%
  unnest_tokens(word,text,strip_numeric=TRUE)%>%
  anti_join(stop_words)%>%
  group_by(document,word)%>%
  summarise(count=n())%>%
  arrange(desc(count))

#Count number of lines in each document
strategies_df%>%
  unnest%>%
  unnest_tokens(word,text,strip_numeric=TRUE)%>%
  group_by(document)%>%
  summarise(total_lines=n())

#count unique words
strategies_words%>%
  group_by(document)%>%
  mutate(unique=n_distinct(word))%>%
  count(document,unique)

#analysis of words
#compute tf_idf
strategic_tfidf<-strategies_words %>%
  count(document,word,sort=TRUE)%>%
  bind_tf_idf(word,document,n)%>%
  arrange(desc(tf_idf))

head(strategic_tfidf)

#visualize tf_idf
library(colorspace)
library(ggplot2)
strategic_tfidf %>%
  group_by(document) %>%
  top_n(15,tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales="free") +
  ylab("tf_idf") +
  coord_flip()

#word cloud
#give example here...
library(wordcloud)
corpus = VCorpus(VectorSource(strategies_words))
wordcloud(corpus,min.freq=10, max.words=50,colors=brewer.pal(8, "Set1"))

#Analyzing the co-occurence of words
strategic_bigrams<-strategies_df%>%
  unnest %>%
  unnest_tokens(bigram,text,token="ngrams",n=2)

strategic_bigrams%>%
  count(bigram,sort=TRUE)

#Remove stopwords
bigrams_separated<-strategic_bigrams%>%
  separate(bigram,c("word1","word2"),sep=" ")

bigrams_filtered<-bigrams_separated%>%
  filter(!word1%in%stop_words$word)%>%
  filter(!word2%in%stop_words$word)

bigram_counts<-bigrams_filtered%>%
  count(word1,word2,sort = TRUE)
bigram_counts

bigrams_united<-bigrams_filtered%>%
  unite(bigram,word1,word2,sep = " ")
bigrams_united

bigram_tf_idf<-bigrams_united%>%
  count(document, bigram)%>%
  bind_tf_idf(bigram,document,n)%>%
  arrange(desc(tf_idf))

head(bigram_tf_idf)

#Visualize the bigrams
bigram_tf_idf%>%
  group_by(document)%>%
  top_n(15,tf_idf)%>%
  ungroup()%>%
  mutate(bigram = reorder(bigram,tf_idf))%>%
  ggplot(aes(bigram,tf_idf,fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales="free") +
  ylab("tf-idf") +
  coord_flip()

library(influential)
library(ggraph)
visualize_bigrams<- function(bigrams) {
  set.seed(2016)
  a<-grid::arrow(type="closed",length=unit(.15,"inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout="fr") +
    geom_edge_link(aes(edge_alpha=n),show.legend=FALSE, arrow=a, end_cap=circle(.05,'inches')) +
    geom_node_point(color="lightblue",size = 4) +
    geom_node_text(aes(label=name),vjust=1,hjust=1) +
    theme_void()
}

count_bigrams<- function(dataset) { dataset %>%
    unnest_tokens(bigram, text, token="ngrams", n=2) %>%
    separate(bigram, c("word1","word2"),sep=" ") %>%
    filter(!word1 %in% stop_words$word)%>%
    filter(!word2 %in% stop_words$word)%>% count(word1,word2,sort=TRUE)
    }

bigrams<- strategies_df%>%
  unnest%>%
  count_bigrams()

library(stringr)

bigrams %>% filter(n>20,
                   !str_detect(word1, "\\d"),
                   !str_detect(word2, "\\d")) %>% visualize_bigrams()

#Trigrams!!!!
strategic_tfidf<-strategies_df%>%
  unnest %>%
  unnest_tokens(trigram,text,token="ngrams", n=3)%>%
  separate(trigram,c("word1","word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word) %>%
  count(word1,word2,word3,sort=TRUE)

#Sentiment Analysis
strategic_sentiments<-strategy_wc%>%
  inner_join(get_sentiments("bing"),by=c(word="word"))

strategic_sentiments %>%
  count(sentiment,word,wt=count)%>%
  ungroup()%>%
  filter(n>=10)%>%
  mutate(n=ifelse(sentiment=="negative", -n, n))%>%
  mutate(word=reorder(word,n))%>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  facet_wrap(~ document, scales="free") +
  coord_flip()

#Topic Modelling
#start by converting to standard document-term matrix
library(topicmodels)
strategic_td<-strategy_wc%>%
  cast_dtm(document, word, count)

#set a seed so that the output model is predictable
strategy_lda<-LDA(strategic_td, k=10, control = list(seed = 1234))
strategy_topics<-tidy(strategy_lda,matrix="beta")

#top n terms in topics
top_terms<-strategy_topics %>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic,-beta)
top_terms

#Need the following two functions to make the facetting work properly
reorder_within<-function(x,by,within,fun=mean,sep="___", ...) {
  new_x<-paste(x,within,sep=sep)
  stats::reorder(new_x,by,FUN=fun)
}

scale_x_reordered<-function(..., sep= "___"){
  reg<-paste(sep, ".+$")
  ggplot2::scale_x_discrete(labels=function(x) gsub(reg, "", x), ...)
}

top_terms%>%
  mutate(term=reorder_within(term,beta,topic))%>%
  ggplot(aes(term,beta,fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic,scales="free") +
  coord_flip()

gamma<-tidy(strategy_lda, matrix="gamma")
gamma%>%
  mutate(title=reorder(document,gamma * topic))%>%
  ggplot(aes(factor(topic),gamma)) +
  geom_boxplot()+
  facet_wrap(~ document)

strategy_lda<-LDA(strategic_td,k=5,control = list(seed=1234))
strategy_topics<-tidy(strategy_lda,matrix="beta")
gamma<-tidy(strategy_lda, matrix="gamma")
gamma%>%
  mutate(title=reorder(document,gamma * topic))%>%
  ggplot(aes(factor(topic),gamma)) +
  geom_boxplot()+
  facet_wrap(~ document)

top_terms<-strategy_topics%>%
  group_by(topic)%>%
  top_n(10,beta)%>%
  ungroup()%>%
  arrange(topic, -beta)

top_terms%>%
  mutate(term=reorder_within(term,beta,topic))%>%
  ggplot(aes(term,beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  scale_x_reordered() +
  facet_wrap(~ topic, scales="free") +
  coord_flip()

#clustering the documents
#Hierarchical clustering
m<-as.matrix(strategic_td)
#compute distance between document vectors
d<-dist(m)
#run hierarchical clustering using Ward's method
groups<-hclust(d,method = "ward.D")
#plot use hang to ensure that lables fall below tree
plot(groups,hang=-1)

#k-means clustering
library(cluster)
#compute distance between document vectors
d<-dist(m)
#kmeans clustering
#kmeans - run nstart=100 and k=2,3,5 to compare results with hclust
kfit<-kmeans(d,3,nstart=100)
#plot
clusplot(as.matrix(d),kfit$cluster, color=T, shade=T, labels=2, lines=0)

#Removing punctuation
corpus <- tm_map(corpus, removePunctuation) 
# remove special characters.
for(j in seq(corpus))   
{   
  corpus[[j]] <- gsub("/", " ", corpus[[j]])   
  corpus[[j]] <- gsub("@", " ", corpus[[j]])   
  corpus[[j]] <- gsub("\\|", " ", corpus[[j]])   
}    
#Removing numbers:
corpus <- tm_map(corpus, removeNumbers)  
#Converting to lowercase:
corpus <- tm_map(corpus, content_transformer(tolower)) 
#Removing "stopwords" (common words) that usually have no analytic value
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))
#Removing common word endings (e.g., "ing", "es", "s")
#writeLines(as.character(corpus[[30]]))
library(SnowballC)   
corpus <- tm_map(corpus, stemDocument) 
#treat your preprocessed documents as text documents.
corpus <- tm_map(corpus, PlainTextDocument)
#Stripping unnecesary whitespace from your documents:
corpus <- tm_map(corpus, stripWhitespace)  
wordcloud(corpus, scale=c(3,0.5), min.freq=5, max.words=50, random.order=TRUE,
          rot.per=0.5, colors=brewer.pal(8, "Set1"), use.r.layout=FALSE)







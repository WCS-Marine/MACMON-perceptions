#############################################################################
## 1. Impact of mgmt on local communities well-being in Madagascar         ##
##    1.3 Word cloud                                                      ##
##    2/08/18                                                              ##
##    Caroline BOUSQUET                                                    ##
#############################################################################


# Load packages
install.packages("tm")  # text mining (supprime les "le", "la", "les"...)
install.packages("tmap")
install.packages("SnowballC") # pour le text stemming (r??duit par ex "partant", "partons", "partir" ?? "partir")
install.packages("wordcloud") # word-cloud generator
install.packages("RColorBrewer") # colors
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tmap")

#load datas
mgmt_commu <- read.csv('hh_Madagascar_CB.csv', header=T, sep=";", stringsAsFactors = FALSE)

#benefits
benefCorpus <- Corpus(VectorSource(mgmt_commu$mgmt_community.benefits_eng))
  #remove useless words
benefCorpus <- tm_map(benefCorpus, removePunctuation)
benefCorpus <- tm_map(benefCorpus, removeWords, stopwords('english'))
  #wordcloud
wordcloud(benefCorpus, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
  #original answers (french)
benefCorpus <- Corpus(VectorSource(mgmt_commu$mgmt_community.benefits))
  #remove useless words
benefCorpus <- tm_map(benefCorpus, removePunctuation)
benefCorpus <- tm_map(benefCorpus, removeWords, stopwords('french'))
  #wordcloud
wordcloud(benefCorpus, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#costs
  #english answers
benefCorpus <- Corpus(VectorSource(mgmt_commu$mgmt_community.costs_eng))
benefCorpus
  #remove useless words
benefCorpus <- tm_map(benefCorpus, removePunctuation)
benefCorpus <- tm_map(benefCorpus, removeWords, stopwords('english'))
benefCorpus <- tm_map(benefCorpus, removeWords, c("none", "answer")) 
  #wordcloud
wordcloud(benefCorpus, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Reds"))
  #original answers (french)
benefCorpus <- Corpus(VectorSource(mgmt_commu$mgmt_community.costs))
benefCorpus
  #remove useless words
benefCorpus <- tm_map(benefCorpus, removePunctuation)
benefCorpus <- tm_map(benefCorpus, removeWords, stopwords('french'))
benefCorpus <- tm_map(benefCorpus, removeWords, c("rien", "aucun", "sais", "aucune", "reponse")) 
#wordcloud
wordcloud(benefCorpus, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Reds"))








#* Packages necessari per l'esecuzione del codice
library(textdata)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(tm)
library(readr)
library(rtweet)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(plotrix)
library(plotly)
library(wordcloud)
library(igraph)
library(ggraph)
library(udpipe)
library(factoextra)
library(proxy)

#* Import libreria di supporto
setwd("C:/Users/fabbr/Desktop/Tesi/Codice")
source("utility.r")

#* Creazione palette
myColors <- colorRampPalette(brewer.pal(7, "Accent"))(30)

#* Import dei dati escludendo i tweet non in inglese e quelli non retweetati
original_df <- read_csv("C:\\Users\\fabbr\\Desktop\\Tesi\\Codice\\dataset.csv") %>% filter(language == "en" & retweet == FALSE)

#* Analisi hashtags

# Elabora gli hashtags piu usati
hdf <- cleanTesto(original_df$hashtags, hashtag = TRUE, mention = TRUE, numeri = TRUE, punteggiatura = TRUE, minuscolo = TRUE) %>% iconv(to="UTF-8")
hdf <- tibble(hashtags = hdf)
hashtags <- hdf[!apply(hdf == "", 1, all),] %>% unnest_tokens(word, hashtags) %>% group_by(word) %>% summarise(n = n()) %>% arrange(desc(n))

#bar plot hashtags
hashtags[3:33, ] %>% filter(n >= 10) %>% ggplot(aes(y = reorder(word,n), x= n, fill=factor(n))) +
    geom_col() +
    labs(x = "Number of occurrences", y = NULL) +
    ggtitle("Hashtag Frequency") +
    theme(axis.title.x = element_text(size = 11),
        # axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5, size=15),
        axis.title = element_text(face="bold")) +
    theme(legend.position = "none") +
    scale_fill_manual(values = myColors)

#* Pulizia dei testi
cleaned_df <- cleanTesto(original_df$tweet, hashtag = TRUE, mention = TRUE, numeri = TRUE, punteggiatura = TRUE, minuscolo = TRUE) %>% iconv(to="UTF-8")

# Array con tutti gli errori grammaticali da sistemare, in particolare gli N-grammi da unire in forme compatte
vcorrez <- c("le pen",NA,
            "pro russian",NA,
            "intercontinental ballistic",NA,
            "nuclear capable" ,NA,
            "pro russian", "pro-russian",
            "invasion of ukraine", NA,
            "united states", NA,
            "vladimir putin", "Putin",
            "don t", "don't",
            "didn t", "didn't",
            "doesn t", "doesn't",
            "u s", "U.S.",
            " g ", "g8",
            "i m", "I'm",
            " s ", " ",
            " isn t ", "isn't",
            " can t ", "can't",
            " won t ", "won't",
            " @ ", " ",
            " p ", " ",
            "p_@_ruskadevushka", "ruskadevushka",
            "imo", "in my opinion",
            "un ", "UN ",
            "'s", "",
            "i ", "I ",
            " i ", " I ",
            " don boss", "donbass",
            "they re", "they're",
            "they ve", "they've",
            "they ll", "they'll",
            "they d", "they'd",
            "we re", "we're",
            "we ve", "we've",
            "we ll", "we'll",
            "we d", "we'd",
            "I ve", "I've",
            "I ll", "I'll",
            "I d", "I'd",
            "I m", "I'm",
            "bori", "boris",
            "tboris", "boris",
            "russiawar", "russia war")
# Correggo tutte gli ngrammi che non voglio appaiano separati
correct_df <- corFrmComp(cleaned_df, vcorrez)
# Assegnamo un id ad ogni tweet per ricostruire tutti i tweet a partire dalle singole parole lemmatizzate
correct_df <- tibble(tweet = correct_df) %>% dplyr::mutate(doc_id = row_number())
# Controllo per trovare tutti gli N-grammi e visualizzarne le relative frequenze
visNGram(correct_df$tweet, ngrI = 2, ngrF = 3 ,nn = 150)

#* Lemmatizzazione dei tweet

# Modello utilizzato per andare a lemmatizzare i tweet ed assegnare un ruolo semantico ai singoli token
ud_model_en = udpipe_load_model("./english-ewt-ud-2.5-191206.udpipe")
# Parole che vogliamo andare a rimuovere dal testo non facenti parte delle stopward di default del package tm
my_stop_words <- stop_words
df_lemm <- lemmaUDP(x = correct_df$tweet, model = ud_model_en, userstopw = my_stop_words)
# Posso andare ad isolare le parole per la loro funzione nella frase con %in% c("ADJ","ADV","VERB")
txt_lemm <- df_lemm %>% filter(STOP == FALSE & !is.na(lemma) & upos != "AUX") %>%
            select(doc_id, lemma) %>%
            group_by(doc_id=as.numeric(doc_id)) %>%
            summarise(txtL=paste(lemma,collapse = " "))

# Inserimento frasi lemmatizzate nel data frame iniziale in base all'id
correct_df <- left_join(correct_df,txt_lemm,by="doc_id")

# Vettorizzazione
SLcorpus <- Corpus( VectorSource(correct_df$txtL) )

# Analisi con ponderazione TF
SLtdm <- TermDocumentMatrix(SLcorpus)
mSLtdm <- as.matrix(SLtdm)
dfSLtdm <- data.frame(words=rownames(mSLtdm),mSLtdm,freq=rowSums(mSLtdm))
rownames(dfSLtdm) <- NULL
freqTF <- dfSLtdm %>%
  select(words,freq) %>%
  top_n(n = 30,wt = freq) %>%
  arrange(-freq)

ggplot(freqTF,aes(x = reorder(words,freq),y=freq, fill=factor(freq)))+
    geom_bar(stat="identity") +
    xlab("")+ylab("TF") +
    ggtitle("Words frequency con ponderazione TF") +
    coord_flip() +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    scale_fill_manual(values = myColors) +
    theme(legend.position = "none")

# Analisi con ponderazione TF-IDF
SLtdmIDF <- weightTfIdf(SLtdm)
mSLtdmIDF <- as.matrix(SLtdmIDF)
dfSLtdmIDF <- data.frame(words=rownames(mSLtdmIDF),mSLtdmIDF,freq=rowSums(mSLtdmIDF))
rownames(dfSLtdmIDF) <- NULL
freqIDF <- dfSLtdmIDF %>%
  select(words,freq) %>%
  top_n(30,freq) %>%
  arrange(-freq)

ggplot(freqIDF,aes(x = reorder(words,freq),y=freq, fill=factor(freq)))+
    geom_bar(stat="identity")+
    xlab("")+ylab("TF-IDF")+
    coord_flip()+
    theme_light() +
    scale_fill_manual(values = myColors)  +
    theme(legend.position = "none")

#* Proseguimento con analisi basata su ponderazione TF

#Store the lemmatized corpus in a data frame
correct_texts <- correct_df$txtL

# Split the text into individual tokens, remove stopwords, sort by descending count
tokens <- tibble(text = correct_texts) %>%
    unnest_tokens(word, text) %>%  # creates a single column containing all single words from the text tibble
    anti_join(stop_words) %>%       # remove stopwords, it's a variable from the tidytext package
    count(word, sort = TRUE)

head(tokens, 30) %>% ggplot(aes(x = n, y= reorder(word,n), fill=factor(n))) +
    geom_col() +
    labs(x = "Number of occurrences", y = NULL) +
    ggtitle("Word Frequency") +
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = myColors)

# Proseguo a fare lo stesso ma con i bigrammi
bigrams <- tibble(text = correct_texts) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("first","second"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word"))

bigrams_count <- bigrams %>%
    group_by(bigram) %>%
    count() %>%
    arrange(desc(n))

head(bigrams_count, 30) %>% ggplot(aes(x = n, y= reorder(bigram,n), fill=factor(n))) +
    geom_col() +
    labs(x = "Number of occurrences", y = NULL) +
    ggtitle("Bigrams Frequency") +
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = myColors)

# Analyzing Bigrams into graphs
bigrams_count <- bigrams %>%
    count(first, second, sort = TRUE)

bigram_graph <- bigrams_count %>%
  filter(n > 30) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.14, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
    ggtitle("Bigrams Correlation") +
    theme(axis.title.x = element_text(size = 0),
        plot.title = element_text(hjust = 0.5, size=12)) +
    theme(legend.position = "none")

# Proseguo a fare lo stesso ma con i trigrammi
trigrams <- tibble(text = correct_texts) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, into = c("first","second", "third"), sep = " ", remove = FALSE) %>%
  anti_join(stop_words, by = c("first" = "word")) %>%
  anti_join(stop_words, by = c("second" = "word")) %>%
  anti_join(stop_words, by = c("third" = "word")) %>%
  filter(str_detect(first, "[a-z]") &
         str_detect(second, "[a-z]"))

trigrams_count <- trigrams %>%
    group_by(trigram) %>%
    count() %>%
    arrange(desc(n))

head(trigrams_count, 30) %>% ggplot(aes(x = n, y= reorder(trigram,n), fill=factor(n))) +
    geom_col() +
    labs(x = "Number of occurrences", y = NULL) +
    ggtitle("Trigrams Frequency") +
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(legend.position = "none") +
    scale_fill_manual(values = myColors)

# Analyzing Bigrams into graphs
trigrams_count <- trigrams %>%
    count(first, second, third, sort = TRUE)

trigrams_graph <- trigrams_count %>%
  filter(n > 20) %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(trigrams_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void() +
  ggtitle("Trigrams Correlation") +
    theme(axis.title.x = element_text(size = 0),
        plot.title = element_text(hjust = 0.5, size=12)) +
    theme(legend.position = "none")

#* Dendrogramma
ndtm <- weightTfIdf(SLtdm) %>% removeSparseTerms(sparse = 0.98)
distJ <- dist(as.matrix(ndtm),method = "jaccard")
hhJ <- hclust(d = distJ, method = "ward.D2")

fviz_dend(hhJ, k = 20,
          k_colors = brewer.pal(8,"Set2"),
          type = "phylogenic",
          repel = TRUE,
          phylo_layout = "layout.gem") +  # in alternativa layout.auto, layout_with_lgl , ...
    ggtitle("Dendrogramma lemmi") +
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())

#* Sentiment Analysis

sdf <- tibble(text = correct_df$txtL, id = correct_df$doc_id)

#? Retrieve the total sentiment for each tweet

# Elaborare il punteggio sentiment per ogni tweet tramite il dizionario nrc
tokens_df <- sdf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc"), by="word") %>%
    mutate(score = ifelse(sentiment=='positive',1,ifelse(sentiment=='joy',1,ifelse(sentiment=='anticipation',1,ifelse(sentiment=='trust',1,ifelse(sentiment=='surprise',1,-1)))))) %>%
    group_by(id) %>%
    summarise(total_score = sum(score)) %>%
    mutate(sentiment = ifelse(total_score>0,'positive',ifelse(total_score<0,'negative','neutral')))

# Recuperare il dataframe iniziale associandogli il sentiment relativo joinando per id
sentiments <- sdf %>% inner_join(tokens_df, by='id') %>% select('text','sentiment')

#Costruzione grafico a torta in base al sentiment
percentages <- sentiments %>% group_by(sentiment) %>% summarise(count = n()) %>% arrange(desc(count)) %>% mutate(percentage = round(count/sum(count)*100,2)) %>% mutate(pstring = paste(percentage, "%", sep=""))
pie(percentages$percentage, labels=percentages$pstring, main="Sentiment Analysis", col=c("brown2", "chartreuse2", "antiquewhite2"))
legend("topright", c("negative", "neutral", "positive"), cex = 0.8, fill = c("brown2", "antiquewhite2", "chartreuse2"))

#* Sentiment Score per tweet
tokens_df %>% group_by(total_score) %>% summarise(n = n()) %>% arrange(desc(n)) %>%
    ggplot(aes(x = total_score, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(x = "Sentiment score", y = "Count") +
    ggtitle("Numero di tweet con un certo Sentiment score") + 
    theme(axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(legend.position = "none")

#* Tweet length by sentiment

for (i in 1:length(sentiments$text)) {
    sentiments$length = nchar(sentiments$text)
}

lengths <- sentiments %>% group_by(length) %>% summarise(count = n()) %>% arrange(desc(count))

ggplot(lengths, aes(x=length, y=count, text=length)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +xlab("Numero parole") +
  ggtitle("Distribuzione del numero di parole per tweet") +
  theme(plot.title = element_text(hjust = 0.5))

# Group by tweet length and sentiment
lengths_by_sentiment <- sentiments %>% group_by(sentiment, length) %>% summarise(count = n()) %>% arrange(desc(count))
lengths_by_sentiment$color = ifelse(lengths_by_sentiment$sentiment=='positive',"brown2",ifelse(lengths_by_sentiment$sentiment=='negative',"antiquewhite2","chartreuse2"))

ggplot(lengths_by_sentiment, aes(x=length, y=count, fill=color, text=length)) +
  geom_bar(stat="identity") +
  facet_wrap(~sentiment, ncol = 1) +
  theme_minimal() +xlab("N.parole") +
  ggtitle("Distribuzione del numero di parole per tweet per sentiment") +
  theme(plot.title = element_text(hjust = 0.5)) +
    theme(
        plot.title = element_text(hjust = 0.5, size=15)) +
    theme(
        axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15),
        legend.position="none")


#? Retrieve the most common words for each sentiment

# Conteggio dell'utilizzo delle parole piu comuni per sentimento
sentiment_df <- tibble(text = sdf$text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    group_by(word) %>%
    summarise(count = n()) %>%
    inner_join(get_sentiments("nrc") %>%  filter(sentiment %in% c("positive", "negative"))) %>%
    arrange(desc(count))

# Plottare il tutto con colore diverso in base al sentiment
sentiment_df %>%
    group_by(sentiment) %>%
    slice_max(count, n = 15) %>%
    ungroup() %>%
    mutate(word = reorder(word, count)) %>%
    ggplot(aes(count, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
        y = NULL)

# Quante parole diverse sono usate per ogni sentiment
sum_used_words_by_sentiment <- sentiment_df %>%
    group_by(sentiment) %>%
    summarise(count = n()) %>%
    arrange(desc(count))  %>%
    mutate(percentage = round(count/sum(count)*100,2)) %>%
    mutate(pstring = paste(percentage, "%", sep=""))

#Create a bar chart
ggplot(sum_used_words_by_sentiment, aes(x=sentiment, y=count, text=pstring)) +
  geom_bar(stat="identity", fill=c("brown2", "chartreuse2"))+
  theme_minimal() +xlab("Sentiment") +
  ggtitle("Parole differenti usate per sentimento") +
  theme(plot.title = element_text(hjust = 0.5))

# Somma totale parole usate per tweet in base al sentiment
sum_total_words_spent_by_sentiment <- sentiment_df %>%
    group_by(sentiment) %>%
    summarise(count = sum(count)) %>%
    arrange(desc(count)) %>%
    mutate(percentage = round(count/sum(count)*100,2)) %>%
    mutate(pstring = paste(percentage, "%", sep=""))

ggplot(sum_total_words_spent_by_sentiment, aes(x=sentiment, y=count, text=pstring)) +
  geom_bar(stat="identity", fill=c("brown2", "chartreuse2"))+
  theme_minimal() +xlab("Sentiment") +
  ggtitle("Parole totali usate per sentimento") +
  theme(plot.title = element_text(hjust = 0.5))

#* Emotions

edf <- tibble(text = correct_df$txtL, id = correct_df$doc_id)

#? Elaborare il punteggio sentiment per ogni tweet tramite il dizionario nrc
emotions_df <- edf %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("nrc"), by="word") %>%
    filter(!(sentiment %in% c("positive", "negative"))) %>%
    group_by(id, sentiment) %>%
    summarise(count = n()) %>%
    arrange(id, desc(count)) %>%
    slice_head(n = 1)

# Recuperare il dataframe iniziale associandogli il sentiment relativo joinando per id
emotions <- emotions_df %>% inner_join(edf, by='id') %>% select('text','sentiment')

#Costruzione grafico a torta in base al sentiment
percentages <- emotions %>% group_by(sentiment) %>% summarise(count = n()) %>% arrange(desc(count)) %>% mutate(percentage = round(count/sum(count)*100,2)) %>% mutate(pstring = paste(percentage, "%", sep=""))
pie(percentages$percentage, labels=percentages$pstring, main="Emotions Analysis", col=c("#F8766D", "#CD9600", "#84B30F", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"))
legend("topright", c("anger", "fear", "anticipation", "trust", "sadness", "joy", "disgust", "surprise"), cex = 0.8, fill = c("#F8766D", "#CD9600", "#84B30F", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"))

#* Tweet length by emotion

for (i in 1:length(emotions$text)) {
    emotions$length = nchar(emotions$text)
}

# Group by tweet length and sentiment
lengths_by_emotion <- emotions %>% group_by(sentiment, length) %>% summarise(count = n())

ggplot(lengths_by_emotion, aes(x=length, y=count, color=sentiment, fill=sentiment, text=length)) +
  geom_bar(stat="identity")+
  facet_wrap(~sentiment, ncol = 1)+
  theme_minimal() +
  xlab("Numero parole") + ylab("Numero tweet") +
  ggtitle("Distribuzione del numero di parole per tweet per user") +
  theme(
        axis.title.x = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size=15),
        legend.position="none")


#? Retrieve the most common words for each emotion

# Conteggio dell'utilizzo delle parole piu comuni per sentimento
emotion_df <- tibble(text = edf$text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    group_by(word) %>%
    summarise(count = n()) %>%
    inner_join(get_sentiments("nrc") %>%  filter(!(sentiment %in% c("positive", "negative")))) %>%
    arrange(desc(count))

# Plottare il tutto con colore diverso in base alla emotion
emotion_df %>%
    group_by(sentiment) %>%
    slice_max(count, n = 15) %>%
    ungroup() %>%
    mutate(word = reorder(word, count)) %>%
    ggplot(aes(count, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Contribution to sentiment",
        y = NULL)

# Quante parole diverse sono usate per ogni sentiment
sum_used_words_by_emotion <- emotion_df %>%
    group_by(sentiment) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(percentage = round(count/sum(count)*100,2)) %>%
    mutate(pstring = paste(percentage, "%", sep=""))

sum_used_words_by_emotion$sentiment

#Create a bar chart
ggplot(sum_used_words_by_emotion, aes(x=sentiment, y=count, text=pstring)) +
  geom_bar(stat="identity", fill=c("#00BE67", "#F8766D",  "#FF61CC", "#00A9FF", "#84B30F",  "#CD9600", "#00BFC4", "#C77CFF"))+
  theme_minimal() +xlab("Sentiment") +
  ggtitle("Parole differenti usate per sentimento") +
  theme(plot.title = element_text(hjust = 0.5))

# Somma totale parole usate per ogni sentiment
sum_total_words_spent_by_emotion <- emotion_df %>%
    group_by(sentiment) %>%
    summarise(count = sum(count)) %>%
    arrange(desc(count)) %>%
    mutate(percentage = round(count/sum(count)*100,2)) %>%
    mutate(pstring = paste(percentage, "%", sep=""))

ggplot(sum_total_words_spent_by_emotion, aes(x=sentiment, y=count, text=pstring)) +
  geom_bar(stat="identity", fill=c("#00BE67", "#F8766D",  "#FF61CC", "#00A9FF", "#84B30F",  "#CD9600", "#00BFC4", "#C77CFF"))+
  theme_minimal() +xlab("Sentiment") +
  ggtitle("Parole totali usate per sentimento") +
  theme(plot.title = element_text(hjust = 0.5))

#* Word Clouds

wcdf <- cleanTesto(original_df$tweet, hashtag = TRUE, mention = TRUE, numeri = TRUE, punteggiatura = TRUE, minuscolo = TRUE) %>% iconv(to="UTF-8")

# Split the text into individual tokens, remove stopwords, sort by descending count
tokens <- tibble(text = wcdf) %>%
    unnest_tokens(word, text) %>%  # creates a single column containing all single words from the text tibble
    anti_join(stop_words) %>%       # remove stopwords, it's a variable from the tidytext package
    count(word, sort = TRUE)

#Usare tutte e 3 le combo
tokens <- tail(tokens, -1)
tokens <- tail(tokens, -3)

wordcloud(words = tokens$word, freq = tokens$n,
          min.freq = 1, max.words=250, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
text(0.5,1,"Wordcloud dei token nei tweet in analisi",cex=1,font = 2)

# Conteggio dell'utilizzo delle parole piu comuni per sentimento
sentiment_df <- tibble(text = wcdf) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(tibble(word = "don", lexicon = "SMART")) %>%
    inner_join(get_sentiments("nrc") %>%  filter(sentiment %in% c("positive", "negative"))) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("brown2","darkgreen"),
                    max.words = 200)
text(0.5,1,"Comparison wordcloud dei token per sentiment",cex=1,font = 2)

emotion_df <- tibble(text = wcdf) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    anti_join(tibble(word = "don", lexicon = "SMART")) %>%
    inner_join(get_sentiments("nrc") %>%  filter(!(sentiment %in% c("positive", "negative")))) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#CD9600", "#84B30F", "#00BE67", "#00BFC4",  "#00A9FF", "#FF61CC", "#C77CFF"),
                    max.words = 200)
text(0.5,1,"Comparison wordcloud dei token per emotion",cex=1,font = 2)
library(ngram)

cleanTesto <- function(xtxt = NULL,
                       hashtag = TRUE,
                       mention = TRUE,
                       numeri = FALSE,
                       punteggiatura = FALSE,
                       minuscolo = TRUE){
  # descrizione parametri della funzione:
  # xtxt = vettore di testi (tweet)
  # hashtag = logico, se TRUE rimuove per intero gli hashtag
  # mention = logico, se TRUE rimuove per intero le mention @
  # numeri = logico, se TRUE rimuove tutti i numeri dai messaggi
  # punteggiatura = logico, se TRUE rimuove la punteggiatura
  # minuscolo = logico, se TRUE trasforma in minuscolo tutti i testi
  #
  # controllo se x è definito ed è un oggetto di tipo carattere
  if(is.null(xtxt) | class(xtxt) != "character"){
    message("vettore testi non valido")
    return()
  }
  # rimuove i link
  xtxt = gsub("(f|ht)(tp)(s?)(://)(.\\S+)[.|/](.\\S+)", " ", xtxt)
  # rimuove i riferimenti nei retweet
  xtxt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  xtxt = gsub("(rt|via)((?:\\b\\W*@\\w+)+)", " ", xtxt)
  # punteggiatura
  if(punteggiatura == TRUE){
    xtxt = gsub("([#@])|[[:punct:]]", " \\1", xtxt)
  }
  # caratteri di controllo
  xtxt = gsub('[[:cntrl:]]', ' ', xtxt)
  # quelli che non sono caratteri grafici (quello che non è [[:alnum:][:punct:]])
  xtxt = gsub('[^[:graph:]]', ' ', xtxt)
  # togliere amp
  xtxt = gsub('amp', '', xtxt)
  # togliere le s genitive
  xtxt = gsub(' s ', '', xtxt)
  # hashtag
  if(hashtag == TRUE) xtxt = gsub("#\\S+", " ", xtxt)
  # mention
  if(mention == TRUE) xtxt = gsub("@\\S+", " ", xtxt)
  # numeri
  if(numeri == TRUE) xtxt = gsub("[[:digit:]]", "", xtxt)
  # tabulazioni e più spazi in mezzo al testo
  xtxt = gsub("[ \t]{2,}", " ", xtxt)
  xtxt = gsub('\\s+',' ',xtxt)
  # spazi all'inizio e alla fine
  xtxt = gsub("^\\s+|\\s+$", "", xtxt)
  # trasforma tutto in minuscolo
  if(minuscolo == TRUE) xtxt = tolower(xtxt)
  return(xtxt)
}

visNGram <- function(x = NULL, ngrI = 2, ngrF = 3 ,nn = 20){
  # Funzione per visualizzare n-grammi
  # x = vettore di testi
  # ngrI = valore minimo lunghezza n-grammi (default = 2)
  # ngrF = valore massimo lunghezza n-grammi (default = 3)
  # nn = numero di n-grammi da visualizzare per ciascuna lunghezza
  require(ngram)
  require(dplyr)
  if(is.null(x) | class(x) != "character"){
    message("vettore testi non valido")
    return()
  }
  if(ngrI < 2){
    message("valore iniziale n-grammi non valido")
    return()
  }
  if(ngrF < ngrI){
    message("valori n-grammi non validi")
    return()
  }
  ngr <- seq(from = ngrI, to = ngrF, by = 1)
  vnWrd <- sapply(x,FUN = wordcount)
  ngr <- rev(ngr)
  for(i in 1:length(ngr)){
    minc <- ngr[i]-1
    ng <- ngram(x[vnWrd>minc], n=ngr[i])
    ngt <- get.phrasetable(ng)
    ngt <- ngt %>% arrange(-freq, ngrams)
    ngt$ngrams <- iconv(ngt$ngrams,from="UTF-8",to = "latin1",sub = "byte")
    cat("--------------------","\n")
    cat("n-gram n = ",ngr[i],"\n")
    print(ngt[1:nn,])
  }
}

corFrmComp <- function(vText=NULL, correzioni=NULL){ # from=NULL, to=NULL
  # funzione per correzione forme composte
  # sostanzialmente effettua i gsub di correzione dopo visNGram
  # vText = vettore di testi su cui effettuare la correzione
  # correzioni = vettore di lunghezza pari in cui si susseguono le correzioni da fare
  #              forma.da.correggere.1, forma.corretta.1, forma.da.correggere.2, forma.corretta.2,...
  #              se la forma.corretta richiede semplicemente la sostiuzione degli spazi con _
  #              la forma.corretta può essere indicata con NA
  # esempio: correz <- c("buona salute",NA, "carta di credito","carta_credito","partita iva", NA)
  # tw$txt <- corFrmComp(vText = tw$txt, correzioni = correz)
  if(is.null(vText) | is.null(correzioni)){
    message("mancano vettore testi o vettere forme da correggere")
    return()
  }
  if(length(correzioni) %% 2 != 0){
    message("la lunghezza del vettore correzioni non è corretta")
    return()
  }
  from <- correzioni[seq(from=1,to=length(correzioni),by = 2)]
  to <- correzioni[seq(from=2,to=length(correzioni),by = 2)]
  for(i in 1:length(from)){
    if(to[i]=="" | is.na(to[i])){
      to[i] <- gsub(" ","_",from[i])
    }
    # print(from[i])
    # print(to[i])
    vText <- gsub(from[i],to[i],vText)
  }
  return(vText)
}

corNGram <- function(x = NULL, ngrI = 2, ngrF = 6 ,nn = 20, dict=frmComposteOpSy, verbose = FALSE){
  # Funzione per modificare gli n-grammi in base al dizionario definito dall'utente
  # x = vettore di testi
  # ngrI = valore minimo lunghezza n-grammi (default = 2)
  # ngrF = valore massimo lunghezza n-grammi (default = 3)
  # nn = numero di n-grammi da visualizzare per ciascuna lunghezza
  # dict = dizionario forme composte, di default frmComposteOpSy da OpeNER+mySyntIT
  #        deve essere in formato lemma, ngram, nparole
  #        i dizionari debbono essere caricati con load prima di eseguire la funzione
  # verbose = logico, se TRUE riporta le sostituzioni effettuate
  require(ngram)
  require(dplyr)
  require(stringr)
  dict <- dict
  if(is.null(x) | class(x) != "character"){
    message("vettore testi non valido")
    return()
  }
  if(exists("dict")==FALSE){
    message("dizionario forme composte non trovato")
    return()
  }
  if(ngrI < 2){
    message("valore iniziale n-grammi non valido")
    return()
  }
  if(ngrF < ngrI){
    message("valori n-grammi non validi")
    return()
  }
  ngr <- seq(from = ngrI, to = ngrF, by = 1)
  vnWrd <- numeric()
  for(i in 1:length(x)){
    vnWrd[i] <- wordcount(x[i])
  }
  ngr <- rev(ngr)
  vout <- c()
  dfmod <- data.frame(orig=character(),modifica=character(),stringsAsFactors = F)
  for(i in 1:length(ngr)){
    minc <- ngr[i]-1
    ng <- ngram(x[vnWrd>minc], n=ngr[i])
    ngt <- get.phrasetable(ng)
    ngt <- ngt %>% arrange(-freq, ngrams)
    ngt$ngrams <- iconv(ngt$ngrams,from="UTF-8",to = "latin1",sub = "byte")
    ngt$ngrams <- str_trim(ngt$ngrams)
    ctrl <- merge(ngt,dict,by.x = "ngrams",by.y = "ngram")
    if(nrow(ctrl)>0){
      for(kk in 1:nrow(ctrl)){
        x <- gsub(ctrl[kk,"ngrams"],ctrl[kk,"lemma"],x)
        dfmod[nrow(dfmod)+1,] <- c(ctrl[kk,"ngrams"],ctrl[kk,"lemma"])
      }
    }
  }
  if(verbose == TRUE){
    cat("--------------------","\n")
    cat("modifiche apportate","\n")
    dfmod <- dfmod %>%
      group_by(orig,modifica) %>%
      summarise(n=n())
    print(as.data.frame(dfmod))
  }
  return(x)
}

lemmaUDP <- function(x = NULL,
                     model = NULL,
                     doc_id = NULL,
                     stopw = tm::stopwords("english"),
                     userstopw=NULL){
  # funzione per lemmatizzazione con UDpipe
  # restituisce un data frame in formato CoNLL-U
  # con l'aggiunta del campo STOP che identifica le stopwords
  # x         = vettore di testi/documenti in formato UTF-8
  # model     = modello di lemmatizzazione
  # doc_id    = identificativo del documento
  # stopw     = elenco stopwords della lingua
  # userstopw = elenco stopwords definito dall'utente
  require(udpipe)
  if(is.null(x)){message("manca vettore testi");return()}
  if(is.null(model)){message("manca modello");return()}
  if(class(x) != "character"){message("il vettore x non è di tipo testo");return()}
  if(class(model) != "udpipe_model"){message("modello non valido");return()}
  if(is.null(doc_id)){doc_id <- 1:length(x)}
  if(!is.null(userstopw)){
    stopw <- c(stopw,userstopw)
  }
  xx <- udpipe_annotate(model, x = x, doc_id = doc_id,trace = F)
  xx <- as.data.frame(xx)
  xx$STOP <- ifelse(xx$lemma %in% stopw | xx$token %in% stopw,TRUE,FALSE)
  return(xx)
}
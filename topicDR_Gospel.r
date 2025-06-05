
library(ggraph)
library(gutenbergr)
library(tidytext)
library(tidyr)
library(dplyr)
library(topicmodels)
library(stringr)
library(tidyverse)
library(dplyr)
library(lsa)
library(readr)
library(tibble)
library(data.table)

#==================================
# CONSTANTS
#==================================
newSTOP<-c("thee","thou","thy","ye", "doth", "hath")
kEstablished<-TRUE

#==================================
# FUNCTIONS
#==================================

#=============================================================
#1.) CONVERT SOURCE TEXT TO TIDY
#convert list of books just read into a structure used in program
#=============================================================
convert_tidy<-function(nt_books) {
  #group df by gutenberg-id and add chpater, linenumber
  tidy_bks <- nt_books %>%
    group_by(gutenberg_id) %>%
    mutate(
      linenumber = row_number(),
      chapter = cumsum(str_detect(text, 
                                regex("^.*chapter [\\divxlc]", 
                                      ignore_case = TRUE))))  
  #after ch and line, get ungroups version of that
  tidy_books<- tidy_bks %>% ungroup()

  return(tidy_books)
}

#=============================================================
#2.) TIDY_UP
#row by row format
#=============================================================
tidy_up<-function(fn_bks){
   gmr_word <- fn_bks %>%
     unnest_tokens(word, text)

   sw<-stop_words
   for (i in 1:length(newSTOP)){
       sw<-sw %>% add_row(word=newSTOP[i], lexicon = "custom")
   }

   #this below is only for relationshiop word anslysis
   #stop_words remove importsant relationship words
    #buit not usding relationship word analysios
   #sw<-newSTOP
   #==============================================
   #    remove above after coding to 
   #    take rel words out of stop_words
   #==============================================
   
   gmr_word_counts <- gmr_word %>%
     #anti_join(stop_words) %>%
     anti_join(sw) %>%
     dplyr::count(gutenberg_id, word, sort = TRUE) %>%
     ungroup()

   gmr_dtm <- gmr_word_counts %>%
     cast_dtm(gutenberg_id, word, n)

   return(gmr_dtm)
}

#=============================================================
#3.) GET RID OF DR CHPT VERSE numbers
#=============================================================
rid_annotations<-function(book){
  fxNT_Corpus<-book
  for (i in 1:length(book[[2]])){
     fxNT_Corpus[[2]][i]<-gsub('[0-9]+.[0-9]+.','', book[[2]][i])
  }
  book<-fxNT_Corpus
  return(book)  
}

#=============================================================
#4.) GET K
#=============================================================
get_k <- function(in_dtm) {
  #get Perplex - k needs to be atleast 2 by direction of LDA function
  pList <- 99999
  for (K in 2:10){ 
    nt_lda <- LDA(in_dtm, k = K, control = list(seed = 1234))
    gmrPerplex<-perplexity(nt_lda)
    pList <- c(pList, gmrPerplex)
  }
  return(which.min(pList))
}

#=============================================================
#5.) CREATE MODEL
#=============================================================
make_lda <- function(nt_dtm, K) {
  nt_lda <- LDA(nt_dtm, k = K, control = list(seed = 1234))
  return(nt_lda)
}

#=============================================================
#6.) READ DOUAY-RHEIMS GUTENBERG BY ID
#=============================================================
read_dr<-function(ID){
  return(as.data.frame(list(gutenberg_download(c(ID),mirror = "http://mirrors.xmission.com/gutenberg/"))))
}

#==================================
# END OF FUNCTIONS
#==================================

#==================================
#  MAIN 
#==================================

book_names<-c("mt","mr","lk","jn","act","rm","fcr","scr", "ga", "eph", "pp", "co", "fth", "sth","ftm","stm","ti","ph","hb","jam","fpt","spt", "fjn","sjn","tjn","jud","rev")  
Num_Books<-c("8347":"8373")

Num_Books_s<-as.character(Num_Books)

#==================================
#Process DR
#==================================

#read Gutenberg
nt_data <- list()
nt_data <- lapply(Num_Books, read_dr)

#get rid of garbage in Rev text
#In line 312 of text
nt_data[[27]]$text[312]<-"of God. The beginning, that is, the principle, the source,"

#finish clean-up
nt_clean <- lapply(nt_data, rid_annotations)

#Combine into a Corpus
ntCorpus<-rbindlist(nt_clean)

#change to tidy format - linenumber chapter added
tidy_books<-convert_tidy(ntCorpus)

#get corpus as dtm
nt_dtm<-tidy_up(tidy_books)

#get Perplex curve - k needs to be integre of atleast 2 by direction of LDA function
#this takes a while: once K is established, skip this section
if(!kEstablished){
  for (K in 2:100){ 
    nt_lda <- LDA(nt_dtm, k = K, control = list(seed = 1234))
    gmrPerplex<-perplexity(nt_lda)
    print(c(K, gmrPerplex))
  }
}

K<-21

#create topic model
nt_lda <- LDA(nt_dtm, k = K, control = list(seed = 1234))
gmrPerplex<-perplexity(nt_lda)

doc_topics <- posterior(nt_lda)$topics  # rows are books,columns are topics
sorted_topics<-doc_topics[Num_Books_s,]

#create and print Cosine Similarity
doc_similarity <- cosine(t(doc_topics))   
sorted_similarity<- doc_similarity[Num_Books_s,Num_Books_s]

#replace Gutenberg ID with book name
rownames(sorted_similarity)<-book_names
colnames(sorted_similarity)<-book_names

print(sorted_similarity)  # Show similarities

#save cos Matrix
write.csv(sorted_similarity,file="c:/labs/Mat.csv")

#=======================================================
#Analyze the cosine matrix
#=======================================================
MT_LK_sim <- sorted_similarity[1,3]
MT_LK_count <- sum(sorted_similarity > MT_LK_sim)
print(MT_LK_count)

MT_MR_sim <- sorted_similarity[1,2]
MT_MR_count <- sum(sorted_similarity > MT_MR_sim)
print(MT_MR_count)

LK_MR_sim<- sorted_similarity[3,2]
LK_MR_count <- sum(sorted_similarity > LK_MR_sim)
print(LK_MR_count)

#=========================================================
#Dendo for entire NT
#=========================================================
# Convert similarity to distance (1 - cosine similarity)
doc_dist <- as.dist(1 - sorted_similarity)

# Perform hierarchical clustering
hc <- hclust(doc_dist, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "New Testament Clustering Based on Topics", xlab = "New Testament Books")


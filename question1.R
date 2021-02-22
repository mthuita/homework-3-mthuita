library(tidyverse)
library(tidytext)
library(textdata)
library(rvest)
library(gridExtra)

bis20 <- read_file("~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/december_2020_bis.txt") # (read in text) absolute path vs relative path

text_bis20 <- tibble(text = bis20) # turn into a tibble
word_tokens_bis20 <- unnest_tokens(text_bis20, word_tokens, text, token = "words") # splits text into words
sentence_tokens_bis20 <- unnest_tokens(text_bis20, sent_tokens, text, token = "sentences")
ngram_tokens_bis20 <- unnest_tokens(text_bis20, ngram_tokens, text, token = "ngrams", n = 3) # every cluster of words in 3s (trigram)
bigram_tokens_bis20 <- unnest_tokens(text_bis20, bigrams, text, token = "ngrams", n = 2)

# remove stopwords
no_sw_bis20 <- anti_join(word_tokens_bis20, stop_words, by = c("word_tokens" = "word")) # filter out stopwords with anti_join

# summary statistics
summary_bis20 <- count(no_sw_bis20, word_tokens, sort = TRUE)

# sentiment analysis

sentiment_nrc <-   get_sentiments("nrc")   #manually created via crowdsourcing, ten categories, not unique!
sentiment_afinn <- get_sentiments("afinn") #product of one researcher, pos/neg integer values
sentiment_bing <-  get_sentiments("bing")  #built on online reviews, pos/neg only

# loop that left joins the results of 3 get_sentiments with word tokens

for (s in c("nrc", "afinn", "bing")) {
  no_sw_bis20 <- no_sw_bis20 %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

#histogram for nrc
plot_nrc <- ggplot(data = filter(no_sw_bis20, !is.na(nrc))) +# remove nas
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Bank of International Settlements Sentiment (NRC): Dec 2020") +
  theme_minimal()

#histogram for afinn
plot_afinn <- ggplot(data = filter(no_sw_bis20, !is.na(afinn))) +
  geom_histogram(aes(afinn), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "Bank of International Settlements Sentiment (AFINN): Dec 2020")+
  theme_minimal()

ggarrange(plot_nrc, plot_afinn, + rremove("x.text"), 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)


# merge the 2 plots and save as png
png(filename="question1_plot.png")

question1_plot <- grid.arrange(plot_nrc, plot_afinn, ncol = 2, 
             nrow = 1) 

dev.off()

# A csv document in tidy format of sentiment results for the two articles, which
#could be expanded to include more than these two quarterly reports.

write.csv(merged_bis,'sentiment_bis.csv')

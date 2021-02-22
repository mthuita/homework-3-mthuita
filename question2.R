library(tidyverse)
library(tidytext)
library(textdata)
library(rvest)

#######

sentiment_nrc <-   get_sentiments("nrc")
sentiment_afinn <- get_sentiments("afinn")
sentiment_bing <-  get_sentiments("bing") 

# collect December 19 report directly from the web

url19 <- "https://www.bis.org/publ/qtrpdf/r_qt1912a.htm" # (December 2019)
request19 <- read_html(url19)
content19 <- html_node(request19, "#cmsContent")
paragraphs19 <- html_nodes(content19, "p") # search cmsContent for p tags
text_list19 <- html_text(paragraphs19)
text19 <- paste(text_list19, collapse = "") #extracts text

writeLines(text19, "~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text19.txt")

bis19 <- read_file("~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text19.txt")
text_bis19 <- tibble(text = bis19)
word_tokens_bis19 <- unnest_tokens(text_bis19, word_tokens, text, token = "words")
no_sw_bis19 <- anti_join(word_tokens_bis19, stop_words, by = c("word_tokens" = "word"))

# loop that left joins the results of 3 get_sentiments with word tokens

for (s in c("nrc", "afinn", "bing")) {
  no_sw_bis19 <- no_sw_bis19 %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

# collect December 20 report directly from the web

url20 <- "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm" # (December 2020)
request20 <- read_html(url20)
content20 <- html_node(request20, "#cmsContent")
paragraphs20 <- html_nodes(content20, "p") # search cmsContent for p tags
text_list20 <- html_text(paragraphs20)
text20 <- paste(text_list20, collapse = "") #extracts text

writeLines(text20, "~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text20.txt")

bis20 <- read_file("~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text20.txt")
text_bis20 <- tibble(text = bis20)
word_tokens_bis20 <- unnest_tokens(text_bis20, word_tokens, text, token = "words")
no_sw_bis20 <- anti_join(word_tokens_bis20, stop_words, by = c("word_tokens" = "word"))

for (s in c("nrc", "afinn", "bing")) {
  no_sw_bis20 <- no_sw_bis20 %>%
    left_join(get_sentiments(s), by = c("word_tokens" = "word")) %>%
    plyr::rename(replace = c(sentiment = s, value = s), warn_missing = FALSE)
}

#bring all together

no_sw_bis19$period <- "2019"
no_sw_bis20$period <- "2020"
merged_bis <- rbind(no_sw_bis19, no_sw_bis20)

count(merged_bis, bing, period, sort = TRUE)

png(filename="question2_plot.png")

question2_plot <- ggplot(data = remove_missing(merged_bis, vars = c("affin"))) +
  geom_histogram(aes(affin, fill = period), position = "dodge", stat = "count") +
  scale_x_continuous(n.breaks = 7)

dev.off()

print(count(merged_bis, bing, period, sort = TRUE))

# A csv document in tidy format of sentiment results for the two articles, which
#could be expanded to include more than these two quarterly reports.

write.csv(merged_bis,'sentiment_bis.csv')

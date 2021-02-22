library(tidyverse)
library(tidytext)
library(textdata)
library(rvest)

# collect December 19 report directly from the web

url19 <- "https://www.bis.org/publ/qtrpdf/r_qt2012a.htm" # (December 2020)
content19 <- html_node(request, "#article")
paragraphs19 <- html_nodes(content19, "p") # search cmsContent for p tags
text_list19 <- html_text(paragraphs19)
text19 <- paste(text_list19, collapse = "") #extracts text

writeLines(text, "~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text19.txt")

bis19 <- read_file("~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text19.txt")
text_bis19 <- tibble(text = bis19)
word_tokens_bis19 <- unnest_tokens(text_df, word_tokens, text, token = "words")
no_sw_bis19 <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

# collect December 20 report directly from the web

url20 <- "https://www.bis.org/publ/qtrpdf/r_qt1912a.htm" # (December 2019)
content20 <- html_node(request, "#article")
paragraphs20 <- html_nodes(content20, "p") # search cmsContent for p tags
text_list20 <- html_text(paragraphs20)
text20 <- paste(text_list20, collapse = "") #extracts text

writeLines(text, "~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text20.txt")

bis20 <- read_file("~/Documents/Graduate School/Winter 2021/Data and Programming II/homework-3-mthuita/homework-3-mthuita/text20.txt")
text_bis20 <- tibble(text = bis20)
word_tokens_bis20 <- unnest_tokens(text_df, word_tokens, text, token = "words")
no_sw_bis20 <- anti_join(word_tokens_df, stop_words, by = c("word_tokens" = "word"))

#######

#bring all together

no_sw_df19$period <- "2019"
no_sw_df20$period <- "2020"
merged_bis <- rbind(no_sw_bis19, no_sw_bis20)

count(merged_bis, bing, period, sort = TRUE)

ggplot(data = remove_missing(merged_bis, vars = c("afinn"))) +
  geom_histogram(aes(afinn, fill = period), position = "dodge", stat = "count") +
  scale_x_continuous(n.breaks = 7)
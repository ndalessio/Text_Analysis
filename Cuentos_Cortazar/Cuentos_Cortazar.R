library(tidyverse)
library(tidytext)
library(readr)

df_cortazar <- read_csv("df_cortazar2.csv")
View(df_cortazar)

# Tokens

stories_words <- df_cortazar %>% 
    select(text) %>% 
    unnest_tokens(word, text)

head(stories_words)

# Most frequent words:

stories_words %>% 
  count(word, sort = TRUE)

# We need to remove stopwords:

stopwords <- read.csv("https://bitsandbricks.github.io/data/stopwords_es.csv",
                      stringsAsFactors = FALSE, fileEncoding="utf-8")
head(stopwords)

# Deleting stop words from out data set:

stories_words_filtered <- stories_words %>% 
    anti_join(stopwords, by = c("word" = "STOPWORD"))

# Let's check most frequents words now :D ...

words_freq <- stories_words_filtered %>% count(word, sort = TRUE)
head(words_freq)

# Time to plot!

# Bar plot:

bars_cortazar <- stories_words_filtered %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 100) %>% 
    mutate(word = reorder(word, -n)) %>% 
    ggplot(aes (word, n)) +
    geom_col(fill = "mediumpurple") +
    theme(axis.text.x = element_text(angle = 45)) +
    ylab("Frecuencia de uso") +
    xlab("Palabra") +
    ggtitle(label = "Palabras más utilizadas en cuentos scrapeados de Julio Cortázar",
    subtitle = 'Fuente del corpus: "Ciudad Seva"')

bars_cortazar

# Ploting word cloud:
library(wordcloud2)
wordcloud2(data = words_freq)

# Bigrams
bigrams <- df_cortazar %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2)

# We separate bigrams in two, so we can delete stopwords
bigrams_separated <- bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")

# Filtering stopwords
bigrams_filtered <- bigrams_separated %>% 
  anti_join(stopwords, by = c("word1" = "STOPWORD")) %>% 
  anti_join(stopwords, by = c("word2" = "STOPWORD"))

bigrams_filtered


# Joining again:
bigrams_joined <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE) %>% 
  filter(bigram != "NA NA")

# Ploting :) :) 
wordcloud2(bigrams_joined)





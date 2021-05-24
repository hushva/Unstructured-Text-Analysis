# package for reading any kind of document

library(textreadr)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(scales)



# Get first episode -------------------------------------------------------


# reading in the first txt file, which is the subtitle for Batban Begins

text <- read_tsv("begins_eng.txt", col_names = TRUE, trim_ws = TRUE, skip_empty_rows = TRUE)

text <- 
  text %>% filter(str_detect(text, "[a-z]|[A-Z]")) 

n = nrow(text)
vec  <- as.vector(text$text)

begins_df <- tibble(line = 1:n, text = vec)

begins_tidy <- begins_df %>%
  unnest_tokens(word, text)

# remove rows with remaining numbers
begins_tidy = filter(begins_tidy, line != "1711" & line != "1712")


begins_tidy <- begins_tidy %>%
  anti_join(stop_words, by = "word")

# If I would need to remove some words that are not important or are not consistent with their sentiment
# check which words are common with bing since I am going to use the bing dictionary with the word cloud

bing_word_count_a <- begins_tidy %>% 
   inner_join(get_sentiments("bing")) %>% 
   count(word, sentiment, sort=TRUE) %>% 
   ungroup()

# master is a positive word and in this case it signals some subordinate relationship, 
# but in this case it is more of the sign of respect and indication of service to someone. Some might be misleading, e.g. master bedroom
# so after all master might need to be removed, also chill, which is again a character with a telling name - no other occurance for chill

words_to_ignore_begins <- data_frame(word = c("sir", "master"))

begins_tidy <- begins_tidy %>%
   anti_join(words_to_ignore_begins, by = "word")

begins_tidy %>%
  count(word, sort = TRUE)

# There seems to be very few unique words in the dialogues, now that I removed stopwords.

# visualize most common words in the first episode

 begins_tidy %>% 
   count(word, sort = TRUE) %>%
   filter(n > 11) %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

   

# Get second episode ------------------------------------------------------



# reading in the second txt file, which is the subtitle for The Dark Knight
    
text <- read_tsv("dark_knight_eng.txt", col_names = TRUE, trim_ws = TRUE, skip_empty_rows = TRUE)
 
text <- 
    text %>% filter(str_detect(text, "[a-z]|[A-Z]")) 
 
n = nrow(text)
vec  <- as.vector(text$text)
 
dk_df <- tibble(line = 1:n, text = vec)
 
dk_tidy <- dk_df %>%
    unnest_tokens(word, text)

# remove rows with remaining numbers 
dk_tidy = filter(dk_tidy, line != "708" & line != "1527" & line != "1703")
 
 
dk_tidy <- dk_tidy %>%
    anti_join(stop_words, by = "word")

# check with bing:
bing_word_count_b <- dk_tidy %>% 
   inner_join(get_sentiments("bing")) %>% 
   count(word, sentiment, sort=TRUE) %>% 
   ungroup()

bing_word_count
# dent also means a 'punch' or 'beat' but it is the name of a character - Harvey Dent - who turns to bad in the movie.
# Also, despite being the secondary antagonist, he became the final antagonist of the film after Joker's defeat. In this case the name is
# indicative of the character, but otherwise it should have been removed.


# Remove words 'Wanna' and 'yeah' as these rather look like filling words 
words_to_ignore_dk <- data_frame(word = c("wanna", "yeah", "gonna"))

dk_tidy <- dk_tidy %>%
   anti_join(words_to_ignore_dk, by = "word")
 
dk_tidy %>%
    count(word, sort = TRUE)
 
 
 # visualize most common words in the second episode
 
 dk_tidy %>% 
    count(word, sort = TRUE) %>%
    filter(n > 15) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


# Get third episode -------------------------------------------------------


    
# reading in the second txt file, which is the subtitle for The Dark Knight Rises
    
text <- read_tsv("dk_rises_eng.txt", col_names = TRUE, trim_ws = TRUE, skip_empty_rows = TRUE)
 
text <- 
    text %>% filter(str_detect(text, "[a-z]|[A-Z]")) 
 
n = nrow(text)
vec  <- as.vector(text$text)
 
dk_rises_df <- tibble(line = 1:n, text = vec)
 
dk_rises_tidy <- dk_rises_df %>%
    unnest_tokens(word, text)
 
#remove rows with remaining numbers 
dk_rises_tidy = filter(dk_rises_tidy, line != "1299")
# & line != "1527" & line != "1703")
 
 
dk_rises_tidy <- dk_rises_tidy %>%
    anti_join(stop_words, by = "word")

# Check with bing
bing_word_count_c <- dk_rises_tidy %>% 
   inner_join(get_sentiments("bing")) %>% 
   count(word, sentiment, sort=TRUE) %>% 
   ungroup()

# again, Bane - a character - means strike, trouble and the like. Is is negative, but in this case the word should be removed.
# It is interesting what names the writers choose so the anti-hero's name reflects the personality.
 
# Remove words 'Wanna' and 'yeah' as these rather look like filling words 
words_to_ignore_dk_rises <- data_frame(word = c("gonna","uh", "hey"))
 
dk_rises_tidy <- dk_rises_tidy %>%
    anti_join(words_to_ignore_dk_rises, by = "word")
 
dk_rises_tidy %>%
    count(word, sort = TRUE)
 
 
# visualize most common words in the third episode
 
dk_rises_tidy %>% 
    count(word, sort = TRUE) %>%
    filter(n > 11) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


# Word frequencies --------------------------------------------------------

word_frequency <- bind_rows(mutate(begins_tidy, episode = "Batman Begins (2005)"),
                            mutate(dk_tidy, episode = "The Dark Knight (2008)"),
                            mutate(dk_rises_tidy, episode = "The Dark Knight Rises (2012)")) %>%
   # mutate(word = str_extract(word, "[a-z']+")) %>%
   count(episode, word) %>%
   group_by(episode) %>% 
   mutate(proportion = n / sum(n)) %>% 
   select(-n) %>% 
   spread(episode, proportion) %>% 
   gather(episode, proportion, `The Dark Knight (2008)`:`The Dark Knight Rises (2012)`)


# visualizing the proportion of words occuring in Batman Begins to the words in The Dark Knight and to The Dark Knight Rises
# words close to the line have similar frequencies in both texts

ggplot(word_frequency, aes(x = proportion, y =`Batman Begins (2005)`, color = abs(`Batman Begins (2005)` - proportion))) +
   geom_abline(color = "gray40", lty = 2) +
   geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
   geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
   scale_x_log10(labels = label_number())+ 
   scale_y_log10(labels = label_number()) +
   scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
   facet_wrap(~episode, ncol = 2) +
   theme(legend.position="none") +
   labs(y = "Batman Begins (2005)", x = NULL)   
   
# Based on the graphs we see that drugs and blood and falcone have higher frequency in hte first episode than in the second

   

# Correlation -------------------------------------------------------------

# we want to know how correlated the word frequencies are among the episodes.
# we quantify how similar or different these sets of word frequencies are.

# cor.test(data = frequency(word_frequency$episode == "The Dark Knight Rises (2012)",),
         ~ proportion + `Batman Begins (2005)`)
   


# Word clouds -------------------------------------------------------------
# the most common words in the three episodes visually
library(wordcloud) 

begins_tidy %>% 
   anti_join(stop_words) %>% 
   count(word) %>% 
   with(wordcloud(word, n, max.words = 100))
   

dk_tidy %>% 
   anti_join(stop_words) %>% 
   count(word) %>% 
   with(wordcloud(word, n, max.words = 100))


dk_rises_tidy %>% 
   anti_join(stop_words) %>% 
   count(word) %>% 
   with(wordcloud(word, n, max.words = 100))


# Word cloud sorted by sentiments
library(reshape2)
library(textdata)


begins_tidy %>%
   inner_join(get_sentiments("bing")) %>%
   count(word, sentiment, sort = TRUE) %>%
   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
   comparison.cloud(colors = c("gray20", "gray80"),
                    max.words = 150)

dk_tidy %>%
   inner_join(get_sentiments("bing")) %>%
   count(word, sentiment, sort = TRUE) %>%
   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
   comparison.cloud(colors = c("gray20", "gray80"),
                    max.words = 150)

dk_rises_tidy %>%
   inner_join(get_sentiments("bing")) %>%
   count(word, sentiment, sort = TRUE) %>%
   acast(word ~ sentiment, value.var = "n", fill = 0) %>%
   comparison.cloud(colors = c("gray20", "gray80"),
                    max.words = 150)



# Sentiment analysis ------------------------------------------------------

# Afinn gives magnitude of sentiments
# Compare the three episodes with afinn
# what should be considered a meaningful chunk?

begins_tidy_row <- begins_tidy %>%
  mutate(
      linenumber = row_number())

dk_tidy_row <- dk_tidy%>%
   mutate(
      linenumber = row_number())

dk_rises_tidy_row <- dk_rises_tidy %>%
   mutate(
      linenumber = row_number())

afinn_begins <- begins_tidy_row %>% 
   inner_join(get_sentiments("afinn")) %>%  
   group_by(index = linenumber %/% 30) %>% 
   summarize(sentiment = sum(value)) %>% 
   mutate(episode = "Batman Begins")

afinn_dk <- dk_tidy_row %>% 
   inner_join(get_sentiments("afinn")) %>% 
   group_by(index = linenumber %/% 30) %>% 
   summarize(sentiment = sum(value)) %>% 
   mutate(episode = "The Dark Knight")

afinn_dk_rises <- dk_rises_tidy_row %>% 
   inner_join(get_sentiments("afinn")) %>% 
   group_by(index = linenumber %/% 30) %>%  # row_number()
   summarize(sentiment = sum(value)) %>% 
   mutate(episode = "The Dark Knight Rises")



bind_rows(afinn_begins, afinn_dk, afinn_dk_rises) %>% 
   ggplot(aes(index, sentiment, fill = episode)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~episode, ncol = 1, scales = "free_y")


# we have somewhat less unique words in the first episode


# n-grams -----------------------------------------------------------------

bigrams <- bind_rows(mutate(begins_df, episode = "Batman Begins"),
                     mutate(dk_df, episode = "The Dark Knight"),
                     mutate(dk_rises_df, episode = "The Dark Knight Rises")) %>% 
   unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
   filter(!is.na(bigram))
   
bigrams = filter(bigrams, line != "2174" & line != "1122")

bigrams_separated <- bigrams %>% 
   separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
   filter(!word1 %in% stop_words$word) %>% 
   filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
   count(episode, word1, word2, sort = TRUE)


# analyize bigrams

# reflects how important the particular bigram is to a document in a corpus, we might find some context

bigrams_united <- bigrams_filtered %>%
   unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
   count(episode, bigram) %>%
   bind_tf_idf(bigram, episode, n) %>%
   arrange(desc(tf_idf))

bigram_tf_idf %>%
   arrange(desc(tf_idf)) %>%
   group_by(episode) %>%
   slice_max(tf_idf, n = 10) %>%
   ungroup() %>%
   mutate(bigram = reorder(bigram, tf_idf)) %>%
   ggplot(aes(tf_idf, bigram, fill = episode)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ episode, ncol = 2, scales = "free") +
   labs(x = "tf-idf of bigram", y = NULL)









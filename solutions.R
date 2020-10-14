## Exercises


# Chapter 2 - Ex 1, p16
# 1. Using the first_name column in people data set, select names that start with ‘A’ or ‘B’
# 2. Similarly, select names that have exactly four letters?
# 3. Extract only the numeric characters from the string "abcd2ef7gh9ij2klmn98op"
# Extension: 4. Think about how you could apply this to search a text for phone 
#  numbers, emails, or similar coded text? For example, consider: 
# c(“example.email@gmail.com”, “07719 377 390”, “User:123ABC”, “SN14 0GB”)


library(stringr)
library(tidyverse)

people_data <- readRDS("data/people_data.rds")
head(people_data)


people_data %>% 
  filter(str_detect(first_name, pattern = "^A|B"))

# set a longer name to test
people_data[1, 1] <- "Abigail"
people_data %>% 
  filter(str_detect(first_name, pattern = "^.{4}$"))

# reset
people_data[1, 1] <- "Abby"

str_c(str_extract_all("abcd2ef7gh9ij2klmn98op", 
                      pattern = "[0-9]")[[1]],
      collapse = "")

str <- c("example.email@gmail.com", "07719 377 390", "User:123ABC", "SN14 0GB")
str_detect(str, pattern = "^[A-Za-z0-9\\.-_]+@[A-Za-z0-9\\.-_]+")
str_detect(str, pattern = "[0-9]{5} [0-9]{3} [0-9]{2}")
str_detect(str, pattern = "[A-Za-z]+:[0-9]{3}[A-Z]{3}")
str_detect(str, pattern = "[A-Z]{2}[0-9]{1,2} [0-9]{1,2}[A-Z]{2}")


# Chapter 2 - EX 2, p23
# 1. Consider the `star_wars_script` text as a whole and tidy the book into the 
#     tidy one-token-per-row structure
# 2. Find the most common words in the dialogue of the films together
# 3. What are the most common words after removing stop words?
# 4. Stem this text - does this change the order of the most common words within the text?

library(tidytext)
library(SnowballC)

star_wars_script <- readRDS("data/star_wars_scripts.rds")

star_wars <- star_wars_script %>% 
  select(movie, line, character, dialogue)

star_wars %>% 
  unnest_tokens(output = word, input = dialogue) %>% 
  count(word, sort = TRUE) %>% 
  head()

star_wars %>% 
  unnest_tokens(output = word, input = dialogue) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>% 
  head()

star_wars %>% 
  unnest_tokens(output = word, input = dialogue) %>% 
  anti_join(stop_words, by = "word") %>% 
  mutate(word = wordStem(word)) %>% 
  count(word, sort = TRUE) %>% 
  head()



# Chapter 2 - EX 3, p27
# 1. Make a word cloud for your favourite character from all the start wars text
# 2. Considering the separate movies - combine/compare the wordclouds for each one


library(wordcloud)

star_wars_script <- readRDS("data/star_wars_scripts.rds")

star_wars <- star_wars_script %>% 
  select(movie, line, character, dialogue)

head(star_wars)

star_wars_cloud <- star_wars %>% 
  unnest_tokens(output = word, input = dialogue) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, character, sort = TRUE)

star_wars_tarkin <- star_wars_cloud %>%
  filter(character == "TARKIN")

wordcloud(words = star_wars_tarkin$word, 
          freq = star_wars_tarkin$n, 
          max.words = 50)


star_wars_compare <- star_wars %>% 
  unnest_tokens(output = word, input = dialogue) %>%
  anti_join(stop_words, by = "word") %>% 
  count(word, movie, sort = TRUE) %>% 
  pivot_wider(names_from = movie, 
              values_from = n, 
              values_fill = list(n = 0)) %>% 
  data.frame() # tibble row names are deprecated

head(star_wars_compare)

row.names(star_wars_compare) <- star_wars_compare$word

star_wars_compare <- star_wars_compare %>% 
  select(-word)

comparison.cloud(term.matrix = star_wars_compare, 
                 max.words = 100, 
                 colors = c("darkred", "darkgreen", "darkblue"))




# Chapter 2 - EX 3, p31
# 1. Tokenize the whole star wars data into word pairs (bigrams)
# 2. Which words co-occur most frequently?
# 3. Re-create the ggraph for luke and force - and examine the differences
# Extension:
#   4. Re-tokenize the whole star wars data into word triplets (trigrams) - now 
#       which words co-occur most frequently?

library(tidyr)
library(ggraph)
  

star_wars_script <- readRDS("data/star_wars_scripts.rds")

star_wars <- star_wars_script %>% 
  select(movie, line, character, dialogue)

head(star_wars)


star_wars %>% 
  unnest_tokens(word, dialogue, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>% 
  head()


star_wars_ngram <- star_wars %>% 
  unnest_tokens(word, dialogue, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>% 
  separate(word, c("firstWord", "secondWord"), sep = " ") %>% 
  anti_join(stop_words, by = c("firstWord" = "word")) %>% 
  anti_join(stop_words, by = c("secondWord" = "word")) %>% 
  drop_na()

star_wars_ngram_luke <- star_wars_ngram %>% 
  filter(firstWord %in% c("luke", "force") |
           secondWord %in% c("luke", "force"))

library(igraph)


star_wars_ngram_luke_graph <- graph_from_data_frame(star_wars_ngram_luke)

ggraph(star_wars_ngram_luke_graph, layout = "stress") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "coral", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


### Extension
star_wars_trigram <- star_wars %>%
  unnest_tokens(word, dialogue, token = "ngrams", n = 3) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord", "thirdWord"), sep = " ")

# Remove stop words and NA's
star_wars_trigram <- star_wars_trigram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word")) %>%
  drop_na()

head(star_wars_trigram)



# Chapter 3 - EX 1, p39
# 1. What are the most frequently expressed positive words in all of the star wars films?
# 2. Are the star wars scripts mostly positive or negative?
# 3. How does sentiment change throughout the other two star wars movies?
#      hint: compare movies using group by movie and scriptsection when summarising


sentiment <- get_sentiments(lexicon = "bing")

star_wars %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  inner_join(sentiment, by="word") %>% 
  filter(sentiment == "positive") %>%
  head()


star_wars %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words) %>%
  inner_join(sentiment, by="word") %>%
  count(sentiment, sort = TRUE)


# get the data into the correct form, ensuring our line number is numeric
afinn_star_wars <- star_wars  %>%  
  mutate(line = as.numeric(line)) %>% 
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"), by = "word")

head(afinn_star_wars)

# split into sections of 100 lines and summarise
afinn_star_wars <- afinn_star_wars %>% 
  mutate(scriptsection = line %/% 100) %>%
  group_by(scriptsection, movie) %>%
  summarise(score = mean(value))

head(afinn_star_wars)

# plot the sentiment though the book
ggplot(afinn_star_wars , aes(scriptsection,  score, col=movie)) +
  geom_line()  +
  geom_point() +
  labs(title = "Sentiment throughout star wars episode IV",
       y = "Average Afinn Sentiment Score")




# Chapter 4 - EX 1
#
# 1. Split the whole star wars scripts into several sections, similar to above,
#      and then manipulate it into the form required for the TF-IDF analysis
# 2. Find the TF-IDF for the words in star wars scripts, by book section
# 3. Visualise your results
#
# Extension:
#   4. After splitting the data into section by line number, lets also consider the movie for that line, 
# create a new column; `movie_scriptsection` which utilises `unite` to join the data in the two columns. 
# Complete the TF-IDF analysis and visualise your results


star_wars_split <- star_wars %>% 
  mutate(scriptsection = as.numeric(line) %/% 200) %>% 
  unnest_tokens(word, dialogue) %>% 
  count(word, scriptsection, sort = TRUE)

# apply tf-idf
star_wars_split_tfidf <- star_wars_split %>% 
  bind_tf_idf(term = word, document = scriptsection, n = n)

head(star_wars_split_tfidf, 8)

star_wars_split_tfidf %>% 
  group_by(scriptsection) %>% 
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = scriptsection),
             y = tf_idf)) +
  geom_col(show.legend = FALSE, fill = "azure4") +
  facet_wrap(vars(scriptsection), ncol = 2, scales = "free_y") +
  coord_flip() +
  scale_x_reordered("words")





star_wars_split <- star_wars %>% 
  mutate(scriptsection = as.numeric(line) %/% 200) %>% 
  unite("movie_scriptsection", movie, scriptsection) %>% 
  unnest_tokens(word, dialogue) %>% 
  count(word, movie_scriptsection, sort = TRUE)

# apply tf-idf
star_wars_split_tfidf <- star_wars_split %>% 
  bind_tf_idf(term = word, document = movie_scriptsection, n = n)

head(star_wars_split_tfidf, 8)

star_wars_split_tfidf %>% 
  group_by(movie_scriptsection) %>% 
  top_n(6, tf_idf) %>% 
  ggplot(aes(reorder_within(x = word, 
                            by = tf_idf, 
                            within = movie_scriptsection),
             y = tf_idf)) +
  geom_col(show.legend = FALSE, fill = "azure4") +
  facet_wrap(vars(movie_scriptsection), ncol = 3, scales = "free_y") +
  coord_flip() +
  scale_x_reordered("words")


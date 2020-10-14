

library(stringr)

str_c("Mango", "the", "cat")
str_c("Mango", "the", "cat", sep = " ")

str_vec <- c("Mango", "the", "cat")
str_c(str_vec)
str_c(str_vec, collapse = " ")

str_c(str_vec, 1:3, sep = "-", collapse = " ")

str <- "Mango the cat"

str_sub(str, start = 1, end = 5)
str_sub(str, start = -3)

people_data <- read_rds("data/people_data.rds")

people_data

# could remove £
people_data %>%
  mutate(amount = str_remove_all(
    amount, pattern = "£"
  ))

# £ or r
people_data %>%
  mutate(amount = str_remove_all(
    amount, pattern = "£|r"
  ))

# [] - means any of these
# could remove £ or r
people_data %>%
  mutate(amount = str_remove_all(
    amount, pattern = "[£r]"
  ))

people_data %>%
  mutate(amount = str_remove_all(
    amount, pattern = "[^0-9]"
  ))

# This gives us the rows with those incorrect characters in
people_data %>% 
  filter(str_detect(amount, 
                    pattern = "[^0-9]"))

# The rows without can be found using the negate argument;
people_data %>% 
  filter(str_detect(amount, 
                    pattern = "[^0-9]", negate = TRUE))

# Currently the column amount is treated as a character;
# we can convert it;
people_data %>%
  mutate(amount = as.numeric(
    str_remove_all(
      amount, pattern = "[^0-9]")
  ))
# we still get the valid NA for 'null'


# ids 

# can pull out the ID's that end in A
people_data %>%
  filter(str_detect(id, pattern = "A$"))

# can pull out the ID's that start in letters in the first half of the alphabet
people_data %>%
  filter(str_detect(id, pattern = "^[A-M]"))

# can pull out the ID's whose's second character is a vowel
people_data %>% filter(str_detect(id, pattern = "^.[AEIOU]"))

# can pull out the ID's where one of the first two characters is a vowel
people_data %>% filter(str_detect(id, pattern = "^.?[AEIOU]"))

# to create a regex like this - it can help to view it
# we know it ends in A or B
str_view(people_data$id, pattern = "[AB]$")

# we know it starts with two letters;
str_view(people_data$id, pattern = "^[A-Z][A-Z]")

# we can also write this like so;
str_view(people_data$id, pattern = "^[A-Z]{2}")

# we also know there are 3 numbers in the middle;
str_view(people_data$id, pattern = "[0-9]{3}")

# lets put this all together;
str_view(people_data$id, pattern = "^[A-Z]{2}[0-9]{3}[AB]$")

# so we can use this to filter out the bad row of data;
people_data %>%
  filter(str_detect(id, 
                    pattern = "^[A-Z]{2}[0-9]{3}[AB]$"))
# we've lost Dan!

library(tidyr)

people_data %>% 
  separate(email, into = c("username", "domain"), sep = "@")

people_data %>% 
  select(first_name, second_name, id) %>% 
  unite("full_name", first_name, second_name, sep = " ", remove = FALSE)


# tidytext star wars -------------------------------------------------------

star_wars_script <- readRDS("data/star_wars_scripts.rds")

str(star_wars_script)

star_wars_script %>%
  distinct(movie, title)

star_wars_script %>%
  distinct(character) %>%
  head()

episode_iv <- star_wars_script %>% 
  filter(movie == "IV") %>%
  select(line, character, dialogue)
head(episode_iv)

library(tidytext)
library(dplyr)

# convert text into tokenized format
tidy_episode_iv <- episode_iv %>% unnest_tokens(word, dialogue)
head(tidy_episode_iv)

tidy_episode_iv %>% 
  count(word, sort = TRUE) %>% 
  head()

head(stop_words)

tidy_episode_iv  %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  head()

characters <- data.frame(word = c("leia", "darth", "vader", "obi", "wan", "kenobi",
                                  "han", "solo", "luke", "skywalker", "chewbacca",
                                  "r2", "d2", "artoo", "detoo")) 
# they spell R2-D2 as artoo detoo too!

tidy_episode_iv_no_characters <-  tidy_episode_iv %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(characters, by = "word") %>%
  count(word, sort = TRUE)

head(tidy_episode_iv_no_characters)

library(SnowballC)

wordStem(c("fear", "fearing", "fearful", "play", "played", "playing"))

tidy_episode_iv_stemmed <-  tidy_episode_iv %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>% 
  count(word, sort = TRUE)

head(tidy_episode_iv_stemmed)

# wordcloud ----------------------------------------------------------------

library(wordcloud)

cloud_episode_iv <-  episode_iv %>%
  unnest_tokens(word, dialogue)%>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

wordcloud(words = cloud_episode_iv$word,
          freq = cloud_episode_iv$n, 
          max.words = 50, colors ="coral")

# get data into the correct format
comp_cloud_episode_iv <- episode_iv %>%
  filter(character %in% c("LEIA", "VADER")) %>% 
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = wordStem(word)) %>%
  count(word, character, sort = TRUE) %>%
  pivot_wider(names_from = character,
              values_from = n,
              values_fill = list(n = 0)) %>%
  data.frame() # tibble row names are deprecated

# set words to row names
rownames(comp_cloud_episode_iv) <- comp_cloud_episode_iv$word
comp_cloud_episode_iv <- select(comp_cloud_episode_iv, - word)

head(comp_cloud_episode_iv)

# plot comparison cloud
comparison.cloud(comp_cloud_episode_iv, 
                 max.words = 50)


# bigrams -------------------------------------------------------------------

# tokenize into bigrams
tidy_episode_iv_ngram <- episode_iv %>%
  unnest_tokens(word, dialogue, token = "ngrams", n = 2)

tidy_episode_iv_ngram  %>%
  count(word, sort = TRUE) %>%
  head()

# make tidy ngram data
tidy_episode_iv_ngram <- episode_iv %>%
  unnest_tokens(word, dialogue, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord"), sep = " ")

head(tidy_episode_iv_ngram)

# remove stop words - and NA's (when there's only one word in the line)
tidy_episode_iv_ngram <- tidy_episode_iv_ngram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word")) %>%
  drop_na()

head(tidy_episode_iv_ngram)

# ggraph ----------------------------------------------------------------

library(ggraph)
library(igraph)
library(tidyr)

# Make tidy ngram data
tidy_episode_iv_ngram <- episode_iv %>%
  unnest_tokens(word, dialogue, token = "ngrams", n = 2) %>%
  count(word, sort = TRUE) %>%
  separate(word, c("firstWord", "secondWord"), sep = " ")

# Remove stop words and NA's
tidy_episode_iv_ngram <- tidy_episode_iv_ngram %>%
  anti_join(stop_words, by = c("firstWord" = "word")) %>%
  anti_join(stop_words, by = c("secondWord" = "word")) %>%
  drop_na()

# Filter so only common pairs about luke or force remain
tidy_episode_iv_ngram <- tidy_episode_iv_ngram %>%
  filter((firstWord  %in% c("luke", "force")) |
           secondWord %in% c("luke", "force"))

head(tidy_episode_iv_ngram)

# Create the igraph object
igraph_episode_iv <-  graph_from_data_frame(tidy_episode_iv_ngram)       

# Plot the ggraph  
ggraph(igraph_episode_iv, layout = 'stress') +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(color = "coral", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# sentiments ---------------------------------------------------------

library(tidytext)
library(tidyverse)
library(SnowballC)

# import the data - and filter
star_wars_script <- readRDS("data/star_wars_scripts.rds")

episode_iv <- star_wars_script %>% 
  filter(movie == "IV") %>%
  select(line, character, dialogue)

# Make tidy data
tidy_episode_iv <- episode_iv %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

head(tidy_episode_iv)

sentiment <- get_sentiments(lexicon = "bing")

head(sentiment)

tidy_episode_iv %>%
  inner_join(sentiment, by="word") %>%
  head()

tidy_episode_iv %>%
  inner_join(sentiment, by="word") %>%
  group_by(sentiment) %>% 
  summarise(total = sum(n))

library(tidyverse)

# get the data into the correct form
afinn_episode_iv <- episode_iv  %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("afinn"), by = "word")

head(afinn_episode_iv)

afinn_episode_iv_character_summary <- afinn_episode_iv %>% 
  group_by(character) %>%
  summarise(score = mean(value), 
            n = n()) %>% 
  filter(n > 20)

head(afinn_episode_iv_character_summary)

ggplot(afinn_episode_iv_character_summary, 
       aes(x = reorder(character, -score), y = score)) +
  geom_col() +
  labs(title = "Average Sentiment for the Main Characters in A New Hope",
       x = "Character", y = "Average Afinn Sentiment Score")

# sentiment progression -------------------------------------------------

# split into sections of 100 lines and summarise
# make sure the line number is numeric
afinn_episode_iv_summary <- afinn_episode_iv %>%
  mutate(line = as.numeric(line)) %>% 
  mutate(scriptsection = line %/% 100) %>%
  group_by(scriptsection) %>%
  summarise(score = mean(value))

head(afinn_episode_iv_summary)

# plot the sentiment though the book
ggplot(afinn_episode_iv_summary , aes(scriptsection,  score)) +
  geom_line()  +
  geom_point() +
  labs(title = "Sentiment Progression in A New Hope",
       x = "Script Section", y = "Average Afinn Sentiment Score")


# tf-idf ---------------------------------------------------------------

library(tidytext)
library(tidyverse)

# import the data and filter
star_wars_script <- readRDS("data/star_wars_scripts.rds")
episode_iv <- star_wars_script %>% 
  filter(movie == "IV") %>%
  select(line, character, dialogue)

# make into tidy form and add line numbers
tidy_episode_iv <- episode_iv %>%
  mutate(line = as.numeric(line)) %>%
  unnest_tokens(word, dialogue) 

# label into sections by line number
tidy_split_episode_iv <- tidy_episode_iv %>%
  mutate(scriptsection = line %/% 200) # split every 200 lines

# add document-term count
tidy_split_episode_iv <- tidy_split_episode_iv %>%
  group_by(word, scriptsection) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(-count)

head(tidy_split_episode_iv, 8)

# apply TF-IDF
tidy_split_episode_iv <- tidy_split_episode_iv %>%
  bind_tf_idf(term = word, document = scriptsection, n = count)

head(tidy_split_episode_iv, 8)


# Visualise top word by section
tidy_split_episode_iv %>%
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

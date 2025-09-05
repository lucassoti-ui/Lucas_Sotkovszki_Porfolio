library(widyr)
library(tidyr)
library(dplyr)
library(tidytext)
library(tidytuesdayR)
library(stringr)
library(scales)
library(ggplot2)
library(jsonlite)
library(textdata)
library(igraph)
library(ggraph)
library(topicmodels)

#### Load and prepare data ######

airbnb_data <- stream_in(file("C:/Users/lucas/Documents/airbnb_listings.json"))

# Focusing in the US so I do not get tokens in other languages
airbnb_data <- airbnb_data %>%
  filter(!is.na(text)) %>%
  filter(airbnb_data$address$country_code == 'US')

colnames(airbnb_data)[6] <- "text"

airbnb_data <- airbnb_data %>%
  mutate(price = price$`$numberDecimal`,
         cleaning = cleaning_fee$`$numberDecimal`,
         guests = guests_included$`$numberDecimal`)

# Created the metric to know the type of the properties 
airbnb_df <- airbnb_data %>%
  mutate(
    price_num = as.numeric(price),
    cleaning_fee_num = as.numeric(cleaning),
    guests_included = as.numeric(guests),
    total_price = price_num + cleaning_fee_num,
    price_per_person = total_price / guests_included
  ) %>%
  filter(!is.na(price_per_person), is.finite(price_per_person)) %>%
  mutate(
    type = case_when(
      price_per_person <= quantile(price_per_person, 0.33, na.rm = TRUE) ~ "cheap",
      price_per_person <= quantile(price_per_person, 0.66, na.rm = TRUE) ~ "normal",
      TRUE ~ "expensive"
    )
  ) %>%
  select(text, type, price, cleaning, guests)

tidy_df <- airbnb_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

###### Correlation Analysis  #############

correltype <- airbnb_df %>%
  mutate(
    price = as.numeric(price),
    cleaning = as.numeric(cleaning)
  ) %>%
  filter(!is.na(price), !is.na(cleaning))

correlations_by_type <- correltype %>%
  group_by(type) %>%
  summarise(correlation = cor(price, cleaning, use = "complete.obs"))

print(correlations_by_type)

ggplot(correltype, aes(x = cleaning, y = price)) +
  geom_point(alpha = 0.8, color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  facet_wrap(~type) +
  labs(
    title = "Correlation Between Cleaning Fee and Price by Airbnb Type",
    x = "Cleaning Fee",
    y = "Price"
  ) +
  theme_minimal()


###### TF-IDF for word importance by type #############

word_counts_tf <- tidy_df %>%
  count(type, word, sort = TRUE)

tf_idf_results <- word_counts_tf %>%
  bind_tf_idf(word, type, n)

top_tf_idf <- tf_idf_results %>%
  group_by(type) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

ggplot(top_tf_idf, aes(x = reorder_within(word, tf_idf, type),
                       y = tf_idf, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free", ncol = 2) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top 10 TF-IDF Words by Airbnb Type",
    x = "Word",
    y = "TF-IDF Score"
  ) +
  theme_minimal()


############ Cororreroglams #########################

df_filtered <- airbnb_df %>%
  filter(type %in% c("cheap", "normal", "expensive"), !is.na(text))

tidy_words <- df_filtered %>%
  unnest_tokens(word, text) %>%
  filter(!is.na(word)) %>%   
  group_by(type) %>% 
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))

frequency <- tidy_words %>%
  count(type, word) %>%
  group_by(type) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(type, proportion) %>%
  gather(type, proportion, `cheap`, `normal`)

ggplot(frequency, aes(x = proportion, y = expensive,
                      color = abs(expensive - proportion))) +
  geom_abline(color = "grey40", lty = 2) +
  geom_jitter(alpha = 0.05, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~type, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    y = "Expensive",
    x = NULL,
    title = "Comparing expensive with cheap and normal airbnbs"
  )

############ Sentiment Analysis (all lexicons) #######

sentiments <- bind_rows(mutate(get_sentiments("afinn"), lexicon = "afinn"),
                        mutate(get_sentiments("nrc"), lexicon = "nrc"),
                        mutate(get_sentiments("bing"), lexicon = "bing"))

bing_sentiment <- sentiments %>% filter(lexicon == "bing")
nrc_sentiment <- sentiments %>% filter(lexicon == "nrc")
afinn_sentiment <- sentiments %>% filter(lexicon == "afinn")

bing_sentiment_count <- tidy_df %>%
  inner_join(bing_sentiment, by = "word") %>%
  count(type, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

bing_sentiment_count %>%
  ggplot(aes(x = type, y = sentiment_score, fill = type)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Score (Bing) by Type",
       x = "Type",
       y = "Positive - Negative Score") +
  theme_minimal()

bing_word_counts <- tidy_df %>%
  inner_join(bing_sentiment, by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

bing_word_counts %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip() +
  theme_minimal()


############ Sentiment Analysis #######

afinn_sentiment <- tidy_words %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(type) %>%
  summarise(sentiment_score = sum(value)) %>%
  mutate(method = "AFINN")

bing_nrc_sentiment <- bind_rows(
  tidy_words %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing"),
  tidy_words %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")
) %>%
  count(method, type, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

combined_sentiment <- bind_rows(
  afinn_sentiment %>% select(type, sentiment_score, method),
  bing_nrc_sentiment %>% select(type, sentiment_score, method)
)

ggplot(combined_sentiment, aes(x = type, y = sentiment_score, fill = method)) +
  geom_col(position = "dodge") +
  labs(
    title = "Sentiment Score by Property Type in New York",
    y = "Sentiment Score (Positive - Negative)",
    x = "Property Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###### Bigram Creation & TF-IDF for Bigrams ###########

airbnb_bigrams <- airbnb_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

bigrams_separated <- airbnb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigram_united %>%
  count(type, bigram) %>%
  bind_tf_idf(bigram, type, n) %>%
  arrange(desc(tf_idf))

top_bigrams <- bigram_tf_idf %>%
  group_by(type) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

ggplot(top_bigrams, aes(x = reorder_within(bigram, tf_idf, type),
                        y = tf_idf, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free", ncol = 2) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top 10 TF-IDF Bigrams by Airbnb Type",
    x = "Bigram",
    y = "TF-IDF Score"
  ) +
  theme_minimal()

###### Quadrogram Creation & TF-IDF ###################

quadrogram <- airbnb_df %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word)

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep = " ")

quadrogram_tf_idf <- quadrogram_united %>%
  count(type, quadrogram) %>%
  bind_tf_idf(quadrogram, type, n) %>%
  arrange(desc(tf_idf))

top_quadrograms <- quadrogram_tf_idf %>%
  group_by(type) %>%
  slice_max(tf_idf, n = 3) %>%
  ungroup()

ggplot(top_quadrograms, aes(x = reorder_within(quadrogram, tf_idf, type),
                            y = tf_idf, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free", ncol = 2) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top 10 TF-IDF Quadrograms by Airbnb Type",
    x = "Quadrogram",
    y = "TF-IDF Score"
  ) +
  theme_minimal()


########## Bigram Network Graph ######################


bigram_graph <- bigram_counts %>%
  filter(n > 30) %>%
  graph_from_data_frame()

graph_plot <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

graph_plot

######### Zipf's Law and Frequency Analysis ##########

total_words <- word_counts_tf %>%
  group_by(type) %>%
  summarise(total = sum(n))

zipf_data <- left_join(word_counts_tf, total_words) %>%
  group_by(type) %>%
  mutate(rank = row_number(), `term frequency` = n / total)

zipf_plot <- zipf_data %>%
  ggplot(aes(rank, `term frequency`, color = type)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = 'gray50', linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Zipf's Law in Airbnb Descriptions",
       x = "Rank",
       y = "Term Frequency")

zipf_plot

######### Topic Modeling using LDA ###################

doc_text <- airbnb_df %>%
  mutate(doc_id = row_number()) %>%
  select(doc_id, text)

tidy_tokens <- doc_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"), nchar(word) > 2)

word_counts <- tidy_tokens %>%
  count(doc_id, word, sort = TRUE)

dtm <- word_counts %>%
  cast_dtm(document = doc_id, term = word, value = n)

lda_model <- LDA(dtm, k = 3, control = list(seed = 123))

topics_beta <- tidy(lda_model, matrix = "beta")
top_terms <- topics_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

ggplot(top_terms, aes(x = reorder_within(term, beta, topic),
                      y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(
    title = "Top Terms per Topic in Airbnb Descriptions",
    x = "Term",
    y = "Probability (Beta)"
  ) +
  theme_minimal()


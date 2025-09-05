library(shiny)
library(ggplot2)
library(dplyr)
library(tidytext)
library(ggraph)
library(igraph)
library(scales)

ui <- fluidPage(
  titlePanel("Airbnb Property category Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_type", "Select Airbnb Type:",
                  choices = c("All", "cheap", "normal", "expensive"),
                  selected = "All"),
      selectInput("ngram_option", "Select Text View:",
                  choices = c("TF-IDF", "Quadrogram"),
                  selected = "TF-IDF"),
      selectInput("sentiment_option", "Select Sentiment Graph:",
                  choices = c("Sentiment Score", "Top Words"),
                  selected = "Sentiment Score")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("ngram_plot")),
        column(6, plotOutput("correlogram_plot"))
      ),
      fluidRow(
        column(6, plotOutput("bigram_network")),
        column(6, plotOutput("sentiment_plot"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    if (input$selected_type == "All") {
      airbnb_df
    } else {
      airbnb_df %>% filter(type == input$selected_type)
    }
  })
  
  output$ngram_plot <- renderPlot({
    if (input$ngram_option == "TF-IDF") {
      tfidf <- tf_idf_results
      if (input$selected_type != "All") {
        tfidf <- tfidf %>% filter(type == input$selected_type)
      }
      top_tf_idf <- tfidf %>%
        group_by(type) %>%
        slice_max(tf_idf, n = 10) %>%
        ungroup()
      
      ggplot(top_tf_idf, aes(x = reorder_within(word, tf_idf, type),
                             y = tf_idf, fill = type)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~type, scales = "free", ncol = 1) +
        scale_x_reordered() +
        coord_flip() +
        labs(title = "Top 10 TF-IDF Words by Airbnb Type",
             x = "Word", y = "TF-IDF Score") +
        theme_minimal()
    } else {
      top_q <- top_quadrograms
      if (input$selected_type != "All") {
        top_q <- top_q %>% filter(type == input$selected_type)
      }
      ggplot(top_q, aes(x = reorder_within(quadrogram, tf_idf, type),
                        y = tf_idf, fill = type)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~type, scales = "free", ncol = 1) +
        scale_x_reordered() +
        coord_flip() +
        labs(title = "Top TF-IDF Quadrograms by Airbnb Type",
             x = "Quadrogram", y = "TF-IDF Score") +
        theme_minimal()
    }
  })
  
  output$sentiment_plot <- renderPlot({
    if (input$sentiment_option == "Sentiment Score") {
      ggplot(combined_sentiment, aes(x = type, y = sentiment_score, fill = method)) +
        geom_col(position = "dodge") +
        labs(title = "Sentiment Score by Property Type in New York",
             y = "Sentiment Score (Positive - Negative)",
             x = "Property Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      bing_word_counts %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment", x = NULL) +
        coord_flip() +
        theme_minimal()
    }
  })
  
  output$correlogram_plot <- renderPlot({
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
      labs(y = "Expensive", x = NULL, title = "Comparing expensive with cheap and normal airbnbs")
  })
  
  output$bigram_network <- renderPlot({
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
  })
}

shinyApp(ui = ui, server = server)
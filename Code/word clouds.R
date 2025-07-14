library(dplyr)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)
library(here)

setwd(here())
df <-read_excel("Data/Table 2.xlsx")
unique_combinations <- df %>%
  distinct(Payload, Type, Platform)


custom_stop_words <- data.frame(
  word = c("e.g.", "etc.", "e.g"),
  stringsAsFactors = FALSE
)

#Combine custom stop words with tidytext stop words
  all_stop_words <- bind_rows(stop_words, custom_stop_words)
  setwd(paste(here(), "/Box1_Word cloud/Result/", sep=""))
  for (i in 1:nrow(unique_combinations)) {
    # Filter the data for the current combination
      payload <- unique_combinations$Payload[i]
      # Filter the dataset for the specific Payload,Ecosystem, Type combination
        subset_df <- df %>%
          filter(Payload == payload) %>%
          select(`Application scenario`, `Frequency`)
        
          # Aggregate by Application scenario to handle duplicates (if any)
          wordcloud_data <- subset_df %>%
            unnest_tokens(word, `Application scenario`) %>%  # Tokenize application scenarios into words
            anti_join(all_stop_words, by = "word") %>%           # Remove common stop words
            uncount(Frequency) %>%                           # Repeat each word based on the Frequency
            count(word, name = "Frequency")  
          
            # Generate the word cloud
            wordcloud <- wordcloud2(
              data = wordcloud_data, 
              size = 0.5, 
              color = c("#4C9F70", "#8FB339", "#6C757D", "#34626C", "#588C7E"), 
              backgroundColor = "white"
              )
            # Save the word cloud to an HTML file
              # Each file is named according to the Payload and Ecosystem
              file_name <- paste0("wordcloud_", payload, ".html")
              htmlwidgets::saveWidget(wordcloud, file_name, selfcontained = TRUE)
  }
  
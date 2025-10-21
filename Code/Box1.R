#===============================================================================
#New version of code for plotting Fig. 2: Allivial plot with"Table2_new.xlsx"
#===============================================================================
library(dplyr)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)
library(here)

setwd(here())
df <-read_excel("Data/Table2_new.xlsx")
df <- df %>%
  mutate(`Application scenario` = str_replace(`Application scenario` , "^.*?\\bto\\b\\s*", ""))
df$Payload[which(df$Payload=="Physical/Chemical sensors")]<-"Physical_Chemical sensors"
df$Payload[which(df$Payload=="Sampler/Releaser")]<-"Sampler_Releaser"

unique_combinations <- df %>%
  distinct(Payload)


custom_stop_words <- data.frame(
  word =  c("e.g.", "etc.", "e.g","imagery", unlist(str_split(tolower(unique(df$Payload)), "[ /]+"))
  ),
  stringsAsFactors = FALSE
)

#Combine custom stop words with tidytext stop words
all_stop_words <- bind_rows(stop_words, custom_stop_words)
setwd(here::here())
for (i in 1:nrow(unique_combinations)) {
  # Filter the data for the current combination
  payload <- unique_combinations$Payload[i]
  # Filter the dataset for the specific Payload,Ecosystem, Type combination
  subset_df <- df %>%
    filter(Payload == payload) %>%
    dplyr::select(`Application scenario`, `Frequency`)
  
  # Aggregate by Application scenario to handle duplicates (if any)
  wordcloud_data <- subset_df %>%
    unnest_tokens(word, `Application scenario`) %>%  # Tokenize application scenarios into words
    anti_join(all_stop_words, by = "word") %>%           # Remove common stop words
    uncount(Frequency) %>%                           # Repeat each word based on the Frequency
    count(word, name = "Frequency") 
 if (payload!="Sampler_Releaser") {
  wordcloud_data<- wordcloud_data %>% 
    filter(Frequency >= max(Frequency)/5) 
 } else {
   
 }
  
  # Generate the word cloud
  wordcloud <- wordcloud2(
    data = wordcloud_data, 
    size = 0.5, 
    color =  
      #c("#6AB04C", "#F9CA24", "#22A6B3", "#EB4D4B", "#E056FD"),
      c("#4C9F70", "#8FB339", "#6C757D", "#34626C", "#588C7E"), 
    backgroundColor = "white"
  )
  # Save the word cloud to an HTML file
  # Each file is named according to the Payload and Ecosystem
  file_name <- paste0("wordcloud_", payload, ".html")
  htmlwidgets::saveWidget(wordcloud, file_name, selfcontained = TRUE)
}




#BELOW CODE REFLECTS THE DATA PROCESSING AND OLD VERSION OF CODE NOT USED IN THE FINAL VERSION
#===============================================================================
#Code needed to get "Table2_new.xlsx" from DR_relevant.xlsx
#===============================================================================
confs_path <- "Data/DR_relevant.xlsx"
confs <- read_excel(confs_path)
table1<-data.frame(
  "Platform"=confs$Platform_o4_mini,
  "Ecosystem"=confs$Biome,
  "Payload"=confs$Sensor_o4_mini,
  "Scenario"=confs$`Application scenario_o4_mini`)
str(table1)
library(dplyr)


table1 <- table1 %>%
  mutate(
    Payload = Payload %>%
      # 1) Replace 'sampler' or 'releaser' (any case) with 'Sampler/Releaser'
      str_replace_all("(?i)\\b(sampler|releaser)\\b", "Sampler/Releaser") %>%
      # 2) If both were present, collapse duplicates like "Sampler/Releaser; Sampler/Releaser"
      str_replace_all("(?i)(Sampler/Releaser)(\\s*;\\s*Sampler/Releaser)+", "Sampler/Releaser") %>%
      # 3) Normalize spaces around semicolons
      str_replace_all("\\s*;\\s*", "; ") %>%
      str_squish()
  )

table1 <- table1 %>%
  mutate(
    Payload = Payload %>%
      # 1) Replace 'sampler' or 'releaser' (any case) with 'Sampler/Releaser'
      str_replace_all("(?i)\\b(thermal infrared|near-infrared)\\b", "Near-infrared/Thermal infrared") %>%
      # 2) If both were present, collapse duplicates like "Sampler/Releaser; Sampler/Releaser"
      str_replace_all("(?i)(Near-infrared/Thermal infrared)(\\s*;\\s*Near-infrared/Thermal infrared)+", "Near-infrared/Thermal infrared") %>%
      # 3) Normalize spaces around semicolons
      str_replace_all("\\s*;\\s*", "; ") %>%
      str_squish()
  )

collapse_table_by_sensors<- function(table1) {
  Data<-NULL
  for (payload in c("RGB", "Multispectral","Hyperspectral", 
                        "Near-infrared/Thermal infrared", "LiDAR",
                        "GPS tracking system",
                        "Physical/chemical sensor",
                        "Sampler/Releaser","Logger","Sonar","Other")) {
        rows <- grep(payload, table1$Payload)
        n_rows<-length(rows)
        temp=data.frame("Application scenario"=table1$Scenario[rows],
                        "Payload"=rep(payload,n_rows),
                        "Frequency"=rep(1, n_rows))
        Data<-rbind(Data,temp)
      }
  return(Data)
}
# All devices
table2_output<-collapse_table_by_sensors(table1)
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
table2_output$Payload<-as.character(table2_output$Payload)
table2_output$Payload[which(table2_output$Payload=="Physical/chemical sensor")]<-"Physical/Chemical sensors"
write_xlsx(table2_output,"Data/Table2_new.xlsx")



#===============================================================================
#Old version of code for ploting Fig. 2: Allivial plot
#===============================================================================
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
setwd(here::here())
for (i in 1:nrow(unique_combinations)) {
  # Filter the data for the current combination
  payload <- unique_combinations$Payload[i]
  # Filter the dataset for the specific Payload,Ecosystem, Type combination
  subset_df <- df %>%
    filter(Payload == payload) %>%
    dplyr::select(`Application scenario`, `Frequency`)
  
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

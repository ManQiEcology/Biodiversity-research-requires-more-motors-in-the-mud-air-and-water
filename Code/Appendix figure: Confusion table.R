################################################################################
#Randomly select 20% paper to generate confusion table
################################################################################

rm (list=ls ())
library(readxl)
library(here)
library(openxlsx)
library(writexl)
library(dplyr)
library(stringr)
library(caret)  
library(caret)
library(ggplot2)
setwd(here())

################################################################################
#Analyze the 20% paper to generate confusion table
################################################################################
Confs_DR<-read_excel("Data/Dataset for confusion table of accuracy.xlsx")
setwd(paste(here(),"Result/",sep=""))
###################################################
#Biome
combined_levels <- union(levels(factor(Confs_DR$Biome)), levels(factor(Confs_DR$Biome_vali)))
Confs_DR$Biome<-factor(Confs_DR$Biome, levels = combined_levels)
Confs_DR$Biome_vali<-factor(Confs_DR$Biome_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Biome, Confs_DR$Biome_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2

tiff("Confusion table_Biome.tiff", unit="in",width = 10, height =8, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()
#########################################
#Country_ISO3
combined_levels <- union(levels(factor(Confs_DR$Country_ISO3)), levels(factor(Confs_DR$Country_ISO3_vali)))
Confs_DR$Country_ISO3<-factor(Confs_DR$Country_ISO3, levels = combined_levels)
Confs_DR$Country_ISO3_vali<-factor(Confs_DR$Country_ISO3_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Country_ISO3, Confs_DR$Country_ISO3_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2
tiff("Confusion table_Country_ISO3.tiff", unit="in",width = 15, height =10, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()

#########################################
#Subdiscipline
combined_levels <- union(levels(factor(Confs_DR$Subdiscipline)), levels(factor(Confs_DR$Subdiscipline_vali)))
Confs_DR$Subdiscipline<-factor(Confs_DR$Subdiscipline, levels = combined_levels)
Confs_DR$Subdiscipline_vali<-factor(Confs_DR$Subdiscipline_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Subdiscipline, Confs_DR$Subdiscipline_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2
tiff("Result/Confusion table_Subdiscipline.tiff", unit="in",width = 10, height =8, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()
#########################################
#Taxa_ITIS
combined_levels <- union(levels(factor(Confs_DR$Taxa_ITIS)), levels(factor(Confs_DR$Taxa_ITIS_vali)))
Confs_DR$Taxa_ITIS<-factor(Confs_DR$Taxa_ITIS, levels = combined_levels)
Confs_DR$Taxa_ITIS_vali<-factor(Confs_DR$Taxa_ITIS_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Taxa_ITIS, Confs_DR$Taxa_ITIS_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2
tiff("Result/Confusion table_Taxa_ITIS.tiff", unit="in",width = 5, height =3.5, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()

#########################################
#Application_Category
combined_levels <- union(levels(factor(Confs_DR$Application_Category)), levels(factor(Confs_DR$Application_Category_vali)))
Confs_DR$Application_Category<-factor(Confs_DR$Application_Category, levels = combined_levels)
Confs_DR$Application_Category_vali<-factor(Confs_DR$Application_Category_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Application_Category, Confs_DR$Application_Category_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2
tiff("Result/Confusion table_Application_Category.tiff", unit="in",width = 5, height =3.5, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()

#########################################
#Environmental_Variable
combined_levels <- union(levels(factor(Confs_DR$Environmental_Variable)), levels(factor(Confs_DR$Environmental_Variable_vali)))
Confs_DR$Environmental_Variable<-factor(Confs_DR$Environmental_Variable, levels = combined_levels)
Confs_DR$Environmental_Variable_vali<-factor(Confs_DR$Environmental_Variable_vali, levels = combined_levels)

conf_matrix <- confusionMatrix(Confs_DR$Environmental_Variable, Confs_DR$Environmental_Variable_vali)
print(conf_matrix)

# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)

# Plot the confusion matrix using ggplot2
tiff("Result/Confusion table_Environmental_Variable.tiff", unit="in",width = 10, height =8, res= 600,pointsize = 10)
ggplot(data = conf_matrix_table, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Actual classification", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()

################################################################################
#Platform and Sensors
norm_label <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_squish() %>%
    str_to_lower() %>%              # case-insensitive compare
    str_replace_all("\\s*&\\s*", ";")
}

# --- 3) (Optional) harmonize typos/synonyms ---------------------------------
# Edit this mapping as needed for your data
recode_map_platform <- c(
  "robot; other" = "other",
  "biorobotsic"  = "biorobotics",
  "biorobotsics" = "biorobotics"
)

harmonize_platform <- function(x) {
  x <- norm_label(x)
  x <- dplyr::recode(x, !!!recode_map_platform, .default = x)
  ifelse(x == "" | is.na(x), NA_character_, x)
}

harmonize_sensor <- function(x) {
  x <- norm_label(x)
  ifelse(x == "" | is.na(x), NA_character_, x)
}

# Optional: map common variants/synonyms to a standard form
sensor_alias <- c(
  "Laser Scanner" = "Laser",
  "physical;chemical sensor" = "physical/chemical sensor",
  "thermal" = "thermal infrared"
)

# Canonicalize order in a semicolon-separated list
canonicalize_semilist <- function(x, priority = NULL, keep_case = FALSE) {
  if (is.na(x) || x == "") return(NA_character_)
  
  # split → trim → lower → alias map
  parts <- str_split(x, ";", simplify = FALSE)[[1]]
  parts <- str_trim(parts)
  if (!keep_case) parts <- str_to_lower(parts)
  parts <- dplyr::recode(parts, !!!sensor_alias, .default = parts)
  
  # drop empties, deduplicate
  parts <- parts[nzchar(parts)]
  parts <- unique(parts)
  
  if (length(parts) == 0) return(NA_character_)
  
  # order: by user priority (if given), otherwise alphabetical
  if (!is.null(priority)) {
    # build factor with given order; unseen items go after, alphabetically
    known <- parts[parts %in% priority]
    unknown <- setdiff(parts, priority)
    parts <- c(known[order(match(known, priority))], sort(unknown))
  } else {
    parts <- sort(parts)
  }
  
  paste(parts, collapse = "; ")
}


# --- 4) Prepare columns: reference (vali) and prediction (chatgpt) --------
Confs_DR_platform_eval <- Confs_DR %>%
  transmute(
    ref  = harmonize_platform(Platform_vali),     # gold standard
    pred = harmonize_platform(Platform_o4_mini)     # ChatGPT output
  ) %>%
  filter(!is.na(ref), !is.na(pred))        # drop NAs for fair eval

Confs_DR_sensor_eval <- Confs_DR %>%
  transmute(
    ref  = harmonize_sensor(Sensor_vali),
    pred = harmonize_sensor(Sensor_o4_mini)
  ) %>%
  filter(!is.na(ref), !is.na(pred)) %>%
  mutate(
    ref  = vapply(ref, canonicalize_semilist, ""),
    pred = vapply(pred, canonicalize_semilist, "")
  )



# Ensure factors share the same level set
Confs_DR_platform_eval <- Confs_DR_platform_eval %>%
  mutate(
    ref  = factor(ref),
    pred = factor(pred, levels = levels(ref))
  )

Confs_DR_sensor_eval <- Confs_DR_sensor_eval %>%
  mutate(
    ref  = factor(ref),
    pred = factor(pred, levels = levels(ref))
  )


# --- 5) Confusion matrix + Kappa --------------------------------------------
cm_platform <- confusionMatrix(data = Confs_DR_platform_eval$pred, reference = Confs_DR_platform_eval$ref)
cm_sensor<- confusionMatrix(data = Confs_DR_sensor_eval$pred, reference = Confs_DR_sensor_eval$ref)

cm_platform$table               # confusion matrix
cm_platform$overall["Accuracy"] # overall accuracy
cm_platform$overall["Kappa"]    # Cohen's Kappa
cm_platform$byClass             # per-class precision/recall/F1, etc.

cm_sensor$table               # confusion matrix
cm_sensor$overall["Accuracy"] # overall accuracy
cm_sensor$overall["Kappa"]    # Cohen's Kappa
cm_sensor$byClass             # per-class precision/recall/F1, etc.

# --- 6) Inspect most common mismatches --------------------------------------
Confs_DR_platform_eval %>%
  filter(pred != ref) %>%
  count(ref, pred, sort = TRUE)

Confs_DR_sensor_eval %>%
  filter(pred != ref) %>%
  count(ref, pred, sort = TRUE)

# --- 7) (Optional) quick heatmap of confusion matrix ------------------------
# install.packages("ggplot2") if needed
library(ggplot2)
tiff("Result/Confusion table_platform.tiff", unit="in",width = 5, height =4, res= 400,pointsize = 10)
cm_platform_Confs_DR <- as.data.frame(cm_platform$table)
ggplot(cm_platform_Confs_DR, aes(Reference, Prediction)) +
  geom_text(aes(label = Freq)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = NULL,
       x = "Actual classification)", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()


tiff("Result/Confusion table_sensor.tiff", unit="in",width = 8, height =8, res= 400,pointsize = 10)
cm_sensor_Confs_DR <- as.data.frame(cm_sensor$table)
ggplot(cm_sensor_Confs_DR, aes(Reference, Prediction)) +
  geom_text(aes(label = Freq)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = NULL,
       x = "Actual classification)", y = "Estimated classification") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()
################################################################################
#Application scenarios
App_scenr_accuracy<-sum(Confs_DR$`Application scenario check (1-consistant, 0-not consistant)`,na.rm = TRUE)/ nrow(Confs_DR)
App_scenr_accuracy

#==============================================================================
#CODE TO SELECT THE 20% PAPERS FROM THE FULL POOL: Publications with extracted parameter values.xlsx
#==============================================================================
#select relevant 
DR<-read_excel("Data/Publications with extracted parameter values.xlsx")
# Set seed for reproducibility
set.seed(123)
#randomly select 20% papers
n<-nrow(DR)
s<-n*0.2
Confs_DR <- DR[sample(n, s), ]

#save the randomly selected 20% paper

write.xlsx(Confs_DR, "Confs_DR.xlsx")

#confirm the percentage of drones papers is consistent
sum(Confs_DR$Instrument=="Drones")/nrow(Confs_DR)
sum(DR$Instrument=="Drones")/nrow(DR)
################################################################################
#Randomly select 20% paper to generate confusion table
################################################################################

rm (list=ls ())
library(readxl)
library(here)
library(openxlsx)
setwd(here())

#select relevant 
DR<-read_excel("Data/DR_relevant.xlsx")
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


################################################################################
#Analyze the 20% paper to generate confusion table
################################################################################
rm (list=ls ())
library(readxl)
setwd(here())
Confs_DR<-read_excel("Data/Confs_DR_new.xlsx")
library(caret)
library(ggplot2)
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


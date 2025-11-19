library(ggplot2)
library(ggalluvial)
library(forcats)
library(ggtext)
library(ggrepel)
library(here)
library(dplyr)
library(readxl)
setwd<-here()

df <-read_excel("Data/Table 1.xlsx")


# Reorder each axis based on the number of publications
df$`Percentage (%)`<-df$Freq/sum(df$Freq)*100

df$Platform <- factor(df$Platform, levels = c("Drone", "ROVs/AUVs", "Glider", "Drifter" ,"Biorobot", "Other"))    
df$Ecosystem <- factor(df$Ecosystem, 
                   levels = c("Forests", "Shrub_Grasslands","Urban",
                              "Deserts","Deltas/Estuaries/Mangroves",
                              "Lakes", "Rivers/Streams","Marine","Coral reefs"))

df$Payload <- factor(df$Payload, 
                   levels = c("RGB", "Multispectral","Hyperspectral", 
                              "Near-infrared/Thermal infrared", "LiDAR","Laser",
                              "GPS tracking system" ,"Radio-position telemetry",
                              "Physical/Chemical sensors","Other",
                              "Sampler/Releaser","Logger","Sonar"))

    
biome_colours <- c(
  "Forests" = "#266e5d", 
  "Shrub_Grasslands" = "#68b38c", 
  "Agriculture" = "#68b34c",
  "Deserts" = "#dbb479",
  "Tundra" = "#dbb499",
  "Deltas/Estuaries/Mangroves" = "#a8662f",
  "Lakes" = "#f28e30",
  "Rivers/Streams" = "#bf62a4",
  "Marine" = "#5d95d9",
  "Coral reefs" = "#7c58ad",
  "Urban" = "#a5a6b5",
  "other" = "#a6cee3",
  "NA" = "#a5a6b5"
)


# Define your color palette
color_palette <- c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", 
                   "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704")


p <- ggplot(
  df,
  aes(axis1 = Ecosystem, axis2 = Platform, axis3 = Payload, y =`Percentage (%)`)
) +
  # flows colored by ecosystem
  geom_alluvium(aes(fill = Ecosystem),
                alpha = 0.55, width = 1/12, knot.pos = 0.4, curve_type = "cubic") +
  # neutral strata (the vertical bars)
  geom_stratum(width = 1/7, fill = "grey85", color = "grey35") +
  # labels on strata
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3.3, family = "sans", color = "grey15") +
  scale_x_discrete(limits = c("Ecosystem", "Platform", "Payload/Sensor"),
                   expand = c(.06, .06)) +
  scale_fill_manual(values = biome_colours, guide = guide_legend(title = "Ecosystem")) +
  labs(x = NULL, y = NULL, title = "Flows from Ecosystem → Platform → Payload/Sensor") +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y  = element_blank(),
    axis.text.x  = element_text(size = 11, color = "grey20"),
    panel.grid   = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 13, face = "bold", hjust = 0)
  )
p

tiff("Result/Fig. 2 Alluvium/Result/Alluvium_with label.tif", width = 7, height = 7, units = "in", res = 600)
par(mar=c(1, 1, 1, 5)) # Outer margins and panel spacing

ggplot(df, aes(axis1 = Ecosystem,axis2 = Platform,  axis3 = Payload, y = `Percentage (%)`)) +
  geom_alluvium(aes(fill = `Percentage (%)`), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray90") +
  geom_label_repel(stat = "stratum", aes(label = after_stat(stratum)),
                   size = 3,                # Adjust label size
                   angle = 0,              # Rotate labels to be vertical
                   hjust = 0.5,             # Center horizontally
                   vjust = 0.6,             # Center vertically
                   color = "black",         # Text color
                   fill = "#FFFFFF80",          # Background color for labels
                   label.size = 0.1,        # Border thickness of the label
                   box.padding = 0.0,       # Padding around labels
                   point.padding = 0.0,     # Padding around the points/strata
                   max.overlaps = Inf
                   #,+      Allows many labels to be displayed
                   #min.segment.length = Inf
  ) +
  
  scale_x_discrete(limits = c("Ecosystem","Platform",  "Payload"), expand = c(0.15, 0.05)) +
  scale_fill_gradientn(colors = biome_colours, guide = guide_colorbar(direction = "vertical"))+
#  scale_fill_gradient(low = "white",  high = "#e97f02",
#                      guide = guide_colorbar(direction = "vertical")) +
  theme_minimal() +
  labs(x = "Appilication scenarios",y = "Percentage (%)") +
  theme(legend.position = c(1.18, 0.8),    # Position legend at (x=0.8, y=0.8) within the plot
        legend.justification = c("right", "top"),  # Justify legend based on the top-right corner
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.margin = margin(10, 70, 10, 10)
        ) # Rotate x-axis text to make it vertical
         

dev.off()

tiff("Result/Alluvium_without label.tif", width = 7, height = 7, units = "in", res = 600)
ggplot(df, aes(axis1 = Ecosystem, axis2 = Platform, axis3 = Payload, y = `Percentage (%)`)) +
  geom_alluvium(aes(fill = `Percentage (%)`), width = 1/12) +
  geom_stratum(width = 1/12, fill = "gray90", color = "black") + # Strata without labels
  scale_x_discrete(
    limits = c("Ecosystem", "Platform", "Payload"), # Fix order of columns
    expand = c(0.15, 0.05) # Adjust spacing
  ) +
  scale_fill_gradientn(colors = color_palette, guide = guide_colorbar(direction = "vertical")) +
  theme_minimal() +
  labs(x = "Application scenarios", y = "Percentage (%)") +
  theme(
    legend.position = c(1.18, 0.8),  # Adjust legend position
    legend.justification = c("right", "top"),
    plot.margin = margin(10, 70, 10, 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1),# Rotate x-axis text vertically
    #axis.text.y = element_blank(), # Remove y-axis text
    axis.title.y = element_text(size = 10) # Keep y-axis title
    #axis.title.x = element_blank(), # Remove x-axis label
    #panel.grid = element_blank() # Remove grid lines
  )
dev.off()




#===============================================================================
#generate Table 1 from Publications with extracted parameter values.xlsx
#===============================================================================
confs_path <- "Data/Publications with extracted parameter values.xlsx"
confs <- read_excel(confs_path)
table1<-data.frame(
  "Platform"=confs$Platform_o4_mini,
  "Ecosystem"=confs$Biome,
  "Payload"=confs$Sensor_o4_mini)
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

collapse_table<- function(table1) {
  Data<-NULL
  for (platform in c("Drone",
                 "ROV",
                 "Robot",
                 "Glider",
                 "Drifter",
                 "Other")) {
    for (ecosystem in c("Forests", 
                        "Shrublands/Grasslands/Savanna/Woodlands",
                        "Rivers/Streams",
                        "Lakes",
                        "Coral reefs",
                        "Deltas/Estuaries/Mangroves",
                        "Marine",
                        "Deserts",
                        "Urban"
                        )) {
      for (payload in c("RGB", "Multispectral","Hyperspectral", 
                        "Near-infrared/Thermal infrared", "LiDAR",
                        "GPS tracking system",
                        "Physical/chemical sensor",
                        "Sampler/Releaser","Logger","Sonar","Other")) {
          
          
          
          contains_all <- grepl(platform, table1$Platform) & grepl(ecosystem, table1$Ecosystem) &
            grepl(payload, table1$Payload)
          num_rows <- sum(contains_all)
          
          temp=data.frame("Platform"=rep(platform,num_rows),
                          "Ecosystem"=rep(ecosystem,num_rows),
                          "Payload"=rep(payload,num_rows))
          Data<-rbind(Data,temp)
      }
    }
  }
  return(Data)
}


# All devices
table1_output<-collapse_table(table1)
table1_output$Ecosystem[which(table1_output$Ecosystem=="Shrublands/Grasslands/Savanna/Woodlands")]<-"Shrublands/Grasslands"
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
df <- as.data.frame(table(table1_output))
df<-subset(df,df$Freq!=0)
df$Ecosystem<-as.character(df$Ecosystem)
df$Ecosystem[which(df$Ecosystem=="Shrublands/Grasslands")]<-"Shrub_Grasslands"
df$Platform<-as.character(df$Platform)
df$Platform[which(df$Platform=="ROV")]<-"ROVs/AUVs"
df$Platform[which(df$Platform=="Robot")]<-"Biorobot"
df$Payload<-as.character(df$Payload)
df$Payload[which(df$Payload=="Physical/chemical sensor")]<-"Physical/Chemical sensors"
write.csv(df,"Data/Table1.csv")

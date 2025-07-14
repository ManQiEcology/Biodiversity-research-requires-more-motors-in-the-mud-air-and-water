library(ggplot2)
library(ggalluvial)
library(forcats)
library(ggtext)
library(ggrepel)
library(here)
library(dplyr)
setwd<-here()

df <-read_excel("Data/Table 1.xlsx")
df$Ecosystem[which(df$Ecosystem=="Shrub_Grasslands")]<-"Shrublands/Grasslands"

# Reorder each axis based on the number of publications
df$`Percentage (%)`<-df$Publications/sum(df$Publications)*100
df$Platform <- factor(df$Platform, levels = c("Drones", "ROVs/AUVs", "Glider", "Drifer","Biorobot"))
df$Ecosystem <- factor(df$Ecosystem, 
                   levels = c("Forests", "Shrublands/Grasslands","Agriculture",
                              "Deserts","Deltas/Estuaries/Mangroves",
                              "Lakes", "River/Streams","Marine",
                              "Others", "Experimental work"))
df$Payload <- factor(df$Payload, 
                   levels = c("RGB", "Multispectral","Hyperspectral", 
                              "Near-infrared/Thermal infrared", "LiDAR","Laser",
                              "GPS location receiver","Radio-position telemetry",
                              "Physical/Chemical sensors","Hydro-acoustic sensor",
                              "Sampler/Releaser","Logger","Sonar", "NA"))



# Define your color palette
color_palette <- c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", 
                   "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704")


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
  scale_fill_gradientn(colors = color_palette, guide = guide_colorbar(direction = "vertical"))+
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

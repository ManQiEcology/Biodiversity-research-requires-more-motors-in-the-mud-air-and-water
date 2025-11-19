# Install and load necessary packages
rm (list=ls ())
library(ggplot2)
library(readxl)
library(here)
setwd(here::here())

DR<-read_excel(paste(here(),"/Data/Publications with extracted parameter values.xlsx",sep=""))
D<-subset(DR, DR$Instrument=="Drones")
R<-subset(DR, DR$Instrument=="Robots")

setwd(paste(here(),"/Result/", sep=""))

collapse_table<- function(DR) {
  Data<-NULL
 for (taxa in c("Plantae",
              "Animalia",
              "Fungi",
              "Bacteria",
              "Protista",
              "Archaea",
              "Chromista")) {
   for (ecosystem in c("Forests", 
                   "Shrublands/Grasslands/Savanna/Woodlands",
                   "Tundra",
                   "Rivers/Streams",
                   "Lakes",
                   "Coral reefs",
                   "Deltas/Estuaries/Mangroves",
                   "Marine")) {
    for (subd in c("Physiology",
                   "Behavior",
                   "Population",
                   "Community",
                   "Ecosystem",
                   "Landscape")) {
      for (Instrument in c("Drones",
                           "Robots")) {
        

      
      contains_all <- grepl(taxa, DR$Taxa_ITIS) & grepl(ecosystem, DR$Biome) &
        grepl(subd, DR$Subdiscipline) & grepl(Instrument, DR$Instrument)
      num_rows <- sum(contains_all)
      
      temp=data.frame("Taxonomy"=rep(taxa,num_rows),
                      "Ecosystem"=rep(ecosystem,num_rows),
                      "Subdiscipline"=rep(subd,num_rows),
                      "Instrument"=rep(Instrument,num_rows))
      Data<-rbind(Data,temp)
      }
    }
  }
 }
  return(Data)
}


# All devices
Data<-collapse_table(DR)
Data$Ecosystem[which(Data$Ecosystem=="Shrublands/Grasslands/Savanna/Woodlands")]<-"Shrublands/Grasslands"
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
X_comb <- as.data.frame(table(Data))
colnames(X_comb) <- c("Taxonomy", "Biome", "Subdiscipline","Instrument", "Freq")
X_comb$Subdiscipline<-factor(X_comb$Subdiscipline, levels=c("Landscape","Ecosystem", "Community",
                                                            "Population","Behavior","Physiology"))
X_comb$Taxonomy<- factor(X_comb$Taxonomy, levels=c("Plantae", "Animalia","Bacteria","Fungi",
                                                   "Protista","Archaea","Chromista"))
X_comb$Biome<-factor(X_comb$Biome,levels = c("Forests", 
                                             "Shrublands/Grasslands",
                                             "Tundra",
                                             "Rivers/Streams",
                                             "Lakes",
                                             "Deltas/Estuaries/Mangroves",
                                             "Coral reefs",
                                             "Marine"))
X_comb$Instrument<-factor(X_comb$Instrument,levels = c("Drones", 
                                             "Robots"))
X_comb$Percentage<-X_comb$Freq/sum(X_comb$Freq)*100
XX_comb<-subset(X_comb,Taxonomy=="Plantae"|Taxonomy=="Animalia")


tiff("Result/Application fequency-Drones and Robots.tif", unit="in",width = 5.5, height =4, res= 600,pointsize = 10)
ggplot(XX_comb, aes(x = Biome, y = Subdiscipline, fill = Percentage)) +
  geom_tile() +
  facet_grid(Instrument ~ Taxonomy ) +
  scale_fill_gradient(low = "white", high = "#2b8c8c", limits = c(0, 10), oob = scales::squish) +
  labs(title = "",
       x = "Ecosystem",
       y = "Scale",
       fill = "Percentage (%)") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()


tiff("Result/Tax_percentage.tif", unit="in",width = 2, height =3, res= 600,pointsize = 10)
Tax_data <- as.data.frame(table(Data$Taxonomy))
Tax_data$Percentage<-Tax_data$Freq/sum(Tax_data$Freq)*100
colnames(Tax_data)<-c("Taxanomy", "Freq","Percentage (%)")
# Horizontal bar plot
ggplot(Tax_data, aes(x = reorder(Taxanomy, `Percentage (%)`), y = `Percentage (%)`)) +
  ylim(0,50)+
  geom_bar(stat = "identity", fill = "#2b8c8c" ) + # Bar plot with specified values
  coord_flip() + # Flip the coordinates to make bars horizontal
  theme_minimal() + # Clean theme
  theme(
    panel.grid.major.y = element_blank(), # Remove major horizontal grid lines
    panel.grid.minor.y = element_blank(),  # Remove minor horizontal grid lines
    panel.grid.minor.x = element_blank()  # Remove minor horizontal grid lines
  )+
  labs(x = "Taxonomy", y = "Percentage (%)", title = "")
dev.off()




###############################################################################
#NON_USED CODE FOR ADDITIONAL PLOTS
###############################################################################
#Drones

Data<-collapse_table(D)
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
X_comb <- as.data.frame(table(Data))
colnames(X_comb) <- c("Taxonomy", "Biome", "Subdiscipline","Instrument", "Freq")
X_comb$Subdiscipline<-factor(X_comb$Subdiscipline, levels=c("Landscape","Ecosystem", "Community",
                                                            "Population","Behavior","Physiology"))
X_comb$Taxonomy<- factor(X_comb$Taxonomy, levels=c("Plantae", "Animalia","Bacteria","Fungi",
                                                   "Protista","Archaea","Chromista"))
X_comb$Biome<-factor(X_comb$Biome,levels = c("Forests", 
                                             "Shrublands/Grasslands/Savanna/Woodlands",
                                             "Tundra",
                                             "Rivers/Streams",
                                             "Lakes",
                                             "Deltas/Estuaries/Mangroves",
                                             "Coral reefs",
                                             "Marine"))
tiff("Result/Application fequency_Drones.tiff", unit="in",width = 6, height =6, res= 600,pointsize = 10)
ggplot(X_comb, aes(x = Biome, y = Subdiscipline, fill = Freq)) +
  geom_tile() +
  facet_wrap(~ Taxonomy) +
  scale_fill_gradient(low = "white", high = "#2b8c8c", limits = c(0, 110), oob = scales::squish) +
  labs(title = "",
       x = "Ecosystem",
       y = "Subdiscipline",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 120, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()


#Robots
Data<-collapse_table(R)
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
X_comb <- as.data.frame(table(Data))
colnames(X_comb) <- c("Taxonomy", "Biome", "Subdiscipline","Instrument", "Freq")
X_comb$Subdiscipline<-factor(X_comb$Subdiscipline, levels=c("Landscape","Ecosystem", "Community",
                                                            "Population","Behavior","Physiology"))
X_comb$Taxonomy<- factor(X_comb$Taxonomy, levels=c("Plantae", "Animalia","Bacteria","Fungi",
                                                   "Protista","Archaea","Chromista"))
X_comb$Biome<-factor(X_comb$Biome,levels = c("Forests", 
                                             "Shrublands/Grasslands/Savanna/Woodlands",
                                             "Tundra",
                                             "Rivers/Streams",
                                             "Lakes",
                                             "Deltas/Estuaries/Mangroves",
                                             "Coral reefs",
                                             "Marine"))
X_comb$Freq<-X_comb$Freq/sum(X_comb$Freq)*100

tiff("Result/Application fequency_Robots.tiff", unit="in",width = 6, height =6, res= 600,pointsize = 10)
ggplot(X_comb, aes(x = Biome, y = Subdiscipline, fill = Freq)) +
  geom_tile() +
  facet_wrap(~ Taxonomy) +
  scale_fill_gradient(low = "white", high = "#2b8c8c", limits = c(0, 110), oob = scales::squish) +
  labs(title = "",
       x = "Ecosystem",
       y = "Subdiscipline",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 120, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()
tiff("Result/Application fequency.tiff", unit="in",width = 6, height =6, res= 600,pointsize = 10)
ggplot(X_comb, aes(x = Biome, y = Subdiscipline, fill = Freq)) +
  geom_tile() +
  facet_wrap(~ Taxonomy) +
  scale_fill_gradient(low = "white", high = "#2b8c8c", limits = c(0, 110), oob = scales::squish) +
  labs(title = "",
       x = "Ecosystem",
       y = "Subdiscipline",
       fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 30, vjust = 1, hjust = 1))
dev.off()



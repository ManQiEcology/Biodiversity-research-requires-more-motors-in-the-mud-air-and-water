################################################################################
#This code will: 
# 1) plot Fig. 1c,Fig. 3a,b, and Fig 4b with 20% of randomly selected papers;
# 2) Calculate the percentage of each categry of biomes, taxonomy and countries 
#    of the 20% randomly selected paper, then compare the percentage with that 
#    from all papers;
# 3) 
################################################################################
#set the library and work directory
rm (list=ls ())
library(here)
setwd(here::here())
library(fields)
library(readxl)
library(janitor)
library(stringr)
library(ggplot2)
library(ggstream)

#load the 20% randomly selected paper
twenty_percent_paper<-read_excel("Data/Dataset for confusion table of accuracy.xlsx")
robots_20<-subset(twenty_percent_paper,Instrument=="Robots")
drones_20<-subset(twenty_percent_paper,Instrument=="Drones")

#load the full pool of papers
robots<-read_excel("Data/Publications of Robots_Jan 18.xlsx")
drones<-read_excel("Data/Publications of Drones_Jan 18.xlsx")

#-------------------------------------------------------------------------------
#plot Fig. 1c with 20% selected paper

drones_20_sub<-drones_20[c(14:19,9)] #c(14-19) give the variables extracted
robots_20_sub<-robots_20[c(14:19,9)]
drones_20_ssub<-drones_20[c(14:19,9)] #c(14-19) give the variables extracted, 9 give the publication year
robots_20_ssub<-robots_20[c(14:19,9)]


#categorize papers by the decade they published in
yr2decade<-function(x) {
  y<-ifelse (x>=1990 & x<2000, 1990,x) 
  y<-ifelse (y>=2000 & x<2010, 2000,y) 
  y<-ifelse (y>=2010 & x<2020, 2010,y)
  y<-ifelse (y>=2020 & x<2030, 2020,y)
  return(y)
}
drones_20_ssub$Decade<-(yr2decade(drones_20_ssub$`Publication Year`))
robots_20_ssub$Decade<-(yr2decade(robots_20_ssub$`Publication Year`))

#define factor variables
robots_20_ssub$Application_category<-factor(robots_20_ssub$Application_Category)
robots_20_ssub$Environmental_variable<-factor(robots_20_ssub$Environmental_Variable)
robots_20_ssub$Taxa_ITIS<-factor(robots_20_ssub$Taxa_ITIS )
robots_20_ssub$Subdiscipline<-factor(robots_20_ssub$Subdiscipline )
robots_20_ssub$Biome<-factor(robots_20_ssub$Biome )
robots_20_ssub$Country_ISO3<-factor(robots_20_ssub$Country_ISO3 )
robots_20_ssub$Decade<-factor(robots_20_ssub$Decade )


drones_20_ssub$Application_category<-factor(drones_20_ssub$Application_Category)
drones_20_ssub$Environmental_variable<-factor(drones_20_ssub$Environmental_Variable)
drones_20_ssub$Taxa_ITIS<-factor(drones_20_ssub$Taxa_ITIS )
drones_20_ssub$Subdiscipline<-factor(drones_20_ssub$Subdiscipline )
drones_20_ssub$Biome<-factor(drones_20_ssub$Biome )
drones_20_ssub$Country_ISO3<-factor(drones_20_ssub$Country_ISO3 )
drones_20_ssub$Decade<-factor(drones_20_ssub$Decade )

setwd(paste(here(),"/Result/",sep=""))

################################################################################
# Fig. 1  Time series of publication for 20% paper
################################################################################

drones_20_tidy <- drones_20_ssub %>% 
  janitor::clean_names() %>% 
  dplyr::select(biome,
                subdiscipline,
                publication_year,
                decade)

robots_20_tidy <- robots_20_ssub %>% 
  janitor::clean_names() %>% 
  dplyr::select(biome,
                subdiscipline,
                publication_year,
                decade)

# How many studies cover more than one biome?
num_multiple_biomes <- drones_20_tidy %>%
  dplyr::filter(stringr::str_detect(biome, ";")) %>%
  dplyr::summarise(count = dplyr::n())

num_multiple_biomes_r <- robots_20_tidy %>%
  dplyr::filter(stringr::str_detect(biome, ";")) %>%
  dplyr::summarise(count = dplyr::n())

# Change biome to multiple
drones_20_tidy <- drones_20_tidy %>%
  dplyr::mutate(biome = dplyr::if_else(stringr::str_detect(biome, ";"), "multiple", biome))

# Summarise the number of publications for each biome and year
drone_20_biome_publications <- drones_20_tidy %>%
  dplyr::group_by(biome, publication_year, decade) %>%
  dplyr::summarise(number_publications = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate()

# Change biome to multiple
robots_20_tidy <- robots_20_tidy %>%
  dplyr::mutate(biome = dplyr::if_else(stringr::str_detect(biome, ";"), "multiple", biome))

# Summarise the number of publications for each biome and year
robots_20_biome_publications <- robots_20_tidy %>%
  dplyr::group_by(biome, publication_year, decade) %>%
  dplyr::summarise(number_publications = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate()


# Define ecosystem colors
biome_colours <- c(
  "Forests" = "#266e5d", 
  "Shrublands/Grasslands/Savanna/Woodlands" = "#68b38c", 
  "Deserts" = "#dbb479",
  "Deltas/Estuaries/Mangroves" = "#a8662f",
  "Lakes" = "#f28e30",
  "Rivers/Streams" = "#bf62a4",
  "Marine" = "#5d95d9",
  "Coral reefs" = "#7c58ad",
  "Urban" = "#a5a6b5",
  "multiple" = "#a6cee3",
  "NA" = "#a5a6b5"
)


# Stacking order
order <- c("Forests",
           "Shrublands/Grasslands/Savanna/Woodlands",
           "Deserts",
           "Deltas/Estuaries/Mangroves",
           "Lakes",
           "Rivers/Streams",
           "Marine",
           "Coral reefs",
           "Urban",
           "multiple",
           "NA"
)

# Save as high-resolution PNG
png("drones_pub_history.png", width = 4000, height = 1500, res = 400)  # Set size and resolution

drone_20_biome_publications %>% 
  dplyr::arrange(number_publications) %>%
  dplyr::mutate(biome = factor(biome, levels = order)) %>% 
  ggplot(aes(publication_year, number_publications, fill = biome)) +
  #geom_area(alpha = 0.6) +
  ggstream::geom_stream(type = "ridge", n_grid = 15, alpha = 0.6,
                        colour = "white",
                        linewidth = 0) +
  scale_fill_manual(values = biome_colours) +
  scale_y_continuous(position = "right",
                     limits = c(0, 175)) +
  scale_x_continuous(
    breaks = c(1995, 2000, 2010, 2013, 2015, 2019, 2020),   
    limits = c(1995, 2024)              
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

# Close the PNG device
dev.off()

drone_biome_publications_sum <- drone_biome_publications %>% 
  dplyr::group_by(publication_year) %>% 
  dplyr::summarise(number_publications = sum(number_publications), .groups = "drop")

drone_biome_publications_sum %>% 
  ggplot(aes(publication_year, number_publications)) +
  geom_line()

# Save as high-resolution PNG
png("robots_pub_history.png", width = 4000, height = 1500, res = 400)  # Set size and resolution

robots_20_biome_publications %>% 
  dplyr::arrange(number_publications) %>%
  dplyr::mutate(biome = factor(biome, levels = order)) %>% 
  ggplot(aes(publication_year, number_publications, fill = biome)) +
  #geom_area(alpha = 0.6) +
  ggstream::geom_stream(type = "ridge",
                        n_grid = 1000,
                        true_range = "max_x",
                        alpha = 0.6,
                        colour = "white",
                        linewidth = 0) +
  scale_fill_manual(values = biome_colours) +
  scale_y_continuous(position = "right",
                     limits = c(0, 175)) +
  scale_x_continuous(
    breaks = c(1995, 2000, 2010, 2013, 2015, 2019, 2020),  
    limits = c(1995, 2023)                
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )



# Close the PNG device
dev.off()

robots_biome_publications_sum <- robots_biome_publications %>% 
  dplyr::group_by(publication_year) %>% 
  dplyr::summarise(number_publications = sum(number_publications), .groups = "drop")

robots_biome_publications_sum %>% 
  ggplot(aes(publication_year, number_publications)) +
  geom_line()


#######################################################################
#Fig 3. Create a world map of case number for 20% paper               #
#######################################################################
library(rworldmap)
library(RColorBrewer)
library(maps)
library(fields)

drones_20$Country_ISO3<-gsub("Frence", "France", drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("United States of America \\(the\\)", "United States of America", drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Netherlands \\(the\\)" ,"Netherlands" , drones_20$Country_ISO3)
#drones_20$Country_ISO3<-gsub("French Polynesia"  ,"French Polynesia" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Korea \\(the Republic of\\)" ,"South Korea" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Iran \\(Islamic Republic of\\)"  ,"Iran" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Philippines \\(the\\)"   ,"Philippines"  , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("United Kingdom of Great Britain and Northern Ireland \\(the\\)", "United Kingdom", drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Russian Federation \\(the\\)","Russia"  , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub( "Czechia" ,"Czech Republic" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub( "Congo \\(the\\)","Democratic Republic of the Congo" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub(  "Cayman Islands \\(the\\)" ,"Cayman Islands" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Cabo Verde","Cape Verde" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Bolivia \\(Plurinational State of\\)","Bolivia" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Hong Kong","Hong Kong S.A.R." , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Serbia","Republic of Serbia" , drones_20$Country_ISO3)
drones_20$Country_ISO3<-gsub("Taiwan \\(Province of China\\)","Taiwan" , drones_20$Country_ISO3)

#robots_20
robots_20$Country_ISO3<-gsub("Frence", "France", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Azores", "Portugal", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("United States of America \\(the\\)", "United States of America", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Netherlands \\(the\\)" ,"Netherlands" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Korea \\(the Republic of\\)" ,"South Korea" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Iran \\(Islamic Republic of\\)"  ,"Iran" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Philippines \\(the\\)"   ,"Philippines"  , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("United Kingdom of Great Britain and Northern Ireland \\(the\\)", "United Kingdom", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Russian Federation \\(the\\)","Russia"  , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub( "Czechia" ,"Czech Rep." , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub( "Congo \\(the\\)","Democratic Republic of the Congo" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub(  "Cayman Islands \\(the\\)" ,"Cayman Is." , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Cabo Verde","Cape Verde" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Virgin Islands \\(U.S.\\)","United States Virgin Islands", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Niger \\(the\\)" ,"Niger" , robots_20$Country_ISO3)
#robots_20$Country_ISO3<-gsub("Solomon Islands" ,"Solomon Islands" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Taiwan \\(Province of China\\)" ,"Taiwan" , robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Dominican Republic \\(the\\)","Dominican Republic", robots_20$Country_ISO3)
robots_20$Country_ISO3<-gsub("Micronesia \\(Federated States of\\)" , "Federated States of Micronesia" , robots_20$Country_ISO3)

Country_drones_20<-unlist(strsplit(drones_20$Country_ISO3, "; "))
Country_robots_20<-unlist(strsplit(robots_20$Country_ISO3, "; "))

Country_drones_20<-table(Country_drones_20)
Country_robots_20<-table(Country_robots_20)

df_drones_20 <- data.frame(country = names(Country_drones_20), samples = as.vector(Country_drones_20))
df_robots_20 <- data.frame(country = names(Country_robots_20), samples = as.vector(Country_robots_20))

df_drones_20$Proportion=df_drones_20$samples/sum(df_drones_20$samples)*100
df_robots_20$Proportion=df_robots_20$samples/sum(df_robots_20$samples)*100  
# Merge with world map data
world_map_drones_20 <- joinCountryData2Map(df_drones_20, joinCode = "NAME", nameJoinColumn = "country")
world_map_robots_20 <- joinCountryData2Map(df_robots_20, joinCode = "NAME", nameJoinColumn = "country")

#########################
# check country names failed to match
unique_country_codes_drones_20 <- unique(df_drones_20$country)
unique_country_codes_robots_20 <- unique(df_robots_20$country)
# Get unique country codes from the map data
map_country_codes_drones_20 <- joinCountryData2Map(df_drones_20, joinCode = "NAME", nameJoinColumn = "country")$ADMIN
mismatched_codes_drones_20 <- setdiff(unique_country_codes_drones_20, map_country_codes_drones_20)
print(mismatched_codes_drones_20)

map_country_codes_robots_20 <- joinCountryData2Map(df_robots_20, joinCode = "NAME", nameJoinColumn = "country")$ADMIN
mismatched_codes_robots_20 <- setdiff(unique_country_codes_robots_20, map_country_codes_robots_20)
print(mismatched_codes_robots_20)

#######################################
# Create a color palette
#color_palette <- colorRampPalette((brewer.pal(9, "YlOrRd")))(length(unique(df$samples)))
#color_palette <- colorRampPalette(brewer.pal(9, "Oranges"))(100) # Adjust pale-to-dark greenish-blue
#color_palette <- colorRampPalette(c("#F7F4F9", "#D4B9DA", "#C994C7", "#DF65B0", "#980043"))(100)
#color_palette <- colorRampPalette(c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"))(100)
# Create a teal color palette
color_palette <- colorRampPalette(c("#e0f3f3", "#abd9d9", "#74c0c0", "#2b8c8c", "#005555"))(100)


# Calculate the consistent color range across both datasets
zlim <- range(c(df_drones_20$Proportion, df_robots_20$Proportion), na.rm = TRUE)
zlim_adjusted <- c(floor(zlim[1] / 5) * 5, ceiling(zlim[2] / 5) * 5)
# Define fixed breaks based on the combined range
breaks <- seq(zlim_adjusted[1], zlim_adjusted[2], length.out = 8)  # Adjust the number of intervals as needed

###########################################
# Create a two-panel plot with consistent color bar

tiff("Geographic_Distribution_DR_20.tiff", unit="in", width=6, height=5, res=600, pointsize=10)
# Set layout for 2 panels with extra space between them
par(mfrow=c(1, 2), oma=c(0, 1, 0, 3), mar=c(0, 0, 0, 1)) # Outer margins and panel spacing

# Plot for drones_20
mapCountryData(
  world_map_drones_20,
  nameColumnToPlot = "Proportion",
  catMethod = breaks,
  mapTitle = "Drones",
  addLegend = FALSE, # Disable individual legend
  colourPalette = color_palette,
  borderCol = "darkgrey", # Set country borders to black
  lwd = 0.3           # Adjust line thickness
)
# Plot for robots_20
mapCountryData(
  world_map_robots_20,
  nameColumnToPlot = "Proportion",
  catMethod = breaks,
  mapTitle = "robots_20",
  addLegend = FALSE, # Disable individual legend
  colourPalette = color_palette,
  borderCol = "darkgrey", # Set country borders to black
  lwd = 0.3           # Adjust line thickness
)

# Add a shared vertical color bar with adjustments
# Add a smaller vertical color bar
image.plot(
  zlim = zlim_adjusted, # Use the consistent color range
  legend.only = TRUE,
  col = color_palette,
  horizontal = FALSE, # Vertical color bar
  legend.mar = 2, # Margin around legend
  legend.line = 3, # Line spacing for legend ticks
  legend.cex = 0.7, # Resize the legend text
  smallplot = c(0.92, 0.94, 0.4, 0.6) # Adjust size and position of the color bar
)
# Add horizontal text at the top of the color bar
mtext(
  "Percentage (%)", # Text for the label
  side = 3,         # Top side of the plot
  line = -11,        # Adjust line position (negative brings it closer to the color bar)
  at = 0.98,        # Horizontal alignment (center it relative to the bar)
  cex = 0.9,        # Text size
  adj = -0.9        # Center alignment
)

dev.off()



#######################################################################
#Fig 4. Create a world map of case number for 20% paper               #
#######################################################################

DR_20<-twenty_percent_paper

D_20<-subset(DR_20, DR_20$Instrument=="Drones")
R_20<-subset(DR_20, DR_20$Instrument=="Robots")

collapse_table<- function(DR_20) {
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
                             "Robot")) {
          
          
          
          contains_all <- grepl(taxa, DR_20$Taxa_ITIS) & grepl(ecosystem, DR_20$Biome) &
            grepl(subd, DR_20$Subdiscipline) & grepl(Instrument, DR_20$Instrument)
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
Data_20<-collapse_table(DR_20)
Data_20$Ecosystem[which(Data_20$Ecosystem=="Shrublands/Grasslands/Savanna/Woodlands")]<-"Shrublands/Grasslands"
# Create a new data frame with the count of combinations of Biome and Subdiscipline under each level of Taxonomy
X_comb_20 <- as.data.frame(table(Data_20))
colnames(X_comb_20) <- c("Taxonomy", "Biome", "Subdiscipline","Instrument", "Freq")
X_comb_20$Subdiscipline<-factor(X_comb_20$Subdiscipline, levels=c("Landscape","Ecosystem", "Community",
                                                            "Population","Behavior","Physiology"))
X_comb_20$Taxonomy<- factor(X_comb_20$Taxonomy, levels=c("Plantae", "Animalia","Bacteria","Fungi",
                                                   "Protista","Archaea","Chromista"))
X_comb_20$Biome<-factor(X_comb_20$Biome,levels = c("Forests", 
                                             "Shrublands/Grasslands",
                                             "Tundra",
                                             "Rivers/Streams",
                                             "Lakes",
                                             "Deltas/Estuaries/Mangroves",
                                             "Coral reefs",
                                             "Marine"))
X_comb_20$Instrument<-factor(X_comb_20$Instrument,levels = c("Drones", 
                                                       "Robot"))
X_comb_20$Percentage<-X_comb_20$Freq/sum(X_comb$Freq)*100
XX_comb_20<-subset(X_comb_20,Taxonomy=="Plantae"|Taxonomy=="Animalia")


tiff("Result/Application fequency-Drones and Robots.tif", unit="in",width = 5.5, height =4, res= 600,pointsize = 10)
ggplot(XX_comb_20, aes(x = Biome, y = Subdiscipline, fill = Percentage)) +
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

taxa<- c("Archaea",
         "Chromista",
         "Fungi",
         "Protista",
         "Bacteria",
         "Animalia",
         "Plantae"
         )

          
tiff("Result/Tax_percentage.tif", unit="in",width = 2, height =3, res= 600,pointsize = 10)
Tax_data_20 <- as.data.frame(table(Data_20$Taxonomy))
Tax_data_20$Percentage<-Tax_data_20$Freq/sum(Tax_data_20$Freq)*100
colnames(Tax_data_20)<-c("Taxonomy", "Freq","Percentage (%)")
Tax_data_20$Taxonomy <- as.character(Tax_data_20$Taxonomy)
# merge with full taxa list
Tax_data_20 <- merge(
  data.frame(Taxonomy = taxa, stringsAsFactors = FALSE),
  Tax_data_20,
  by = "Taxonomy",
  all.x = TRUE
)
# replace NAs (for missing taxa) with 0
Tax_data_20$Freq[is.na(Tax_data_20$Freq)] <- 0
Tax_data_20$`Percentage (%)`[is.na(Tax_data_20$`Percentage (%)`)] <- 0


Tax_data_20$Taxonomy <- factor(Tax_data_20$Taxonomy, levels = taxa)


# Horizontal bar plot
ggplot(Tax_data_20, aes(x = Taxonomy, y = `Percentage (%)`)) +
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
#Chi-squared test for distribution of biome, geographic location and taxonomy
###############################################################################

#Taxonomy: 
#compare Tax_data_20 (in this script) with Tax_data (in script 'Fig. 4.R')
chisq.test(x = Tax_data_20$Freq, p = Tax_data$`Percentage (%)`/100)

## Chi-square GOF with simulated p-value (recommended)

chisq.test(Tax_data_20$Freq,
           p = Tax_data$Percentage/100,
           simulate.p.value = TRUE, B = 1e6)

tab2k <- rbind(Tax_data_20$Freq, Tax_data$Freq)
V <- sqrt(13.587 / (sum(tab2k) * (min(dim(tab2k)) - 1)))
V

#Biome
#compare Biome column in Data_20 (in this script) with that in Data (in script 'Fig. 4.R')
Biome_data_20 <- as.data.frame(table(Data_20$Ecosystem))


Biome_data<- as.data.frame(table(Data$Ecosystem))

Biome_data_20$Percentage<-Biome_data_20$Freq/sum(Biome_data_20$Freq)*100
Biome_data$Percentage<-Biome_data$Freq/sum(Biome_data$Freq)*100

colnames(Biome_data_20)<-c("Biome", "Freq","Percentage (%)")
colnames(Biome_data)<-c("Biome", "Freq","Percentage (%)")


chisq.test(x = Biome_data_20$Freq, p = Biome_data$`Percentage (%)`/100)
chisq.test(Biome_data_20$Freq,
           p = Biome_data$Percentage/100,
           simulate.p.value = TRUE, B = 1e6)

tab2k <- rbind(Biome_data_20$Freq, Biome_data$Freq)
V <- sqrt(4.2534 / (sum(tab2k) * (min(dim(tab2k)) - 1)))
V

#Scale
#compare subiscipline column in Data_20 (in this script) with that in Data (in script 'Fig. 4.R')
Scale_data_20 <- as.data.frame(table(Data_20$Subdiscipline))
Scale_data<- as.data.frame(table(Data$Subdiscipline))

Scale_data_20$Percentage<-Scale_data_20$Freq/sum(Scale_data_20$Freq)*100
Scale_data$Percentage<-Scale_data$Freq/sum(Scale_data$Freq)*100

colnames(Scale_data_20)<-c("Scale", "Freq","Percentage (%)")
colnames(Scale_data)<-c("Scale", "Freq","Percentage (%)")


chisq.test(x = Scale_data_20$Freq, p = Scale_data$`Percentage (%)`/100)
chisq.test(Scale_data_20$Freq,
           p = Scale_data$Percentage/100,
           simulate.p.value = TRUE, B = 1e6)

tab2k <- rbind(Scale_data_20$Freq, Scale_data$Freq)
V <- sqrt(8.8701 / (sum(tab2k) * (min(dim(tab2k)) - 1)))
V

#Geographic location
#apply drones_20, robots_20 (in this script)and drones, robots (in script 'Fig. 4.R')

Country_20<-rbind(drones_20,robots_20)
Country<-rbind(drones,robots)

Country_20<-unlist(strsplit(Country_20$Country_ISO3, "; "))
Country<-unlist(strsplit(Country$Country_ISO3, "; "))

Country_20<-table(Country_20)
Country<-table(Country)

df_Country_20 <- data.frame(country = names(Country_20), samples = as.vector(Country_20))
df_Country <- data.frame(country = names(Country), samples = as.vector(Country))

df_Country_20$Proportion=df_Country_20$samples/sum(df_Country_20$samples)*100
df_Country$Proportion=df_Country$samples/sum(df_Country$samples)*100

# merge with full country list from full samlple
df_Country_20<- merge(
  data.frame(country = df_Country$country, stringsAsFactors = FALSE),
  df_Country_20,
  by = "country",
  all.x = TRUE
)
# replace NAs (for missing taxa) with 0
df_Country_20$samples[is.na(df_Country_20$samples)] <- 0
df_Country_20$Proportion[is.na(df_Country_20$Proportion)] <- 0

chisq.test(x = df_Country_20$samples, p =df_Country$Proportion/100)
chisq.test(country_data_20$samples,
           p =df_drones$Proportion/100,
           simulate.p.value = TRUE, B = 1e6)

tab2k <- rbind(df_Country_20$samples, df_Country$samples)
V <- sqrt(55.499 / (sum(tab2k) * (min(dim(tab2k)) - 1)))
V
write.csv(df_Country,"Publication number sorted by country.csv")

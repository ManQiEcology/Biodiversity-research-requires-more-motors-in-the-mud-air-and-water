#this is for data analysis
rm (list=ls ())
library(here)
setwd(here::here())
library(fields)

library(readxl)
robots<-read_excel("Data/Robots Jan 18.xlsx")
drones<-read_excel("Data/Drones Jan 18.xlsx")
col1<- rgb(108, 166, 205, max = 255, alpha = 125, names = "skyblue50")
col2<- rgb(34, 139, 34, max = 255, alpha = 125, names = "forestgreen_50")

drones_sub<-drones[c(13:18)] #c(17, 20, 23, 25, 28, 31) give the variables extracted
robots_sub<-robots[c(13:18)]
drones_ssub<-drones[c(13:18,8)] #c(17, 20, 23, 25, 28, 31) give the variables extracted, 12 give the publication year
robots_ssub<-robots[c(13:18,8)]



#categorize papers by decade they pubished in
yr2decade<-function(x) {
  y<-ifelse (x>=1990 & x<2000, 1990,x) 
  y<-ifelse (y>=2000 & x<2010, 2000,y) 
  y<-ifelse (y>=2010 & x<2020, 2010,y)
  y<-ifelse (y>=2020 & x<2030, 2020,y)
  return(y)
}
drones_ssub$Decade<-(yr2decade(drones_ssub$`Publication Year`))
robots_ssub$Decade<-(yr2decade(robots_ssub$`Publication Year`))

#define factor variables
robots_ssub$Application_category<-factor(robots_ssub$Application_Category)
robots_ssub$Environmental_variable<-factor(robots_ssub$Environmental_Variable)
robots_ssub$Taxa_ITIS<-factor(robots_ssub$Taxa_ITIS )
robots_ssub$Subdiscipline<-factor(robots_ssub$Subdiscipline )
robots_ssub$Biome<-factor(robots_ssub$Biome )
robots_ssub$Country_ISO3<-factor(robots_ssub$Country_ISO3 )
robots_ssub$Decade<-factor(robots_ssub$Decade )

drones_ssub$Application_category<-factor(drones_ssub$Application_Category)
drones_ssub$Environmental_variable<-factor(drones_ssub$Environmental_Variable)
drones_ssub$Taxa_ITIS<-factor(drones_ssub$Taxa_ITIS )
drones_ssub$Subdiscipline<-factor(drones_ssub$Subdiscipline )
drones_ssub$Biome<-factor(drones_ssub$Biome )
drones_ssub$Country_ISO3<-factor(drones_ssub$Country_ISO3 )
drones_ssub$Decade<-factor(drones_ssub$Decade )

setwd(paste(here(),"/Result/",sep=""))

################################################################################
# Fig. 1  Time series of publication
################################################################################
library(janitor)
library(stringr)
library(ggplot2)
library(ggstream)

drones_tidy <- drones_ssub %>% 
  janitor::clean_names() %>% 
  dplyr::select(biome,
                subdiscipline,
                publication_year,
                decade)

robots_tidy <- robots_ssub %>% 
  janitor::clean_names() %>% 
  dplyr::select(biome,
                subdiscipline,
                publication_year,
                decade)

# How many studies cover more than one biome?
num_multiple_biomes <- drones_tidy %>%
  dplyr::filter(stringr::str_detect(biome, ";")) %>%
  dplyr::summarise(count = dplyr::n())

num_multiple_biomes_r <- robots_tidy %>%
  dplyr::filter(stringr::str_detect(biome, ";")) %>%
  dplyr::summarise(count = dplyr::n())

# Change biome to multiple
drones_tidy <- drones_tidy %>%
  dplyr::mutate(biome = dplyr::if_else(stringr::str_detect(biome, ";"), "multiple", biome))

# Summarise the number of publications for each biome and year
drone_biome_publications <- drones_tidy %>%
  dplyr::group_by(biome, publication_year, decade) %>%
  dplyr::summarise(number_publications = dplyr::n(), .groups = "drop") %>% 
  dplyr::mutate()

# Change biome to multiple
robots_tidy <- robots_tidy %>%
  dplyr::mutate(biome = dplyr::if_else(stringr::str_detect(biome, ";"), "multiple", biome))

# Summarise the number of publications for each biome and year
robots_biome_publications <- robots_tidy %>%
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

drone_biome_publications %>% 
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

robots_biome_publications %>% 
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
#Fig 3. Create a world map of case number                                     #
#######################################################################
library(rworldmap)
library(RColorBrewer)
library(maps)
library(fields)
drones$Country_ISO3<-gsub("Frence", "France", drones$Country_ISO3)
drones$Country_ISO3<-gsub("United States of America \\(the\\)", "United States of America", drones$Country_ISO3)
drones$Country_ISO3<-gsub("Netherlands \\(the\\)" ,"Netherlands" , drones$Country_ISO3)
#drones$Country_ISO3<-gsub("French Polynesia"  ,"French Polynesia" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Korea \\(the Republic of\\)" ,"South Korea" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Iran \\(Islamic Republic of\\)"  ,"Iran" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Philippines \\(the\\)"   ,"Philippines"  , drones$Country_ISO3)
drones$Country_ISO3<-gsub("United Kingdom of Great Britain and Northern Ireland \\(the\\)", "United Kingdom", drones$Country_ISO3)
drones$Country_ISO3<-gsub("Russian Federation \\(the\\)","Russia"  , drones$Country_ISO3)
drones$Country_ISO3<-gsub( "Czechia" ,"Czech Republic" , drones$Country_ISO3)
drones$Country_ISO3<-gsub( "Congo \\(the\\)","Democratic Republic of the Congo" , drones$Country_ISO3)
drones$Country_ISO3<-gsub(  "Cayman Islands \\(the\\)" ,"Cayman Islands" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Cabo Verde","Cape Verde" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Bolivia \\(Plurinational State of\\)","Bolivia" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Hong Kong","Hong Kong S.A.R." , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Serbia","Republic of Serbia" , drones$Country_ISO3)
drones$Country_ISO3<-gsub("Taiwan \\(Province of China\\)","Taiwan" , drones$Country_ISO3)
 
#robots
robots$Country_ISO3<-gsub("Frence", "France", robots$Country_ISO3)
robots$Country_ISO3<-gsub("Azores", "Portugal", robots$Country_ISO3)
robots$Country_ISO3<-gsub("United States of America \\(the\\)", "United States of America", robots$Country_ISO3)
robots$Country_ISO3<-gsub("Netherlands \\(the\\)" ,"Netherlands" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Korea \\(the Republic of\\)" ,"South Korea" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Iran \\(Islamic Republic of\\)"  ,"Iran" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Philippines \\(the\\)"   ,"Philippines"  , robots$Country_ISO3)
robots$Country_ISO3<-gsub("United Kingdom of Great Britain and Northern Ireland \\(the\\)", "United Kingdom", robots$Country_ISO3)
robots$Country_ISO3<-gsub("Russian Federation \\(the\\)","Russia"  , robots$Country_ISO3)
robots$Country_ISO3<-gsub( "Czechia" ,"Czech Rep." , robots$Country_ISO3)
robots$Country_ISO3<-gsub( "Congo \\(the\\)","Democratic Republic of the Congo" , robots$Country_ISO3)
robots$Country_ISO3<-gsub(  "Cayman Islands \\(the\\)" ,"Cayman Is." , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Cabo Verde","Cape Verde" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Virgin Islands \\(U.S.\\)","United States Virgin Islands", robots$Country_ISO3)
robots$Country_ISO3<-gsub("Niger \\(the\\)" ,"Niger" , robots$Country_ISO3)
#robots$Country_ISO3<-gsub("Solomon Islands" ,"Solomon Islands" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Taiwan \\(Province of China\\)" ,"Taiwan" , robots$Country_ISO3)
robots$Country_ISO3<-gsub("Dominican Republic \\(the\\)","Dominican Republic", robots$Country_ISO3)
robots$Country_ISO3<-gsub("Micronesia \\(Federated States of\\)" , "Federated States of Micronesia" , robots$Country_ISO3)

Country_drones<-unlist(strsplit(drones$Country_ISO3, "; "))
Country_robots<-unlist(strsplit(robots$Country_ISO3, "; "))

Country_drones<-table(Country_drones)
Country_robots<-table(Country_robots)

df_drones <- data.frame(country = names(Country_drones), samples = as.vector(Country_drones))
df_robots <- data.frame(country = names(Country_robots), samples = as.vector(Country_robots))

df_drones$Proportion=df_drones$samples/sum(df_drones$samples)*100
df_robots$Proportion=df_robots$samples/sum(df_robots$samples)*100  
# Merge with world map data
world_map_drones <- joinCountryData2Map(df_drones, joinCode = "NAME", nameJoinColumn = "country")
world_map_robots <- joinCountryData2Map(df_robots, joinCode = "NAME", nameJoinColumn = "country")

#########################
# check country names failed to match
unique_country_codes_drones <- unique(df_drones$country)
unique_country_codes_robots <- unique(df_robots$country)
# Get unique country codes from the map data
map_country_codes_drones <- joinCountryData2Map(df_drones, joinCode = "NAME", nameJoinColumn = "country")$ADMIN
mismatched_codes_drones <- setdiff(unique_country_codes_drones, map_country_codes_drones)
print(mismatched_codes_drones)

map_country_codes_robots <- joinCountryData2Map(df_robots, joinCode = "NAME", nameJoinColumn = "country")$ADMIN
mismatched_codes_robots <- setdiff(unique_country_codes_robots, map_country_codes_robots)
print(mismatched_codes_robots)

#######################################
# Create a color palette
#color_palette <- colorRampPalette((brewer.pal(9, "YlOrRd")))(length(unique(df$samples)))
#color_palette <- colorRampPalette(brewer.pal(9, "Oranges"))(100) # Adjust pale-to-dark greenish-blue
#color_palette <- colorRampPalette(c("#F7F4F9", "#D4B9DA", "#C994C7", "#DF65B0", "#980043"))(100)
#color_palette <- colorRampPalette(c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"))(100)
# Create a teal color palette
color_palette <- colorRampPalette(c("#e0f3f3", "#abd9d9", "#74c0c0", "#2b8c8c", "#005555"))(100)


# Calculate the consistent color range across both datasets
zlim <- range(c(df_drones$Proportion, df_robots$Proportion), na.rm = TRUE)
zlim_adjusted <- c(floor(zlim[1] / 5) * 5, ceiling(zlim[2] / 5) * 5)
# Define fixed breaks based on the combined range
breaks <- seq(zlim_adjusted[1], zlim_adjusted[2], length.out = 8)  # Adjust the number of intervals as needed

###########################################
# Create a two-panel plot with consistent color bar

tiff("Geographic_Distribution_DR.tiff", unit="in", width=6, height=5, res=600, pointsize=10)
# Set layout for 2 panels with extra space between them
par(mfrow=c(1, 2), oma=c(0, 1, 0, 3), mar=c(0, 0, 0, 1)) # Outer margins and panel spacing

# Plot for drones
mapCountryData(
  world_map_drones,
  nameColumnToPlot = "Proportion",
  catMethod = breaks,
  mapTitle = "Drones",
  addLegend = FALSE, # Disable individual legend
  colourPalette = color_palette,
  borderCol = "darkgrey", # Set country borders to black
  lwd = 0.3           # Adjust line thickness
)
# Plot for robots
mapCountryData(
  world_map_robots,
  nameColumnToPlot = "Proportion",
  catMethod = breaks,
  mapTitle = "Robots",
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







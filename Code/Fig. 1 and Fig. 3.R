#this is for data analysis
rm (list=ls ())
library(here)
setwd(here())
library(fields)

library(readxl)
robots<-read_excel("Result/Robots Jan 18.xlsx")
drones<-read_excel("Result/Drones Jan 18.xlsx")
col1<- rgb(108, 166, 205, max = 255, alpha = 125, names = "skyblue50")
col2<- rgb(34, 139, 34, max = 255, alpha = 125, names = "forestgreen_50")

drones_sub<-drones[c(17, 20, 23, 25, 28, 31)] #c(17, 20, 23, 25, 28, 31) give the variables extracted
robots_sub<-robots[c(17, 20, 23, 25, 28, 31)]
drones_ssub<-drones[c(17, 20, 23, 25, 28, 31,12)] #c(17, 20, 23, 25, 28, 31) give the variables extracted, 12 give the publication year
robots_ssub<-robots[c(17, 20, 23, 25, 28, 31,12)]



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

##############################################
#plot Number of publications over time           #
##############################################
robot_yr_min <- min(robots$`Publication Year`,na.rm = T)-0.5
robot_yr_max <- max(robots$`Publication Year`,na.rm = T)+0.5
drone_yr_min <- min(drones$`Publication Year`,na.rm = T)-0.5
drone_yr_max <- max(drones$`Publication Year`,na.rm = T)+0.5
Year_min<-min(robot_yr_min,drone_yr_min)
Year_max<-max(robot_yr_max,drone_yr_max)
ax<-seq (Year_min,Year_max,1)

hist_robots <- hist(robots$`Publication Year`, breaks = ax, plot = FALSE) # Save first histogram data
hist_drones <- hist(drones$`Publication Year`, breaks = ax, plot = FALSE) # Save 2nd histogram data
ymax<-max(hist_drones$counts,hist_drones$counts)

#plot the Number of publications over time
tiff("Number of publications_smaller pointsize.tiff", unit="in",width = 4, height =1.5, res= 600,pointsize = 5 )

plot(hist_drones, col = col1,xlab = "Year", ylab="Number of publications",
     main= "",ylim=c(0,200)) # Plot 1st histogram using a transparent color, ymax=159, approx 200
plot(hist_robots, col = col2,  add = TRUE) # Add 2nd histogram using different color
dev.off()

tiff("Number of publications.tiff", unit="in",width = 4, height =3, res= 600,pointsize = 10 )

plot(hist_drones, col = col1,xlab = "Year", ylab="Number of publications",
     main= "",ylim=c(0,200)) # Plot 1st histogram using a transparent color
plot(hist_robots, col = col2,  add = TRUE) # Add 2nd histogram using different color
legend(1995,150,c("Drones","Robots"),fill=c(col1,col2), box.lty=0)
dev.off()


tiff("Number of publications_drones.tiff", unit="in",width = 4, height =3, res= 600,pointsize = 10 )
plot(hist_drones, col = col1,xlab = "Year", ylab="Number of publications",
     main= "",ylim=c(0,200)) # Plot 1st histogram using a transparent color
#plot(hist_robots, col = col2,  add = TRUE) # Add 2nd histogram using different color
#legend(1995,150,c("Drones","Robots"),fill=c(col1,col2), box.lty=0)
legend(1995,150,c("Drones"),fill=c(col1), box.lty=0)
dev.off()

tiff("Number of publications_robots.tiff", unit="in",width = 4, height =3, res= 600,pointsize = 10 )
plot(hist_robots, col = col2,xlab = "Year", ylab="Number of publications",
     main= "",ylim=c(0,200)) # Plot 1st histogram using a transparent color
#plot(hist_robots, col = col2,  add = TRUE) # Add 2nd histogram using different color
#legend(1995,150,c("Drones","Robots"),fill=c(col1,col2), box.lty=0)
legend(1995,150,c("Robots"),fill=c(col2), box.lty=0)
dev.off()



#plot number of shared paper
tiff("Number of publications_shared papers.tiff", unit="in",width = 4, height =3, res= 600,pointsize = 10 )
shared_papers<-intersect(robots$`UT (Unique WOS ID)`,drones$`UT (Unique WOS ID)`)

publication_yr<-robots$`Publication Year`[which(robots$`UT (Unique WOS ID)` %in% shared_papers)] 
shared_papers<-data.frame(
  paper_doi=shared_papers,
  year=publication_yr
)

hist_shared_papers<-hist(shared_papers$year,breaks=ax,plot = FALSE)
plot(hist_shared_papers, col = "purple",ylim=c(0,200),xlab = "Year", ylab="Number of publications",
     main= "")
legend(1995,150,c("Drones","Robots","Both"),fill=c(col1,col2,"purple"), box.lty=0)    
dev.off()


#######################################################################
#analyze the variables                                                #
#######################################################################
library(tidyr)
library(dplyr)
library(VennDiagram)
library(venneuler)
library(reshape2) # install reshapse 2 if not yet
library(vegan)

#count number of levels
 #for drones
drones_ssub[drones_ssub=="NA"]<-NA
robots_ssub[robots_ssub=="NA"]<-NA


variables<-c("Application_category","Biome","Subdiscipline","Taxa_ITIS","Environmental_Variable")
merge_fill<-function(x) {
  D_data<-drones_ssub[c(x,"Decade")]
  D_x<-as.data.frame(table(D_data))
  D_x$Model<-"Drones"
  
  R_data<-robots_ssub[c(x,"Decade")]
  R_x<-as.data.frame(table(R_data))
  R_x$Model<-"Robots"
  
  Data<-rbind(D_x,R_x)
  colnames(Data)<-c("Var","Decade","Freq","Model")
  # Create data frame with all combinations of levels of X and Y
  all_combinations <- expand.grid(Var = unique(Data$Var), 
                                  Model = unique(Data$Model), 
                                  Decade=unique(Data$Decade))
  # Merge with original data frame
result <- merge(all_combinations, Data, by = c("Var", "Model","Decade"), all.x = TRUE)
# Replace NA values in Z column with 0
result$Freq[is.na(result$Freq)] <- 0
#colnames(result)<- c(x, "Model","Decade","Freq")
return(result)
}

Application_category<-merge_fill(variables[1])
Biome<-merge_fill(variables[2])
Subdiscipline<-merge_fill(variables[3])
Taxa_ITIS<-merge_fill(variables[4])
Environmental_Variable<-merge_fill(variables[5])


#Adjust row names
Application_category$Var<-gsub("; ", "&", Application_category$Var)
Biome$Var<-gsub("; ", "&", Biome$Var)
Subdiscipline$Var <-gsub("; ", "&", Subdiscipline$Var)
Taxa_ITIS$Var<-gsub("; ", "&", Taxa_ITIS$Var)
Environmental_Variable$Var<-gsub("; ", "&", Environmental_Variable$Var)


#automatically plot ven diagram of a specific variable x
set_yr<-function(x,m){
  drones<-subset(x,x$Model=="Drones")
  robots<-subset(x,x$Model=="Robots")
  #turn to wide form
  drones<-dcast(drones, Var ~ Decade, value.var = "Freq")
  robots<-dcast(robots, Var ~ Decade, value.var = "Freq")
  for (k in c("drones", "robots")) {
    y<-get(k)
    
    # Convert relevant part of the data frame into a matrix
    data_matrix <- as.matrix(y[, -1])
    # Create the contingency table
    contingency_tbl <- as.table(data_matrix)
    # Set row names to 'Var' column
    rownames(contingency_tbl) <- y$Var
    # Print the contingency table
    y<-contingency_tbl
    j<-dim(y)[2]
    
  tiff(paste(k,m,".tiff",sep=""), unit="in",width = 4*j, height =4, res= 600,pointsize = 10 )
  par(mfrow = c(1, j))
  for (i in 1:j) {
    
    if (sum(y[,i])==0) {
      i=i+1
     } else {
      plot(venneuler(y[,i]))
    }
  }
  dev.off() 
  }
}


#plot the chart
set_yr(Application_category,"Application_category")
set_yr(Biome,"Biome")
set_yr(Subdiscipline,"Subdiscipline")
set_yr(Taxa_ITIS,"Taxa_ITIS")
set_yr(Environmental_Variable,"Environmental_Variable")

Prop<-function(x) {
  x<-aggregate(Freq ~ Model + Decade, data = x, FUN = sum)
  M<-max(x$Freq,rm.na=TRUE)
  print(paste("the maximum item is:",x$Freq[which(x$Freq==M)]))
  x$Prop<-(x$Freq/M)^0.5
  print(x)
}
Prop(Biome[grep("Marine", Biome$Var),])

#######################################################################
#Create a world map of case number                                     #
#######################################################################
library(rworldmap)
library(RColorBrewer)
library(maps)
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
color_palette <- colorRampPalette(brewer.pal(9, "Oranges"))(100) # Adjust pale-to-dark greenish-blue
#color_palette <- colorRampPalette(c("#F7F4F9", "#D4B9DA", "#C994C7", "#DF65B0", "#980043"))(100)
#color_palette <- colorRampPalette(c("#F2F0F7", "#CBC9E2", "#9E9AC8", "#756BB1", "#54278F"))(100)

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
  colourPalette = color_palette
)
# Plot for robots
mapCountryData(
  world_map_robots,
  nameColumnToPlot = "Proportion",
  catMethod = breaks,
  mapTitle = "Robots",
  addLegend = FALSE, # Disable individual legend
  colourPalette = color_palette
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








####################################################################
#CODE BELOW IS NOT USED IN FIGURE GENERATING
####################################################################
# Load world map data with ISO standard names
world_map_iso <- getMap(resolution = "coarse")
# Check the available columns in the map data

# Use ISO_A3 column for matching (assuming ISO_A3 is the ISO standard column)
world_map_matched <- joinCountryData2Map(df_robot, joinCode = "ISO_A3", nameJoinColumn = "country")



##########################################################################
unmatched_data <- setdiff(df_robot$country, world_map$NAME)

# Identify unmatched country codes in the map
unmatched_map <- setdiff(world_map$NAME, df_robot$country)

# Print the results
print("Unmatched country names in your data:")
print(unmatched_data)

print("Unmatched country codes in the map:")
print(unmatched_map)

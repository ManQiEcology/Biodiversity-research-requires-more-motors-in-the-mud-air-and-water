################################################################################
#This is the R code that was used by the paper
#——Biodiversity research requires more motors in the mud, air and water
#to extract various information from the abstract of scientific literature reporting 
#application of drones and robots in biodiversity research
################################################################################

rm (list=ls ())
setwd("E:/drone and robots")

library(readxl)
robots<-read_excel("robots-Nov 25.xls",sheet = "variables") #input your own raw dataset
drones<-read_excel("drones-Nov 25.xlsx", sheet = "variables") #input your own raw dataset


#####################################################
#extract variables                                 #
#####################################################
library(stringr)

remove_words <- function(text, words_to_remove) {
       pattern <- paste(words_to_remove, collapse="|")
       result <- gsub(pattern, "", text)
       return(result)
}


# Biome
extract_category <- function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    
    if (any(str_detect(tolower(text), categories[[category]]))) {
      label<-c(label,category)
    }
  }
  return(paste(label,collapse="; "))
}

extract_category_cap_sens <- function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    
    if (any(str_detect(text, categories[[category]]))) {
      label<-c(label,category)
    }
  }
  return(paste(label,collapse="; "))
}


extract_variable <-function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    var_lab<-NULL
    for (variable in categories[[category]]) {
      if (str_detect(tolower(text),variable)) {
        var_lab<-c(var_lab,variable)
      }
    }
    label<-c(label,var_lab)
  }
 return(paste(label,collapse="; ")) 
}

extract_variable_cap_sens <-function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    var_lab<-NULL
    for (variable in categories[[category]]) {
      if (str_detect(text,variable)) {
        var_lab<-c(var_lab,variable)
      }
    }
    label<-c(label,var_lab)
  }
  return(paste(label,collapse="; ")) 
}


extract_variable_note<-function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    var_lab_note<-NULL
    for (variable in categories[[category]]) {
      if (str_detect(tolower(text),variable)) {
        note<-show_context(tolower(text),target_word = variable,window_size = 15)
        lab_note<-paste(variable," (",note,")",sep="")
        var_lab_note<-c(var_lab_note,lab_note)
      }
    }
    label<-c(label,var_lab_note)
  }
  return(paste(label,collapse="; ")) 
}

extract_variable_note_cap_sens<-function(text) {
  label<-NULL
  for (category in names(categories)) {
    if (is.element(category, names(categories_exception))) {
      text<-remove_words(text,categories_exception[[category]])
    }
    var_lab_note<-NULL
    for (variable in categories[[category]]) {
      if (str_detect(text,variable)) {
        note<-show_context(text,target_word = variable,window_size = 15)
        lab_note<-paste(variable," (",note,")",sep="")
        var_lab_note<-c(var_lab_note,lab_note)
      }
    }
    label<-c(label,var_lab_note)
  }
  return(paste(label,collapse="; ")) 
}



show_context <- function(text, target_word=c("forest"), window_size = 15) {
  content<-NULL
  target_index <- str_locate_all(text, target_word)
  target_index<-as.data.frame(target_index)
  number_of_index<-dim(target_index)[1]

    for (i in 1:number_of_index) {
      index_i<-target_index[i,]
      index_min<-max(index_i[[1]]-window_size, 1)
      index_max<-min(index_i[[2]]+window_size, nchar(text))
      context <- substr(text,index_min,index_max)
      content<-c(content,context)
    }
  
  
  return(paste(content,collapse=" | "))
}


Biome<-list(
  "Forests"=c("forest"),
  "Shrublands/Grasslands/Savanna/Woodlands" = c("shrubland","grassland","savanna","woodland"),
  "Rivers/Streams" = c("river", "stream"),
  "Lakes" = c("lake"),
  "Deltas/Estuaries/Mangroves" = c("delta", "estuar", "mangrove",
                                   "coastal wetland", "coastal marsh", "intertidal", "salt marsh","seagrass","mudflat"),
  "Marine" = c("marine","kelp forest"),
  "Coral reefs" = c("coral reef"))
Biome_exception <-list (
  "Forests"=c("mangrove forest", "kelp forest", "seagrass forest", "coral forest","subaqueous forest", "submerged forest","random forest", "animal forest", "deforestation"),
  "Marine" = c("coral reef"),
  "Rivers/Streams" = c("driver", "streamline","streaming")
)
  
categories<-Biome
categories_exception <- Biome_exception
  
robots$Biome<-sapply(robots$Abstract, extract_category)
robots$Biome_specific<-sapply(robots$Abstract, extract_variable)
robots$Biome_specific_note<-sapply(robots$Abstract, extract_variable_note)

drones$Biome<-sapply(drones$Abstract, extract_category)
drones$Biome_specific<-sapply(drones$Abstract, extract_variable)
drones$Biome_specific_note<-sapply(drones$Abstract, extract_variable_note)
  
#Country
  library(countrycode)
  
  country_names<-read_excel("Countries_ISO3166.xlsx")
     # List of country names
  country_names<-lapply(split(country_names$Term, country_names$Country), as.character)
  country_names$`United States of America (the)`<-c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
    "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
    "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
    "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
    "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
    "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", 
    "USA"
  )
  country_names$Canada <- c(
    "Alberta","British Columbia", "Manitoba","New Brunswick",
    "Newfoundland and Labrador", "Nova Scotia","Ontario", 
    "Prince Edward Island", "Quebec", "Saskatchewan",
    "Canada"
  )
  country_names$Australia <- c(
    "New South Wales", "Victoria", "Queensland","Tasmania", "Northern Territory",
    "Australia"
  )
  country_names$`United Kingdom of Great Britain and Northern Ireland (the)`<- c(
    "United Kingdom of Great Britain and Northern Ireland",
    "Great Britain", "Endland", "Northern Ireland","Wales","Scotland"
  )
  
  country_names_exception<-list(
    "India" = c("Indian Ocean", "Indian Ridge","Indian ridge"),
    "Thailand"=c("Gulf of Thailand"),
    "China" =c("Chinare"),
    "Jersey" = c("New Jersey"),
    "Mexico" = c("New Mexico", "Gulf of Mexico"),
    "Cuba"=c("Cubage"),
    "Niger" = c("Nigeria melichthys"),
    "Oman" = c("Omanensis"),
    "Ireland" = c("Northern Ireland","northern Ireland", "Elsevier Ireland","elsevier Ireland"),
    "United Kingdom of Great Britain and Northern Ireland (the)" = c("Elsevier Ireland","elsevier Ireland","New South Wales" ),
    "Mongolia" = c("Mongolian Plateau", "Mongolian plateau",  "Inner Mongolia","inner Mongolia"),
    "Idaho" = c("Idahoensis"),
    "Israel" = c("Israelensis"),
    "Congo (the)" = c("Demorgraphic Republic of Congo"),
    "Samoa" = c("American Samoa"),
    "Tonga" =c("Tonga Arc", "Tonga Trench")
  )
    # Function to extract countries names (following ISO 3166) from the abstract
  categories<-country_names
  categories_exception <- country_names_exception
  robots$Country_ISO3<-sapply(robots$Abstract, extract_category_cap_sens)  
  robots$Country_ISO3_specific<-sapply(robots$Abstract, extract_variable_cap_sens)
  robots$Country_ISO3_specific_note<-sapply(robots$Abstract, extract_variable_note_cap_sens)
  
  drones$Country_ISO3<-sapply(drones$Abstract, extract_category_cap_sens) 
  drones$Country_ISO3_specific<-sapply(drones$Abstract, extract_variable_cap_sens)
  drones$Country_ISO3_specific_note<-sapply(drones$Abstract, extract_variable_note_cap_sens)

  #subdiscipline
  subdiscipline<-list(
    "Landscape"=c("landscape"),
    "Ecosystem"=c("ecosystem"),
    "Community"=c("community"),
    "Population"=c("population"),
    "Physiology"=c("physiolog"),
    "Behavior"=c("behavior","behaviour"),
    "Paleoecology"=c("paleoecolog")
  )
  
  categories<-subdiscipline
  categories_exception<-NULL
  
  #robots$Subdiscipline<-NA
  #drones$Subdiscipline<-NA
  
  robots_text<-paste(robots$`Article Title`, robots$Abstract,robots$`Author Keywords`, sep = " ")
  drones_text<-paste(drones$`Article Title`, drones$Abstract,drones$`Author Keywords`, sep = " ")
  
  
  robots$Subdiscipline<-sapply(robots_text, extract_category)  
  robots$Subdiscipline_note<-sapply(robots_text, extract_variable_note)
  drones$Subdiscipline<-sapply(drones_text, extract_category) 
  drones$Subdiscipline_note<-sapply(drones_text, extract_variable_note)
  
#Taxa
 library(taxize)
  get_families <- function(kingdom) {
    families <- downstream(kingdom, "family", db = "itis")
    return(families)
  }
  
  # Get families from each kingdom using ITIS
  plant_families <-get_families("Plantae")
  animal_families<-get_families("Animalia")
  fungi_families <- get_families("Fungi")
  bacteria_families <- get_families("Bacteria") #tsn=50
  archaea_families <- get_families("Archaea")
  chromista_families <- get_families("Chromista") #tsn=590735
  
  
  get_families<-function(kingdom) {
    kingdom_tsn<-get_tsn(kingdom,db="itis")
    if (!is.na(kingdom_tsn)) {
      families<-downstream(kingdom_tsn,"family",db="itis")
      return(families)
    } else {
      cat("Kingdom not found:", kingdom, "\n")
      return(NULL)
    }
  }
  protozoa_families <- get_families("Protozoa")#tsn=47380

  
  
  #assign search terms to each kingdom
  Taxa<-list(
    "Plant"=c("plant","plantae","vegetation",plant_families$Plantae$taxonname),
    "Animal"=c("animal","fish","bird","zooplankton","whale","seal","shark", "insect", "turtle",
               "penguin", animal_families$Animalia$taxonname ),
    "Fungal"=c("fungi","mushrooms", "yeasts", "molds",fungi_families$Fungi$taxonname),
    "Protozoa"=c("protist", "protozoa", "algae", protozoa_families$`630577`$taxonname),
    "Bacteria"=c("bacteria",bacteria_families$Bacteria$taxonname ),
    "Archaea"=c("archaea", archaea_families$Archaea$taxonname),
    "Chromista"=c("Chromista", chromista_families$Chromista$taxonname)
  )
  
  categories<-Taxa
  categories_exception <- NULL
  
  robots$Taxa_ITIS<-sapply(robots_text, extract_category_cap_sens)
  robots$Taxa_ITIS_specific<-sapply(robots_text, extract_variable_cap_sens)
  robots$Taxa_ITIS_specific_note<-sapply(robots_text, extract_variable_note_cap_sens)
  
  drones$Taxa_ITIS<-sapply(drones_text, extract_category_cap_sens)
  drones$Taxa_ITIS_specific<-sapply(drones_text, extract_variable_cap_sens)
  drones$Taxa_ITIS_specific_note<-sapply(drones_text, extract_variable_note_cap_sens)
#scenarios
  scenarios<-list(
    "Interaction"=c("change environment","modify environment"), #this might need to be looked more into
    "Sampling"=c("sampl"),
    "Monitoring"=c("remote sensing","survey", "monitor","observ",
                   "detect","measure","video","trace","mapping","record","footage","imager"
                   )
  )
  categories<-scenarios
  categories_exception<-NULL
  robots$Application_Category<-sapply(robots_text, extract_category)
  robots$Application_specific<-sapply(robots_text, extract_variable)
  robots$Application_specific_note<-sapply(robots_text, extract_variable_note)
  
  drones$Application_Category<-sapply(drones_text, extract_category)
  drones$Application_specific<-sapply(drones_text, extract_variable)
  drones$Application_specific_note<-sapply(drones_text, extract_variable_note)
#environmental variables
  chemicals<-c("chemical","salinity","sulfate","carbonate","silica","sodium",
               "organic matter","chloride","ammonia",
               " pH"," pH,", "pH.", "alkalinity","dissolved oxygen","electrical conductivity","redox",
               "total dissolved solids","nutrients","nitrogen","phosphorus","potassium",
               "biochemical oxygen demand","chemical oxygen demand",
               "heavy metals","mercury","cadnium","arsenic","copper","zinc","boron",
               "calcium","magnesium","aluminum","sulfur","iron","manganese",
               "cation exchange capacity",
               "mineral composition",
               "trace element","strontium","barium","cobalt","selenium","lithium","vanadium",
               "isotopic composition","radioactive isotope","radioactive element",
               "carbon-13","oxygen-18","uranium","thorium",
               "carbonate content",
               "Particulate Matter","pm10","pm2.5","nitrogen dioxide","sulfar dioxide",
               "ozone","carbon monoxide","carbon dioxide","lead","volatile organic compounds","vocs","methane",
               "hydrocarbons","particulate number","black carbon","formaldehyde","hydrogen sulfide","air toxic",
               "suspented particulate metals","mercury"
               )
  physicals<-c("temperature","velocity","wind speed", "ralative humidity","visibility","pressure","texture","porosity",
               "permeability","turbidity,","hardness","water holding capacity")
  environment<-list(
    "chemicals"=chemicals,
    "physicals"=physicals
  )
  medium<-list(
    "Water"=c(" water"), # to avoid underwater
    "Soil"=c("soil"),
    "Rock"=c("rock"),
    "Atmosphere"=c("air","atmospher"))
  
  extract_category2 <- function(text) {
    label<-NULL
    for (i in names(medium)) {
      for (j in names(environment)){
       if (any(str_detect(tolower(text), medium[[i]]))&any(str_detect(tolower(text),environment[[j]]))) {
             label<-c(label,paste(i,j,sep=" ")) 
      }
           
           }
    }
    return(paste(label,collapse="; "))
  }
 
  extract_variable2 <-function(text) {
      label<-NULL
      for (i in names(medium)) {
        for (j in names(environment)){
             var_lab<-NULL
             for (m in medium[[i]]) {
              for (n in environment[[j]]) {
                if (str_detect(tolower(text), m) & str_detect(tolower(text),n)) {
                  v<-paste(m,n,sep = "&")
                  var_lab<-c(var_lab,v)
                }
              }
             }
             label<-c(label,var_lab) 
        }
      }
      return(paste(label,collapse="; "))
  }
  
  
  
  
  extract_variable_note2 <-function(text) {
    label<-NULL
    for (i in names(medium)) {
      for (j in names(environment)){
        var_lab_note<-NULL
        for (m in medium[[i]]) {
          for (n in environment[[j]]) {
            if (str_detect(tolower(text), m) & str_detect(tolower(text),n)) {
               note1<-show_context(tolower(text),target_word = m,window_size = 15)
               note2<-show_context(tolower(text),target_word = n,window_size = 15)
               note<-paste(note1,note2,sep = "&")
               lab_note<-paste(m, "&", n," (",note,")",sep="")
               var_lab_note<-c(var_lab_note,lab_note)
            }
          }
        }
        label<-c(label,var_lab_note)
      }
    }
    return(paste(label,collapse="; ")) 
  }
      
  robots$Environmental_Variable<-sapply(robots_text, extract_category2)
  robots$Environmental_Variable_specific<-sapply(robots_text, extract_variable2)
  robots$Environmental_Variable_specific_note<-sapply(robots_text, extract_variable_note2)

  drones$Environmental_Variable<-sapply(drones_text, extract_category2)  
  drones$Environmental_Variable_specific<-sapply(drones_text, extract_variable2)
  drones$Environmental_Variable_specific_note<-sapply(drones_text, extract_variable_note2)
    
###########################################################333
  library(writexl)
  writexl::write_xlsx(robots,"Robots Dec. 04.xlsx")
  writexl::write_xlsx(drones,"Drones Dec. 04.xlsx")
 
  
 
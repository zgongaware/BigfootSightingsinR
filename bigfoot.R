# Load libraries
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)


# Load bigfoot csv
loadBigfoot <- function() {
  df <- read.csv('bfro_reports_geocoded.csv',stringsAsFactors = TRUE)
  return (df)
}


# Load map data - USA
loadUSA <- function() {
  df <- map_data('usa')
  return (df)
}

# Load map data - States
loadStates <- function(state) {
  df <- map_data('state')
  if(missing(state)) {
    return (df)
  } else {
    return (subset(df,region %in% state))
  }
}

# Load map data - Counties
loadCounties <- function(state) {
  df <- map_data('county')
  if(missing(state)) {
    return (df)
  } else {
    return(subset(df,region %in% state))
  }
}

# Filter bigfoot data
filterBigfoot <- function(subset) {
  df <- loadBigfoot()
    
  df2 <- df %>%
    #filter(longitude >= -130) %>%
    mutate(state = tolower(state)) %>%
    mutate(county = gsub(" County","",county)) %>%
    mutate(county = tolower(county)) %>%
    na.omit(subset(latitude,longitude)) %>%
    filter(longitude >= -130)
    
  if(missing(subset)) {
    return (df2)
  } else {
    clean <- df2 %>%
      filter(state %in% subset)
    
    return(clean)
  }
}

# Aggregate bigfoot data
aggBigfoot <- function(level,filter) {
  if(missing(filter)) {
    df <- filterBigfoot()
  } else {
    df <- filterBigfoot(filter)
  }
  
  if(missing(level)) {
    return (df)
  } else if (level == 'state') {
    final <- df %>%
      group_by(state) %>%
      tally()
    return (final)
  } else if (level == 'county') {
    final <- df %>%
      group_by(state,county) %>%
      tally()
    return (final)
  }
}

# Plot base layer
plotLevel <- function(level,filter) {
  if(missing(level)) {
    df <- loadUSA()
  } else if (level == 'state') {
    df <- loadStates(filter)
  } else if (level == 'county') {
    df <- loadCounties(filter)
  }
  
  if(missing(level)) {
    plot <- ggplot() +
      geom_polygon(data = df, aes(x=long, y = lat, group = group),color='black',fill='gray')
      coord_fixed(1.3) +
      theme_nothing()
  } else {
    plot <- ggplot(data = df,mapping = aes(x=long,y=lat,group=group)) +
      coord_fixed(1.3) +
      geom_polygon(color = "black", fill = "gray") #+
     # theme_nothing()
  }
  
  return(plot)
}

# Final plotting function
plotBigfoot <- function(level,filter) {
  df <- aggBigfoot(level,filter)
  base <- plotLevel(level,filter)
  
  if(missing(level)) {
  
    sightings <- geom_point(data = df,aes(x=longitude,y=latitude),color='black',size=0.5)
    
  } else if (level == 'state') {
    
    state <- loadStates(filter)
    combined <- left_join(state,df,by=c('region' = 'state'))
    
    sightings <- geom_polygon(data = combined,aes(fill=n),color='white')
    
  } else if (level == 'county') {
    county <- loadCounties(filter)
    combined <- left_join(county,df,by=c('region'='state','subregion'='county'))
    
    sightings <- geom_polygon(data = combined,aes(fill=n),color='white')
  }
  
  return (base + sightings)
}

# Plot all data points accross country
plotBigfoot()

# Aggregate to states
plotBigfoot('state',)

# Aggregate to counties
plotBigfoot('county',)

# Counties on West Coast only
plotBigfoot('county',c('washington','oregon','california','idaho','nevada'))







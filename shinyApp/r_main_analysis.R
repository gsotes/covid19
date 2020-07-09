library(utils)
library(httr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(stringr)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
    authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

covid_data <- read.csv(tf, stringsAsFactors = FALSE)
names(covid_data) <- gsub("[[:punct:]]|ï", '', names(covid_data))

# save a local copy of the covid data
write.csv(covid_data, "covid_data.csv", row.names = FALSE)

covid_data <- covid_data %>% mutate(dateRep=as.Date(dateRep, "%d/%m/%Y"))

dailyCases_func <-
  function(countryCode) {
  
  df <- covid_data %>%
    filter(geoId == countryCode) %>%
    arrange(dateRep)
  
  countryName <- unique(df$countriesAndTerritories)
  firstCase <- min(df[df$cases >= 1,]$dateRep)
  
  df <- df[df$dateRep >= firstCase,]
  
  p <- df %>%
    arrange(dateRep) %>%
    ggplot(aes(x = dateRep, y = cases, fill = "tomato3")) +
    #theme_bw() +
    theme(plot.title = element_text(size=30),
          text = element_text(size = 22),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          plot.caption = element_text(size = 12),
          panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                          size = 2, linetype = "solid"), 
          panel.border = element_blank(),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                           colour = "white"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"),
          axis.line = element_line(colour = "black"),
          legend.position = "none"
          ) +
    geom_bar(stat = "identity") +
    geom_smooth(span = 0.33) +
    scale_x_date(date_breaks = '20 days',
                 labels = date_format('%b-%d'),
                 limits = c(firstCase, as.Date('2020-07-31'))) +
    scale_y_continuous(limit = c(0, NA)) +
    labs(title = paste("Cases Reported Per Day,", countryName),
         y = "Number of Cases",
         x = "Date Reported",
         caption  = "Data Source: European Centre for Disease Prevention and Control \nhttps://www.ecdc.europa.eu/en"
         )
  
  return(p)
}

dailyDeaths_func <-
  function(countryCode) {
    
    df <- covid_data %>%
      filter(geoId == countryCode) %>%
      arrange(dateRep)
    
    countryName <- unique(df$countriesAndTerritories)
    firstCase <- min(df[df$cases >= 1,]$dateRep)
    
    df <- df[df$dateRep >= firstCase,]
    
    p <- df %>%
      arrange(dateRep) %>%
      ggplot(aes(x = dateRep, y = deaths, fill = "tomato3")) +
      #theme_bw() +
      theme(plot.title = element_text(size=30),
            text = element_text(size = 22),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            plot.caption = element_text(size = 12),
            panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"), 
            panel.border = element_blank(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "white"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "white"),
            axis.line = element_line(colour = "black"),
            legend.position = "none"
      ) +
      geom_bar(stat = "identity") +
      geom_smooth(span = 0.20) +
      scale_x_date(date_breaks = '20 days',
                   labels = date_format('%b-%d'),
                   limits = c(firstCase, as.Date('2020-07-31'))) +
      scale_y_continuous(limit = c(0, NA)) +
      labs(title = paste("Deaths Reported Per Day,", countryName),
           y = "Deaths",
           x = "Date Reported",
           caption  = "Data Source: European Centre for Disease Prevention and Control \nhttps://www.ecdc.europa.eu/en"
      )
    
    return(p)
  }


dailyDeaths_func('PH')

dailyCases_func('PH')
dailyCases_func('US')
dailyCases_func('IT')
dailyCases_func('CN')
dailyCases_func('ES')
dailyCases_func('SG')
dailyCases_func('MY')
dailyCases_func('VN')
dailyCases_func('ID')
dailyCases_func('TW')
dailyCases_func('CA')
dailyCases_func('KR')
dailyCases_func('KP')
dailyCases_func('RU')


grid.arrange(
  dailyCases_func('PH'),
  dailyCases_func('US'),
  ncol = 2
  )

# top 50 countries
top50 <-
  covid_data %>%
  group_by(County = countriesAndTerritories) %>%
  summarise(Cases=sum(cases)) %>%
  arrange(desc(Cases)) %>%
  print(n = 50)
library(tidyverse) # clean data
library(rvest)  #scrap website
library(geojsonio)  # read in US map file
#library(usmap)
#library(maps)


# scraping perscribe opioid data from CDC website
op_county_url<-"https://www.cdc.gov/drugoverdose/maps/rxcounty"
op_state_url<-"https://www.cdc.gov/drugoverdose/maps/rxstate"
# make a function to scrape the table from a given url
scrape_table <- function(basic_url, year){
  # basic url has the generaal frame of the url, where "year" varies in a loop
  data_url<-paste0(basic_url,year,".html")
  # scrape table from url and parse it into a table
  data<-data_url%>% read_html() %>% html_nodes('table') %>%.[1] %>%html_table()
  #assign(paste0("data_",year),data[[1]])
  }

#save scraped data to a list
result_county<-list()
result_state<-list()
# data of 2017 has 172 fewer counties and different format, drop 2017 data 
for (i in 2006:2016){
  # used (i-2005) is to ensure the list of result starts with index of 1 rather than index of "year"
  # this makes the later dataframe joining easier
  print(i)
  result_county[[i-2005]]<-scrape_table(op_county_url,i)[[1]]
  result_state[[i-2005]]<-scrape_table(op_state_url,i)[[1]]
}

# merge all dataframe saved in $result as list format to a big dataframe and replace '-' to NA
final_county<-reduce(result_county, full_join, by = c("County","State","FIPS County Code"))
final_county[final_county=="–"]<-NA
final_state<-reduce(result_state, full_join, by = c("State","State ABBR"))  # No missing in state dataset

#exam # of NA rows
length(which(rowSums(is.na(final_county))!=0))
length(which(rowSums(is.na(final_state))!=0))

# reshape final dataframe
#--reshape final_county
reshape_final_county<-gather(final_county,key, value, -County, -State, -`FIPS County Code`)
reshape_final_county<-reshape_final_county  %>%mutate(key=substr(reshape_final_county$key,1,4)) %>% 
                    mutate(value=as.numeric(value))
# fix some state code so they have all 5 digits, some states like AK, CA missed the 0 as the first digit in fips
reshape_final_county$`FIPS County Code`<-ifelse(nchar(reshape_final_county$`FIPS County Code`)==4, paste0(0,reshape_final_county$`FIPS County Code`), reshape_final_county$`FIPS County Code`)

#--reshape final_state
reshape_final_state<-gather(final_state,key, value, -State, -`State ABBR`)
reshape_final_state<-reshape_final_state %>%mutate(key=substr(reshape_final_state$key,1,4)) %>% 
  mutate(value=as.numeric(value))

# ---------------load US county map-------------
#county<-map_data("county") # this doesn't work as well becuase the precision of opioid data is reflected by FIPS code
mapcountyurl<-'http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_20m.json'
# read in json file, convert to dataframe
map_us_county<-geojson_read(mapcountyurl,what="sp",parse=T)
map_us_county.df<-fortify(map_us_county)

#map FIPS to longitude and latitude
myid <- as.data.frame(map_us_county@data)
myid$FIPS<-paste0(myid$STATE,myid$COUNTY)
myid$id <- seq.int(nrow(myid))-1
myid <- myid  %>% select(id, FIPS)
map_us_county.df <- merge(map_us_county.df, myid,  by.x="id", by.y="id")

# ---------------load US county map-------------
library(mapdata)
states <- map_data("state")


# plotting map, background settings
theme_map <- function(base_size = 20) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.position = c(0.1,0.2), legend.title = element_text(size=8),legend.text=element_text(size=6),
      panel.border = element_blank()
    )
}



for (year in 2006:2016){
  print(paste("The year is", year))
  #----------------plotting county--------------------------
  final_x_county <- reshape_final_county[reshape_final_county$key==year,]
  map_us_county.df_x <- merge(map_us_county.df, final_x_county,  by.x="FIPS", by.y="FIPS County Code")
  # look at the distribution
  #hist(map_us.df_x$value)
  summary(map_us_county.df_x$value)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #   0.00   56.20   80.90   86.37  109.70  583.80    3278 
  # add intensity: 
  map_us_county.df_x$intensity<-NA
  map_us_county.df_x[is.na(map_us_county.df_x$value) ,"intensity"]<-0
  map_us_county.df_x[map_us_county.df_x$value<50 & !is.na(map_us_county.df_x$value) ,"intensity"]<-1
  map_us_county.df_x[map_us_county.df_x$value>50 & map_us_county.df_x$value<80 & !is.na(map_us_county.df_x$value),"intensity"]<-2
  map_us_county.df_x[map_us_county.df_x$value>80 & map_us_county.df_x$value<110 & !is.na(map_us_county.df_x$value),"intensity"]<-3
  map_us_county.df_x[map_us_county.df_x$value>110 & map_us_county.df_x$value<200 &!is.na(map_us_county.df_x$value),"intensity"]<-4
  map_us_county.df_x[map_us_county.df_x$value>200 &!is.na(map_us_county.df_x$value),"intensity"]<-5
  #recode the leftout region to 0 intensity
  map_us_county.df_x[is.na(map_us_county.df_x$intensity) ,"intensity"]<-0
  map_us_county.df_x$intensity<-as.factor(map_us_county.df_x$intensity)
  
  # idea is to plot the border first, with light blue as background, then plot the data, then plot the border again
  # with NA filling to not overwrite the filling but overwrite the border
  uscountyplot <- ggplot(map_us_county.df_x) +borders(database="county", fill="#add8e6",colour="#666666")+
    geom_polygon(aes(x = long, y = lat, fill = intensity, group = group)) +
    theme_map()+scale_fill_manual(name="Rate per 100 persons", labels=c("NA", "<50%", ">50% & <80%",">80% & <110%",">110% & <200%",">200%"),
      values =c("5"='#d2618c',"4"='#e29ab5' , "3"="#edbed0","2"="#f4d5e1","1"="#f9e9f0" ,"0"="#9db6c7")) +
    ylim(20,51)+ 
    xlim(-125,-66)+ 
    labs(x = NULL, 
         y = NULL, 
         title = "U.S. County Prescribing Rate Maps", 
         subtitle = paste0("Year: ", year),
         caption = paste("U.S. County Prescribing Rate", year, "(made by Jiayi Cox)"))+borders(database="county",colour="#666666") 
  filename <- paste0("maps/img_county" , str_pad(year, 7, pad = "0"),  ".png")
  ggsave(filename = filename, plot = uscountyplot, width = 5, height = 5, dpi = 320, type = "cairo-png")

  #------------------plotting state
  final_x_state <- reshape_final_state[reshape_final_state$key==year,] %>% mutate(State=tolower(State))
  map_us_state.df_x <- merge(states, final_x_state,  by.x="region", by.y="State")
  #map_us.df_x <- merge(map_us.df, final_x,  by.x="NAME", by.y="State")
  # look at the distribution
  #hist(map_us.df_x$value)
  summary(map_us_state.df_x$value)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #   0.00   56.20   80.90   86.37  109.70  583.80    3278 
  # add intensity: 
  map_us_state.df_x$intensity<-NA
  map_us_state.df_x[is.na(map_us_state.df_x$value) ,"intensity"]<-"NA"
  map_us_state.df_x[map_us_state.df_x$value<20 & !is.na(map_us_state.df_x$value) ,"intensity"]<-"<20%"
  map_us_state.df_x[map_us_state.df_x$value>20 & map_us_state.df_x$value<50 & !is.na(map_us_state.df_x$value),"intensity"]<-">20% & <50%"
  map_us_state.df_x[map_us_state.df_x$value>50 & map_us_state.df_x$value<70 & !is.na(map_us_state.df_x$value),"intensity"]<-">50% & <70%"
  map_us_state.df_x[map_us_state.df_x$value>70 & map_us_state.df_x$value<90 &!is.na(map_us_state.df_x$value),"intensity"]<-">70% & <90%"
  map_us_state.df_x[map_us_state.df_x$value>90 &!is.na(map_us_state.df_x$value),"intensity"]<-">90%"
  #recode the leftout region to 0 intensity
  map_us_state.df_x[is.na(map_us_state.df_x$intensity) ,"intensity"]<-"NA"
  map_us_state.df_x$intensity<-as.factor(map_us_state.df_x$intensity)
  
  # idea is to plot the border first, with light blue as background, then plot the data, then plot the border again
  # with NA filling to not overwrite the filling but overwrite the border
  uscountyplot <- ggplot(map_us_state.df_x) +#borders(database="state", fill="#add8e6",colour="#666666")+
    geom_polygon(aes(x = long, y = lat, fill = intensity, group = group)) +
    theme_map()+scale_fill_manual(name="Rate per 100 persons",limits=c("NA", "<20%" ,">20% & <50%",">50% & <70%",">70% & <90%",">90%"), 
                                  values =c("NA"="#9db6c7","<20%"="#00ff00",">20% & <50%"="#009d00",">50% & <70%"="#007600",">70% & <90%"="#004e00",">90%"='#002700'))+
                    #values =c("5"='#002700',"4"='#004e00' , "3"="#007600","2"="#009d00","1"="#00ff00" , "0"="#9db6c7")
                                  #) +
    ylim(20,51)+ 
    xlim(-125,-66)+ 
    labs(x = NULL, 
         y = NULL, 
         title = "U.S. State Prescribing Rate Maps", 
         subtitle = paste0("Year: ", year),
         caption = paste("U.S. State Prescribing Rate", year,"(made by Jiayi Cox)"))+borders(database="state",colour="#666666") 
  filename <- paste0("maps/img_state" , str_pad(year, 7, pad = "0"),  ".png")
  ggsave(filename = filename, plot = uscountyplot, width = 5, height = 5, type = "cairo-png")
}
# from image to movie
makemovie_cmd_county <- paste0("/usr/local/bin/ffmpeg -framerate 5 -y -i ", paste0(getwd(), "/maps/img_county0%*.png"),  " -r 56 -pix_fmt yuv420p ",  paste0(getwd(), "/maps/"), "us_op_county_perscribing_rate_movie.mp4")
system(makemovie_cmd_county)
makemovie_cmd_state <- paste0("/usr/local/bin/ffmpeg -framerate 5 -y -i ", paste0(getwd(), "/maps/img_state%*.png"),  " -r 56 -pix_fmt yuv420p ",  paste0(getwd(), "/maps/"), "us_op_state_perscribing_rate_movie.mp4")
system(makemovie_cmd_state)

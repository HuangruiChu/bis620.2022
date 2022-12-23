Sys.setlocale("LC_TIME", "English")


integrated_data_cn_2022 <- read.csv("integrated_data_cn_2022.csv", header = TRUE)
integrated_data_cn_2021 <- read.csv("integrated_data_cn_2021.csv", header = TRUE)
#Yongzhou_data = integrated_data_cn_2022[which(integrated_data_cn_2022$City_cn == "Yongzhou"),]



library(grid)
library(ggnewscale)
library(ggtext)
library(tidyverse)
library(shadowtext)
library(patchwork)







##########################################

Draw_Baidu_index_each_city <- function(integrated_data,city_name){
  
  city_characters <- c(
    "Feeling.Sad", 
    "Insomnia", 
    "Depression",
    "Anxiety",
    "Fatigue"
  )
  
  title = paste(city_name,"'s searching counts")
  
  city_data = integrated_data[which(integrated_data$City_cn == city_name),]
  
  city_data_formated <- data.frame(Date = as.Date(rep(city_data$Date,5)), 
                                       num_count = c(city_data$Feeling.Sad,city_data$Insomnia,city_data$Depression,city_data$Anxiety,city_data$Fatigue),
                                       city_character = factor(rep(city_characters, each = nrow(city_data)), levels = city_characters))
  
  ggplot(city_data_formated,aes(Date,num_count,fill=city_character,color = city_character,group = 1))+
    geom_line(size = 0.02)+
    #geom_point(size = 2)+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    #axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75)
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
    
}

Draw_Baidu_index_each_city(integrated_data_cn_2022,"Yongzhou")
Draw_Baidu_index_each_city(integrated_data_cn_2022,"Beijing")


#########################################


Draw_Working_Entertainment_each_city <- function(integrated_data,city_name){
  
  city_characters <- c(
    "Work", 
    "Entertainment"
  )
  
  title = paste(city_name,"'s working and entertainment index")
  
  city_data = integrated_data[which(integrated_data$City_cn == city_name),]
  
  
  city_data_formated <- data.frame(Date = as.Date(rep(city_data$Date,2)), 
                                   num_count = c(city_data$Work_index,city_data$Entertainment_index),
                                   city_character = factor(rep(city_characters, each = nrow(city_data)), levels = city_characters))
  
  ggplot(city_data_formated,aes(Date,num_count,fill=city_character,color = city_character,group = 1))+
    geom_line(size = 0.35)+
    #geom_point(size = 2)+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    #axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75)
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  
}

Draw_Working_Entertainment_each_city(integrated_data_cn_2022,"Beijing")


################################################


Draw_Move_in_each_city_2021_2022 <- function(integrated_data_2021,integrated_data_2022,city_name){
  
  city_years <- c(2021,2022)
  
  temp_1<-as.Date(integrated_data_2021$Date, format="%Y-%m-%d")
  Date_without_year_1<-format(temp_1, format="%m-%d")
  
  temp_2<-as.Date(integrated_data_2022$Date, format="%Y-%m-%d")
  Date_without_year_2<-format(temp_2, format="%m-%d")
  
  Date_without_year = intersect(Date_without_year_1,Date_without_year_2)
  
  title = paste(city_name,"'s Baidu Move in index")
  
  city_data_2021 = integrated_data_2021[which(integrated_data_2021$City_cn == city_name),]
  city_data_2021_subset = city_data_2021[format(as.Date(city_data_2021$Date, format="%Y-%m-%d"), format="%m-%d")  %in% Date_without_year,]
  city_data_2022 = integrated_data_2022[which(integrated_data_2022$City_cn == city_name),]
  city_data_2022_subset = city_data_2022[format(as.Date(city_data_2022$Date, format="%Y-%m-%d"), format="%m-%d")  %in% Date_without_year,]
  
  city_data_formated <- data.frame(Date = rep(Date_without_year,2), 
                                   num_count = c(city_data_2021_subset$Baidu_Move_in_Index,city_data_2022_subset$Baidu_Move_in_Index),
                                   city_year = factor(rep(city_years, each = nrow(city_data_2021_subset)), levels = city_years))
  
  ggplot(city_data_formated,aes(Date,num_count,fill=city_year,color = city_year,group = 1))+
    geom_line(size = 0.2)+
    #geom_point(size = 2)+
    ggtitle(title)+
    #scale_x_discrete(guide = guide_axis(n.dodge = 10))+
    #scale_x_discrete(breaks = 5,guide = guide_axis(angle = 90))+

    theme(plot.title = element_text(hjust = 0.5))+
    #axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75)
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  
}

Draw_Move_in_each_city_2021_2022(integrated_data_cn_2021,integrated_data_cn_2022,"Beijing")




#######################################

Draw_Move_out_each_city_2021_2022 <- function(integrated_data_2021,integrated_data_2022,city_name){
  
  city_years <- c(2021,2022)
  
  temp_1<-as.Date(integrated_data_2021$Date, format="%Y-%m-%d")
  Date_without_year_1<-format(temp_1, format="%m-%d")
  
  temp_2<-as.Date(integrated_data_2022$Date, format="%Y-%m-%d")
  Date_without_year_2<-format(temp_2, format="%m-%d")
  
  Date_without_year = intersect(Date_without_year_1,Date_without_year_2)
  
  title = paste(city_name,"'s Baidu Move out index")
  
  city_data_2021 = integrated_data_2021[which(integrated_data_2021$City_cn == city_name),]
  city_data_2021_subset = city_data_2021[format(as.Date(city_data_2021$Date, format="%Y-%m-%d"), format="%m-%d")  %in% Date_without_year,]
  city_data_2022 = integrated_data_2022[which(integrated_data_2022$City_cn == city_name),]
  city_data_2022_subset = city_data_2022[format(as.Date(city_data_2022$Date, format="%Y-%m-%d"), format="%m-%d")  %in% Date_without_year,]
  
  city_data_formated <- data.frame(Date = rep(Date_without_year,2), 
                                   num_count = c(city_data_2021_subset$Baidu_Move_out_Index,city_data_2022_subset$Baidu_Move_out_Index),
                                   city_year = factor(rep(city_years, each = nrow(city_data_2021_subset)), levels = city_years))
  
  ggplot(city_data_formated,aes(Date,num_count,fill=city_year,color = city_year,group = 1))+
    geom_line(size = 0.2)+
    #geom_point(size = 2)+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))+
    #axis.text.x = element_text(angle = 30,vjust = 0.85,hjust = 0.75)
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
  
}

Draw_Move_out_each_city_2021_2022(integrated_data_cn_2021,integrated_data_cn_2022,"Beijing")


####### create data
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"




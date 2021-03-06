# New version 

library(sf)
library(ggplot2)
library(albersusa)
library(maps)
library(dplyr)
library(readr)

#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))

####INPUT#####
# Below has to be changed based on the location of the appropriate file on host computer 

cases <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/12-21-2020.csv")
ophtho <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/OphthoDataFiles/newVersionAAMCStateData.csv")
cards <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/Cardiovascular Disease.csv")
obgyn <- read_csv("C:/Chandru/CWRU/Research/CCF/COVIDProject/OBGyn.csv")

cases <- cases %>%
  rename(state = Province_State) %>% 
  filter(!(state %in% c("District of Columbia", "Puerto Rico"))) %>%
  rename(long = Long_) %>%
  rename(lat = Lat) %>%
  # mutate(LogCases = log10(`Confirmed`)) %>%
  # mutate(LogWomenCases = log10('ConfirmedWomen')) %>%
  filter(`Confirmed` > 0) %>%
  filter(lat > 25 & lat < 50 & long > -130 & long < -65) 

states_pleth <- map_data("state")
# Change for desired speciality 
# state_data <- left_join(states_pleth, ophtho, by = "region")
# state_data <- left_join(states_pleth, cards, by = "region")
state_data <- left_join(states_pleth, obgyn, by = "region")

# Original without log cases 
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
	geom_polygon(data = state_data,
	           aes(x = long, y = lat, group = group, fill = PercentOver60),
	           color = "black") +
	# scale_fill_gradient(name = "Ophthalmologists (%) > 60", low = "antiquewhite1", high = "darkblue", 
	#                   breaks = c(30,35,40,45), labels=c("30%","35%","40%", "45%"), na.value="black") +
	# scale_fill_gradient(name = "Cardiologists (%) > 60", low = "antiquewhite1", high = "darkblue", 
	#                   breaks = c(30,40,50,60), labels=c("30%","40%","50%", "60%"), na.value="black") +
	scale_fill_gradient(name = "OB/GYN (%) > 60 y/o", low = "antiquewhite1", high = "darkblue", 
	                  breaks = c(20,25,30,35,40,45,50,55), labels=c("20%","25%","30%", "35%", "40%", "45%", "50%", "55%"), na.value="black") +
	geom_point(data = cases, 
			 aes(x = long, y = lat, size = `ConfirmedWomen`),
         colour = "firebrick2", alpha = 0.9, shape = 20) +
	# geom_point(data = cases, 
	# 			 aes(x = long, y = lat, size = `Confirmed`),
	#          colour = "firebrick2", alpha = 0.9, shape = 20) +
	# scale_fill_discrete(name="COVID-19 cases", labels = c("100-199k, 200-299k, 300k+")) + 
	theme(axis.title = element_blank(),
	    axis.text = element_blank(),
	    axis.ticks = element_blank(),
	    panel.grid = element_blank(),
	    panel.background = element_blank(),
	    legend.key = element_rect(fill = "white")) +
	scale_size_continuous(name = "Cases (thousands)",
		range = c(0,7),
		breaks = c(100000,200000,300000),
		labels = c("100-199", "200-299", "300+"))
ggsave(filename = "BubbleMap.png", path = "C:/Chandru/CWRU/Research/CCF/COVIDProject/",
       width = 8, height = 4)

# Vaccination rates as gradtient 
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
	geom_polygon(data = state_data,
	           aes(x = long, y = lat, group = group, fill = VacRate),
	           color = "black") +
	scale_fill_gradient(name = "Vaccination Rate (%)", low = "antiquewhite1", high = "green", 
	                  breaks = c(0.25,0.5,0.75,1,1.25,1.5), labels=c("0.25%","0.5%","0.75%", "1.00%", "1.25%", "1.5%"), na.value="black") +
	geom_point(data = cases, 
			 aes(x = long, y = lat, size = `ConfirmedWomen`),
         colour = "firebrick2", alpha = 0.9, shape = 20) +
	# geom_point(data = cases, 
	# 			 aes(x = long, y = lat, size = `Confirmed`),
	#          colour = "firebrick2", alpha = 0.9, shape = 20) +
	# scale_fill_discrete(name="COVID-19 cases", labels = c("100-199k, 200-299k, 300k+")) + 
	theme(axis.title = element_blank(),
	    axis.text = element_blank(),
	    axis.ticks = element_blank(),
	    panel.grid = element_blank(),
	    panel.background = element_blank(),
	    legend.key = element_rect(fill = "white")) +
	scale_size_continuous(name = "Cases (thousands)",
		range = c(0,7),
		breaks = c(100000,200000,300000),
		labels = c("100-199", "200-299", "300+"))


		
# ggplot() +
#   # map w/ states colored by # `physicians over 60` per 100K population
# 	geom_polygon(data = state_data,
# 	           aes(x = long, y = lat, group = group, fill = PercentOver60),
# 	           color = "black") +
# 	scale_fill_gradient("% Ophthalmologists > 60", low = "antiquewhite1", high = "darkblue", 
# 	                  breaks = c(30,35,40,45), labels=c("30%","35%","40%", "45%"), na.value="black") +
# 	geom_point(data = cases, 
# 				 aes(x = long, y = lat, size = `LogCases`),
# 	         colour = "firebrick2", alpha = 0.9, shape = 20) +
# 	theme(axis.title = element_blank(),
# 	    axis.text = element_blank(),
# 	    axis.ticks = element_blank(),
# 	    panel.grid = element_blank(),
# 	    panel.background = element_blank(),
# 	    legend.key = element_rect(fill = "white")) +
# 	scale_size(range = c(0,3)) 

ggsave(filename = "BubbleMap2.png", path = "C:/Chandru/CWRU/Research/CCF/COVIDProject/",
       width = 8, height = 4)





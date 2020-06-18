#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))

#####INPUT#####
pcare <- read_csv("aamc-state-data.csv")
covid <- read_csv("covid-confirmed.csv")
lat_long <- read_csv("usa_lat_long.csv")

#####PROCESS#####
# pcare:
#   - add column for # of physicians over 60 per 100K population
#   - only select states that can be plotted (remove DC, PR)
pcare <- pcare %>%
  mutate(Adjust_Num = 1 / Number * 100000,
         Risk = Adjust_Num * Over60 / 100) %>%
  filter(!(state %in% c("DC", "PR")))

# covid:
#   - rename "Province_State" column as "region"
#   - only select date column that will actually be used
#   - remove rows w/ zero cases (won't be plotted anyway, won't cause logarithm issues)
#   - remove rows not in continental United States. Remove DC
#   - take logarithm to make bubbles more aesthetically pleasing
#   - take the sum of "cases" and "LogCases" for each state
#   - make state names lowercase 
covid <- rename(covid, region = Province_State)
covid <- covid %>%
  select(region, `6/4/2020`) %>%
  filter(`6/4/2020` > 0) %>%
  mutate(LogCases = log10(`6/4/2020`))
covid <- rename(covid, cases = "6/4/2020")
covid <- covid[-c(1,2,3,4,305),]
covid <- covid %>% group_by(region) %>%
  summarize(cases = sum(cases),
            Logcases = sum(LogCases))
covid <- covid %>% mutate(region = tolower(region))


# prepare map data
lat_long <- lat_long %>% mutate(region = tolower(region))
lat_long <- lat_long[-c(24,25),]

# Use the dplyr package to merge the states_pleth and covid files to assign a longitude and latitude for each state
MergedStates <- merge(lat_long, covid, by = "region")

#arrange states to be lowest to highest in covid cases
MergedStates <- MergedStates[order(MergedStates$cases),]

# prepare map data
states_pleth <- map_data("state")
state_data <- left_join(states_pleth, pcare, by = "region")

#####VISUALIZE & SAVE#####
## bubble map of 50 U.S. states
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
  geom_polygon(data = state_data,
               aes(x = long, y = lat, group = group, fill = Over60),
               color = "black") +
  scale_fill_gradient("Percentage of Ob/Gyns over 60", low = "lightblue", high = "darkblue", breaks = c(27,30,33,36), labels=c("27%","30%","33%", "36%")) +
  # bubbles w/ log(COVID cases)
  geom_point(data = MergedStates,
             aes(x = Longitude, y = Latitude, size = cases),
             colour = "firebrick2", alpha = 0.45, shape=20) +
  scale_size_continuous("SARS-CoV-2 Cases",
                        range = c(0.1, 20),
                        breaks = 50000 * c(1, 2, 4, 6, 8),
                        labels = c("< 51,000", "51-100,000",
                                   "101-200,000", "201-300,000",
                                   "301-400,000")) +
  # remove elements we don't need
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# save bubble chloropleth
ggsave(filename = "BubbleMap.png",
       width = 8, height = 4)

#Merge covid table with ObGyn over 60 table
#order from high risk states to low risk states
#Rename columns
#Create table with top 5 states and bottom 5 states
ObGynCovid <- merge(pcare, covid, by = "region")
ObGynCovid <- ObGynCovid[with(ObGynCovid, order(-Risk, -Logcases)), ]
ObGynCovid <- ObGynCovid %>% rename(State = region) %>% 
    rename("Percentage of Ob/Gyns over 60" = "Over60") %>%
    rename("Covid Cases" = "cases") %>%
    select(, c(1,4,7))

#Create high risk states and low risk states table
HighRiskStates <- head(ObGynCovid, 5)
print(HighRiskStates)
LowRiskStates <- tail(ObGynCovid, 5)
print(LowRiskStates)

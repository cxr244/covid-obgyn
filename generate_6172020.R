#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))
suppressPackageStartupMessages(library("gridExtra"))
suppressPackageStartupMessages(library("DT"))

#####INPUT#####
obgyn <- read_csv("aamc-state-data.csv")
covid <- read_csv("covid-confirmed.csv")
lat_long <- read_csv("usa_lat_long.csv")
state_births <- read_csv("state_births.csv")

#####PROCESS#####
# obgyn:
#   - add column for # of physicians over 60 per 100K population
#   - only select states that can be plotted (remove DC, PR)
#   - Adjusted number if the number of physicians per 100,000 people in the state
obgyn <- obgyn %>%
  mutate(Adjust_Num = 1 / Number * 100000,
         Risk = Adjust_Num * Over60 / 100) %>%
  filter(!(state %in% c("DC", "PR")))

# covid:
#   - rename `Province_State` column to `region` (and make lowercase in process)
#   - rename `6/4/2020` to `cases`
#   - only select date column that will actually be used
#   - remove rows w/ zero cases (won't be plotted anyway, won't cause logarithm issues)
#   - remove states not in continental U.S.
#   - aggregate (sum) cases by state (`region`)
#   - take logarithm to make bubbles more aesthetically pleasing
covid <- covid %>%
  mutate(region = tolower(Province_State), Province_State = NULL,
         cases = `6/4/2020`) %>%
  select(region, cases) %>%
  filter(cases > 0,
         region %in% obgyn$region) %>%
  group_by(region) %>%
  summarize(cases = sum(cases)) %>%
  mutate(logcases = log(cases))

# lat_long:
#   - make state names (`region`) lowercase
#   - remove states not in continental U.S.
lat_long <- lat_long %>%
  mutate(region = tolower(region)) %>%
  filter(region %in% obgyn$region)

# merge datasets for bubble plot data (log covid cases represented by bubble radius)
bubble_data <- merge(lat_long, covid)

#arrange states to be lowest to highest in covid cases
MergedStates <- MergedStates[order(MergedStates$cases),]

# prepare map data
states_pleth <- map_data("state")
state_data <- left_join(states_pleth, obgyn, by = "region")

#####VISUALIZE & SAVE#####
## bubble map of 50 U.S. states
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
  geom_polygon(data = state_data,
               aes(x = long, y = lat, group = group, fill = Over60),
               color = "black") +
  scale_fill_gradient("Percentage of OBGYNs over 60", low = "lightblue", high = "darkblue", breaks = c(27,30,33,36), labels=c("27%","30%","33%", "36%")) +
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
#Merge new table with births table 
#create new column for births per physician
#create new column for covid individuals per physician (/2 for only females)
#order from high risk states to low risk states
#Rename columns
#Create table with top 5 states and bottom 5 states
ObGynCovid <- merge(obgyn, covid, by = "region")
ObGynCovid <- merge(ObGynCovid, state_births, by = "region")
ObGynCovid[, "BirthsperOBGYN"] <- ObGynCovid[, "total births"] / ObGynCovid[, "Physicians"]
ObGynCovid[, "COVIDperOBGYN"] <- ObGynCovid[, "cases"] / ObGynCovid[, "Physicians"]
ObGynCovid <- transform(ObGynCovid, COVIDperOBGYN=COVIDperOBGYN/2)

ObGynCovid <- ObGynCovid[with(ObGynCovid, order(-Risk, -Logcases, -BirthsperOBGYN, -COVIDperOBGYN)), ]
ObGynCovid <- ObGynCovid %>% select(region, Over60, cases, BirthsperOBGYN, COVIDperOBGYN)
ObGynCovid <- ObGynCovid %>% rename(State = region) %>% 
  rename("Percentage of OBGYNs over 60" = "Over60") %>%
  rename("Covid Cases" = "cases") %>% 
  rename("Births per OBGYN" = "BirthsperOBGYN") %>%
  rename("COVID per OBGYN" = "COVIDperOBGYN") 

  
#Create high risk states and low risk states table
HighRiskStates <- head(ObGynCovid, 5)
#print(HighRiskStates)
LowRiskStates <- tail(ObGynCovid, 5)
#print(LowRiskStates)

tbl1 <- ggplot() + annotation_custom(tableGrob(HighRiskStates, rows = NULL)) + ggtitle('High Risk States')
tbl2 <- ggplot() + annotation_custom(tableGrob(LowRiskStates, rows = NULL)) + ggtitle('Low Risk States')
ggsave(grid.arrange(tbl1, tbl2), filename = "RiskTable.png", width = 10, height = 5)

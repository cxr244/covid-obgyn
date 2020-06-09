#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))

#####INPUT#####
pcare <- read_csv("aamc-state-data.csv")
covid <- read_csv("covid-confirmed.csv")

#####PROCESS#####
# pcare:
#   - add column for # of physicians over 60 per 100K population
#   - only select states that can be plotted (remove DC, PR)
pcare <- pcare %>%
  mutate(Adjust_Num = 1 / Number * 100000,
         Risk = Adjust_Num * Over60 / 100) %>%
  filter(!(state %in% c("DC", "PR")))

# covid:
#   - only select date column that will actually be used
#   - rename Long_ to long (contains longitude)
#   - rename Lat to lat (contains latitude)
#   - remove rows w/ zero cases (won't be plotted anyway, won't cause logarithm issues)
#   - remove rows not in continental United States
#   - take logarithm to make bubbles more aesthetically pleasing
covid <- covid %>%
  select(Lat, Long_, `6/4/2020`) %>%
  rename(long = Long_) %>%
  rename(lat = Lat) %>%
  filter(`6/4/2020` > 0) %>%
  filter(lat > 25 & lat < 50 & long > -130 & long < -65) %>%
  mutate(LogCases = log10(`6/4/2020`))

# prepare map data
states_pleth <- map_data("state")
state_data <- left_join(states_pleth, pcare, by = "region")

#####VISUALIZE & SAVE#####
## bubble map of 50 U.S. states
ggplot() +
  # map w/ states colored by # `physicians over 60` per 100K population
  geom_polygon(data = state_data,
               aes(x = long, y = lat, group = group, fill = Risk),
               color = "black") +
  scale_fill_gradient("At-risk", low = "lightblue", high = "darkblue") +
  # bubbles w/ log(COVID cases)
  geom_point(data = covid,
             aes(x = long, y = lat, size = LogCases),
             colour = "red", alpha = 0.45, shape = 16) +
  scale_size_continuous("# cases",
                        range = c(0, 1.15),
                        labels = c("1-9", "10-99", "100-999",
                                   "1,000-9,999", "10,000-99,999",
                                   ">100,000")) +
  # remove elements we don't need
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# save bubble chloropleth
ggsave(filename = "BubbleMap.png",
       width = 8, height = 4)

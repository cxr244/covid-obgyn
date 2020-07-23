#####SETUP#####
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("ggplot2"))
suppressPackageStartupMessages(library("maps"))

#####INPUT & PROCESS#####
# obgyn:
#   - Read in "aamc-state-data.csv" file
#   - Add column 'Adjusted number': number of physicians per 100,000 people in the state
#   - add column 'risk': number of physicians over 60 per 100K population
#   - only select states that can be plotted (remove DC, PR, AK, HI)
obgyn <- read_csv("aamc-state-data.csv") %>%
  mutate(Adjust_Num = 1 / pts_per_obgyn * 100000,
         Risk = Adjust_Num * Over60 / 100) %>%
  filter(!(state %in% c("DC", "PR", "AK", "HI")))

# covid:
#   - Read in "covid-confirmed.csv" file
#   - rename `Province_State` column to `region` (and make lowercase in process)
#   - rename `6/21/20` to `cases`
#   - only select date column that will actually be used
#   - remove rows w/ zero cases (won't be plotted anyway, won't cause logarithm issues)
#   - remove states not in continental U.S.
#   - aggregate (sum) cases by state (`region`)
#   - create new column for logarithm of cases
covid <- read_csv("covid-confirmed.csv") %>%
  mutate(region = tolower(Province_State), Province_State = NULL,
         cases = `7/22/2020`) %>%
  select(region, cases) %>%
  filter(cases > 0,
         region %in% obgyn$region) %>%
  group_by(region) %>%
  summarize(cases = sum(cases)) %>%
  mutate(logcases = log(cases))

# lat_long:
#   - make state names (`region`) lowercase
#   - remove states not in continental U.S.
lat_long <- read_csv("usa_lat_long.csv") %>%
  mutate(region = tolower(region)) %>%
  filter(region %in% obgyn$region)

# state_sex:
#   - filter for states in continental U.S.
#   - use Male/Female ratio (`mf_ratio`) to calculate proportion Female (`female`)
state_sex <- read_csv("state_sex.csv") %>%
  filter(region %in% obgyn$region) %>%
  mutate(female = 1 - (mf_ratio / (mf_ratio + 1)))

# merge datasets for bubble plot data
bubble_data <- left_join(lat_long, covid)

# prepare map data
states_pleth <- map_data("state")
state_data <- left_join(states_pleth, obgyn)

#####VISUALIZE & SAVE#####
## bubble map of 50 U.S. states
ggplot() +
  # map w/ states colored by % `physicians over 60
  geom_polygon(data = state_data,
               aes(x = long, y = lat, group = group, fill = Over60),
               color = "black") +
  scale_fill_gradient("% Ob/Gyn > 60 yo", low = "antiquewhite1", high = "darkblue") +
  # bubbles w/ # COVID cases as of June 21, 2020
  geom_point(data = bubble_data,
             aes(x = Longitude, y = Latitude, size = cases),
             colour = "firebrick2", alpha = 0.8, shape=20) +
  scale_size_continuous("Confirmed cases",
                        range = c(1, 15),
                        breaks = 50000 * c(1, 2, 4, 6, 8),
                        labels = c("50K", "100K",
                                   "200K", "300K",
                                   "400K")) +
  # remove elements we don't need
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank())

# save bubble chloropleth
ggsave(filename = "BubbleMap.png",
       width = 8, height = 4)

#####MEGA TABLE#####
# read state-level birth data in preparation for later use
state_births <- read_csv("state_births.csv")

# merge state-level covid cases with state-level workforce data
# merge previous table w/ state-level birth data
# merge previous table w/ state-level sex distribution data
# create column for births per obgyn (state-level)
# create column for confirmed covid+ cases per obgyn (adjusted for female proportion)
# calculate # obgyn over 60 (state-level)
# calculate # births per obgyn > 60 yo
# calculate # confirmed cases per obgyn > 60 yo (adjusted for female population)
# calculate # obgyn under 60 yo
# calculate # births per obgyn < 60 yo
# calculate # confirmed cases per obgyn < 60 yo (adjusted for female population)
# rename `region` to `state` for readability
# remove unused columns (or directly from data source)
# save to file
mega_table <- left_join(obgyn, covid) %>%
  left_join(state_births) %>%
  left_join(state_sex) %>%
  mutate(births_per_ob = total_births / num_ob,
         covid_per_ob = cases / num_ob * female,
         num_ob_over60 = num_ob * Over60 / 100,
         births_per_over60 = total_births / num_ob_over60,
         covid_per_over60 = cases / num_ob_over60 * female,
         num_ob_under60 = num_ob - num_ob_over60,
         births_per_under60 = total_births / num_ob_under60,
         covid_per_under60 = cases / num_ob_under60 * female) %>%
  rename(State = region) %>%
  select(-state, -pts_per_obgyn, -Adjust_Num, -Risk,
         -logcases, -total_births, -mf_ratio, -female) %>%
  write_csv(path = "mega-table.csv")

#####HIGH RISK STATES TABLE#####
# Create top 10 high risk states: sort in descending order by covid cases per OB/GYN over 60
# Select columns for High Risk States Tables
# Rename columns
# Select top 10 states
highriskstates <- mega_table[with(mega_table, order(-covid_per_over60, -Over60, -cases)), ]
highriskstates <- highriskstates %>% 
  select(State, Over60, num_ob, cases, covid_per_over60) %>%
  rename("Percentage of OB/GYNs over 60" = Over60) %>%
  rename("Number of Practicing OB/GYNs" = num_ob) %>%
  rename("COVID-19 cases" = cases) %>%
  rename("COVID-19 cases per OB/GYN over 60" = covid_per_over60)
highriskstates <- highriskstates[1:10,]  
  
# code to create table image (cannot use for manuscript b/c need
# text-editable tables in MS Word and need to decide metric to sort by)
tbl1 <- ggplot() + annotation_custom(tableGrob(highriskstates, rows = NULL)) + ggtitle('High Risk States')
ggsave(grid.arrange(tbl1), filename = "RiskTable.png", width = 11.5, height = 4)

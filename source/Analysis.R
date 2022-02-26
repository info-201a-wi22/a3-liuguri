library(tidyverse)
library(dplyr)
library(ggplot2)
library(mapproj)

filename1 <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
trends <- read.csv(filename1, header=TRUE, stringsAsFactors=FALSE)

# Introduction + Summary Information
#1. average value of black jail population across all the countries in the current year
ave_black_current <- trends %>%
  filter(year == max(year)) %>%
  summarize(mean(black_jail_pop, na.rm = TRUE))

#2. percentage of average value of black jail population across all the countries in the current year
per_black_current <- trends %>%
  filter(year == max(year)) %>%
  summarize(mean(black_jail_pop, na.rm = TRUE) / mean(total_jail_pop, na.rm = TRUE))

#3. where is the highest black jail population
highest_black <- trends %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name, black_jail_pop)

#4. where is the lowest black jail population
lowest_black <- trends %>%
  filter(black_jail_pop == min(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name, black_jail_pop)

#5. average value of black jail population across all the countries in 2008
ave_black_2008 <- trends %>%
  filter(year == 2008) %>%
  summarize(mean(black_jail_pop, na.rm = TRUE))

#6. change from 2008 to 2018 (10 years)
change_10_years <- ave_black_2008 - ave_black_current

# Trends over time chart
# percentage of value of population of different races across all the countries
ratio_by_races <- trends %>%
  group_by(year) %>%
  summarize(black_jail = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
            white_jail = sum(white_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
            aapi_jail = sum(aapi_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
            latinx_jail = sum(latinx_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
            native_jail = sum(native_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE),
            other_jail = sum(other_race_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)
  )

trends_chart <- ggplot(ratio_by_races, aes(x = year)) +
  geom_line(aes(y = black_jail, color = "Black")) +
  geom_line(aes(y = white_jail, color = "White")) +
  geom_line(aes(y = aapi_jail, color = "Asian American / Pacific Islander")) +
  geom_line(aes(y = latinx_jail, color = "Latinx")) +
  geom_line(aes(y = native_jail, color = "Native American")) +
  geom_line(aes(y = other_jail, color = "Unknown or Other Racial Category")) +
  xlab("year") +
  ylab("percentage") +
  ggtitle("percentage of value of population of different races")

# Variable comparison chart
races_total <- trends %>%
  group_by(year) %>%
  filter(year >= 2000 & year <= 2018) %>%
  summarize(black_prop = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE),
            white_prop = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE)
  )


compare_chart <- ggplot(races_total, aes(x = year)) +
  geom_line(aes(y = black_prop, color = "Black")) +
  geom_line(aes(y = white_prop, color = "White")) +
  xlab("Year") +
  ylab("Racial Jail Population") +
  ggtitle("Black Jail Population to White Jail Population Over Time") 

# Map
us_map <- trends %>%
  filter(year == max(year)) %>%
  group_by(state) %>%
  summarize(black_pop = sum(black_jail_pop, na.rm = TRUE) / sum(total_jail_pop, na.rm = TRUE)) %>%
  select(state, black_pop)

map <- plot_usmap(data = us_map, values = "black_pop", color = "black") +
  labs(title = "US Map of Black Jail Percentage") +
  scale_fill_continuous(
    low = "white", high = "red",
    name = "Black Jail Percentage",
    label = scales::comma
  ) + theme(legend.position = "right")

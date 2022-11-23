library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#

## Section 2  ---- 
#----------------------------------------------------------------------------#
find_black_2018 <- incarceration_df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  summarize(Avg_Black_Jail_Pop = round(mean(black_jail_pop, na.rm = TRUE)), digits = 2)
black_2018 <- find_black_2018$Avg_Black_Jail_Pop

type_of_area <- incarceration_df %>%
  group_by(year) %>%
  filter(year == 2018) %>%
  group_by(urbanicity) %>%
  summarize(pop = mean(total_jail_pop, na.rm = TRUE))%>%
  filter(pop == max(pop, na.rm = TRUE)) %>%
  pull(urbanicity)
paste(type_of_area)

modified_df <- rename(incarceration_df, Code = state)
# View(modified_df)

state_code <- read.csv("https://raw.githubusercontent.com/info201b-au2022/a4-sukhd12/main/source/state_names_and_codes.csv")
# View(state_code)

full <- right_join(modified_df, state_code, by = "Code")
# View(full)

find_state_black_2018 <- full %>%
  group_by(year) %>%
  group_by(State) %>%
  filter(year == 2018) %>%
  summarize(Avg_Black_Jail_Pop = round(mean(black_jail_pop, na.rm = TRUE)), digits = 2) %>%
  filter(Avg_Black_Jail_Pop == max(Avg_Black_Jail_Pop, na.rm = TRUE)) %>%
  pull(State)
paste(find_state_black_2018)

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
library("dplyr")
library("ggplot2")
#----------------------------------------------------------------------------#
# This function returns a data frame with the year and total jail population in the U.S. that year. This function takes no parameters. 
get_year_jail_pop <- function() {
  the_totalpop <- incarceration_df %>% 
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
return(the_totalpop)   
}
# View(get_year_jail_pop())

# This function returns a bar graph. This function: (1) Takes no parameters; and (2) Calls the get_year_jail_pop() function.
plot_jail_pop_for_us <- function()  {
  graph <- ggplot(data = get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Increase of Jail Population in the U.S. (1970-2018)", x = "Year", y = "Total Jail Population")
  return(graph)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# This function returns a data frame with the states called and the total jail population for each year in that state. This function takes in a vector of states. 
get_jail_pop_by_states <- function(states) {
  the_statepop <- incarceration_df %>% 
    group_by(state, year) %>%
    filter(state %in% states) %>%
    select(total_jail_pop) %>%
    summarize(total_jail_pop1 = sum(total_jail_pop, na.rm = TRUE))
  return(the_statepop)   
}
# View(get_jail_pop_by_states(c("CA", "NE", "AL", "NY")))

# This function returns the line graph. This function: (1) Takes in a vector of states; and (2) Calls the get_jail_pop_by_states() function.
plot_jail_pop_by_states <- function(states)  {
  graph1 <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = total_jail_pop1, color = state)) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Increase of Jail Population by State (1970-2018)", x = "Year", y = "Total Jail Population", color = "State")
  return(graph1)   
}
plot_jail_pop_by_states(c("CA", "NE", "AL", "NY"))
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# This function returns a data frame with the jailed Black population, the total population, region, and state in 2018. This function takes no parameters. 
region_inequality <- function() {
  the_inequality <- incarceration_df %>% 
    group_by(year) %>%
    filter(year == 2018) %>%
    select(region, state, year, region, total_jail_pop, black_jail_pop)
  return(the_inequality)   
}
# View(region_inequality())

# This function returns the scatterplot. This function: (1) Takes no parameters; and (2) Calls the region_inequality() function
plot_region_inequality <- function()  {
  chart <- ggplot(data = region_inequality()) +
    geom_point(mapping = aes(x = black_jail_pop, y = total_jail_pop, color = region)) +
    ylim(0, 5000) +
    scale_x_continuous(labels = scales::comma) +
    labs(title = "Jailed Black Population vs. Total Jailed Population by Region (2018)", x = "Jailed Black Population", y = "Total Jailed Population", color = "Region")
  return(chart)   
} 
plot_region_inequality()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
full2 <- right_join(modified_df, state_code, by = "Code")
# View(full2)
fuller <- full2 %>%
  rename(state = State) %>%
  mutate(state = tolower(state))
# View(fuller)

# This function returns a data frame with each state and the proportion of Black people in jail out of the total population in jail in 2018. This function takes no parameters. 
geo_inequality <- function() {
  the_ineq <- fuller %>% 
    filter(year == 2018) %>%
    group_by(state) %>%
    summarize(black_jail_pop = sum(black_jail_pop, na.rm = TRUE), total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
    mutate(prop = (black_jail_pop / total_jail_pop)) %>%
    select(state, prop) %>%
  return(the_ineq)   
}
# View(geo_inequality())

# This function returns the map. This function: (1) Takes no parameters; and (2) Calls the geo_inequality() function.
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(geo_inequality(), by = "state")
# View(state_shape)

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

plot_geo_inequality <- function() {
  usa <- ggplot(state_shape) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = prop), color = "white", size = .1) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(title = "Proportion of Black People Jailed by State in 2018", fill = "Proportion") +
    blank_theme
  return(usa)
}
plot_geo_inequality()

# source: I used the textbook (p. 250) to help me create the code for my map.
#----------------------------------------------------------------------------#

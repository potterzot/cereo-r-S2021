# Explore the bike data characteristics
#
# Explore the bike data to look at the relationship between
# temperature and number of riders.
#
# Step 1: Load the bike data and look at the metadata

library(tidyverse)

### Read in the data ----
bike <- read_csv("data/daily_bike_data.csv")

head(bike)
str(bike)
sort(names(bike))

# Time trend of ridership
ggplot(data = bike) +
  geom_line(aes(x = dteday, y = cnt))

summary(bike$dteday)

ggplot(data = bike) +
  geom_point(aes(x = temp, y = cnt))


### Data cleaning ----
# dplyr verbs for data transformations
#   select: select columns that we want to keep
#   filter: select rows that we want to keep
#   mutate: transforms data while keeping other columns
#   transmute: creates new columns and does not keep old columns
#   %>%: "pipe" pipes the output from one command as the input 
#        for the next command
bike %>% select(dteday, season, weathersit, temp, cnt)
select(bike, dteday, season, weathersit, temp, cnt)

# One way of selecting spring records and just a few cols
spring_bike <- filter(bike, season == "spring")
spring_bike_temp_cnt <- select(spring_bike, temp, cnt)

spring_bike_temp_cnt2 <- bike %>%
  filter(season == "spring") %>%
  select(temp, cnt)

## Exercise: select weathersit and cnt for all winter records
spring_bike_temp_cnt2 <- bike %>%
  filter(season == "winter") %>%
  select(weathersit, cnt)

### Mutate and Transmute with Factors and Dates
summary(bike$weathersit)
unique(bike$weathersit)

# Can reference the data documentation:
# https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
bike2 <- bike %>%
  dplyr::mutate(
    weather_fac = factor(weathersit, 
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain")))

bike2 %>% select(dteday, weathersit, weather_fac)

### Converting to and from dates
sort(names(bike))
bike_dates <- bike %>% transmute(
  instant,
  date = dteday,
  date_num = as.numeric(dteday),
  date_char = as.character(dteday)
)

bike_dates %>% 
  transmute(
    instant,
    date,
    date_num = as.Date(date_num, origin = "1970-01-01"),
    date_char = as.Date(date_char)
  )

### Additional Filtering and Selecting
bike %>% select(dteday, cnt)
bike %>% select(dteday, cnt, temp) %>% select(-temp)

keep_vars <- c("dteday", "cnt", "temp")
keep_vars <- paste0("temp", 1:12)
bike %>% select( all_of(keep_vars))

### Filtering
bike %>% filter(season == "spring")

#'! ='
bike %>% 
  filter(season != "spring") %>% 
  select(season) %>%
  distinct()

bike %>%
  filter(season == "summer" | season == "winter")

seasons <- c("summer", "winter")
bike %>%
  filter(season %in% seasons)


## More dplyr verbs
# summarize: summary of multiple rows for a col/variable
# group_by: perform an operation separately for each group
bike2 %>% 
  filter(season == "summer") %>%
  summarize(
    temp_mean = mean(temp),
    cnt_mean = mean(cnt),
    cnt_sum = sum(cnt)
)

bike2 %>%
  group_by(season) %>%
  summarize(
    temp_mean = mean(temp),
    ride_sum = sum(cnt)
  )

# What are the season definitions?
# Create a new season with meteoroligical definitions
sort(names(bike))
bike %>% select(season, mnth) %>% distinct()

bike3 <- bike2 %>%
  mutate(
    season2 = 1 * (mnth %in% c("December", "January", "February")) +
      2 * (mnth %in% c("March", "April", "May")) +
      3 * (mnth %in% c("June", "July", "August")) +
      4 * (mnth %in% c("September", "October", "November"))) %>%
  mutate(
    season2 = factor(season2, levels = 0:4,
                     labels = c("Unknown", "Winter", "Spring", "Summer", "Fall"))
  )

bike3 %>%
  group_by(season2) %>%
  summarize(
    temp_mean = mean(temp),
    ride_sum = sum(cnt)
  )

## Facetting in ggplot
ggplot(data = bike3) +
  geom_point(aes(x = temp, y = cnt)) +
  geom_smooth(aes(x = temp, y = cnt), 
              method = "lm",
              formula = y ~ poly(x,2)) + 
  facet_wrap(~ season2)
  
## Pivoting wider to long and longer to wide
# Long to wide: data in multiple columns. e.g. temp1, temp2, temp3... temp12
# Wide to long: data in one column, classifier in other columns
# tidyr is the package that allows transformations
months <- c("January", "February", "March", "April",
            "May", "June", "July", "August",
            "September", "October", "November", "December")
tidybike <- bike3 %>%
  select(yr, mnth, temp, cnt) %>%
  mutate(month = factor(mnth, 
                        levels = months,
                        labels = months)) %>%
  group_by(yr, month) %>%
  summarize(temp_mean = mean(temp),
            rides = sum(cnt))

# Tidyr functions for long to wide
# spread
# pivot_wider
tidybike %>% 
  select(-rides) %>%
  pivot_wider(values_from = temp_mean, 
                         names_from = month,
                         names_prefix = "temp_")

tidybike %>%
  select(-rides) %>%
  tidyr::spread(key = month, value = temp_mean)

rides <- tidybike %>% 
  select(-temp_mean) %>%
  pivot_wider(values_from = rides, 
              names_from = month,
              names_prefix = "rides_") %>%
  rename_with(tolower) %>%
  rename(year = yr)

## Going from wide to long
# pivot_longer
# gather
rides %>% gather(key = "month", value = "rides", -year)
rides %>% 
  select(year, rides_january, rides_february) %>%
  pivot_longer(names_to = "month", 
               values_to = "rides", 
               cols = c("rides_january", "rides_february")) %>%
  mutate(month = substr(month, 7, nchar(month))) %>%
  mutate(month = toupper(month))

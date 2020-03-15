library(maps)
library(tidyverse)
library(lubridate)
library(viridis)
library(gganimate)
library(magick)

covid <- paste0("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/",
                "csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>%
  read_csv() %>%
  pivot_longer(cols = -c(`Province/State`:Long),
               names_to = "time",
               values_to = "cases") %>%
  mutate(time = mdy(time)) %>%
  janitor::clean_names() %>%
  filter(country_region == "US") %>%
  select(-country_region) %>%
  rename(region = province_state) %>%
  filter(long > -124.6813 & long < -66.5901) %>%
  filter(time > mdy("03/01/2020"))
  
usa <- map_data("state")

animate_map <- usa %>%
  ggplot() +
  theme_bw() +
  geom_polygon(aes(x = long, y = lat, group = group),
               fill="white", color="grey60") +
  geom_point(data = covid,
             aes(x = long, y = lat, size = cases, color = cases)) +
  scale_color_viridis() +
  coord_fixed(1.3) +
  labs(title = "Date: {frame_time}") +
  transition_time(time) +
  ease_aes('linear')

map_gif <- animate(animate_map, width = 600, height = 400, fps = 30)

animate_cases <- covid %>%
  group_by(time) %>%
  summarise(cases = sum(cases)) %>%
  ggplot(aes(x = time, y = cases)) +
  theme_bw() +
  geom_line() +
  transition_reveal(time)

cases_gif <- animate(animate_cases, width = 200, height = 400, fps = 30)

# The following shamelessly stolen from:
# https://github.com/thomasp85/gganimate/wiki/Animation-Composition

a_mgif <- image_read(map_gif)
b_mgif <- image_read(cases_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

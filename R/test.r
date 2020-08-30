library(sf)
library(tidyverse)
library(USAboundaries)

states <- USAboundaries::us_states()

tmp <- states %>% 
  dplyr::filter(grepl('North',name))

#plot(tmp$geometry, col = 'red')

nearest_state_plot <- function(name) {
  state.of.interest = name

states = us_states() %>% st_transform(5070)

soi = states %>% filter(state_name == state.of.interest)

adjoining <- st_filter(states, soi, .predicate = st_touches)

sample = st_make_grid(soi, n = 70) %>% 
  st_sf() %>% 
  st_centroid()

#plot(sample$geometry)

closest = st_join(sample, adjoining, join = st_nearest_feature)

# ggplot() + 
#   geom_sf(data = closest, aes(fill = state_name))

voronoi = closest %>% 
  st_union() %>%
  st_voronoi() %>% 
  st_cast() %>%
  st_as_sf()

#plot(voronoi)

v_state <- st_join(voronoi, closest)

combined <- v_state %>% 
  group_by(state_name) %>% 
  summarize() %>% 
  st_intersection(soi)

# ggplot() + 
#   geom_sf(data =  v_state, aes(fill = state_name), col = NA)

ggplot() + 
  geom_sf(data=adjoining, aes(fill = state_name)) +
  geom_sf(data = combined, aes(fill = state_name), col = NA) + 
  geom_sf(data = soi, col = 'black', fill = 'white', alpha = .5) + 
  theme_minimal() +
  labs(fill = '')

}

nearest_state_plot('California')
  
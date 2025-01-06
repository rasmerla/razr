## code to prepare `DATASET` dataset goes here

usethis::use_data(world_map, internal = TRUE)

world_map <- sf::st_read("c:/Users/raer1762/Box/r-code mixed/world.geo.json") %>%
  dplyr::select(name, iso_a3, continent)

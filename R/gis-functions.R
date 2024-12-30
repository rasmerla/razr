utils::globalVariables(c("name_of_city", "geometry", "x"))


#' City coordinates from name
#'
#' @param city_name Name to look for in Open Street Map
#'
#' @return A simple feature with the name and the coordinates of the centre of the city.
#' @export
#'
#' @examples alingsas <- city_coords_from_op_str_map("Alingsås")
city_coords_from_op_str_map <- function(city_name){
  city_coordinates <- osmdata::getbb(city_name) %>% # Obtain the bounding box corners fro open street map
    t() %>% # Transpond the returned matrix so that you get x and y coordinates in different columns
    data.frame() %>% # The next function takes a data frame as input
    sf::st_as_sf(coords = c("x", "y")) %>%  # Convert to simple feature
    sf::st_bbox() %>% # get the bounding box of the corners
    sf::st_as_sfc() %>% # convert bounding box to polygon
    sf::st_centroid() %>% # get the centroid of the polygon
    sf::st_as_sf() %>% # store as simple feature
    sf::`st_crs<-`(4326)  # set the coordinate system to WGS84 (GPS etc.)

  city_coordinates %>%
    dplyr::mutate(name_of_city = city_name) %>% # add input city name in a column
    dplyr::rename(geometry = x) %>% # Rename the coordinate column
    dplyr::relocate(name_of_city, geometry) %>% # reorder the columns
    return()
}

utils::globalVariables(c("Y","X", "npts"))

# Map over study sites of the Greenseeker manuscript:
# library(dplyr)
# library(tmap)
# library(tmaptools)
# library(sf)
# library(grid)
# library(spData)
# library(magrittr)


# Globe_function ----------------------------------------------------------------

# library(sf)
# library(lwgeom)
# library(dplyr)
# library(mapview)


# globe_map(57.93, 12.530833, alpha_border=0.3,
# #          col_land = "grey60", col_water = "white",
# #          alpha_grid = 0.5,
#           col_grid = "grey60",
#           col_border = "white")
#
#
# globe_map(-25, -120, alpha_border=0)


#' Title
#'
#' @param lat Latitude of centre point
#' @param lon Longitude of centre point
#' @param col_land Land colour
#' @param col_water Water colour
#' @param col_grid Grid colour
#' @param col_border Border colour
#' @param alpha_border Transparency of border
#' @param alpha_grid Transparency of grid
#' @param n_longs Number of longitudes to show
#' @param n_lats Number of latitudes to show
#' @param show_coordinate_text Logical, show coordinates figures?
#' @param bbox_input Boundary box if you want to display less
#' @param custom_map Custom globe map to project
#' @param get_only_crs Logical: Do you wish only to return the CRS of the projection?
#'
#' @return An orhtographic map of the world centred on the given coordinates.
#' @export
#'
globe_map <- function(lat, lon,
                      col_land = "#e9d6bd",
                      col_water = "#abc1b1",
                      col_grid = "aliceblue",
                      col_border = "grey45",
                      alpha_border = 1,
                      alpha_grid = 0.8,
                      n_longs = NULL,
                      n_lats = NULL,
                      show_coordinate_text=F,
                      bbox_input = NULL,
                      custom_map = NULL,
                      get_only_crs = NULL) {

  #restore.point("globe", to.global = F)
  # The script does not support s2 geometry
  s2_mode <- sf::sf_use_s2()
  if(s2_mode==TRUE) {
    sf::sf_use_s2(FALSE)
  }

  # Define the orthographic projection
  # Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
  lat # <- 45
  lon # <- 2
  ortho <-
    paste0(
      '+proj=ortho +lat_0=',
      lat,
      ' +lon_0=',
      lon,
      ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs'
    )

  if(!is.null(get_only_crs)){
  if(get_only_crs == TRUE) {
    return(ortho)
  }}

  # Define the polygon that will help you finding the "blade"
  # to split what lies within and without your projection
  circle <- sf::st_point(x = c(0, 0)) %>%
    sf::st_buffer(dist = 6371000) %>% sf::st_sfc(crs = ortho)

  # Project this polygon in lat-lon
  circle_longlat <- circle %>% sf::st_transform(crs = 4326)

  # circle_longlat cannot be used as it is
  # You must decompose it into a string with ordered longitudes
  # Then complete the polygon definition to cover the hemisphere
  if (lat != 0) {
    circle_longlat <- sf::st_boundary(circle_longlat)

    circle_coords <- sf::st_coordinates(circle_longlat)[, c(1, 2)]
    circle_coords <- circle_coords[order(circle_coords[, 1]), ]
    circle_coords <- circle_coords[!duplicated(circle_coords), ]

    # Rebuild line
    circle_longlat <-
      sf::st_linestring(circle_coords) %>%
      sf::st_sfc(crs = 4326)

    if (lat > 0) {
      rectangle <- list(rbind(
        circle_coords,
        c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
        c(X = 180, Y = 90),
        c(X = -180, Y = 90),
        c(X = -180, circle_coords[1, 'Y']),
        circle_coords[1, c('X', 'Y')]
      )) %>%
        sf::st_polygon() %>% sf::st_sfc(crs = 4326)
    } else {
      rectangle <- list(rbind(
        circle_coords,
        c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
        c(X = 180, Y = -90),
        c(X = -180, Y = -90),
        c(X = -180, circle_coords[1, 'Y']),
        circle_coords[1, c('X', 'Y')]
      )) %>%
        sf::st_polygon() %>% sf::st_sfc(crs = 4326)
    }

    circle_longlat <-
      sf::st_union(sf::st_make_valid(circle_longlat), sf::st_make_valid(rectangle))
  }

  # This visualization shows the visible hemisphere in red
  # qtm(circle_longlat,
  #     color = 'red',
  #     fill = 'red',
  #     alpha = 0.3)

  # A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
  # It should not change the shapes too much
  if(!is.null(custom_map)) {world <- custom_map}

  visible <- sf::st_intersection(sf::st_make_valid(world),
                                 sf::st_buffer(circle_longlat,-0.09)) %>%
    sf::st_transform(crs = ortho)

  # DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach
  # Resulting polygons are often broken and they need to be fixed
  # Get reason why they're broken
  broken_reason <- sf::st_is_valid(visible, reason = TRUE)

  # First fix NA's by decomposing them
  # Remove them from visible for now
  na_visible <- visible[is.na(broken_reason), ]
  visible <- visible[!is.na(broken_reason), ]

  # Treat invisible polygons, if there are any:
  if (nrow(na_visible)>0) {
    # Open and close polygons
    na_visible <- sf::st_cast(na_visible, 'MULTILINESTRING') %>%
      sf::st_cast('LINESTRING', do_split = TRUE)
    na_visible <-
      na_visible %>% dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE))
    # Exclude polygons with less than 4 points
    na_visible <- na_visible %>%
      dplyr::filter(npts >= 4) %>%
      dplyr::select(-npts) %>%
      sf::st_cast('POLYGON')
  }


  # Fix other broken polygons
  # List number of broken polygons:
  visible_invalid <- which(!sf::st_is_valid(visible)) %>% length()

  # If there are any, fix them:
  if (visible_invalid > 0) {

    broken <- which(!sf::st_is_valid(visible))
    for (land in broken) {
      result = tryCatch({
        # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
        visible[land, ] <- sf::st_make_valid(visible[land, ]) %>%
          sf::st_collection_extract()
      }, error = function(e) {
        visible[land, ] <<- sf::st_buffer(visible[land, ], 0)
      })
    }
  }


  if (nrow(na_visible)>0) {
    # Bind together the two tables
    visible <- rbind(visible, na_visible)
  }


  # Defined graticules:
  if (is.null(n_longs)) {
    n_longs <- 19
  }

  if (is.null(n_lats)) {
    n_lats <- 13
  }

  longs = seq(from=-180, to = 180, length.out = n_longs)
  longs <- longs[1:length(longs)-1]
  lats = seq(from=-90, to = 90, length.out = n_lats)

  # Final plot
  output_map <-
    {if(is.null(bbox_input))tmap::tm_shape(circle)}+
    {if(!is.null(bbox_input))tmap::tm_shape(circle, bbox = bbox_input, is.master = T)} + # Line to cut to bbox, if wanted.
    tmap::tm_polygons(fill = col_water) +
    tmap::tm_shape(visible) +
    tmap::tm_polygons(col = col_land, border.col = col_border, border.alpha = alpha_border) +
    #tm_graticules(col = col_grid, alpha = alpha_grid, n.x = 36, n.y = 8) +
    tmap::tm_graticules(col = col_grid, alpha = alpha_grid,
                  x = longs,
                  y = lats,
                  labels.show=show_coordinate_text) +
    tmap::tm_shape(circle) +
    tmap::tm_polygons(alpha = 0, border.col = "grey45")

  #sf::sf_use_s2(s2_mode)

  return(output_map)

}






cat("Examples:\n
globe_map(57.93, 12.530833, alpha_border=0.3,
              col_land = \"grey60\", col_water = \"white\",
              alpha_grid = 0.5,
              col_grid = \"grey60\",
              col_border = \"white\")
    \n
globe_map(-25, -120, alpha_border=0)")

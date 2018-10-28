library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(geosphere)
library(grid)
library(readr)
register_google(key="AIzaSyDOVxhdeqza6Akwmv9rSBS0sMl-oUd4Y0c")

########################################################################################
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- suppressMessages(read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99"))
ext_tracks %<>%
  dplyr::mutate(storm_id = paste0(storm_name, "-", year)) %>%
  dplyr::mutate(date = paste0(year, "-", month, "-", day, " ", hour, ":00:00")) %>%
  dplyr::mutate(latitude = latitude, longitude = -longitude) %>%
  dplyr::filter(distance_to_land <= 0)

########################################################################################

long_hur <- ext_tracks %>%
  dplyr::mutate(wind_speed = 34,
                ne = radius_34_ne,
                se = radius_34_se,
                sw = radius_34_sw,
                nw = radius_34_nw) %>%
  dplyr::select(storm_id, date, latitude, longitude, distance_to_land, wind_speed, ne, se, nw, sw)

long_hur <- rbind(long_hur, ext_tracks %>%
                    dplyr::mutate(wind_speed = 50,
                                  ne = radius_50_ne,
                                  se = radius_50_se,
                                  sw = radius_50_sw,
                                  nw = radius_50_nw) %>%
                    dplyr::select(storm_id, date, latitude, longitude, distance_to_land, wind_speed, ne, se, nw, sw)
)

long_hur <- rbind(long_hur, ext_tracks %>%
                    dplyr::mutate(wind_speed = 64,
                                  ne = radius_64_ne,
                                  se = radius_64_se,
                                  sw = radius_64_sw,
                                  nw = radius_64_nw) %>%
                    dplyr::select(storm_id, date, latitude, longitude, distance_to_land, wind_speed, ne, se, nw, sw)
)

ike <- long_hur %>%
  filter(storm_id == "IKE-2008") %>%
  filter(latitude == 30.3)

########################################################################################

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x," ^o", "*W")), ifelse(x > 0, parse(text=paste0(x," ^o", "*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x," ^o", "*S")), ifelse(x > 0, parse(text=paste0(x," ^o", "*N")),x))))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}  

########################################################################################
#' StatHurricane stat definition
#' 
#' This is a simple definition of StatHurricane using ggplot2 ggproto function
#' Defines aesthethics and compute_group function, which processes the data to create the
#' (x,y) points for GeomHurricane geom
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom geosphere destPoint
#' 
#' @param x Hurricane center longitude (in degrees)
#' @param y Hurricane center latitude (in degrees)
#' @param r_ne Radii in NE direction from hurricane center (in Nautical miles) 
#' @param r_se Radii in SE direction from hurricane center (in Nautical miles) 
#' @param r_sw Radii in SW direction from hurricane center (in Nautical miles) 
#' @param r_nw Radii in NW direction from hurricane center (in Nautical miles)
#' @param scale_radii Allows scaling of the geom object (1 = 100%)
#' 
#' @return This function returns a grid object, which represents how far winds
#' of a certain intensity extend from a hurricane’s center
#' 
#' @export
StatHurricane <- ggproto("StatHurricane", Stat,
                         required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw", "scale_radii"),
                         compute_group = function(data, scales){
                           circle <- data.frame()
                           circle <- rbind(circle, geosphere::destPoint(p = c(data$x, data$y),
                                                                        b =    0:90, d = data$r_ne * 1852 * data$scale_radii))
                           circle <- rbind(circle, geosphere::destPoint(p = c(data$x, data$y),
                                                                        b =  90:180, d = data$r_se * 1852 * data$scale_radii))
                           circle <- rbind(circle, geosphere::destPoint(p = c(data$x, data$y),
                                                                        b = 180:270, d = data$r_sw * 1852 * data$scale_radii))
                           circle <- rbind(circle, geosphere::destPoint(p = c(data$x, data$y),
                                                                        b = 270:360, d = data$r_nw * 1852 * data$scale_radii))
                           colnames(circle) <- c("x", "y")
                           circle <- cbind(circle, r_ne = data$r_ne, r_se = data$r_se, r_sw = data$r_sw, r_nw = data$r_nw)
                           circle
                         }
)

#' GeomHurricane geom definition
#' 
#' This is a simple definition of GeomHurricane using ggplot2 ggproto function
#' Defines aesthethics, draw_key and draw_group function, which creates the polygon grob
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom grid polygonGrob gpar
#' @importFrom scales alpha
#' 
#' @param x Hurricane center longitude (in degrees)
#' @param y Hurricane center latitude (in degrees)
#' @param fill Parameter along which the contours will be filled with colour
#' @param color Parameter along which the records will be grouped
#' 
#' @return This function returns a grid object, which represents how far winds
#' of a certain intensity extend from a hurricane’s center
#' 
#' @export
GeomHurricane <- ggproto("GeomHurricane", Geom,
                         required_aes = c("x", "y", "fill", "colour"),
                         default_aes = aes(shape = 19, colour = NA, fill = "grey20", size = 1,
                                           linetype = 1, alpha = 0.7),
                         draw_key = draw_key_polygon,
                         draw_group = function(data, panel_params, coord){
                           coords <- coord$transform(data, panel_params)
                           first_row <- coords[1, , drop = FALSE]
                           grid::polygonGrob(
                             coords$x, coords$y,
                             gp = grid::gpar(
                               col = first_row$colour,
                               fill = scales::alpha(first_row$fill, first_row$alpha),
                               lwd = first_row$size * .pt,
                               lty = first_row$linetype
                             )
                           )
                         }
)

#' geom_hurricane layer function
#' 
#' This is a layer function, which allows parsing parameters from the geom_hurricane
#' to a call of ggplot2 layer function
#' 
#' @importFrom ggplot2 layer
#' 
#' @param mapping Set of aesthetic mappings created by aes() or aes_()
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param na.rm Parameter to be passed to stat and geom
#' @param show.legend (logical) Should this layer be included in the legends? NA includes if any aes are mapped
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them
#' @param x Hurricane center longitude (in degrees)
#' @param y Hurricane center latitude (in degrees)
#' @param r_ne Radii in NE direction from hurricane center (in Nautical miles) 
#' @param r_se Radii in SE direction from hurricane center (in Nautical miles) 
#' @param r_sw Radii in SW direction from hurricane center (in Nautical miles) 
#' @param r_nw Radii in NW direction from hurricane center (in Nautical miles) 
#' @param fill Parameter along which the contours will be filled with colour
#' @param color Parameter along which the records will be grouped
#' @param scale_radii Allows scaling of the geom object (1 = 100%)
#' 
#' @return This function returns a grid object, which represents how far winds
#' of a certain intensity extend from a hurricane’s center
#' 
#' @examples
#' geom_hurricane(data = ike[-c(1,2,5)], aes(x = longitude, y = latitude,
#'                r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                fill = factor(wind_speed), color = factor(wind_speed),
#'                scale_radii = 0.8))
#' 
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "Hurricane",
                         position = "identity", na.rm = FALSE, 
                         show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHurricane, mapping = mapping,  
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

########################################################################################

HoustonMap <- suppressMessages(get_map("Houston", zoom = 6, maptype = "toner-background"))
base_map <- ggmap(HoustonMap, extent = "device")

base_map +
  geom_hurricane(data = ike[-c(1,2,5)], aes(x = longitude, y = latitude,
                                            r_ne = ne, r_se = se,
                                            r_nw = nw, r_sw = sw,
                                            fill = factor(wind_speed),
                                            color = factor(wind_speed),
                                            scale_radii = 0.8)) +
  scale_color_manual(name = "Wind speed (kts)",
                     values = c("red", "orange", "yellow")) +
  scale_fill_manual(name = "Wind speed (kts)",
                    values = c("red", "orange", "yellow")) +
  ggtitle("Hurricane Ike (2008) near Houston, TX") +
  scale_x_longitude(xmin=-102, xmax=-88, step=4) +
  scale_y_latitude(ymin=23, ymax=35, step=2) +
  theme(axis.text = element_text(size=8))
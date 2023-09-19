#' Convert glider GPS to decimal degrees
#'
#' Helper function that converts m_gps_* and
#' m_lat/m_lon to decimal degrees.
#'
#' @param gliderGPS input value to be converted
#' @return numeric output value
#' @export
gliderGPS_to_dd <- function(gliderGPS) {
    df <- data.frame(gps = as.character(gliderGPS))

    dd <- df %>%
      tidyr::separate("gps", paste0("gpss", c("d","m")), sep="\\.", remove = FALSE) %>% #have to double escape to sep by period
      dplyr::mutate(gpsd = substr(.data$gpssd, 1, nchar(.data$gpssd)-2)) %>% #pull out degrees
      dplyr::mutate(gpsm = paste0(stringr::str_sub(.data$gpssd, start= -2),".", .data$gpssm)) %>% #pull out minutes
      dplyr::mutate(dplyr::across(.data$gpsd:.data$gpsm, as.numeric)) %>% #coerce back to numeric
      dplyr::mutate(gpsdd = ifelse(sign(.data$gpsd) == -1, (abs(.data$gpsd) + (.data$gpsm/60))*-1, (abs(.data$gpsd) + (.data$gpsm/60)))) # check if neg and multiply by -1 if needed

    return(dd$gpsdd)
  }

#' Interpolate depth over time
#'
#' Uses zoo package to build dataframe with interpolated
#' depth values through all available times
#'
#' @param inGliderdf input dataframe that must contain a depth measure
#' @param CTD boolean, use CTD-derived depth, else use m_depth
#' @return outputted dataframe with time and depth
#' @export
depthInt <- function(inGliderdf, CTD = TRUE){
    qf <- inGliderdf

    #rename depthVar for processing
    if (CTD == TRUE){
      qf$depthVar = qf$osg_depth
    } else {
      qf$depthVar = qf$m_depth
    }

    ef <- qf %>%
      dplyr::select(c(.data$m_present_time, .data$depthVar))

    #coerce as dataframe
    ef <- as.data.frame(ef) %>%
      dplyr::arrange(.data$m_present_time) #ensure chronological order

    #cutoff at seconds
    ef$m_present_time <- lubridate::as_datetime(floor(lubridate::seconds(ef$m_present_time)))

    #depth interpolation
    full.time <- with(ef, seq(m_present_time[1], utils::tail(m_present_time, 1), by = 1)) #grab full list of timestamps
    depth.zoo <- zoo::zoo(ef$depthVar, ef$m_present_time) #convert to zoo
    result <- zoo::na.approx(depth.zoo, xout = full.time) #interpolate

    idepth <- zoo::fortify.zoo(result) %>% #extract out as DF
      dplyr::rename(osg_i_depth = result) %>%
      dplyr::rename(m_present_time = .data$Index) %>%
      dplyr::mutate(m_present_time = lubridate::as_datetime(.data$m_present_time))

    #force both time sets to match (i.e., round to 1sec)
    idepth$m_present_time <- lubridate::as_datetime(floor(lubridate::seconds(idepth$m_present_time)))

    return(idepth)
  }

#' Track glider state through time
#'
#' Using glider depth, determine if the vehicle is ascending, descending,
#' or at the surface and label it as such.
#'
#' @param data input dataframe that must contain a depth column
#' @param surface_threshold numeric, how deep is considered "surface"?
#' @param rolling_window_size numeric, how many depth samples to check across?
#' @return appended dataframe with new "cast" column indicating state
#' @export
identify_casts_smooth <- function(data, surface_threshold, rolling_window_size) {
    data$cast <- NA
    cast_state <- "Downcast"

    # Smooth the depth data with a rolling average
    data$smoothed_depth <- zoo::rollapply(data$osg_i_depth, rolling_window_size, mean, align = "right", fill = NA)

    for (i in 1:nrow(data)) {
      if (is.na(data$smoothed_depth[i])) {
        cast_state <- "Unknown"
      } else if (is.na(data$smoothed_depth[i - 1])) {
        cast_state <- "Unknown"
      } else if (data$smoothed_depth[i] <= surface_threshold) {
        cast_state <- "Surface"
      } else if (cast_state == "Surface" && data$smoothed_depth[i] > surface_threshold) {
        cast_state <- "Downcast"
      } else if (data$smoothed_depth[i] > data$smoothed_depth[i - 1]) {
        cast_state <- "Downcast"
      } else if (data$smoothed_depth[i] < data$smoothed_depth[i - 1]) {
        cast_state <- "Upcast"
      }

      data$cast[i] <- cast_state
    }

    # Remove the temporary smoothed_depth column
    data <- data[, -ncol(data)]

    return(data)
  }

#' Add yo ID to gliderdf
#'
#' Sequentially count downcast and upcast cycles
#'
#' @param df input dataframe that must contain a column named "cast"
#' with values "Downcast" and "Upcast"
#' @return appended dataframe with new "yo_id" column
#' @export
add_yo_id <- function(df) {
    df$yo_id <- cumsum(df$cast == "Downcast" & c(FALSE, df$cast[-length(df$cast)] == "Upcast")) + 1
    return(df)
  }

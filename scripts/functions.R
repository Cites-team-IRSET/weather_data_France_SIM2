# this function allows to zoom in an area depending on latitude and longitude  ####
filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- sf::st_bbox(.data)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}

# Function: join heatwave episodes that are separated by less than 3 days
fuse_episodes <- function(hw, max_gap) {
  hw <- ifelse(is.na(hw), 0L, as.integer(hw))
  
  repeat {
    r      <- rle(hw)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    merged <- FALSE
    
    for (k in seq_along(r$values)) {
      if (r$values[k] == 0L       &&   # gap entre deux épisodes
          r$lengths[k] < max_gap  &&   # gap strict < 3 jours (1 ou 2 jours)
          k > 1L                  &&
          k < length(r$values)    &&
          r$values[k - 1L] == 1L &&
          r$values[k + 1L] == 1L) {
        hw[starts[k]:ends[k]] <- 1L
        merged <- TRUE
        break
      }
    }
    if (!merged) break
  }
  hw
}

# Function: join heatwave episodes that are separated by less than 3 days
# only if none of these days have temp<Sint
fuse_episodes_if <- function(hw, Sint, max_gap) {
  hw <- ifelse(is.na(hw), 0L, as.integer(hw))
  
  repeat {
    r      <- rle(hw)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    merged <- FALSE
    
    for (k in seq_along(r$values)) {
      if (r$values[k] == 0L          &&   # gap entre deux épisodes
          r$lengths[k] < max_gap     &&   # gap strict < 3 jours
          k > 1L                     &&
          k < length(r$values)       &&
          r$values[k - 1L] == 1L    &&
          r$values[k + 1L] == 1L) {
        
        gap_idx <- starts[k]:ends[k]
        
        # Fusionner SEULEMENT si aucun jour du gap n'est < Sint
        if (all(Sint[gap_idx] == 0L)){
          hw[gap_idx] <- 1L
          merged <- TRUE
          break
        }
      }
    }
    if (!merged) break
  }
  hw
}
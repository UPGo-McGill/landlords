### HELPER FUNCTIONS

## Load libraries and fonts


library(tidyverse)
library(binaryLogic)
library(corrplot)
library(corrr)
library(igraph)
library(ggraph)
library(sf)
library(dplyr)
library(data.table)

#library(caret)



## Multilistings function

strr_multilistings <- function(daily, EH = 2, TL = 3, listing_type, host_ID,
                               date, cores){
  
  listing_type <- enquo(listing_type)
  host_ID <- enquo(host_ID)
  date <- enquo(date)
  
  daily %>%
    group_by(!! host_ID, !! listing_type, !! date)  %>%
    mutate(ML = ifelse(
      n() >= EH & !! listing_type == "Entire home/apt", TRUE,
      ifelse(n() >= TL, TRUE, FALSE))) %>%
    ungroup()
}


##FREH function

strr_FREH <- function(daily, start_date, end_date, R_cut = 90, AR_cut = 183,
                      cores = 1) {
  # Wrangle dates
  start_date <- as.Date(start_date, origin = "1970-01-01")
  end_date <- as.Date(end_date, origin = "1970-01-01")
  # Filter daily file
  setDT(daily)
  daily <-
    daily[Status %in% c("A", "R") & Date >= start_date - 364 &
            Date <= end_date & Listing_Type == "Entire home/apt"]
  if (cores > 1) {
    cl <- parallel::makeForkCluster(cores)
    pbapply::pblapply(start_date:end_date, function(date) {
      daily <- daily[Date >= date - 364 & Date <= date]
      daily[, AR := .N, by = Property_ID]
      daily[, R := sum(Status == "R"), by = Property_ID]
      daily[, list(Date = as.Date(date, origin = "1970-01-01"),
                   FREH = as.logical((mean(AR) >= AR_cut) * (mean(R) >= R_cut))),
            by = Property_ID]
    }) %>% rbindlist()
  } else {
    lapply(start_date:end_date, function(date) {
      daily <- daily[Date >= date - 364 & Date <= date]
      daily[, AR := .N, by = Property_ID]
      daily[, R := sum(Status == "R"), by = Property_ID]
      daily[, list(Date = as.Date(date, origin = "1970-01-01"),
                   FREH = as.logical((mean(AR) >= AR_cut) * (mean(R) >= R_cut))),
            by = Property_ID]
    }) %>% rbindlist()
  }
}

        ##set start and end date for FREH function
        start_date <- "2018-05-01"
        end_date <- "2019-04-30"


## Ghost hotel function

strr_ghost <- function(
  points, property_ID, host_ID, created = NULL, scraped = NULL,
  start_date = NULL, end_date = NULL, distance = 200, min_listings = 3,
  listing_type = NULL, private_room = "Private room", EH_check = NULL,
  cores = 1) {
  
  ## ERROR CHECKING AND ARGUMENT INITIALIZATION
  
  # Check that cores is an integer > 0
  cores <- floor(cores)
  if (cores <= 0) {
    stop("The argument `cores` must be a positive integer.")
  }
  
  # Check that distance > 0
  if (distance <= 0) {
    stop("The argument `distance` must be a positive number.")
  }
  
  # Check that min_listings is an integer > 0
  min_listings <- floor(min_listings)
  if (min_listings <= 0) {
    stop("The argument `min_listings` must be a positive integer.")
  }
  
  # Check if EH_check and listing_type agree
  if (missing(listing_type)) EH_check <- NULL
  
  # Parse dates
  ##### DEAL WITH MISSING DATE FIELDS, MAYBE WITH SEPARATE FUNCTION?
  
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  
  # Convert points from sp
  if (is(points, "Spatial")) {
    points <- st_as_sf(points)
  }
  
  # Check that points is sf
  if (is(points, "sf") == FALSE) {
    stop("The object `points` must be of class sf or sp.")
  }
  
  # Convert points to tibble
  points <- as_tibble(points) %>% st_as_sf()
  
  # Quote variables
  property_ID <- enquo(property_ID)
  host_ID <- enquo(host_ID)
  created <- enquo(created)
  scraped <- enquo(scraped)
  
  ## POINTS SETUP
  
  # Remove invalid listings
  points <-
    points %>%
    filter(!! host_ID != 0, is.na(!! host_ID) == FALSE)
  
  # Filter to private rooms if listing_type != NULL
  if (!missing(listing_type)) {
    listing_type <- enquo(listing_type)
    
    # Save entire-home listings for later if EH_check != NULL
    if (!missing(EH_check)) {
      EH_points <- filter(points, !! listing_type == EH_check)
    }
    
    points <-
      points %>%
      filter(!! listing_type == private_room)
  }
  
  # Filter points to clusters >= min_listings, and nest by Host_ID
  points <-
    points %>%
    arrange(!! host_ID) %>%
    group_by(!! host_ID) %>%
    filter(n() >= min_listings) %>%
    tidyr::nest()
  
  # Identify possible clusters by date
  points <-
    points %>%
    mutate(
      starts = map(.data$data, ~{
        filter(.x, !! created > start_date) %>%
          pull(!! created) %>%
          unique() %>%
          c(start_date)
      }),
      ends = map(.data$data, ~{
        filter(.x, !! scraped < end_date) %>%
          pull(!! scraped) %>%
          unique() %>%
          c(end_date)
      }),
      date_grid = map2(.data$starts, .data$ends, expand.grid),
      date_grid = map(.data$date_grid, filter, .data$Var1 <= .data$Var2)
    )
  
  # Create a nested tibble for each possible cluster
  points <-
    points %>%
    mutate(data = map2(.data$date_grid, .data$data, function(x, y) {
      apply(x, 1, function(z) {
        filter(y, !! created <= z[1], !! scraped >= z[2])
      }) %>%
        unique() %>%
        `[`(lapply(., nrow) >= min_listings)
    })) %>%
    tidyr::unnest(.data$data)
  
  
  ## CLUSTER CREATION AND GHOST HOTEL IDENTIFICATION
  
  # Multi-threaded version
  if (cores >= 2) {
    
    clusters <- pbapply::splitpb(nrow(points), cores, nout = 100)
    points_list <- lapply(clusters, function(x) points[x,])
    cl <- parallel::makeForkCluster(cores)
    
    points <-
      points_list %>%
      pbapply::pblapply(function(x) {
        x %>%
          ghost_cluster(distance, min_listings) %>%
          ghost_intersect(!! property_ID, !! host_ID, distance,
                          min_listings) %>%
          ghost_intersect_leftovers(!! property_ID, !! host_ID, distance,
                                    min_listings)
      }, cl = cl) %>%
      do.call(rbind, .)
    
  } else {
    # Single-threaded version
    points <- ghost_cluster(points, distance, min_listings)
    points <- ghost_intersect(points, !! property_ID, !! host_ID, distance,
                              min_listings)
    points <- ghost_intersect_leftovers(points, !! property_ID, !! host_ID,
                                        distance, min_listings)
  }
  
  
  ## GHOST TABLE CREATION
  
  points <-
    points %>%
    mutate(data = map2(.data$data, .data$property_IDs, ~{
      filter(.x, !! property_ID %in% .y)}))
  
  # Store CRS for later
  crs <- st_crs(points$intersects[[1]])
  
  # Generate compact table of ghost hotels, suppress SF geometry warnings
  points <- suppressWarnings(
    tidyr::unnest(points, .data$intersects, .preserve = .data$data)
  )
  
  # Remove duplicates
  points <- points[!duplicated(points$property_IDs),]
  
  # Create Ghost_ID
  points <- mutate(points, ghost_ID = 1:n())
  
  # Extract geometry, coerce to sf, join back to ghost_points, clean up
  points <-
    points[c("ghost_ID","geometry")] %>%
    st_as_sf() %>%
    left_join(st_drop_geometry(st_as_sf(points)), ., by = "ghost_ID") %>%
    st_as_sf() %>%
    rename(listing_count = .data$n.overlaps) %>%
    mutate(housing_units = as.integer(ceiling(.data$listing_count / 4))) %>%
    select(.data$ghost_ID, !! host_ID, .data$listing_count,
           .data$housing_units, .data$property_IDs, .data$data, .data$geometry)
  
  # Reattach CRS
  st_crs(points) <- crs
  
  # Calculate date ranges
  points <-
    points %>%
    mutate(
      start = map_dbl(.data$data, ~{
        pull(.x, !! created) %>%
          max(c(., start_date))
      }) %>%
        as.Date(origin = "1970-01-01"),
      end = map_dbl(.data$data, ~{
        pull(.x, !! scraped) %>%
          min(c(., end_date))
      }) %>%
        as.Date(origin="1970-01-01")
    )
  
  # Identify subsets
  points <-
    points %>%
    mutate(
      subsets = map(.data$property_IDs, function(y) {
        which(map_lgl(points$property_IDs, ~all(.x %in% y)))
      }),
      subsets = map2(.data$ghost_ID, .data$subsets, ~{.y[.y != .x]}))
  
  # Optionally add EH_check field
  if (!missing(EH_check)) {
    
    # Split points and EH_points
    points_split <- split(points, pull(points, !! host_ID))
    EH_points <- filter(EH_points, !! host_ID %in% pull(points, !! host_ID))
    EH_split <- map(points_split, ~{
      EH_points %>%
        filter(!! host_ID %in% pull(.x, !! host_ID))
    })
    
    EH_intersects <-
      map2(points_split, EH_split, ~{
        suppressWarnings(st_intersection(.x, st_buffer(.y, dist = distance)))
      }) %>%
      do.call(rbind, .) %>%
      select(.data$ghost_ID, !! property_ID) %>%
      st_drop_geometry() %>%
      nest(quo_name(property_ID)) %>%
      mutate(EH_check = map(.data$data, pull, !! property_ID)) %>%
      select(-.data$data)
    
    points <- left_join(points, EH_intersects, "ghost_ID")
  }
  
  ## TIDY TABLE CREATION
  
  # Create tidy version of ghost_points
  points <-
    points[c("ghost_ID", "start", "end")] %>%
    st_drop_geometry() %>%
    mutate(date = map2(.data$start, .data$end, ~{
      seq(unique(.x), unique(.y), 1)
    })) %>%
    tidyr::unnest(.data$date) %>%
    select(-.data$start, -.data$end) %>%
    filter(.data$date >= start_date, .data$date <= end_date) %>%
    left_join(points, by = "ghost_ID") %>%
    select(-.data$start, -.data$end) %>%
    st_as_sf()
  
  # Remove rows from ghost_tidy which are in ghost_subset overlaps
  points <-
    points %>%
    group_by(.data$date) %>%
    filter(!.data$ghost_ID %in% unlist(.data$subsets)) %>%
    select(-.data$subsets) %>%
    ungroup()
  
  points
}


ghost_cluster <- function(points, distance, min_listings) {
  
  # Create intersect predicate lists
  points <-
    points %>%
    mutate(
      predicates = map(.data$data, ~st_intersects(st_buffer(.x, distance))) %>%
        map(function(pred) {
          map(seq_along(pred), ~{
            reduce(pred, function(x, y){
              if (any(y %in% x)) unique(c(x, y)) else x
            }, # Merge lists with common elements
            .init = pred[[.]]) # Compile lists starting at each position
          }) %>%
            map(sort) %>%
            unique() # Remove duplicate lists
        }))
  
  # Remove lists < min_listings
  points <-
    points %>%
    mutate(predicates = map(.data$predicates, ~{.[lengths(.) >= min_listings]}))
  
  # Use predicates to split points into clusters with length >= min_listings
  points <-
    points %>%
    filter(map(.data$predicates, length) > 0) %>%
    mutate(data = map2(.data$data, .data$predicates, function(x, y) {
      map(y, ~{x[.,]})
    })) %>%
    tidyr::unnest(.data$data, .drop = TRUE)
  
  # Remove duplicate clusters
  points <- points[!duplicated(points$data),]
  
  points
}


ghost_combine <- function(buffers, predicates, n) {
  
  # Get valid buffers and predicates for combinations
  valid_pr <- predicates
  valid_pr2 <- valid_pr
  invalid_pr <- which(tabulate(unlist(valid_pr)) < n)
  if (length(invalid_pr) > 0) {
    valid_pr <- map(predicates[-invalid_pr], ~{.[!(. %in% invalid_pr)]})
  }
  
  while (!identical(valid_pr, valid_pr2)) {
    valid_pr2 <- valid_pr
    invalid_pr <- unique(c(invalid_pr, which(tabulate(unlist(valid_pr)) < n)))
    valid_pr <- map(predicates[-invalid_pr], ~{.[!(. %in% invalid_pr)]})
    if (length(valid_pr) < n) break
  }
  
  # Test if any valid combinations could exist, and exit if not
  if (length(valid_pr) < n) return(matrix(nrow = 0, ncol = 0))
  
  # Identify sets to generate combinations from
  combinations <- map(valid_pr, function(x){
    x[map(valid_pr, ~which(x %in% .)) %>%
        unlist() %>%
        tabulate() >= n]
  }) %>%
    unique()
  
  # Test if reducer will be necessary to avoid too many combinations
  while(sum(map_dbl(combinations, ~{
    factorial(length(.)) / {factorial(n) * factorial(length(.) - n)}
  })) > 1e7) {
    
    # Establish collective centroid
    if (length(invalid_pr) > 0) {
      valid_bf <- buffers[-invalid_pr,]
    } else {valid_bf <- buffers}
    
    centroid <-
      valid_bf %>%
      st_centroid() %>%
      st_union() %>%
      st_centroid()
    
    # Remove furthest point from predicate list
    invalid_pr <- c(invalid_pr,
                    which.max(st_distance(st_centroid(valid_bf), centroid)))
    
    # Repeat previous steps to generate new combinations
    valid_pr <- map(predicates[-invalid_pr], ~{.[!(. %in% invalid_pr)]})
    while (!identical(valid_pr, valid_pr2)) {
      valid_pr2 <- valid_pr
      invalid_pr <- unique(c(invalid_pr, which(tabulate(unlist(valid_pr)) < n)))
      valid_pr <- map(predicates[-invalid_pr], ~{.[!(. %in% invalid_pr)]})
      if (length(valid_pr) < n) break
    }
    if (length(valid_pr) < n) return(matrix(nrow = 0, ncol = 0))
    combinations <- map(valid_pr, function(x){
      x[map(valid_pr, ~which(x %in% .)) %>%
          unlist() %>%
          tabulate() >= n]
    }) %>%
      unique()
  }
  
  # Generate combinations from valid buffers and discard duplicates
  combinations <-
    map(combinations, combn, n) %>%
    do.call(cbind, .) %>%
    unique(MARGIN = 2)
  
  # Reduce combinations to valid options
  combinations <- combinations[, apply(combinations, 2, function(x) {
    sapply(valid_pr, function(y) all(x %in% y))
  }) %>%
    colSums() >= n]
  
  # Convert combinations matrix to tibble for future steps
  combinations <- suppressWarnings(as_tibble(combinations))
  
  combinations
}


ghost_intersect_with_done <- function(x, y) {
  result <- st_intersection(x, y)
  if (nrow(result) == 0) done(result) else result
}


ghost_stepwise_intersect <- function(buffers, min_listings) {
  
  # Deal with 0-length input
  if (nrow(buffers) == 0) return(buffers)
  
  # Build predicates and h_score
  predicates <- st_intersects(buffers)
  matrix <- matrix(
    c(sort(lengths(predicates), decreasing = TRUE), seq_along(predicates)),
    nrow=length(predicates)
  )
  h_score <- max(matrix[matrix[,2] >= matrix[,1],1])
  
  # Setup variables
  st_agr(buffers) = "constant"
  n <- h_score # Get working number of rows for combinations
  cols <- ncol(buffers) - 1 # Get total columns for clean-up
  
  # Generate combinations from predicates
  combinations <- ghost_combine(buffers, predicates, n)
  
  # Ensure valid combination list
  while (ncol(combinations) == 0 & n >= min_listings) {
    n <- n - 1
    combinations <- ghost_combine(buffers, predicates, n)
  }
  
  # Exit function if no valid combinations exist
  if (n < min_listings) {
    return(
      buffers[0,] %>%
        mutate(n.overlaps = as.integer(NA), origin = list(c(0)))
    )
  }
  
  # Master while-loop
  while (n >= min_listings) {
    
    # Try all combinations for a given n
    intersect_output <-
      map(combinations, function(x, n) {
        intersect <- suppressWarnings(
          split(buffers[x,], seq_len(nrow(buffers[x,]))) %>%
            reduce(ghost_intersect_with_done))
        intersect <- intersect[,1:cols]
        mutate(intersect, n.overlaps = n, origins = list(x))
      }, n = n)
    
    # Discard null results and rbind to single sf tibble
    intersect_output <-
      intersect_output[map(intersect_output, nrow) > 0] %>%
      do.call(rbind, .) %>%
      as_tibble()
    
    # Conditional to decide if the while-loop should continue
    if (nrow(intersect_output) == 0) {
      n <- n-1
      combinations <- ghost_combine(buffers, predicates, n)
    } else {
      intersect_output <-
        intersect_output %>%
        st_as_sf() %>%
        distinct(.data$geometry, .keep_all = TRUE)
      
      if (any(st_is(intersect_output, "POLYGON") == FALSE)) {
        stop("Invalid geometry produced")
      }
      
      return(intersect_output)
    }
  }
  
  intersect_output
}


ghost_intersect <- function(
  points, property_ID, host_ID, distance, min_listings) {
  
  property_ID <- enquo(property_ID)
  host_ID <- enquo(host_ID)
  
  # Prepare buffers
  points <-
    points %>%
    mutate(buffers = map(.data$data, st_buffer, dist = distance))
  
  # Create intersects using ghost_stepwise_intersect
  points <-
    points %>%
    mutate(
      intersects = map(.data$buffers, ghost_stepwise_intersect, min_listings))
  
  # Remove empty clusters
  points <-
    points %>%
    filter(map(.data$intersects, nrow) > 0)
  
  # Choose intersect with max area
  points_to_add <-
    points %>%
    filter(map(.data$intersects, nrow) > 1) %>%
    mutate(intersects = map(.data$intersects, ~.x[which.max(st_area(.x)),]))
  
  # Consolidate list of clusters
  points <-
    points %>%
    filter(map(.data$intersects, nrow) == 1) %>%
    rbind(points_to_add) %>%
    arrange(!! host_ID)
  
  # Add $property_IDs field
  data_PIDs <- map(points$data, `$`, !! property_ID)
  int_origs <- map(points$intersects, `$`, "origins")
  points <-
    points %>%
    mutate(property_IDs = map2(int_origs, data_PIDs, ~{.y[.x[[1]]]}))
  
  points
}


ghost_identify_leftovers <- function(points, property_ID, host_ID,
                                     min_listings) {
  property_ID <- enquo(property_ID)
  host_ID <- enquo(host_ID)
  
  points %>%
    filter(map_int(.data$data, nrow) - lengths(.data$property_IDs) >=
             min_listings) %>%
    mutate(data = map2(.data$data, .data$property_IDs, ~{
      filter(.x, !(!! property_ID %in% .y))
    })) %>%
    select(!! host_ID, .data$data)
}


ghost_intersect_leftovers <- function(points, property_ID, host_ID, distance,
                                      min_listings) {
  
  property_ID <- enquo(property_ID)
  host_ID <- enquo(host_ID)
  
  # Subset leftover candidates
  leftovers <- ghost_identify_leftovers(points, !! property_ID, !! host_ID,
                                        min_listings)
  
  # Condition to run ghost_intersect on leftovers
  while (nrow(leftovers) > 0) {
    
    # Remove leftovers from points$data
    points <-
      rbind(
        points %>%
          filter(map_int(.data$data, nrow) - lengths(.data$property_IDs) <
                   min_listings),
        points %>%
          filter(map_int(.data$data, nrow) - lengths(.data$property_IDs) >=
                   min_listings) %>%
          mutate(data = map2(.data$data, .data$property_IDs, ~{
            filter(.x, !! property_ID %in% .y)
          }))
      ) %>%
      arrange(!! host_ID)
    
    # Apply ghost_intersect to leftovers
    leftover_outcome <- ghost_intersect(leftovers, !! property_ID, !! host_ID,
                                        distance, min_listings)
    
    # Add leftover_outcome to points and remove duplicate Property_IDs
    points <- rbind(points, leftover_outcome) %>% arrange(!! host_ID)
    
    # Subset leftover candidates again
    leftovers <- ghost_identify_leftovers(points, !! property_ID, !! host_ID,
                                          min_listings)
  }
  
  points
}


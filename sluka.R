sluka <- read.csv('data/slukadf.csv')
museum <- read.csv('data/museumsluka.csv')

#making some columns i need and adding data from museum sheet to sluka sheet
sluka$location <- museum$State[match(sluka$Catalog_Number, museum$Catalog_Number)]
sluka$ecoregion <- museum$Ecoregion[match(sluka$Catalog_Number, museum$Catalog_Number)]
sluka$species <- museum$Species[match(sluka$Catalog_Number, museum$Catalog_Number)]
sluka$Population.designation <- museum$X.UrbanPopulation[match(sluka$Catalog_Number, museum$Catalog_Number)]
sluka$Population.designation <- ifelse(sluka$Population.designation >= 0.5, 1, 0)
sluka$Year <- museum$Year[match(sluka$Catalog_Number, museum$Catalog_Number)]

#getting rid of nas for either braincase volume or skull length to leave only those we can calc rcc for
sluka <- sluka[!is.na(sluka$BCV), ]
sluka <- sluka[!is.na(sluka$CBL), ]
sluka$BCV <- as.numeric(sluka$BCV)
sluka$CBL <- as.numeric(sluka$CBL)

#relativecc calc
sluka$relativecc <- sluka$BCV / sluka$CBL

#subsetting data by location
eastsluka <- sluka[sluka$ecoregion=="East",]
westsluka <- sluka[sluka$ecoregion=="PacificWest",]
plainssluka <- sluka[sluka$ecoregion=="Plains",]
desertsluka <- sluka[sluka$ecoregion=="Desert",]


#elm helped me here as it was taking absolutely ages to do analogue (as I did with snell_rood and depas)
# Define a function to calculate mean, SD, Hedges' g, and 95% confidence intervals
calculate_stats_with_hedges_ci <- function(data) {
  data$relativecc <- data$BCV / data$CBL
  
  # Initialize a result list
  results <- list()
  
  # Subset data for rural (0) and calculate mean and sd
  rural_data <- data[data$Population.designation == "0", ]
  if (nrow(rural_data) > 0) {
    rural_mean <- mean(rural_data$relativecc, na.rm = TRUE)
    rural_sd <- sd(rural_data$relativecc, na.rm = TRUE)
    n_rural <- nrow(rural_data)
  } else {
    rural_mean <- NA
    rural_sd <- NA
    n_rural <- 0
  }
  
  # Subset data for urban (1) and calculate mean and sd
  urban_data <- data[data$Population.designation == "1", ]
  if (nrow(urban_data) > 0) {
    urban_mean <- mean(urban_data$relativecc, na.rm = TRUE)
    urban_sd <- sd(urban_data$relativecc, na.rm = TRUE)
    n_urban <- nrow(urban_data)
  } else {
    urban_mean <- NA
    urban_sd <- NA
    n_urban <- 0
  }
  
  # Calculate Hedges' g and its confidence intervals if both groups have data
  if (!is.na(rural_mean) && !is.na(urban_mean)) {
    pooled_sd <- sqrt(((n_rural - 1) * rural_sd^2 + (n_urban - 1) * urban_sd^2) / (n_rural + n_urban - 2))
    cohen_d <- (urban_mean - rural_mean) / pooled_sd
    correction_factor <- 1 - (3 / (4 * (n_rural + n_urban) - 9))
    hedges_g <- cohen_d * correction_factor
    
    # Standard error of Hedges' g
    se_g <- sqrt((n_rural + n_urban) / (n_rural * n_urban) + (hedges_g^2 / (2 * (n_rural + n_urban))))
    
    # Get critical t-value
    t_crit <- qt(0.975, df = n_rural + n_urban - 2)
    
    # Calculate confidence interval
    ci_lower <- hedges_g - t_crit * se_g
    ci_upper <- hedges_g + t_crit * se_g
    
    ci <- c(lower = ci_lower, upper = ci_upper)
  } else {
    hedges_g <- NA
    ci <- c(lower = NA, upper = NA)
  }
  
  # Store results
  results$rural <- if (!is.na(rural_mean)) list(mean = rural_mean, sd = rural_sd) else NULL
  results$urban <- if (!is.na(urban_mean)) list(mean = urban_mean, sd = urban_sd) else NULL
  results$hedges_g <- hedges_g
  results$ci <- if (!is.na(hedges_g)) ci else NULL
  
  # Remove empty results
  results <- Filter(Negate(is.null), results)
  
  return(results)
}

eastsluka_stats <- calculate_stats_with_hedges_ci(eastsluka)
westsluka_stats <- calculate_stats_with_hedges_ci(westsluka)
plainssluka_stats <- calculate_stats_with_hedges_ci(plainssluka)
desertsluka_stats <- calculate_stats_with_hedges_ci(desertsluka)

eastsluka_stats
length(eastsluka$Population.designation=='0')
length(eastsluka)
westsluka_stats
plainssluka_stats
desertsluka_stats

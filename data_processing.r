library(f1dataR)
library(dplyr)
library(reticulate)

# Set Python explicitly at the start
use_python("C:\\Program Files\\Python312\\python.exe", required = TRUE)
py_available(initialize = TRUE)

load_race_data <- function(season, round) {
  if (!py_module_available("fastf1")) {
    stop("FastF1 Python module not available. Install it with 'pip install fastf1' for Python at: ", py_config()$python)
  }
  
  # Set custom cache directory
  cache_dir <- file.path(Sys.getenv("HOME"), "f1dataR_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  options(f1dataR_cache = cache_dir)
  
  # Validate inputs
  if (!is.numeric(season) || season < 1950 || season > 2024) {
    stop("Invalid season: must be a number between 1950 and 2024")
  }
  if (!is.numeric(round) || round < 1) {
    stop("Invalid round: must be a positive number")
  }
  
  # Load lap data
  lap_data <- tryCatch({
    message("Loading data for season ", season, ", round ", round)
    data <- load_session_laps(
      season = season,
      round = round,
      session = "R",
      add_weather = TRUE
    )
    message("Raw data loaded: ", nrow(data), " rows")
    data
  }, error = function(e) {
    message("Failed to load lap data: ", e$message)
    NULL
  })
  
  if (is.null(lap_data) || nrow(lap_data) == 0) {
    message("No lap data available for season ", season, ", round ", round)
    return(NULL)
  }
  
  # Process data
  processed_data <- tryCatch({
    data <- lap_data %>%
      select(lap_time, lap_number, stint, tire_compound = compound,
             weather = track_condition) %>%
      mutate(
        lap_time = as.numeric(lap_time),
        tire_wear = pmin(100, pmax(0, cumsum(lap_time) / max(cumsum(lap_time), na.rm = TRUE) * 100)),
        tire_compound = recode(tire_compound,
                              "SOFT" = "Soft", "MEDIUM" = "Medium", "HARD" = "Hard",
                              "INTERMEDIATE" = "Intermediate", "WET" = "Wet"),
        weather = case_when(
          grepl("Wet", weather) ~ "Wet",
          grepl("Dry", weather) ~ "Dry",
          TRUE ~ "Mixed"
        )
      ) %>%
      filter(!is.na(lap_time), !is.na(tire_wear))
    message("Processed data: ", nrow(data), " rows")
    data
  }, error = function(e) {
    message("Error processing data: ", e$message)
    NULL
  })
  
  if (is.null(processed_data) || nrow(processed_data) == 0) {
    message("No valid data after processing")
    return(NULL)
  }
  
  return(processed_data)
}

get_race_list <- function(season) {
  tryCatch({
    schedule <- load_schedule(season)
    if (is.null(schedule) || nrow(schedule) == 0) {
      stop("No schedule data for season ", season)
    }
    return(schedule$round)
  }, error = function(e) {
    message("Error loading schedule: ", e$message)
    return(1:20)
  })
}
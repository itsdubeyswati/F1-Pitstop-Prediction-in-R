convert_tire <- function(tire) {
  case_when(
    tire == "Soft" ~ 1,
    tire == "Medium" ~ 2,
    tire == "Hard" ~ 3,
    tire == "Intermediate" ~ 4,
    tire == "Wet" ~ 5,
    TRUE ~ 2
  )
}

normalize_weather <- function(weather) {
  case_when(
    weather == "Dry" ~ 1,
    weather == "Wet" ~ 1.2,
    weather == "Mixed" ~ 1.1,
    TRUE ~ 1
  )
}
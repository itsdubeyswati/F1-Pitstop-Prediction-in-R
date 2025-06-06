library(randomForest)

train_pitstop_model <- function(historical_data) {
  train_data <- historical_data %>%
    mutate(
      needs_pitstop = ifelse(tire_wear > 70 | lap_time > mean(lap_time, na.rm = TRUE) * 1.1, 1, 0)
    ) %>%
    select(lap_number, lap_time, tire_wear, stint, needs_pitstop)
  
  model <- randomForest(
    needs_pitstop ~ lap_number + lap_time + tire_wear + stint,
    data = train_data,
    ntree = 100,
    importance = TRUE
  )
  
  return(model)
}

predict_pitstop <- function(season, round, weather, tire, laps_done, tire_wear, historical_data) {  # Changed 'race' to 'round'
  model <- train_pitstop_model(historical_data)
  
  new_data <- data.frame(
    lap_number = laps_done,
    lap_time = mean(historical_data$lap_time, na.rm = TRUE),
    tire_wear = tire_wear,
    stint = 1
  )
  
  base_pred <- predict(model, new_data, type = "response")
  
  tire_adjust <- case_when(
    tire == "Soft" ~ -3,
    tire == "Hard" ~ 3,
    tire == "Intermediate" & weather == "Wet" ~ -2,
    tire == "Wet" & weather == "Wet" ~ -1,
    TRUE ~ 0
  )
  weather_adjust <- case_when(
    weather == "Wet" ~ 5,
    weather == "Mixed" ~ 3,
    TRUE ~ 0
  )
  
  optimal_lap <- max(laps_done, min(50, round(laps_done + base_pred * 10 + tire_adjust + weather_adjust)))
  return(optimal_lap)
}

plot_feature_importance <- function(historical_data) {
  model <- train_pitstop_model(historical_data)
  imp <- importance(model, type = 1)
  imp_data <- data.frame(
    Feature = rownames(imp),
    Importance = imp[, 1]
  )
  
  ggplot(imp_data, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Feature Importance in Pitstop Prediction",
         x = "Features", y = "Importance") +
    theme_minimal()
}
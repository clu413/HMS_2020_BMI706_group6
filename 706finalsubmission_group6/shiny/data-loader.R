fetch_dat_with_change_rate <- function(csv_file='../data/merged_data.csv') {
  library(dplyr)
  
  merged_data <- read_csv(csv_file)
  merged_data$date_diff <- as.numeric(merged_data$date - merged_data$StateClosureStartDate)
  merged_data$percent_positive <- merged_data$positive/merged_data$total
  merged_data$state <- as.factor(merged_data$state)
  merged_data <- merged_data %>% arrange(desc(date))
  
  percent_change <- function(row, value, df) {
    df <- df %>%
      filter(grepl(row['state'], state)) %>% # filter by the row's state
      filter(date < row['date']) %>% # filter out the date later than the row's date
      slice(1) # already sorted in the beginning, so just take the first one
    if (nrow(df) > 0) {
      return ((as.numeric(row[[value]])-df[[value]])/df[[value]])
    }
    else {
      return (NA)
    }
  }
  
  merged_data$positive_percent_change <- apply(merged_data, 1, percent_change, 'positive', merged_data)
  merged_data$percent_positive_percent_change <- apply(merged_data, 1, percent_change, 'percent_positive', merged_data)
  merged_data$hospitalized_percent_change <- apply(merged_data, 1, percent_change, 'hospitalized', merged_data)
  merged_data$death_percent_change <- apply(merged_data, 1, percent_change, 'death', merged_data)
  
  plot_data <- merged_data %>%
    pivot_longer(c(positiveIncrease, positive_percent_change, percent_positive,
                   percent_positive_percent_change, hospitalizedIncrease, hospitalized_percent_change,
                   deathIncrease, death_percent_change))
  return(plot_data)
}


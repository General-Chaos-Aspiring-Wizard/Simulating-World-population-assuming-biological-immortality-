# Modeling world population code
# It includes UN data from 2024 with estimates of historical data and 
# estimates of future based on different assumptions 
library(dplyr) 
library(tidyverse) 
library(ggplot2) 
library(scales)

# Importing the data 
# Substitute path with your own file paths
WPP2024_Demographic_Indicators_OtherVariants.csv <- read.csv("WPP2024_Demographic_Indicators_OtherVariants.csv.gz")
WPP2024_Demographic_Indicators_Medium.csv <- read.csv("WPP2024_Demographic_Indicators_Medium.csv.gz")

# Data processing (saving only the necessary columns)
UN_demographics_medium <- WPP2024_Demographic_Indicators_Medium.csv %>% 
      filter(Location == 'World', Variant == c('Medium', 'Low', 'High')) %>% 
      select(c(Time, Location, Variant, TPopulation1July, PopGrowthRate, TFR))
UN_demographics_other_scenarios <- WPP2024_Demographic_Indicators_OtherVariants.csv %>% 
      filter(Location == 'World', Variant == c('Medium', 'Low', 'High')) %>% 
      select(c(Time, Location, Variant, TPopulation1July, PopGrowthRate, TFR))

# Saving new CSV to the computer 
write.csv(UN_demographics_medium, "UN_demographics_estimates_2024_medium.csv", row.names = FALSE)
write.csv(UN_demographics_other_scenarios, "UN_demographics_estimates_2024_other_scenarios.csv", row.names = FALSE)


# Combine the datasets
data <- bind_rows(UN_demographics_medium, UN_demographics_other_scenarios)

# Convert population to billions for easier interpretation
data$Population_Billions <- data$TPopulation1July / 1e6
data <- data %>% select(-TPopulation1July)
# Save data
write.csv(data, "data.csv", row.names = FALSE)

# Clean data
data <- data %>% 
      filter(!is.na(Population_Billions))

# Calculating the population growth rate
growth_rate <- function(population_size, fertility_rate, mortality_rate) {
      # Births per year
      # Fertility rate is defined as births per woman in a lifetime, 
      # 39 is roughly the number of biological reproductive years for women currently (Nabhan et al. 2022)
      # There are a bit more males than females in the population, bc of higher male mortality but improved medicine
      # Current value is roughly 49.7% are women, with improved medicine I assume 49% to be women
      births_year <- fertility_rate/39*(population_size*0.49)
      growth__rate <- 1+(births_year/population_size)-mortality_rate
      return(growth__rate)
}

# Function to get interpolated fertility rate for a given year
get_fertility_rate <- function(year, data) {
    if (length(data) == 1) {
        return(data)
    }
    
    # If we're after 2100, return the last available rate
    if (year > 2100) {
        return(tail(data$TFR, 1))
    }
    
    # Find the closest previous fertility rate
    available_rates <- data[data$Time <= year,]
    if (nrow(available_rates) == 0) {
        return(data$TFR[1])  # Return first available if no previous rate
    }
    return(tail(available_rates$TFR, 1))
}

# Function to simulate population with different mortality rates
simulate_population <- function(base_population, fertility_input, mortality_rate, end_year = 2200) {
    years <- seq(2070, end_year)
    population <- numeric(length(years))
    population[1] <- base_population
    
    for (i in 2:length(years)) {
        current_year <- years[i]
        fertility_rate <- get_fertility_rate(current_year, fertility_input)
        population[i] <- population[i-1] * growth_rate(population[i-1], fertility_rate, mortality_rate)
    }
    
    return(population)
}

# Setting up variables for the simulation
population_2070_low <- data$Population_Billions[data$Time == 2070 & data$Variant == 'Low']

# Prepare fertility data frames for each variant
fertility_low <- data %>% 
    filter(Variant == "Low" & Time >= 2070) %>%
    select(Time, TFR)

fertility_medium <- data %>%
    filter(Variant == "Medium" & Time >= 2070) %>%
    select(Time, TFR)

fertility_high <- data %>%
    filter(Variant == "High" & Time >= 2070) %>%
    select(Time, TFR)

# Simulate scenarios 
scenarios <- list(
    'UN Low' = data$Population_Billions[data$Variant == "Low"],
    'UN Medium' = data$Population_Billions[data$Variant == "Medium"],
    'UN High' = data$Population_Billions[data$Variant == "High"],
    'Immortality low risk, 0.1 fertility' = simulate_population(population_2070_low, 0.1, 0.000046),
    'Immortality low risk low growth' = simulate_population(population_2070_low, fertility_low, 0.000046),
    'Immortality low risk medium growth' = simulate_population(population_2070_low, fertility_medium, 0.000046),
    'Immortality low risk high growth' = simulate_population(population_2070_low, fertility_high, 0.000046),
    'Immortality high risk, 0.1 fertility' = simulate_population(population_2070_low, 0.1, 0.000543),
    'Immortality high risk low growth' = simulate_population(population_2070_low, fertility_low, 0.000543),
    'Immortality high risk medium growth' = simulate_population(population_2070_low, fertility_medium, 0.000543),
    'Immortality high risk high growth' = simulate_population(population_2070_low, fertility_high, 0.000543)
)

# Prepare data for plotting
scenario_names <- names(scenarios)

un_scenario_low_df <- tibble(
      Year = data$Time[data$Variant == "Low"],
      Population = scenarios[['UN Low']],
      Scenario = 'UN Low'
)

un_scenario_mid_df <- tibble(
      Year = data$Time[data$Variant == "Medium"],
      Population = scenarios[['UN Medium']],
      Scenario = 'UN Medium'
)

un_scenario_high_df <- tibble(
      Year = data$Time[data$Variant == "High"],
      Population = scenarios[['UN High']],
      Scenario = 'UN High'
)

simulated_scenario_df <- map_dfr(scenario_names[4:length(scenario_names)], function(scenario_name) {
      tibble(
            Year = seq(2070, 2200), 
            Population = scenarios[[scenario_name]],
            Scenario = scenario_name
      )
})

simulations <- bind_rows(
      un_scenario_low_df,
      un_scenario_mid_df,
      un_scenario_high_df,
      simulated_scenario_df
)


# Function to create population graph
create_population_graph <- function(data, log_scale = FALSE) {
      plot <- ggplot(data, aes(x = Year, y = Population, color = Scenario, linetype = Scenario)) +
            geom_line(linewidth = 1) +
            scale_color_manual(values = c(
                  'UN Low' = '#2b83ba',           # Changed colors to a more professional palette
                  'UN Medium' = '#abdda4',
                  'UN High' = '#d7191c',
                  'Immortality low risk, 0.1 fertility' = '#92c5de',
                  'Immortality low risk low growth' = '#2b83ba',
                  'Immortality low risk medium growth' = '#abdda4',
                  'Immortality low risk high growth' = '#d7191c',
                  'Immortality high risk, 0.1 fertility' = '#92c5de',
                  'Immortality high risk low growth' = '#2b83ba',
                  'Immortality high risk medium growth' = '#abdda4',
                  'Immortality high risk high growth' = '#d7191c'
            )) +
            scale_linetype_manual(values = c(
                  'UN Low' = 'solid',
                  'UN Medium' = 'solid',
                  'UN High' = 'solid',
                  'Immortality low risk, 0.1 fertility' = 'dashed',
                  'Immortality low risk low growth' = 'dashed',
                  'Immortality low risk medium growth' = 'dashed',
                  'Immortality low risk high growth' = 'dashed',
                  'Immortality high risk, 0.1 fertility' = 'dotted',
                  'Immortality high risk low growth' = 'dotted',
                  'Immortality high risk medium growth' = 'dotted',
                  'Immortality high risk high growth' = 'dotted'
            )) +
            scale_x_continuous(
                  breaks = seq(min(data$Year), max(data$Year), by = 20),  # Add year labels every 20 years
                  labels = function(x) format(x, scientific = FALSE, big.mark = ""),  # Remove spaces in numbers
                  limits = c(min(data$Year), 2200)  # Ensure x-axis goes to 2200
) +
            labs(
                  title = paste("World Population Projections Under Different Scenarios",
                              ifelse(log_scale, "\n(Logarithmic Scale)", "")),
                  subtitle = "Comparing UN projections with immortality scenarios",
                  x = "Year",
                  y = ifelse(log_scale, "Population (log scale, billions)", "Population (billions)"),
                  color = "Scenario",
                  linetype = "Scenario"
            ) +
            theme_minimal() +
            theme(
                  text = element_text(family = "Arial"),  # Use a clean font
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
                  axis.title = element_text(size = 11),
                  axis.text = element_text(size = 10),
                  legend.title = element_text(size = 11),
                  legend.text = element_text(size = 10),
                  legend.position = "bottom",  # Move legend to bottom
                  legend.box = "vertical",
                  panel.grid.minor = element_blank(),  # Remove minor gridlines
                  panel.grid.major = element_line(color = "grey90"),
                  plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
                  plot.background = element_rect(fill = "white", color = NA),
                  panel.background = element_rect(fill = "white", color = NA)
            )
      
      if (log_scale) {
            plot <- plot + 
                  scale_y_log10(labels = scales::number_format(accuracy = 0.1))
      } else {
            plot <- plot + 
                  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
      }
      
      return(plot)
}

# Generate graphs
linear_plot <- create_population_graph(simulations)
log_plot <- create_population_graph(simulations, log_scale = TRUE)
print(linear_plot)
#print(log_plot)

# Save plots
ggsave("immortal_population_projection_linear.png", linear_plot, width = 12, height = 7)
ggsave("immortal_population_projection_log.png", log_plot, width = 12, height = 7)

print("Graphs have been saved as immortal_population_projection_linear.png and immortal_population_projection_log.png")

# Create extended simulation data for 0.1 fertility scenario
years_3000 <- seq(2070, 3000)  
population_3000 <- simulate_population(population_2070_low, 0.1, 0.000046, end_year = 3000)

# Create dataframe for the extended scenario
extended_scenario_df <- tibble(
      Year = years_3000,
      Population = population_3000,
      Scenario = 'Immortality low risk, 0.1 fertility'
)

# Create a modified plotting function for the extended timeline
create_extended_population_graph <- function(data, log_scale = FALSE) {
      plot <- ggplot(data, aes(x = Year, y = Population)) +
            geom_line(linewidth = 1, color = '#92c5de') +
            scale_x_continuous(
                  breaks = seq(2000, 3000, by = 100), 
                  labels = function(x) format(x, scientific = FALSE, big.mark = "")
            ) +
            labs(
                  title = paste("Extended World Population Projection to Year 3000\nLow Mortality (0.000046) with 0.1 Fertility Rate",
                              ifelse(log_scale, "\n(Logarithmic Scale)", "")),
                  subtitle = "Assuming continued technological progress in life extension",
                  x = "Year",
                  y = ifelse(log_scale, "Population (log scale, billions)", "Population (billions)")
            ) +
            theme_minimal() +
            theme(
                  text = element_text(family = "Arial"),
                  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
                  axis.title = element_text(size = 11),
                  axis.text = element_text(size = 10),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_line(color = "grey90"),
                  plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
                  plot.background = element_rect(fill = "white", color = NA),
                  panel.background = element_rect(fill = "white", color = NA)
            )
      
      if (log_scale) {
            plot <- plot + 
                  scale_y_log10(labels = scales::number_format(accuracy = 0.1))
      } else {
            plot <- plot + 
                  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
      }
      
      return(plot)
}

# Generate extended timeline graphs
extended_linear_plot <- create_extended_population_graph(extended_scenario_df)
#extended_log_plot <- create_extended_population_graph(extended_scenario_df, log_scale = TRUE)
#print(extended_linear_plot) 

# Save extended timeline plots
ggsave("immortal_population_projection_3000_linear.png", extended_linear_plot, width = 12, height = 7)
#ggsave("immortal_population_projection_3000_log.png", extended_log_plot, width = 12, height = 7)

# Create a summary dataframe of final values
final_values <- bind_rows(
      # For UN scenarios (keeping original order)
      un_scenario_low_df %>% 
            filter(Year == max(Year)) %>%
            select(Scenario, Year, Population),
      
      un_scenario_mid_df %>%
            filter(Year == max(Year)) %>%
            select(Scenario, Year, Population),
      
      un_scenario_high_df %>%
            filter(Year == max(Year)) %>%
            select(Scenario, Year, Population),
      
      # For simulated scenarios (keeping original order)
      simulated_scenario_df %>%
            group_by(Scenario) %>%
            filter(Year == max(Year)) %>%
            select(Scenario, Year, Population),
      
      # For extended scenario to 3000
      extended_scenario_df %>%
            filter(Year == max(Year)) %>%
            select(Scenario, Year, Population)
) %>%
      mutate(
            Scenario = format(Scenario, justify = "left"),
            Year = format(Year, justify = "left"),
            Population = sprintf("%8.1f", Population)  # Right-align with 8 spaces and 1 decimal
      )

# Print the results in a formatted way
print("Final Year Values for Each Scenario:")
print("=====================================")
print.data.frame(final_values, row.names = FALSE, right = FALSE)  

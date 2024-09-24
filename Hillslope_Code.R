library(gsheet)
library(tidyverse)
library(ggplot2)

# import data
lab_data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1bA7Lr39AQy0SYbxXztEZ3PNdQrY_ujv0sqOz7q8Unto/edit?usp=share_link")

# defining parameters
dx <- 1 # meter
dt <- 1 # years
Kappa <- 0.25 # m2/yr
maxtime <- 10 # run time of model in years


#plot the original hillslope profile
ggplot (lab_data, aes(x = Distance, y = Elevation)) +
  geom_point (shape = 21, color = "black") +  # circle markers with blacl border
  geom_line (color = "black") +  # solid black line
  labs (title = "Hillslope Profile", x = "Distance (m)", y = "Elevation (m)") + # creating labels for the title, and axes 
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + # setting the graph axis values
  scale_x_continuous(limits = c(0, 55), breaks = seq(0, 55, 5)) + # setting the graph axis values
  theme_bw() # this is a general visual theme for the plot -- there are others, I like bw (BlackWhite)


# Function to apply the erosion process
apply_erosion <- function(lab_data, maxtime, Kappa, dx, dt) {
  for (time_now in seq(0, maxtime, by=dt)) {
    lab_data <- lab_data %>% 
      mutate(Elevation = ifelse(Distance > 1 & Distance < max(Distance),
                                Elevation - (-Kappa * ((lag(Elevation, default = first(Elevation)) - Elevation) / dx - (Elevation - lead(Elevation, default = last(Elevation))) / dx) * dt / dx), 
                                Elevation))}
  return(lab_data) # returns (creates) a new data set for each erosion cycle sequence
}

# runs the different slope erosion models 
erosion_10yrs <- apply_erosion(lab_data, maxtime, Kappa, dx, dt)
erosion_50yrs <- apply_erosion(lab_data, maxtime * 5, Kappa, dx, dt)
erosion_100yrs <- apply_erosion(lab_data, maxtime * 10, Kappa, dx, dt)
erosion_250yrs <- apply_erosion(lab_data, maxtime * 25, Kappa, dx, dt)


# Create a data frame with all slope profiles combined
slope_combined <- bind_rows(
  "Original Slope" = lab_data,
  "10 yrs" = erosion_10yrs,
  "50 yrs" = erosion_50yrs,
  "100 yrs" = erosion_100yrs,
  "250 yrs" = erosion_250yrs,
  .id = "Time_Period"  # This adds a new column named Time_Period
) %>%
  mutate(Time_Period = factor(Time_Period, levels = c("Original Slope", "10 yrs", "50 yrs", "100 yrs", "250 yrs")))
  

# Plotting Hillslope Evolution
ggplot(slope_combined, aes(x = Distance, y = Elevation, color = Time_Period, group = Time_Period)) +
  geom_point() + # telling the program to produce points for the data
  geom_line() + # telling the program to produce a line between points
  scale_color_manual(values = c("black", "green", "blue", "cyan", "magenta")) + # setting colors for the different erosion years manually
  labs(title = "Hillslope Evolution Profiles", x = "Distance (m)", y = "Elevation (m)") + # creating labels for the title, and axes 
  labs(color = "Erosion Time") + # Updating the legend title
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + # setting the graph axis values
  scale_x_continuous(limits = c(0, 55), breaks = seq(0, 55, 5)) + # setting the graph axis values
  theme_bw() # this is a general visual theme for the plot -- there are others, I like bw (BlackWhite)




calculate_erosion_mass <- function(lab_data, eroded_slope, dx = 1, density = 1500) { # <1>
  erosion_volume <- sum((lab_data$Elevation - eroded_slope$Elevation) * dx) # <2>
  erosion_mass <- abs(erosion_volume) * 1500 # <3>
  return(erosion_mass) 
}

# Calculate mass of erosion for each time step
mass_10yrs <- calculate_erosion_mass(lab_data, erosion_10yrs, dx, density)
mass_50yrs <- calculate_erosion_mass(lab_data, erosion_50yrs, dx, density)
mass_100yrs <- calculate_erosion_mass(lab_data, erosion_100yrs, dx, density)
mass_250yrs <- calculate_erosion_mass(lab_data, erosion_250yrs, dx, density)


# Create a data frame for the mass changes (in kilograms)
mass_data <- tibble(
  Time_Period = c("10 years", "50 years", "100 years", "250 years"),
  Mass_Erosion_kg = c(mass_10yrs, mass_50yrs, mass_100yrs, mass_250yrs)  # Use mass values instead of volume
)

# Ensure Time_Period is a factor and set the levels in the desired order (youngest to oldest)
mass_data$Time_Period <- factor(mass_data$Time_Period, levels = c("10 years", "50 years", "100 years", "250 years"))

print(mass_data)


# Convert the mass from kg to tons
mass_data$Mass_Erosion_tons <- mass_data$Mass_Erosion_kg / 1000

print(mass_data)

# Plot the bar chart
ggplot(mass_data, aes(x = Time_Period, y = Mass_Erosion_tons, fill = Time_Period)) +
  geom_bar(stat = "identity", width = .8) +
  geom_text(aes(label = round(Mass_Erosion_tons, 2)), vjust = -0.3, color = "black") + # Add the mass values rounded to whole numbers
  labs(title = "Mass of Erosion Over Time", x = "Time Period", y = "Mass of Erosion (tons)") +
  scale_fill_manual(values = c("green", "blue", "cyan", "magenta")) + # setting colors for the different erosion years manually
  scale_y_continuous(limits = c(0, 22), breaks = seq(0, 22, 2)) + # setting the graph axis values
  theme_bw() +  # Clean theme
  theme(legend.position = "none")  # Remove legend





# Plot the bar chart
ggplot(mass_data, aes(x = Time_Period, y = Mass_Erosion_kg, fill = Time_Period)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(Mass_Erosion_kg, 0)),  # Add the mass values rounded to whole numbers
            vjust = -0.3,  # Adjust vertical position, slightly above the bar
            color = "black") +  # Color of the text
  labs(title = "Mass of Erosion Over Time",
       x = "Time Period",
       y = "Mass of Erosion (kg)") +
  scale_fill_manual(values = c("green", "blue", "cyan", "magenta")) + # setting colors for the different erosion years manually
  scale_y_continuous(limits = c(0, 22000), breaks = seq(0, 22000, 2000)) + # setting the graph axis values
  theme_bw() +  # Clean theme
  theme(legend.position = "none")  # Remove legend


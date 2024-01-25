# ProjectMe - Health Data Analysis Dashboard

🔗 [Shiny App Link](https://michael-pytel.shinyapps.io/PersonalVisualizationProject/) | 🎥 [Video Presentation (Polish)](https://wutwaw-my.sharepoint.com/:v:/g/personal/01180740_pw_edu_pl/EUR5hDXGu9pJgmXDd3OPmjABAzVy145RMjcT9I6TyaIIBQ?e=Aw0d0S&nav=eyJyZWZlcnJhbEluZm8iOnsicmVmZXJyYWxBcHAiOiJTdHJlYW1XZWJBcHAiLCJyZWZlcnJhbFZpZXciOiJTaGFyZURpYWxvZy1MaW5rIiwicmVmZXJyYWxBcHBQbGF0Zm9ybSI6IldlYiIsInJlZmVycmFsTW9kZSI6InZpZXcifX0%3D)

This Shiny web application offers a comprehensive analysis of health data collected from individuals participating in the 'ME project.' The app focuses on three key health metrics: steps taken, water consumption, and sleep duration. Users can explore patterns, correlations, and trends in the health behaviors of three distinct individuals—Gentleman1, Gentleman2, and Gentleman3.

## Features

1. **🏡 Home Tab:**
   - **📊 About Collecting Data:** Details the process and purpose of data collection.
   - **👥 About Us:** Introduces the three individuals and their unique health journeys. The data has been anonymized, and individuals are represented as Gentleman1, Gentleman2, and Gentleman3 for privacy.

2. **💧 Water Tab:**
   - **📈 Water Consumption by Weekday:** Examines the correlation between daily steps and water intake.
   - **📊 Water Consumption Distribution:** Explores the distribution of water consumption on a specific weekday.

3. **🏃‍♂️ Steps Tab:**
   - **📅 Daily Steps by Weekday:** Displays the average number of steps taken by each individual on different weekdays.
   - **📈 Steps Over Time:** Provides an interactive line plot of daily steps over a customizable time frame.

4. **😴 Sleep Tab:**
   - **📊 Sleep Duration by Weekday:** Shows the distribution of sleep duration for each individual on different weekdays.
   - **🕒 Sleep Start and End Times:** Analyzes the distribution of sleep start and end times, offering insights into sleep patterns.

## Usage

1. Select tabs from the sidebar menu to navigate between different analyses.
2. Interact with plots and visualizations using checkboxes, dropdowns, and sliders.
3. Gain insights into the health behaviors of Gentleman1, Gentleman2, and Gentleman3.

**Important:**
- This app is currently configured to work with our anonymized data collected using Samsung Health.
- To implement your own data collected using Samsung Health, follow these steps:
    1. Use the `data_import.R` script to import your data into R.
    2. Further process the data using the `data_prepare.R` script for compatibility with the app.
- Implement with caution, as this app is not intended for medical or diagnostic purposes.

## Compatibility

- This app is designed for individuals who use Samsung Health.
- Data cleaning scripts ensure seamless integration with the app.

## Important Note

- This application is designed for exploratory data analysis and personal insights.
- The data used in this application may be fictional and is anonymized, with individuals represented as Gentleman1, Gentleman2, and Gentleman3 for privacy.
- For a detailed understanding, refer to the accompanying R script and explore the code for data processing and visualization.

## Developers

Developed by: [@Michał Pytel](https://github.com/Michael-Pytel), [@Gaspar Sekula](https://github.com/GasparSekula), [@Michał Piechota](https://github.com/Frejzy)

**Disclaimer:** This app is not intended for medical or diagnostic purposes. Consult with a healthcare professional for personalized health advice.

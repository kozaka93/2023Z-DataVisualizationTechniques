# Messenger-Analyser Project
*Authors: [@Pawlo77](https://github.com/Pawlo77), [@KAdamczykk](https://github.com/KAdamczykk), [@iwanm1](https://github.com/iwanm1)*

## 1. Introduction
The project aimed to analyze data from Messenger on four dimensions:
* Analysis of the most frequently used emojis
* Analysis of sent messages over time
* Analysis of average response time
* Analysis of the most frequently repeated word sequences

## 2. Data
The data was provided by Meta, and each user can download it by following these steps:
*My Account -> Settings -> Download Information*
The data arrives within 2 days and contains complete information about our accounts. However, for the project, we only used data from our conversations, stored in JSON files.

## 3. Processing
The entire data processing process into data frames was written in Python. Detailed information about the code and its usage can be found in this [readme.md](processing/readme.md) file.

## 4. Visualization
The data visualization process was done in R-Shiny, and the result of our work can be seen in this [app](https://pipikiddomisiu.shinyapps.io/projekt2_shiny/). On the 4 panels, you can see many interactive charts, namely:
1. **Panel 1: Emojis**
   - Bar chart with options to choose: person, date range, number of emojis, and whether the message was written in a group or not.
2. **Panel 2: Sent Messages**
   - *Subpanel 1: Individual data (with the option to choose a person)*
     - Bar chart describing the number of messages with a particular person over time. Option to change date range and number of displayed conversations.
     - Animation showing changes over time.
   - *Subpanel 2: Aggregated data (all with the option to change dates)*
     - Line plot showing the average number of sent messages in a given hour with a focus on 30 minutes.
     - Line plot showing the number of messages sent in a given time period over the years with a precision of 7 days.
     - Line plot with the total number of messages over the entire timeline.
3. **Panel 3: Average Response Time (with options to change date range and person)**
   - Heatmap with the average response time at a given time of day on a specific day of the week.
   - Line plot with the average response time in a given time period.
4. **Panel 4: Most Common Strings**
   - Bar plot showing the most common sequences of words written by us, with the possibility to modify the number of bars, the number of words in the sequence (1, 2, or 3), and the choice of a person.
5. **Panel 5: Summary**
   - Stating all the most important conclusions that can be drawn from the data for each person.
  
*Example of information operation can be found in this [video](visualisation/Adamczyk_Iwaniuk_Pozorski.mp4).*

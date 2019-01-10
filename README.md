# NHL_metrics.R
My work on data from the National Hockey League (NHL). The repository contains the following scripts:
* compile_all.R: would scrape all the data at once. Due to sheer amount of data (it was 2014) I had to switch to 
* compile_seasons.R: scrapes data season-by-season. This runs smoothly and scrapes succesfully the desired data
* descriptive_1314.R: Data cleaning and tidying, exploratory analysis and visualizations
* true_scorers_1314.R: Idea: There are more important goals than others. I propose an weighted goal measure that takes a goal's relative importance into account
* gini_scoring.R:  How important is secondary scoring really? I use the well-known Gini-coefficient to assess a team's scoring inequality. With this metric, I then find a solid positive link between secondary scoring and a team's success
* weaklink_stronglink.R: How to improve a team? Is it better to just buy another star player? Or should one rather improve from "bottom-up" and strengthen a team's weak spots? Based on Goal-above-Replacement data (GAR), I find support for the latter strategy

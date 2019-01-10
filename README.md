# NHL_metrics.R
My work on data from the National Hockey League (NHL). The repository contains the following scripts:
* 1compile_seasons.R: scrapes data season-by-season. This runs smoothly and scrapes succesfully the desired data
* 2descriptive_1314.R: Data cleaning and tidying, exploratory analysis and visualizations
* 3true_scorers_1314.R: Idea: There are more important goals than others. I propose an weighted goal measure that takes a goal's relative importance into account
* 4gini_scoring.R:  How important is secondary scoring really? I use the well-known Gini-coefficient to assess a team's scoring inequality. With this metric, I then find a solid positive link between secondary scoring and a team's success
* 5weaklink_stronglink.R: How to improve a team? Is it better to just buy another star player? Or should one rather improve from "bottom-up" and strengthen a team's weak spots? Based on Goal-above-Replacement data (GAR), I find support for the latter strategy

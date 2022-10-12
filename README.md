# Project 2: Shiny App Development

### [Project Description](doc/project2_desc.md)

![screenshot](doc/figs/map.jpg)

In this second project of GR5243 Applied Data Science, we develop a *Exploratory Data Analysis and Visualization* shiny app on the work of a **NYC government agency/program** of your choice using NYC open data released on the [NYC Open Data By Agency](https://opendata.cityofnewyork.us/data/) website. In particular, many agencies have adjusted their work or rolled out new programs due to COVID, your app should provide ways for a user to explore quantiative measures of how covid has impacted daily life in NYC from different prospectives. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## NYCHA Utility Data
Term: Fall 2022

+ Team #9
+ NYCHA Utility Data:
	+ Zeya Ahmad
	+ Christopher Halim
	+ Ting Han Ko
	+ Sameer Kolluri
	+ Weijia Wang

+ **Project summary**: We created an R Shiny app to help the NYCHA visualize the utility consumption across its residencies. The app consistents of a heatmap, pie charts, a line chart and a data table to allow the user to track utility usage over time. The data we use is from NYCHA (New York City Housing Authority). The Consumption and cost data of Electric, Water and Cooking Gas are being plotted in this dashboard. Our data range from January 2010 to February 2022. With the help of the app, we came to some useful findings.

+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members assisted in the data processing and cleaning process. Zeya and Christopher worked on the heatmap, Ting worked on the line graph, and Weijia and Sameer worked on the pie charts and data table. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.


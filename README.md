# Faceted-plots-with-significance
Faceted bar graphs in ggplot2  with different significance bars and symbols added to each facet

geomtext() and geomsegment() are great for adding significance lines and symbols to your plots, but when the data are split into two plots using facet_wrap,
these functions will apply significance lines and symbols to the same x,y positions on both plots.  This is a problem if you are analyzing the data shown
in each plot separately (i.e. analyzing and graphing sexes separately).  Maybe you only see an effect in one group and therefore need to add significance
bars to only one of your faceted plots.

This code allows you to place significance bars and symbols in different locations (or not at all) on each plot.

It was informed by the code and video instruction of Pat Schloss: https://riffomonas.org/code_club/2021-04-23-faceted-bars-stars 

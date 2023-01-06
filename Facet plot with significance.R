
#Body and Brain Weight Figure
## The effects of phthalate and LPS treatment
### Factors: Sex, Phthalate Dose (Dose), LPS treatment (Tx)
#### Data subsetted by Age

library(ggplot2)
library(plyr)
library(car)
library(visreg)

#Make Tables
p10.body.weight_table = ddply(p10, c("Sex", "Dose", "Tx"), summarise,
                              N = length(Body.Weight),
                              mean = mean(Body.Weight),
                              sd = sd(Body.Weight),
                              se = sd/sqrt(N))

p10.brain.weight_table = ddply(p10, c("Sex", "Dose", "Tx"), summarise,
                               N = length(Brain.Weight),
                               mean = mean(Brain.Weight),
                               sd = sd(Brain.Weight),
                               se = sd/sqrt(N))

p60_body_table = ddply(lps_full, c("Sex", "Dose", "Trt"), summarise,
                       N = length(body.weight),
                       mean = mean(body.weight),
                       sd = sd(body.weight),
                       se = sd/sqrt(N))

p60_brain_table = ddply(lps_full, c("Sex", "Dose", "Trt"), summarise,
                        N = length(brain.weight),
                        mean = mean(brain.weight),
                        sd = sd(brain.weight),
                        se = sd/sqrt(N))

#Graph A: Body weight on Postnatal Day 10
p10.body.graph = 
  ggplot(p10.body.weight_table, aes(group = Tx, x = Dose, y = mean, fill = Tx))+  #Tx is saline or LPS; Dose is phthalate dose
  geom_bar(stat="identity", position= position_dodge(0.9))+ 
  geom_errorbar(aes(ymin=mean-se, ymax= mean+se), width= 0.1, position = position_dodge(0.9))+ 
  scale_fill_manual(values=c("dodgerblue", "gold")) +
  facet_wrap("Sex")+
  stat_summary(aes(x=as.numeric(Dose), group=Tx),fun.data = mean_se, geom = "errorbar", width=0.05,position=position_dodge(width=0.6)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(
    xlim = c(0.5,2.5),
    ylim = c(0,30),
    expand = FALSE,
    default = FALSE,
    clip = "on"
  )+
  theme(axis.text.x= element_text(angle=0)) +
  ggtitle("P10 Body Weight")+
  labs(tag = "A")+ #labels the plot as figure A
  theme(legend.title = element_blank(), #removes legend title
        plot.title = element_text(face = "bold", size = 16, hjust = .5),
        plot.tag = element_text(size = 20, face = "bold"), #adjusts size of "A" plot tag
        legend.text = element_text(size = 12), #changes text size used in legend
        panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line.x = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.line.y = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        axis.title.x = element_text(family="Arial", size=16, color="black"), #size of x-axis title
        axis.title.y = element_text(family="Arial", size=16, color="black"), #size of y-axis title
        axis.text.x = element_text(family="Arial", size=16, color="black"), #size of x-axis text
        axis.text.y = element_text(family="Arial", size=16, color="black"))+ #size of y-axis text)+
  theme(strip.background = element_blank(), strip.text = element_text(family="Arial", size=16, color="black"), strip.placement = "outside") +
  ylab(expression(bold(paste("Body Weight (g)  "))))+ #makes y-axis title bold
  xlab("Phthalate Dose (µg/kg)")
p10.body.graph

#Graph B: Brain Weight on Postnatal Day 10
p10.brain.graph = 
  ggplot(p10.brain.weight_table, aes(group = Tx, x = Dose, y = mean, fill = Tx))+
  geom_bar(stat="identity", position= position_dodge(0.9))+ 
  geom_errorbar(aes(ymin=mean-se, ymax= mean+se), width= 0.1, position = position_dodge(0.9))+ 
  scale_fill_manual(values=c("dodgerblue", "gold")) +
  facet_wrap("Sex")+
  stat_summary(aes(x=as.numeric(Dose), group=Tx),fun.data = mean_se, geom = "errorbar", width=0.05,position=position_dodge(width=0.6)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(
    xlim = c(0.5,2.5),
    ylim = c(0,1.2),
    expand = FALSE,
    default = FALSE,
    clip = "on"
  )+
  theme(axis.text.x= element_text(angle=0)) +
  ggtitle("P10 Brain Weight")+
  labs(tag = "B")+
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = .5),
        plot.tag = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12),
        panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line.x = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.line.y = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        axis.title.x = element_text(family="Arial", size=16, color="black"), #size of x-axis title
        axis.title.y = element_text(family="Arial", size=16, color="black"), #size of y-axis title
        axis.text.x = element_text(family="Arial", size=16, color="black"), #size of x-axis text
        axis.text.y = element_text(family="Arial", size=16, color="black"))+ #size of y-axis text)+
  theme(strip.background = element_blank(), strip.text = element_text(family="Arial", size=16, color="black"), strip.placement = "outside") +
  ylab(expression(bold(paste("Brain Weight (g)  "))))+
  xlab("Phthalate Dose (µg/kg)")

p10.brain.graph

#Add lines that will denote significance between groups
lines = tibble(
  Sex = c("Female", "Male"), #one line will go over the left plot/facet (Female) and one over the right plot (Male)
  x = c(0,1), #first number is the x start for line 1 (we don't want one here so we put 0); second number is x start for line 2
  xend = c(0, 2), #first number is the x end for line 1 (we don't want one here so we put 0); second number is x end for line 2
  y = c(0, 1.05), #first number is the y start for line 1 (we don't want one here so we put 0); second number is y start for line 2
  yend = c(0, 1.05) #first number is the y end for line 1 (we don't want one here so we put 0); second number is y end for line 2
)
lines

stars = tibble(
  Sex = c("Female", "Male"), #one star will go over the left plot/facet (Female) and one over the right plot (Male)
  x = c(0,1.5), # first number is x position for left plot (we don't want one here so we put 0); second number is x position on right
  y = c(0, 1.06), # first number is y position for left plot (we don't want one here so we put 0); second number is y position on right
  label = c("", "*") # included a blank because no asterisk is needed on the left plot; we want an asterisk on the right plot
)

p10.brain.graph.final = p10.brain.graph+ #add graph, lines, and stars together
  geom_segment(data = lines, aes(x=x, xend = xend, y=y, yend = yend), inherit.aes = FALSE)+
  geom_text(data = stars, aes(x=x, y=y, label=label), size = 8, inherit.aes = FALSE)
p10.brain.graph.final

#Graph C: Body weight on Postnatal Day 60
p60.body.graph = 
  ggplot(p60_body_table, aes(group = Trt, x = Dose, y = mean, fill = Trt))+
  geom_bar(stat="identity", position= position_dodge(0.9))+ 
  geom_errorbar(aes(ymin=mean-se, ymax= mean+se), width= 0.1, position = position_dodge(0.9))+ 
  scale_fill_manual(values=c("dodgerblue", "gold")) +
  facet_wrap("Sex")+
  stat_summary(aes(x=as.numeric(Dose), group=Trt),fun.data = mean_se, geom = "errorbar", width=0.05,position=position_dodge(width=0.6)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(
    xlim = c(0.5,2.5),
    ylim = c(0,400),
    expand = FALSE,
    default = FALSE,
    clip = "on"
  )+
  theme(axis.text.x= element_text(angle=0)) +
  ggtitle("P60 Body Weight")+
  labs(tag = "C")+
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = .5),
        plot.tag = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12),
        panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line.x = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.line.y = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        axis.title.x = element_text(family="Arial", size=16, color="black"), #size of x-axis title
        axis.title.y = element_text(family="Arial", size=16, color="black"), #size of y-axis title
        axis.text.x = element_text(family="Arial", size=16, color="black"), #size of x-axis text
        axis.text.y = element_text(family="Arial", size=16, color="black"))+ #size of y-axis text)+
  theme(strip.background = element_blank(), strip.text = element_text(family="Arial", size=16, color="black"), strip.placement = "outside") +
  ylab(expression(bold(paste("Body Weight (g)  "))))+
  xlab("Phthalate Dose (µg/kg)")
p60.body.graph

#Graph D: Brain weight on Postnatal Day 60
p60.brain.graph = 
  ggplot(p60_brain_table, aes(group = Trt, x = Dose, y = mean, fill = Trt))+
  geom_bar(stat="identity", position= position_dodge(0.9))+ 
  geom_errorbar(aes(ymin=mean-se, ymax= mean+se), width= 0.1, position = position_dodge(0.9))+ 
  scale_fill_manual(values=c("dodgerblue", "gold")) +
  facet_wrap("Sex")+
  stat_summary(aes(x=as.numeric(Dose), group=Trt),fun.data = mean_se, geom = "errorbar", width=0.05,position=position_dodge(width=0.6)) + 
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(
    xlim = c(0.5,2.5),
    ylim = c(0,2.08),
    expand = FALSE,
    default = FALSE,
    clip = "on"
  )+
  theme(axis.text.x= element_text(angle=0)) +
  labs(tag = "D")+
  theme(legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = .5),
        plot.tag = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 12),
        panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
        panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
        panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
        panel.border=element_blank(), #gets rid of square going around the entire graph
        axis.line.x = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.line.y = element_line(colour = 'black', size = 0.5),#sets the axis line size
        axis.ticks=element_line(colour = 'black', size = 0.5), #sets the tick lines
        axis.title.x = element_text(family="Arial", size=16, color="black"), #size of x-axis title
        axis.title.y = element_text(family="Arial", size=16, color="black"), #size of y-axis title
        axis.text.x = element_text(family="Arial", size=16, color="black"), #size of x-axis text
        axis.text.y = element_text(family="Arial", size=16, color="black"))+ #size of y-axis text)+
  theme(strip.background = element_blank(), strip.text = element_text(family="Arial", size=16, color="black"), strip.placement = "outside") +
  ylab(expression(bold(paste("Brain Weight (g)  "))))+
  xlab("Phthalate Dose (µg/kg)")
p60.brain.graph 

lines = tibble(
  Sex = c("Female", "Male", "Male"),
  x = c(0,.75, 1.75),
  xend = c(0, 1.25, 2.25),
  y = c(0, 1.95, 1.95),
  yend = c(0, 1.95, 1.95)
)
lines

stars = tibble(
  Sex = c("Female", "Male", "Male"),
  x = c(0, 1, 2),
  y = c(0, 1.96, 1.96),
  label = c("", "*", "*")
)

p60.brain.graph.final = p60.brain.graph+
  geom_segment(data = lines, aes(x=x, xend = xend, y=y, yend = yend), inherit.aes = FALSE)+
  geom_text(data = stars, aes(x=x, y=y, label=label), size = 8, inherit.aes = FALSE)
p60.brain.graph.final

# Arrange all 4 graphs into a panel figure
library(ggpubr)
ggarrange(p10.body.graph, p10.brain.graph.final, p60.body.graph, p60.brain.graph.final, ncol = 2, nrow = 2)



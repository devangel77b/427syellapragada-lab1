library(ggplot2)
library(dplyr)

raw <- read.csv('vdata.csv',header=TRUE)
data <- tibble(raw)

# prediction
v_predicted <- function(height_m,g){
  (2*g*height_m)^(0.5)
}


# make plots
fig <- ggplot(data)+
    geom_point(aes(x=height_m,y=v_ms,color=type))+
    geom_function(fun=v_predicted,args=list(g=6.91),color='pink')+
    geom_function(fun=v_predicted,args=list(g=10.18),color='cyan')
# format and adjust sizes etc for print


# do stats to get g and compare
data <- data %>%
     select(type, height_m, v_ms) %>%
     mutate(vsquared=v_ms^2)
model1 <- lm(vsquared~height_m+0,data)
model2 <- lm(vsquared~height_m:type+0,data)
anova(model1,model2)

# get g for cricket versus ping pong cases
# LATER
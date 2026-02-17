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
    geom_hline(yintercept=0,color='gray50')+
    geom_point(aes(x=height_m,y=v_ms,color=type))+
    geom_function(fun=v_predicted,args=list(g=6.91),color='pink')+
    geom_function(fun=v_predicted,args=list(g=10.18),color='cyan')+
    theme_classic(base_size=8)+
    xlab('$h$ (\\unit{\\meter})')+
    ylab('$v$ (\\unit{\\meter\\per\\second})')+
    theme(legend.position="inside",
	legend.position.inside=c(0.05,0.95),
	legend.justification.inside=c("left","top"),
	legend.title=element_blank())
	
# save the figure
ggsave("fig1.svg",plot=fig,width=3.4167,height=2,units="in")


# do stats to get g and compare
data <- data %>%
     select(type, height_m, v_ms) %>%
     mutate(vsquared=v_ms^2)
model1 <- lm(vsquared~height_m+0,data)
model2 <- lm(vsquared~height_m:type+0,data)
anova(model1,model2)

# get g for cricket versus ping pong cases
print(summary(lm(vsquared~height_m+0,filter(data,type=='ping_pong'))))
print(summary(lm(vsquared~height_m+0,filter(data,type=='cricket'))))

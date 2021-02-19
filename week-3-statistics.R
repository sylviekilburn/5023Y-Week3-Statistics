wood_density <- read_csv("Data/wood_density.csv")
wood_density %>% ggplot(aes(x=Density,
                            y=Hardness))+
  geom_point()+
  geom_smooth(method="lm")
density_model <- lm(Hardness~Density, data=wood_density)

#Task
(24.7*57.507)+-1160.5

coef(density_model)[1]+
  coef(density_model)[2]*
  24.7

fitted(density_model)

#Task
wood_density_augmented <- wood_density %>% 
  mutate(predictions=fitted(density_model)) %>% 
  mutate(residuals=Hardness-predictions)

#plots
p1 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=Hardness))+
  geom_line()+
  ggtitle("Full Data")

p2 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=predictions))+
  geom_line()+
  ggtitle("Linear trend")

p3 <- wood_density_augmented %>% 
  ggplot(aes(x=Density, y=residuals))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining pattern")

p1+p2+p3

broom::glance(density_model)

broom::tidy(density_model, conf.int=TRUE)

broom::augment(density_model, wood_density, interval="confidence") 

plot1 <- broom::augment(density_model, wood_density, interval="confidence") %>% ggplot(aes(x=Density, y=Hardness))+geom_line(aes(x=Density, y=.fitted))+geom_line(aes(x=Density, y=.upper), linetype="dashed")+geom_line(aes(x=Density, y=.lower), linetype="dashed")+geom_point() +ggtitle("Manually fitting linear model \n and confidence intervals")

plot2 <- wood_density %>% ggplot(aes(x=Density, y=Hardness))+geom_smooth(method=lm)+geom_point()+ggtitle("Geom smooth method to plotting \n a linear model")

plot1+plot2

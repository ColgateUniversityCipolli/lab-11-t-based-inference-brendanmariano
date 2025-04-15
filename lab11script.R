library(tidyverse)
library(pwr)
library(ggplot2)
################################################################################
# Lab 10
################################################################################

#############
# Step 1
#############
n.value = pwr.t.test(d = 0.65, # large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample")$n
############
# Step 2 and 3
############
data = read_csv("data.csv") |>
  mutate(difference = `Closer Values` - `Farther Values`)

numerical.summary = data |>
  pivot_longer(cols = c(`Farther Values`, `Closer Values`, "difference"), values_to = "Dopamine", 
               names_to = "Type") |>
  group_by(Type) |>
  summarize(mean = mean(Dopamine), sd = sd(Dopamine))

numerical.summary
  
# Further data
ggplot(data = data) +
  geom_boxplot(aes(x = `Farther Values`), 
                fill = "royalblue") + 
  theme_bw() + 
  ggtitle("Farther Data")
# Closer data
ggplot(data = data) + 
  geom_boxplot(aes(x = `Closer Values`), 
                 fill = "purple") +
  theme_bw() +
  ggtitle("Closer Data")

#Difference data
ggplot(data = data) + 
  geom_boxplot(aes(x = difference), fill = "blue") + 
  theme_bw() + 
  ggtitle("Difference")

view(data$`Farther Values`)
###############
# Step 4
###############

val = t.test(data$`Closer Values`, mu = 0,
             alternative =  "greater", 
             conf.level = .95)

val2 = t.test(data$`Farther Values`, mu = 0,
             alternative =  "less", 
             conf.level = .95)
val3 = t.test(data$difference, mu = 0,
              alternative =  "two.sided", 
              conf.level = .95)

####################################
# Step 5
####################################

################
# Closer values (less than test)
################
#Null distribution
ggdat.t <- tibble(t=seq(-10,10,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1)) 
# t-distribution
ggdat.test.stat = tibble(t = val$statistic, y = 0)
mu0 = 0
length = length(data$`Closer Values`)
resamples <- tibble(t=numeric(1000))

for(i in 1:1000){
  curr.sample = sample(data$`Closer Values`, size = length, replace = TRUE)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(length))
}


#Hypothesis test two

ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
  # geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="grey", alpha=0.5)+
  # geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  # geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.test.stat, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples, 
               aes(x=t),
               geom="line", color="grey")

################
#Farther values (greater than test)
################
#Computing the test statistic
ggdat.test.stat2 = tibble(t = val2$statistic, y = 0)
mu0 = 0
length = length(data$`Farther Values`)
resamples2 <- tibble(t=numeric(1000))

for(i in 1:1000){
  curr.sample = sample(data$`Farther Values`, size = length, replace = TRUE)
  resamples2$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(length))
}

# Plotting

ggplot() +
  # null distribution
  geom_line(data=ggdat.t, 
            aes(x=t, y=pdf.null))+
  geom_hline(yintercept=0)+
  # rejection regions
   geom_ribbon(data=subset(ggdat.t, t<=qt(0.025, df=n-1)), 
               aes(x=t, ymin=0, ymax=pdf.null),
               fill="grey", alpha=0.5)+
  # geom_ribbon(data=subset(ggdat.t, t>=qt(0.975, df=n-1)), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="grey", alpha=0.5)+
  # plot p-value (not visible)
  # geom_ribbon(data=subset(ggdat.t, t>=t.stat), 
  #             aes(x=t, ymin=0, ymax=pdf.null),
  #             fill="reg", alpha=0.25)+
  # plot observation point
  geom_point(data=ggdat.test.stat2, aes(x=t, y=y), color="red")+
  # Resampling Distribution
  stat_density(data=resamples2, 
               aes(x=t),
               geom="line", color="grey")
  #clean up aesthetics
  theme_bw()+
  scale_x_continuous("t",
                     breaks = round(t.breaks,2),
                     sec.axis = sec_axis(~.,
                                         name = bquote(bar(x)),
                                         breaks = t.breaks,
                                         labels = round(xbar.breaks,2)))+
  ylab("Density")+
  ggtitle("T-Test for Mean Perceived Whiteness of Social Security Recipients",
          subtitle=bquote(H[0]==3.5*";"~H[a]!=3.5))






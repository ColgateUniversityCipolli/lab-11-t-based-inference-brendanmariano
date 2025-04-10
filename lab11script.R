library(tidyverse)
library(pwr)
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

################
# Step 5
################
mu0 = 0
length = length(data$`Farther Values`)
resamples <- tibble(t=numeric(1000))
ggdat.t <- tibble(t=seq(-10,10,length.out=1000))|>
  mutate(pdf.null = dt(t, df=n-1)) 

for(i in 1:1000){
  curr.sample = sample(data$`Farther Values`, size = length, replace = TRUE)
  resamples$t[i] = (mean(curr.sample)-mu0)/(sd(curr.sample)/sqrt(length))
}

ggplot()+
  geom_line(data=ggdat.t, aes(x=t, y=pdf.null, 
                              color="T-distribution (Null)")) +
  geom_density(data = resamples, aes(x = t, y = after_stat(density)))
  theme_bw()

#Hypothesis test one










library(tidyverse)
library(readxl)
library(patchwork)

class_data_1 <- (read_excel(path = "data/GGG_1.xlsx", na = "NA"))
class_data_2 <- (read_excel(path = "data/GGG_2.xlsx", na = "NA"))
class_data_3 <- (read_excel(path = "data/GGG_3.xlsx", na = "NA"))
class_data_4 <- (read_excel(path = "data/GGG_4.xlsx", na = "NA"))

mean_class <- (read_excel(path = "data/mean_GGG.xlsx", na = "NA"))

long_mean_class <- mean_class %>% 
  pivot_longer(cols = ("37/37":"37/30"), names_to = "temperatures", values_to = "colony_count")

lm1 <- lm(colony_count ~ temperatures, data = long_mean_class)

summary(lm1)
broom::tidy(lm1)

long_class_data_2 <- class_data_2 %>% 
  pivot_longer(cols = ("30/30":"30/30"), names_to = "temperatures", values_to = "colony_count")

class_data_summary_2 <- long_class_data_2 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))

class_data_plot_2 <- class_data_summary_2 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_2,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "non-selective/ selective temp (°C)",
       y =  "Mean (+/- S.E.) number of colonies on plate", 
       title = "mutS (30 °C/ 30 °C)")+
  theme_minimal()
conf.int=T)

class_data_3 <- (read_excel(path = "data/GGG_3.xlsx", na = "NA"))

long_class_data_3 <- class_data_3 %>% 
  pivot_longer(cols = ("37/30":"37/30"), names_to = "temperatures", values_to = "colony_count")

class_data_summary_3 <- long_class_data_3 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))

class_data_plot_3 <- class_data_summary_3 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_3,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "non-selective/ selective temp (°C)",
       y = "The number of colonies on plate", 
       title = "mutS- (37 °C/ 30 °C)")+
  theme_minimal()
conf.int=T)

class_data_4 <- (read_excel(path = "data/GGG_4.xlsx", na = "NA"))

long_class_data_4 <- class_data_4 %>% 
  pivot_longer(cols = ("30/37":"30/37"), names_to = "temperatures", values_to = "colony_count")

class_data_summary_4 <- long_class_data_4 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))

class_data_plot_4 <- class_data_summary_4 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_4,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "non-selective/ selective temp (°C)",
       y = "Mean (+/- S.E.) number of colonies on plate", 
       title = "mutS (30 °C/ 37 °C)")+
  theme_minimal()
conf.int=T)

patchwork1 <- class_data_plot_1 + class_data_plot_2 
patchwork2 <- class_data_plot_3 + class_data_plot_4

patchwork1 
patchwork2

patchwork1+patchwork2

patchwork1  + class_data_plot_3 + class_data_plot_4

class_data_plot_1 + class_data_plot_2 + class_data_plot_3 + class_data_plot_4

class_data <- (read_excel(path = "data/GGG_summative.xlsx", na = "NA"))

long_class_data <- class_data %>% 
  pivot_longer(cols = ("37/37":"30/37"), names_to = "temperatures", values_to = "colony_count")

lm01 <- lm(colony_count ~ temperatures, data = long_class_data)

summary(lm01)
broom::tidy(lm01)



#____ Making the data long 
long_class_data_1 <- class_data_1 %>% 
  pivot_longer(cols = ("37/37":"37/37"), names_to = "temperatures", values_to = "colony_count")
#_____ Making a summary of the data 
class_data_summary_1 <- long_class_data_1 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
class_data_plot_1 <- class_data_summary_1 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_1,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "",
       y = "Mean (+/- S.E.) number of colonies on plate", 
       title = "mutS (37 °C/ 37 °C)")+
  theme_minimal()
conf.int=T)



sum(is.na(class_data))
class_data_new <- na.omit(class_data)

sum(is.na(class_data_new))



class_data_2 <- (read_excel(path = "data/GGG_summative.xlsx", na = "NA"))
#____ Making the data long 
long_class_data_1 <- class_data_1 %>% 
  pivot_longer(cols = ("37/37":"37/37"), names_to = "temperatures", values_to = "colony_count")
#_____ Making a summary of the data 
class_data_summary_1 <- long_class_data_1 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
class_data_plot_1 <- class_data_summary_1 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_1,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "non-selective/ selective temp (°C)",
       y = "Mean (+/- S.E.) number of colonies on plate",
       title = "mutS (37 °C/ 37 °C)")+
  theme_minimal()
conf.int=T)


class_data <- (read_excel(path = "data/GGG_summative.xlsx", na = "NA"))

#____ Making the data long 
long_class_data_new <- class_data_new %>% 
  pivot_longer(cols = ("37/37":"30/37"), names_to = "temperatures", values_to = "colony_count")
#_____ Making a summary of the data 
class_data_summary_new <- long_class_data_new %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))
#---------- Visualise the data of egg counting
class_data_plot_new <- class_data_summary_new %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_new,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "",
       y = "",
       title = "30/37")+
  theme_minimal()
conf.int=T)



class_data_01 <- (read_excel(path = "data/GGG_01.xlsx", na = "NA"))

long_class_data_01 <- class_data_01 %>% 
  pivot_longer(cols = ("37/37WT":"37/37mutS-"), names_to = "temperatures", values_to = "colony_count")

class_data_summary_01 <- long_class_data_01 %>% 
  group_by(temperatures) %>% 
  summarise(mean = mean(colony_count),
            sd = sd(colony_count),
            n = n(),
            se = sd/sqrt(n))

class_data_plot_01 <- class_data_summary_01 %>% 
  ggplot(aes(x = temperatures, y = mean))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           colour = "salmon",
           alpha = 0.6)+
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), 
                colour = "salmon",
                width = 0.2)+
  geom_jitter(data = long_class_data_01,
              aes(x = temperatures,
                  y = colony_count),
              fill = "salmon",
              colour = "white",
              width = 0.2,
              shape = 21)+
  ylim(0,2000)+
  labs(x = "",
       y = "", 
       title = "mutS 30/30")+
  theme_minimal()
conf.int=T)

alldata <- rbind(long_class_data_1, long_class_data_2, long_class_data_3, long_class_data_4)




lm1 <- lm(temperatures ~ colony_count, data = long_class_data_1)
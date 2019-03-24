library(tidyverse)
######################################################
## DATA CLEANING
######################################################
mydata <- as.data.frame(read_csv("Documents/2019_GRADUATE/Statistical Graphics/HW3data.csv"))
for(i in 1:ncol(mydata)) {
  mydata[,i] <- replace(mydata[,i], mydata[,i] == '<0.500', NA)
  if(i > 2) {mydata[,i] <- as.numeric(mydata[,i])}
}
A1 <- mydata %>% 
  select(ID, Group, colnames(mydata)[str_detect(colnames(mydata), 'Var_A1')]) %>% 
  gather(-ID, -Group, key = 'Weeks', value = 'value') 
A2 <- mydata %>% 
  select(ID, Group, colnames(mydata)[str_detect(colnames(mydata), '002_A2')]) %>% 
  gather(-ID, -Group, key = 'Weeks', value = 'value') 
A3 <- mydata %>% 
  select(ID, Group, colnames(mydata)[str_detect(colnames(mydata), 'Var_A3')]) %>% 
  gather(-ID, -Group, key = 'Weeks', value = 'value') 
Total <- rbind(data.frame(A1, Measure = 'A1'), 
               data.frame(A2, Measure = 'A2'),
               data.frame(A3, Measure = 'A3')) %>% 
  separate(Weeks, into = c('Exercise','Weeks'), sep = '_V')
Total$Exercise <- ifelse(str_detect(Total$Exercise, 'tread_after'), TRUE, FALSE)
Total$Weeks <- str_extract(Total$Weeks, pattern = c('-2wk|0wk|8wk'))
######################################################
## 기본 & 고강도 운동의 차이(CONTROL/TEST)
######################################################
Total %>% ggplot(aes(x = Group, fill = Weeks, y = value)) +
  geom_boxplot() +
  facet_wrap(Measure ~., scales = 'free') +
  theme_bw() + theme(axis.title = element_text(size = 15),
                     axis.text.x = element_text(size = 13),
                     strip.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     legend.text = element_text(size = 12),
                     legend.position = 'bottom')
######################################################
## 1시간 운동 전후의 차이
######################################################
Total %>% ggplot(aes(x = Exercise, y = value, fill = Weeks)) +
  geom_boxplot() +
  facet_wrap(. ~ Measure, scales = 'free') +
  theme_bw() + theme(axis.title = element_text(size = 15),
                     axis.text.x = element_text(size = 13),
                     strip.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     legend.text = element_text(size = 12),
                     legend.position = 'bottom')
######################################################
## INTERACTION
######################################################
Total %>% ggplot(aes(x = Group, fill = Exercise, y = value)) +
  geom_boxplot() +
  facet_grid(Measure ~ Weeks, scales = 'free') +
  theme_bw() + theme(axis.title = element_text(size = 15),
                     axis.text.x = element_text(size = 13),
                     strip.text = element_text(size = 13),
                     legend.title = element_text(size = 13),
                     legend.text = element_text(size = 12),
                     legend.position = 'bottom')


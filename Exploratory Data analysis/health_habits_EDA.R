## Amy's pick for variables

#log_avgBPXSY is one of the target variables
#log_avgBPXDI is the other target variable

#from questionnare:
SLD010H #how much sleep normall get at night on weekdays or workdays
PAQ650 #In a typical week do any vigorous-intensity sports,fitness, or recreational acitivies
PAQ655 #in a typical week do you do any moderate-intensity
PAD660 #how much time do you spend doing vigorou-intnsity acitivities
PAD675 # how much time do you spend doing moderate-intensity activities
PAQ670 #in a week how many days do you do moderat-intensity sports

#demographic
RIDRETH1 #hispanic origin
DMDCITZN #citizens
DMDBORN4 #country       

summary(dataCleaned$SLD010H) #how much sleep normall get at night on weekdays or workdays, #3431 NA #2-99 hours of sleep on weekdays #NEED TO CLEAN -- NEED TO REMOVE 77 AND 99 AND . 'MISSING'
summary(dataCleaned$PAQ650) #In a typical week do you do any  vigorous-intensity sports,fitness, or recreational acitivies, 1 is yes, 2 is no #2784 na #1 AND 2 AS OPTIONS (remove 7,9, . (missing))
#TOO MANY MISSING summary(dataCleaned$PAQ655) #In a typical week, on how many days do vigorous-intensity sports, fitness or recreational activities? #7507 NA's, need to have max be 7, and not 99
#TOO MANY MISSING summary(dataCleaned$PAD660) #how much time do you spend doing vigorou-intensity acitivities on a typical day #7510 Nas #need to clean, MAX IS 660, EXCLUDE 7777 AND 9999
summary(dataCleaned$PAD675) # how much time do you spend doing moderate-intensity activities in a typical day # 6544 NAs # need to clean, 10 TO 900, REMOVE 7777 AND 9999, AND .(MISSING)
summary(dataCleaned$PAQ670) #in a week how many days do you do moderat-intensity sports#6544 NAs #should be min 1 max 7, need to clean REMOVE 99 AND . (MISSING)
summary(dataCleaned$RIDRETH1) #hispanic origin #1969 NAs #option 1-5# 1- mexican american 2-other hispanic 3-non-hispanic white 4-non hispanic black 5 other race
summary(dataCleaned$DMDCITZN) #citizens # 1973 NAs #REMOVE 7,9 AND . (MISSING)          
summary(dataCleaned$DMDBORN4) #country #1969 NAs#1 to 77 max #remove 77, 99, . (missing) 1 - USA 2 - Other, delete 77, 99, .(missing)
PAq635 # LOOKS LIKE A NICE ONE, travel to and from places, do you walk or use a bicycle for at elast 10 minutes in a typical week ot get from places - remove 7 and 9 and x (missing)
#PAQ650 do you do any vigorous=intense sports in a typical week for at least 10 min- only keep 1 and 2 (remove 7,9, . (missing))

data = data.frame(log_avgsystolic = dataCleaned$log_avgBPXSY, log_avgdiastolic = dataCleaned$log_avgBPXDI,
                  sleep_night = dataCleaned$SLD010H, vigorous_week_binary = dataCleaned$PAQ650,  Race = dataCleaned$RIDRETH1, 
                  citizen = dataCleaned$DMDCITZN, USA_binary = dataCleaned$DMDBORN4,
                  Week_min10_travel = dataCleaned$PAQ635)

#removed due to too many NAs: moderate_day_minutes = dataCleaned$PAD675,moderate_week_days = dataCleaned$PAQ670,

# filter meaningless values
data = na.omit(data) #still have 6014
data = data[!(data$sleep_night %in% c(77,99) | data$vigorous_week_binary %in% c(7,9)
              | data$citizen %in% c(7,9) | data$USA_binary %in% c(77,99)
              | data$Week_min10_travel %in% c(7,9)),]
str(data) #still have 6001 observations



#EXPLORATORY DATA ANALYSIS, 

#SLEEP_NIGHT
g1 <- ggplot(data = data, aes(x = sleep_night, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g2 <- ggplot(data, aes(factor(sleep_night), log_avgsystolic, fill=sleep_night)) + 
  geom_boxplot() + labs(x = 'Avg Sleep Night', y = "Avg Systolic", title = "BoxPlot of Avg Sleep Nightly and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g3<- ggplot(data = data, aes(x = sleep_night, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g4 <- ggplot(data, aes(factor(sleep_night), log_avgdiastolic, fill=sleep_night)) + 
  geom_boxplot() + labs(x = 'Avg Sleep Night', y = "Avg Diastolic", title = "BoxPlot of Avg Sleep Nightly and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))
#g4 <- ggplot(data = data, aes(x = sleep_night, y = log_avgdiastolic)) +
# geom_boxplot(aes(fill=sleep_night))
g1
g2
g3
g4
ggarrange(g2,g4, nrow = 1, ncol=2)

##In a typical week do you do any  vigorous-intensity sports,fitness, or recreational acitivies
#vigorous_week_binary = dataCleaned$PAQ650,  
g5 <- ggplot(data = data, aes(x = vigorous_week_binary, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g6 <- ggplot(data, aes(factor(vigorous_week_binary), log_avgsystolic, fill=vigorous_week_binary)) + 
  geom_boxplot() + labs(x = 'Vigorous Activities (week)', y = "Avg Systolic", title = "BoxPlot of Vigorous Activities (week) and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g7<- ggplot(data = data, aes(x = vigorous_week_binary, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g8 <- ggplot(data, aes(factor(vigorous_week_binary), log_avgdiastolic, fill=vigorous_week_binary)) + 
  geom_boxplot() + labs(x = 'Vigorous Activities (week)', y = "Avg Diastolic", title = "BoxPlot of Vigorous Activities (week) and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

#g8 <- ggplot(data = data, aes(x = sleep_night, y = log_avgdiastolic)) +
# geom_boxplot(aes(fill=sleep_night))

g5
g6
g7
g8

ggarrange(g6,g8, nrow = 1, ncol=2)

#Race = dataCleaned$RIDRETH1, summary(dataCleaned$RIDRETH1) #hispanic origin #1969 NAs #option 1-5# 1- mexican american 2-other hispanic 3-non-hispanic white 4-non hispanic black 5 other race

g9 <- ggplot(data = data, aes(x = Race, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g10 <- ggplot(data, aes(factor(Race), log_avgsystolic, fill=Race)) + 
  geom_boxplot() + labs(x = 'Race', y = "Avg Systolic", title = "BoxPlot of Race and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g11<- ggplot(data = data, aes(x = Race, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g12 <- ggplot(data, aes(factor(Race), log_avgdiastolic, fill=Race)) + 
  geom_boxplot() + labs(x = 'Race', y = "Avg Diastolic", title = "BoxPlot of Race and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))


g9
g10
g11
g12

ggarrange(g10, g12, nrow = 1, ncol=2)


#citizen = dataCleaned$DMDCITZN, ,summary(dataCleaned$DMDCITZN) #citizens # 1973 NAs #REMOVE 7,9 AND . (MISSING)1 = citizen 2= not a citizen   
g13 <- ggplot(data = data, aes(x = citizen, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g14 <- ggplot(data, aes(factor(citizen), log_avgsystolic, fill=citizen)) + 
  geom_boxplot() + labs(x = 'Citizenship Status', y = "Avg Systolic", title = "BoxPlot of Citizenship Status and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g15<- ggplot(data = data, aes(x = citizen, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g16 <- ggplot(data, aes(factor(citizen), log_avgdiastolic, fill=citizen)) + 
  geom_boxplot() + labs(x = 'Citizenship Status', y = "Avg Diastolic", title = "BoxPlot of Citizenship Status and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))



g13
g14
g15
g16

ggarrange(g14, g16, nrow = 1, ncol=2)

#USA_binary = dataCleaned$DMDBORN4, 1 born in US, 2 born in other 
g17 <- ggplot(data = data, aes(x = USA_binary, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g18 <- ggplot(data, aes(factor(USA_binary), log_avgsystolic, fill=USA_binary)) + 
  geom_boxplot() + labs(x = 'Born in USA', y = "Avg Systolic", title = "BoxPlot of Bon in USA and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g19<- ggplot(data = data, aes(x = USA_binary, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g20 <- ggplot(data, aes(factor(USA_binary), log_avgdiastolic, fill=USA_binary)) + 
  geom_boxplot() + labs(x = 'Born in USA', y = "Avg Diastolic", title = "BoxPlot of Born in USA and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))



g17
g18
g19
g20

ggarrange(g18, g20, nrow = 1, ncol=2)

#Week_min10_travel = dataCleaned$PAQ635) 1 is yes, 2 is no, # LOOKS LIKE A NICE ONE, travel to and from places, do you walk or use a bicycle for at elast 10 minutes in a typical week ot get from places - remove 7 and 9 and x (missing)

g21 <- ggplot(data = data, aes(x = Week_min10_travel, y = log_avgsystolic)) +
  geom_point() +
  geom_smooth(method = lm)

g22 <- ggplot(data, aes(factor(Week_min10_travel), log_avgsystolic, fill=Week_min10_travel)) + 
  geom_boxplot() + labs(x = 'Walk/Bike at least once as transport (week)', y = "Avg Systolic", title = "BoxPlot of Walk/Bike as Transport and Avg Systolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))

g23<- ggplot(data = data, aes(x = Week_min10_travel, y = log_avgdiastolic)) +
  geom_point() +
  geom_smooth(method = lm)

g24 <- ggplot(data, aes(factor(Week_min10_travel), log_avgdiastolic, fill=Week_min10_travel)) + 
  geom_boxplot() + labs(x = 'Walk/Bike at least once as transport (week)', y = "Avg Diastolic", title = "BoxPlot of Walk/Bike as Transport and Avg Diastolic") + 
  theme(legend.position = 'none', plot.title = element_text(size = 9, hjust=0.5))



g21
g22
g23
g24

ggarrange(g22, g24, nrow = 1, ncol=2)


#TABLES: 
table(data$sleep_night)
table(data$vigorous_week_binary)
table(data$Race)
table(data$citizen)
table(data$USA_binary)
table(data$Week_min10_travel)

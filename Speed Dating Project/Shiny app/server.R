
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)        
library(reshape2)     
library(ggplot2)       
library(readxl)
library(zipcode)
library(maps)
library(ggmap)

Speed_dating = read.csv("~/Desktop/SpeedDating.csv", na.strings = c("",".","NA"), stringsAsFactors =  FALSE)
Speed_dating[Speed_dating$gender == 0,]$gender = "W"
Speed_dating[Speed_dating$gender == 1,]$gender = "M"

gender_waves = subset(Speed_dating, !duplicated(Speed_dating[, 1])) %>%
  group_by(wave, gender) %>%
  summarise(count = n()) %>%
  melt(id.vars = c("gender", "wave"))

age_waves = subset(Speed_dating, !duplicated(Speed_dating[, 1])) %>% 
  filter(!is.na(age)) %>%
  group_by(wave, gender) %>%
  summarise(count = mean(age)) %>%
  melt(id.vars = c("gender", "wave"))

age_analysis = Speed_dating %>%
  group_by(age, age_o) %>%
  summarise(
    people = n(), 
    matches = sum(match)) %>%
  filter(!is.na(age) & !is.na(age_o))
age_diff = age_analysis %>%
  filter(age - age_o >= 0) %>%
  mutate(years = age - age_o) %>%
  group_by(years) %>%
  summarise(
    matches = sum(matches)) %>%
  arrange(years)

fields_cd = c(
  "Law",
  "Math",
  "Social Science, Psychologist" ,
  "Medical Science, Pharmaceuticals, and Bio Tech",
  "Engineering",
  "English/Creative Writing/ Journalism",
  "History/Religion/Philosophy",
  "Business/Econ/Finance",
  "Education, Academia",
  "Biological Sciences/Chemistry/Physics",
  "Social Work" ,
  "Undergrad/undecided" ,
  "Political Science/International Affairs" ,
  "Film",
  "Fine Arts/Arts Administration",
  "Languages",
  "Architecture",
  "Other"
)

# Create career codes
career_cd = c(
  "Lawyer",
  "Academic/Research", 
  "Psychologist", 
  "Doctor/Medicine",
  "Engineer", 
  "Creative Arts/Entertainment",
  "BankingBusiness/CEO/Admin",
  "Real Estate",
  "International/Humanitarian Affairs",
  "Undecided" ,
  "Social Work",
  "Speech Pathology",
  "Politics",
  "Pro sports/Athletics",
  "Other",
  "Journalism",
  "Architecture"
)

# Find number of men/women on each study field
fields = Speed_dating[!is.na(Speed_dating$field_cd),] %>%
  group_by(gender, field_cd) %>%
  summarise(
    count = n()
  )

# Find number of men/women on each career
careers = Speed_dating[!is.na(Speed_dating$career_c),] %>%
  group_by(gender, career_c) %>%
  summarise(
    count = n()
  )

race_c = c(
  "Black/African American",
  "European/Caucasian-American",
  "Latino/Hispanic American",
  "Asian/Pacific Islander/Asian-American",
  "Native American",
  "Other"
)

# Find number of men/women for each race
races = Speed_dating[!is.na(Speed_dating$race),] %>%
  group_by(gender, race) %>%
  summarise(
    my = n()
  )

match_by_gender = Speed_dating %>%
  group_by(gender) %>%
  summarise(
    matches = sum(match == 1),
    fails = sum(match == 0)) %>% 
  melt(id.vars = "gender")

match_by_waves = Speed_dating[Speed_dating$match == 1,] %>%
  group_by(wave) %>%
  summarise(
    matches = sum(match == 1)
  )

male = Speed_dating[Speed_dating$gender == "M",]
first_col = head(grep("sports", colnames(Speed_dating)),1)
last_col = head(grep("yoga", colnames(Speed_dating)),1)
match_col = head(grep("match", colnames(Speed_dating)),1)
male = male[complete.cases(male[first_col:last_col]),]
combined_male = male %>% group_by(iid) %>% summarise(match_sum = sum(match))
number_male = combined_male %>% group_by(match_sum) %>% summarise(count = n())

female = Speed_dating[Speed_dating$gender == "W",]
first_col = head(grep("sports", colnames(Speed_dating)),1)
last_col = head(grep("yoga", colnames(Speed_dating)),1)
match_col = head(grep("match", colnames(Speed_dating)),1)
female = female[complete.cases(female[first_col:last_col]),]
combined_female = female %>% group_by(iid) %>% summarise(match_sum = sum(match))
number_female = combined_female %>% group_by(match_sum) %>% summarise(count = n())

Speed_dating_combined = Speed_dating[!duplicated(Speed_dating$iid), ]
Speed_dating_combined$zipcode = as.numeric(gsub(",","",Speed_dating_combined$zipcode))
Speed_dating_zip = clean.zipcodes(Speed_dating_combined$zipcode)
data(zipcode)
us = map_data('state')
plot = subset(zipcode, subset = (zip %in% Speed_dating_zip))

men_matches =  male[Speed_dating$match == 1,]
men_feature = men_matches %>%
  group_by(gender) %>%
  summarise(
    sports = sum(sports, na.rm=T),
    tvsports = sum(tvsports, na.rm=T),
    exercise = sum(exercise, na.rm=T),
    dining = sum(dining, na.rm=T),
    museums = sum(museums, na.rm=T),
    art = sum(art, na.rm=T),
    hiking = sum(hiking, na.rm=T),
    gaming = sum(gaming, na.rm=T),
    clubbing = sum(clubbing, na.rm=T),
    reading = sum(reading, na.rm=T),
    tv = sum(tv, na.rm=T),
    music = sum(music, na.rm=T),
    theater = sum(theater, na.rm=T),
    movies = sum(movies, na.rm=T),
    concerts = sum(concerts, na.rm=T),
    shopping = sum(shopping, na.rm=T),
    yoga = sum(yoga, na.rm=T)
  ) %>%
  melt(id_vars = "gender")

women_matches =  female[Speed_dating$match == 1,]
women_feature = women_matches %>%
  group_by(gender) %>%
  summarise(
    sports = sum(sports, na.rm=T),
    tvsports = sum(tvsports, na.rm=T),
    exercise = sum(exercise, na.rm=T),
    dining = sum(dining, na.rm=T),
    museums = sum(museums, na.rm=T),
    art = sum(art, na.rm=T),
    hiking = sum(hiking, na.rm=T),
    gaming = sum(gaming, na.rm=T),
    clubbing = sum(clubbing, na.rm=T),
    reading = sum(reading, na.rm=T),
    tv = sum(tv, na.rm=T),
    music = sum(music, na.rm=T),
    theater = sum(theater, na.rm=T),
    movies = sum(movies, na.rm=T),
    concerts = sum(concerts, na.rm=T),
    shopping = sum(shopping, na.rm=T),
    yoga = sum(yoga, na.rm=T)
  ) %>%
  melt(id_vars = "gender")


shinyServer(function(input, output) {
  active_plot = reactive({
    req(input$select)
    if(input$select == 1 ){
        ggplot(gender_waves, aes(x = wave, y = value, fill = factor(gender))) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_discrete(name = "Gender") +
          xlab("Wave") + ylab("Population") + ggtitle("Gender repartition in waves")
    } else if(input$select == 2){
         ggplot(age_waves, aes(x = wave, y = value, fill = factor(gender))) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_discrete(name = "Gender") +
          xlab("Wave") + ylab("Population") + ggtitle("Age repartition in waves") 
    } else if(input$select == 3){
        ggplot(age_diff[age_diff$years < 20,], aes(x = years, y = matches)) +
          geom_bar(stat = "identity", position = "dodge") +
          xlab("Number of years of difference between people's age") + 
          ylab("Number of matches") + ggtitle("Does age really matter?")
    } else if(input$select == 4){
      ggplot(fields, aes(x = field_cd, y = count, fill = factor(gender))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_discrete(name = "Gender") +
        xlab("Field") + ylab("Count") + ggtitle("Study fields repartition") +
        scale_x_continuous(labels = fields_cd, breaks = 1:18) +
        coord_flip()
    } else if(input$select == 5){
      ggplot(careers, aes(x = career_c, y = count, fill = factor(gender))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_discrete(name = "Gender") +
        xlab("Career") + ylab("Count") + ggtitle("Careers repartition") +
        scale_x_continuous(labels = career_cd, breaks = 1:17) +
        coord_flip()
    } else if(input$select == 6){
      ggplot(races, aes(x = race, y = my, fill = factor(gender))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_discrete(name = "Gender") +
        xlab("Race") + ylab("Count") + ggtitle("Race repartition") +
        scale_x_continuous(labels = race_c, breaks = 1:6) +
        coord_flip()
    } else if(input$select == 7){
      ggplot(match_by_gender, aes(x = variable, y = value, fill = factor(gender))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_discrete(name = "Gender") + ggtitle("Matches by gender") +
        xlab("Result") + ylab("Count")
    } else if(input$select == 8){
      ggplot(match_by_waves, aes(x = wave, y = matches)) +
        geom_bar(stat = "identity", position = "dodge") + ggtitle("Matches by waves") +
        xlab("Wave number") + ylab("Matches")
    } else if(input$select == 9){
      ggplot(number_male, aes(x = match_sum, y = count))+ geom_bar(stat = "identity", position = "dodge", colour = "black") + ggtitle("Number of men per number of matches") + xlab("Number of matches")
    } else if(input$select == 10){
      ggplot(number_female, aes(x = match_sum, y = count))+ geom_bar(stat = "identity", position = "dodge", colour = "black") + ggtitle("Number of women per number of matches") + xlab("Number of matches")
    } else if(input$select == 11){
      ggplot(men_feature[,c(2,3)], aes(x = reorder(variable, -value), y = value)) +
        geom_bar(stat = "identity", position = "dodge", colour="black") +
        xlab("Feature") + ylab("Count") + ggtitle("Importance of a feature for men") +
        coord_flip() 
    } else if(input$select == 12){
      ggplot(women_feature[,c(2,3)], aes(x = reorder(variable, -value), y = value)) +
        geom_bar(stat = "identity", position = "dodge", colour="black") +
        xlab("Feature") + ylab("Count") + ggtitle("Importance of a feature for Women") +
        coord_flip()
    } else if(input$select == 13){
      ggplot(plot, aes(longitude,latitude))+geom_polygon(data=us,aes(x=long,y=lat,group=group),alpha=.35) + geom_point(size = 1.3, colour="#0072B2", alpha = .25) + xlim(-125,-65)+ylim(20,50) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                                                                                                                                    panel.grid.minor = element_blank(), axis.line = element_blank(), axis.text = element_blank(), axis.title = element_blank(),axis.ticks = element_blank(),plot.title = element_text(hjust = 0.5)) + ggtitle("Location of Participants")
    }
  })
  output$active_plot = renderPlot({active_plot()})
})


  

library(ggplot2)
library(zoo)
library(lubridate)
library(reshape2)
library(scales)
library(ggthemes)

setwd("~/Developer/2022-life-booleans")

#######################################
# READ / CLEAN/ FORMAT DATA
#######################################

data = read.csv("data.csv")

data[data==0] = "False"
data[data==1] = "True"
data[data==""] = "False"
data[data=="o"] = "False"
data[data=="l"] = "False"
data[is.na(data)] = "False"

data$date = as.Date(data$X.1, format = "%m/%d/%Y")
data$week = week(data$date)
data$month = format(data$date,"%B")
data$month = factor(data$month, list(
  "January", "February", "March", 
  "April", "May", "June", 
  "July", "August", "September", 
  "October", "November", "December"
))
data$yearmonth = as.yearmon(data$date)
data$yearmonthf = factor(data$yearmonth)
data$day_of_week = factor(data$X, list(
  "Sun", 
  "Mon", 
  "Tue", 
  "Wed", 
  "Thu", 
  "Fri", 
  "Sat"
))

# calculate week of month, where sunday is new week
weekofmonth = rep(0, length(data$date))
weekNum = 1
currentMonth = 1
for (i in (1:length(data$date))) {
  date = data$date[i]
  if (weekdays(date) == "Sunday") {
    weekNum = weekNum + 1
  }
  if (month(date) > currentMonth) {
    currentMonth = currentMonth + 1
    weekNum = 1
  }
  weekofmonth[i] = weekNum
}
data$monthweek = weekofmonth
data$monthweek = factor(data$monthweek, list(6, 5, 4, 3, 2, 1))

#######################################
# DEFINE VARIABLES LABELS
#######################################

variable_labels = c(
  `felt.enough.sleep` = "Enough\nSleep",
  `slept.at.home` = "Slept\nat Home",
  `shower` = "Shower",
  `wash.hair` = "Wash Hair", # NEW IN 2022
  `toothbrush.morning` = "Brush Teeth Morning",
  `toothbrush.night` = "Brush Teeth Night",
  `poop` = "Poop",
  `breakfast` = "Break-\nfast",
  `lunch` = "Lunch",
  `dinner` = "Dinner",
  `period` = "Period",
  `ate.at.home` = "Eat In", # NEW IN 2022
  `cooked` = "Cooked", # NEW IN 2022
  `ate.at.restaurant` = "Eat Out", # NEW IN 2022
  `deliver...pick.up.food` = "Delivery", # NEW IN 2022
  `midnight.snack` = "Midnight\nSnack", # NEW IN 2022
  `birth.control` = "Birth Control",
  `anti.depressant` = "Anti-Depressant",
  `vitamins` = "Vitamins",
  `advil` = "Advil",
  `excederin` = "Excederin",
  `adderall` = "Adderall",
  `caffeine` = "Caffeine",
  `ambien` = "Ambien",
  `alcohol` = "Alcohol",
  `weed` = "Weed",
  `chores` = "Chores",
  `personal.coding` = "Personal\nCoding",
  `went.to.work` = "Went to\nWork",
  `worked.remotely` = "WFH",
  `sad.about.work` = "Sad about Work",
  `happy.about.work` = "Happy about Work",
  `productive.at.work` = "Productive at Work",
  `happy` = "Happy",
  `sad` = "Sad",
  `angry` = "Angry",
  `stressed.or.anxious` = "Felt\nStressed",
  `annoyed` = "Annoyed",
  `journaled` = "Journaled",
  `cried` = "Cried",
  `happy.about.relationship` = "Happy about Relationship",
  `sad.about.relationship` = "Sad about Relationship",
  `sex` = "Sex",
  `abandoned.by.sean` = "Abandoned",
  `had.own.activities..not.at.home.` = "Own Activities",
  `upset.at.sean` = "Upset at Sean",
  `sean.cigar` = "Sean Cigar", # NEW IN 2022
  `sean.drunk` = "Sean Drunk",
  `sean.alcohol` = "Sean Alcohol",
  
  `January` = "Jan", 
  `February` = "Feb", 
  `March` = "Mar", 
  `April` = "Apr", 
  `May` = "May", 
  `June` = "Jun", 
  `July` = "Jul", 
  `August` = "Aug", 
  `September` = "Sep", 
  `October` = "Oct", 
  `November` = "Nov", 
  `December` = "Dec"
)

#######################################
# DEFINE DATA SUBSETS & PLOT
#######################################

#--------------------------------------
# All
#--------------------------------------

data.all_sfw = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "felt.enough.sleep" = data$felt.enough.sleep,
  "slept.at.home" = data$slept.at.home,
  "shower" = data$shower,
  "wash.hair" = data$wash.hair,
  "toothbrush.morning" = data$toothbrush.morning,
  "toothbrush.night" = data$toothbrush.night,
  "poop" = data$poop,
  
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "midnight.snack" = data$midnight.snack,
  "ate.at.home" = data$ate.at.home,
  "cooked" = data$cooked,
  "deliver...pick.up.food" = data$deliver...pick.up.food,
  
  "vitamins" = data$vitamins,
  "caffeine" = data$caffeine,
  "adderall" = data$adderall,
  "alcohol"= data$alcohol,
  "weed"= data$weed,
  "ambien"= data$ambien,
  
  "chores" = data$chores,
  "personal.coding" = data$personal.coding,
  
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,
  "happy.about.work" = data$happy.about.work,
  "sad.about.work" = data$sad.about.work,
  "felt.work.stress" = data$felt.work.stress,
  "productive.at.work" = data$productive.at.work,
  
  "happy" = data$happy,
  "sad" = data$sad,
  "angry" = data$angry,
  "annoyed" = data$annoyed,
  "cried" = data$cried,
  "journaled" = data$journaled,
  "stressed.or.anxious" = data$stressed.or.anxious,
  
  "abandoned.by.sean" = data$abandoned.by.sean,
  "had.own.activities..not.at.home." = data$had.own.activities..not.at.home.,
  "upset.at.sean" = data$upset.at.sean,
  "sean.alcohol" = data$sean.alcohol,
  "sean.cigar" = data$sean.cigar,
  "sean.drunk" = data$sean.drunk,
  "happy.about.relationship" = data$happy.about.relationship,
  "sad.about.relationship" = data$sad.about.relationship,
  "sex" = data$sex
)

melt_and_plot(data.all_sfw, "2022 Daily Life Attributes")

#--------------------------------------
# Hygiene
#--------------------------------------

data.hygiene = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,

  "felt.enough.sleep" = data$felt.enough.sleep,
  "slept.at.home" = data$slept.at.home,
  "shower" = data$shower,
  "wash.hair" = data$wash.hair,
  "toothbrush.morning" = data$toothbrush.morning,
  "toothbrush.night" = data$toothbrush.night,
  "poop" = data$poop,
  "period" = data$period
)

melt_and_plot(data.hygiene, "2022 Daily Hygiene")

#--------------------------------------
# Eating
#--------------------------------------

data.food = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "midnight.snack" = data$midnight.snack,
  "ate.at.home" = data$ate.at.home,
  "cooked" = data$cooked,
  "deliver...pick.up.food" = data$deliver...pick.up.food
)

melt_and_plot(data.food, "2022 Daily Food")

#--------------------------------------
# Drugs
#--------------------------------------

data.drugs = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "vitamins" = data$vitamins,
  "caffeine" = data$caffeine,
  "adderall" = data$adderall,
  "alcohol"= data$alcohol,
  "weed"= data$weed,
  "ambien"= data$ambien
)

melt_and_plot(data.drugs, "2022 Daily Drugs")

#--------------------------------------
# Productivity
#--------------------------------------

data.productivity = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "chores" = data$chores,
  "personal.coding" = data$personal.coding
)

melt_and_plot(data.productivity, "2022 Daily Productivity")

#--------------------------------------
# Work
#--------------------------------------

data.work = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,
  "happy.about.work" = data$happy.about.work,
  "sad.about.work" = data$sad.about.work,
  "felt.work.stress" = data$felt.work.stress,
  "productive.at.work" = data$productive.at.work
)

melt_and_plot(data.work, "2022 Daily Work")

#--------------------------------------
# Feelings 
#--------------------------------------

data.feelings = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "happy" = data$happy,
  "sad" = data$sad,
  "angry" = data$angry,
  "annoyed" = data$annoyed,
  "cried" = data$cried,
  "journaled" = data$journaled,
  "stressed.or.anxious" = data$stressed.or.anxious
)

melt_and_plot(data.feelings, "2022 Daily Emotions")


#--------------------------------------
# Sean 
#--------------------------------------

data.sean = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "abandoned.by.sean" = data$abandoned.by.sean,
  "had.own.activities..not.at.home." = data$had.own.activities..not.at.home.,
  "upset.at.sean" = data$upset.at.sean,
  "sean.alcohol" = data$sean.alcohol,
  "sean.cigar" = data$sean.cigar,
  "sean.drunk" = data$sean.drunk,
  "happy.about.relationship" = data$happy.about.relationship,
  "sad.about.relationship" = data$sad.about.relationship,
  "sex" = data$sex
)

melt_and_plot(data.sean, "2022 Relationships & Sean")

#--------------------------------------
# FOR REDDIT 
#--------------------------------------

data.reddit = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "slept.at.home" = data$slept.at.home,
  "shower" = data$shower,
  "poop" = data$poop,
  
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "midnight.snack" = data$midnight.snack,
  "cooked" = data$cooked,
  "deliver...pick.up.food" = data$deliver...pick.up.food,
  
  "caffeine" = data$caffeine,
  "adderall" = data$adderall,
  "alcohol"= data$alcohol,
  "weed"= data$weed,
  "ambien"= data$ambien,
  
  "personal.coding" = data$personal.coding,
  
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,

  "cried" = data$cried
)

melt_and_plot(data.reddit, "2022: Life in Booleans")

#--------------------------------------
# FOR INSTAGRAM 
#--------------------------------------

data.instagram = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
)

melt_and_plot(data.instagram, "2022: Life in Booleans")

#######################################
# HELPER FUNCTION: melt_and_plot()
#######################################

melt_and_plot = function(data, title) {
  melt = melt(
    data = data, 
    id = names(data)[1:4])
  
  ggplot(
    melt, 
    aes(
      day_of_week, 
      monthweek, 
      fill = as.factor(value)
    )
  ) + 
    coord_equal(ratio = 1) + 
    geom_tile(color = "white") + 
    facet_grid(
      variable ~ month, 
      switch = "y", 
      space = "free", 
      labeller = as_labeller(variable_labels)
    ) +
    labs(
      y = "",
      x = "",
      title = title,
      fill = "Legend",
    ) + 
    theme(
      text = element_text(family = "mono", face = "bold", size = 12, color = "white"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(), 
      axis.ticks = element_blank(),
      plot.subtitle = element_blank(),
      plot.title = element_text(size = 20, face = "bold", margin = margin(t = 10, b = 10)),
      legend.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black")
    )
}


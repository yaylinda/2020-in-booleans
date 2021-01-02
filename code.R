library(ggplot2)
library(zoo)
library(reshape2)

setwd("~/Developer/2020-in-booleans")

data = read.csv("raw.csv")
data$date = as.Date(data$X.1, format = "%m/%d/%y")
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
  "Sunday", 
  "Monday", 
  "Tuesday", 
  "Wednesday", 
  "Thursday", 
  "Friday", 
  "Saturday"
))

data[data==0] = "False"
data[data==1] = "True"

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

#==========================================================
# Variable labels
#==========================================================

variable_labels = c(
  `felt.enough.sleep` = "Enough\nSleep",
  `slept.at.home` = "Slept at Home",
  `shower` = "Shower",
  `toothbrush.morning` = "Brush Teeth Morning",
  `toothbrush.night` = "Brush Teeth Night",
  `poop` = "Poop",
  `breakfast` = "Break-\nfast",
  `lunch` = "Lunch",
  `dinner` = "Dinner",
  `period` = "Period",
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
  `feel.sad.about.leaving.C1` = "Sad about Leaving C1",
  `feel.happy.about.leaving.C1` = "Happy about Leaving C1",
  `went.to.work` = "Went to\nWork",
  `happy.about.work` = "Happy about Work",
  `productive.at.work` = "Productive at Work",
  `social.with.my.friends..non.work` = "",
  `interaction.with.my.family` = "",
  `happy` = "Happy",
  `angry` = "Angry",
  `Annoyed` = "Annoyed",
  `cried` = "Cried",
  `sad.about.relationship` = "Sad about Relationship",
  `abandoned.by.sean` = "Abandoned",
  `upset.at.sean` = "Upset at Sean",
  `sean.drunk` = "Sean Drunk",
  `worked.remotely` = "Worked\nRemotely",
  `sad.about.work` = "Sad about Work",
  `social.with.my.friends..work` = "",
  `social.with.sean.s.friends` = "",
  `interaction.with.sean.s.family` = "",
  `sad` = "Sad",
  `stressed.or.anxious` = "Felt\nStressed",
  `Journaled` = "Journaled",
  `happy.about.relationship` = "Happy about Relationship",
  `sex` = "Sex",
  `had.own.activities..other.than.work` = "",
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

#==========================================================
# Helper function to melt and plot data
#==========================================================

melt_and_plot = function(data, subtitle) {
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
  geom_tile(color = "white") + 
  facet_grid(
    variable ~ month, 
    switch = "y", 
    space = "free", 
    labeller = as_labeller(variable_labels)
  ) +
  coord_equal(ratio = 1) + 
  labs(
    y = "",
    x = "",
    title = "2020* in Booleans",
    subtitle = subtitle,
    fill = "Legend",
    caption = "* Forgot to record data for a couple days in mid-November, and got too depressed to keep going for the rest of the year :("
  ) + 
  theme(
    text = element_text(family = "mono"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    plot.title = element_text(size = 60, face = "bold"),
    plot.subtitle = element_text(size = 30, margin = margin(t = 20, b = 40)),
    plot.caption = element_text(size = 25, face = "bold", margin = margin(t = 40, b = 20)),
    strip.text.x = element_text(size = 25, face = "bold"),
    strip.text.y = element_text(size = 25, face = "bold"),
    legend.title = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 25),
    legend.box.margin = margin(l = 40),
    plot.margin = margin(t = 40, r = 40, b = 20, l = 40)
  )
}

#==========================================================
# Plots with data columns subset
#==========================================================

#------------------
# Main
#------------------

data.main = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "felt.enough.sleep" = data$felt.enough.sleep,
  "shower" = data$shower,
  "poop" = data$poop,
  "breakfast" = data$breakfast,
  "lunch" = data$lunch,
  "dinner" = data$dinner,
  "caffeine" = data$caffeine,
  "alcohol"= data$alcohol,
  "weed" = data$weed,
  "chores" = data$chores,
  "personal.coding" = data$personal.coding,
  "went.to.work" = data$went.to.work,
  "worked.remotely" = data$worked.remotely,
  "stressed.or.anxious" = data$stressed.or.anxious,
  "sex" = data$sex
)

melt_and_plot(data.main, "Daily life attributes of a 26 year-old Asian female, living in the US")

#------------------
# Work
#------------------

data.work = data.frame(
  "date" = data$date, 
  "month" = data$month, 
  "day_of_week" = data$day_of_week, 
  "monthweek" = data$monthweek,
  
  "went.to.work" = data$went.to.work,
  "happy.about.work" = data$happy.about.work,
  "sad.about.work" = data$sad.about.work,
  "productive.at.work" = data$productive.at.work,
  "worked.remotely" = data$worked.remotely
)

melt_and_plot(data.work, "Work")

















if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(olsrr)) install.packages("RCurl", repos = "http://cran.us.r-project.org")
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org")

#This script was made on R 3.6.2
#Download the data from my github repo 
dl <-getURL("https://raw.githubusercontent.com/lifnr/video_games_machine_learning/master/Video_Games_Sales_as_at_22_Dec_2016.csv")
Video_Games<-read.csv(text = dl)
head(Video_Games)
vg<-Video_Games


#Checking the first entries of the dataset
head(vg)
#Checking the structure of the data
str(vg)
#Checking the dimension of the data
dim(vg)
#Number of NA in the dataset
sum(is.na(vg))

#Changing the year into integers and user score as numeric
vg<-vg%>%mutate(Year_of_Release=as.integer(as.character(Year_of_Release)), User_Score=as.numeric(as.character(User_Score)))

##Analyse

#Plot of sales per year
slperYear<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(Year_of_Release, Sales))+
  geom_col()

#Plot of number of games per year
gameperYear<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Games_per_Year=n())%>%
  ggplot(aes(Year_of_Release, Games_per_Year))+
  geom_col()

plot_grid(slperYear, gameperYear, labels = "AUTO")



#Sales per region
#Plot of sales EU per year
pl_EU<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Sales= sum(EU_Sales))%>%
  ggplot(aes(Year_of_Release, Sales))+
  geom_col()+
  ylab("Sales EU")

#Plot of sales Jap per year
pl_Jap<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Sales= sum(JP_Sales))%>%
  ggplot(aes(Year_of_Release, Sales))+
  geom_col()+
  ylab("Sales Japan")

#Plot of sales NA per year
pl_NA<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Sales= sum(NA_Sales))%>%
  ggplot(aes(Year_of_Release, Sales))+
  geom_col()+
  ylab("Sales NA")

#Plot of sales OTher per year
pl_Ot<-vg%>%filter(Year_of_Release<=2016)%>%
  group_by(Year_of_Release)%>%
  summarize(Sales= sum(Other_Sales))%>%
  ggplot(aes(Year_of_Release, Sales))+
  geom_col()+
  ylab("Sales Other")

#The four region together
plot_grid(pl_EU, pl_NA, pl_Jap,pl_Ot, labels = "AUTO")



#Plot of sales per genre
vg%>%
  group_by(Genre)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(x=reorder(Genre, -Sales), Sales))+
  geom_col()+
  xlab("Genre")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

#Plot of sales per plateform
vg%>%
  group_by(Platform)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(x=reorder(Platform, -Sales), Sales))+
  geom_col()+
  xlab("Platform")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 


#Grouping by gaming system and plotting it
nes <- c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sony <- c("PS","PS2","PSP","PS3","PS4","PSV")
sega <- c("GEN","SCD","DC","GG","SAT")
xbox <- c("XB","X360", "XOne")
other <- c("2600","3DO","NG","PCFX","TG16","WS")
pc <- c("PC")

vg$System[vg$Platform %in% nes] <- "Nintendo"
vg$System[vg$Platform %in% sony] <- "Sony"
vg$System[vg$Platform %in% xbox] <- "XBox"
vg$System[vg$Platform %in% sega] <- "Sega"
vg$System[vg$Platform %in% pc] <- "PC"
vg$System[vg$Platform %in% other] <- "Other"

vg%>% group_by(System)%>% 
  summarise(Sales = sum(Global_Sales))%>%
  ggplot(aes(x=reorder(System, -Sales), Sales))+
  geom_bar(stat = "identity")+
  xlab("Gaming system")


#Publisher with most sales
vg %>% group_by(Publisher) %>%
  summarize(count = sum(Global_Sales)) %>%
  arrange(desc(count))

#Games with most sales
vg %>% group_by(Name) %>%
  summarize(count = sum(Global_Sales)) %>%
  arrange(desc(count))


#Removing NA from the set and keeping only variable we are going to use
vg_short <- vg%>%filter(vg$Year_of_Release<=2016)%>%
  select(Global_Sales,Name,Year_of_Release,Publisher,Genre, Critic_Score, Critic_Count,User_Score,User_Count,System)%>%
  na.omit(vg) 
sum(is.na(vg_short))
dim(vg_short)



#we look at the impact of critic rating
cs<-vg_short%>%
  group_by(Critic_Score)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(Critic_Score, Sales))+
  geom_col()+
  scale_x_continuous()

#Sales vs critic_count
cc<-vg_short%>%
  group_by(Critic_Count)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(Critic_Count, Sales))+
  geom_col()+
  scale_x_continuous()

plot_grid(cs, cc, labels = "AUTO")

#user count impact on sales
us<-vg_short%>%
  group_by(User_Score)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(User_Score, Sales))+
  geom_col()


#Sales vs user_count
uc<-vg_short%>%
  group_by(User_Count)%>%
  filter(User_Count<200)%>%
  summarize(Sales= sum(Global_Sales))%>%
  ggplot(aes(User_Count, Sales))+
  geom_col()+
  scale_x_continuous()

plot_grid(us, uc, labels = "AUTO")

###
#Modelling
####

#Creating a test_set and a training set

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = vg_short$Global_Sales, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- vg_short[-test_index,]
test_set <- vg_short[test_index,]

#Subset selection
mod <- lm(Global_Sales~Year_of_Release+Genre+Critic_Score+ Critic_Count+User_Score+User_Count+System, data=train_set)
pred<-ols_step_best_subset(mod)
pred

# pred2<-ols_step_both_p(mod)
# pred2

#LM Model 
#with 2 variables
fit <- train(Global_Sales~Critic_Count+User_Count, data=train_set, method="lm")
trn <- predict(fit,train_set)
RMSE(trn, test_set$Global_Sales)

fit$results['RMSE']


#with 5 variables
fit2 <- train(Global_Sales~Critic_Score+Critic_Count+User_Score+User_Count+System, data=train_set, method="lm")
trn2<- predict(fit2,train_set)
RMSE(trn2, test_set$Global_Sales)

fit2$results['RMSE']


#GLM Model
glm_fit <- train(Global_Sales~Critic_Score+Critic_Count+User_Score+User_Count+System,
             data=train_set, method="glm")
trn_glm <- predict(glm_fit,train_set)
RMSE(trn_glm, test_set$Global_Sales) 

glm_fit$results['RMSE']


#Random forest
rf_fit <- train(Global_Sales~Critic_Score+Critic_Count+User_Score+User_Count+System,
                 data=train_set, tuneLength=2, method="rf")
trn_rf <- predict(rf_fit,train_set)
RMSE(trn_rf, test_set$Global_Sales) 

rf_fit$results[row.names(rf_fit$bestTune),'RMSE']


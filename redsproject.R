# purpose: Cincinnati Reds Take Home Project



# load packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(rpart)
library(rpart.plot)
library(BART)
library(tidytreatment)

# load the data -----------------------------------------------------------

library(readxl)
data <- read_excel("C:/Users/jacki/Desktop/senior/2023 Analytics Internship 
                   Problem Dataset.xlsx")
View(data)

# get basic summary statistics
skim(data)

# filter to the pitchers of interest (A and B)
pitchersAB <- data %>% 
  filter(PITCHER_KEY %in% c("A", "B"))

# add variable to indicate if INDUCED_VERTICAL_BREAK is missing
added <- pitchersAB %>% 
  mutate(missing_vb = case_when(is.na(INDUCED_VERTICAL_BREAK) ~ "missing",
                                TRUE ~ "present"))

# make INDUCED_VERTICAL_BREAK numeric
added$INDUCED_VERTICAL_BREAK <- as.numeric(added$INDUCED_VERTICAL_BREAK)

# explore the data --------------------------------------------------------


# figure out how many of the pitches are missing INDUCED_VERTICAL_BREAK
pitchersAB %>% filter(is.na(INDUCED_VERTICAL_BREAK)) %>% View()
pitchersAB %>% filter(is.na(INDUCED_VERTICAL_BREAK)) %>% 
  group_by(PITCHER_KEY) %>% summarize(n())

# number of pitches by each pitcher
pitchersAB %>% 
  group_by(PITCHER_KEY) %>% 
  ggplot(aes(x = PITCHER_KEY)) + geom_bar(fill = c("red", "firebrick4")) + 
  theme_bw()
added %>% 
  group_by(PITCHER_KEY, missing_vb) %>% 
  ggplot(aes(x = PITCHER_KEY, fill = factor(missing_vb))) + 
  geom_bar(position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", colour = "white",
            vjust = 1.5, position = position_dodge((.9))) +
  labs(title = "Pitch Counts for Pitchers A and B", 
       x = "Pitcher", y = "Count", 
       fill = "Induced Vertical Break") +
  theme_bw()

# create individual data frame for both to find means
Apitcher <- added %>% filter(PITCHER_KEY == "A")
Bpitcher <- added %>% filter(PITCHER_KEY == "B")
summary(Apitcher)
summary(Bpitcher)

# make copy of data and convert character variables to numeric numeric
data2 <- data
data2$SPIN_DIRECTION <- as.numeric(data2$SPIN_DIRECTION)
data2$HORIZONTAL_BREAK <- as.numeric(data2$INDUCED_VERTICAL_BREAK)
data2$INDUCED_VERTICAL_BREAK <- as.numeric(data2$INDUCED_VERTICAL_BREAK)

# machine learning --------------------------------------------------------

# filter to data to be predicted (missing INDUCED_VERTICAL_BREAK)
filtered <- added %>% 
  filter(is.na(INDUCED_VERTICAL_BREAK))

# create regression tree model
init_tree <- rpart(formula = INDUCED_VERTICAL_BREAK ~ RELEASE_SPEED +
                     PLATE_SPEED + SPIN_RATE + PLATE_Z + RELEASE_HEIGHT +
                     HORIZONTAL_APPROACH_ANGLE, data = data2, 
                   method = "anova")
init_tree
predictions <- predict(init_tree, newdata = filtered)
# convert predictions to data frame
predictions_df <- as.data.frame(predictions) ### THIS WORKS!!!

# create data frame without INDUCED_VERTICAL_BREAK variable
no_ivb <- subset(filtered, select = -INDUCED_VERTICAL_BREAK)

# add predictions back to data
data_with_pred <- cbind(no_ivb, predictions_df)

# rename column to fit with others
data_with_pred <- rename(data_with_pred, PREDICTED_IVB = predictions)


data_with_pred %>% 
  group_by(PITCHER_KEY) %>% 
  ggplot(aes(x = SPIN_RATE, y = PREDICTED_IVB, color = PITCHER_KEY)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "black") +
  # geom_line() +
  labs(x = "Spin Rate", y = "Induced Vertical Break", color = "Pitcher") +
  theme_bw()

added %>% 
  group_by(PITCHER_KEY) %>% 
  ggplot(aes(x = PLATE_SPEED, y = INDUCED_VERTICAL_BREAK, color = PITCHER_KEY)) +
  geom_point(alpha = 0.5) + geom_line() + theme_bw()
added %>% 
  ggplot(aes(x = INDUCED_VERTICAL_BREAK)) +
  geom_histogram(color = "white") +
  facet_wrap(~PITCHER_KEY)

breaks <- data_with_pred
write_csv(breaks, "breaks.csv")

### IGNORE
### morning attempt - following lecture 19
pred <- added %>% 
  mutate(num_vb = as.numeric(missing_vb == "present")) %>% 
  filter(!is.na(VERTICAL_APPROACH_ANGLE), !is.na(RELEASE_SPEED),
         !is.na(num_vb))
table(pred$num_vb)

#
pred %>% 
  ggplot(aes(x = VERTICAL_APPROACH_ANGLE, y = RELEASE_SPEED,
             color = as.factor(num_vb))) +
  geom_point(alpha = 0.5) + theme_bw()

caret_tree <- train(as.factor(num_vb) ~ VERTICAL_APPROACH_ANGLE + RELEASE_SPEED,
                       data = pred, method = "rpart",
                       trControl = trainControl(method = "cv", number = 10),
                       tuneLength = 20)
ggplot(caret_tree) + theme_bw()
rpart.plot(caret_tree$finalModel)
### IGNORE


# BART --------------------------------------------------------------------
# don't need this section

try <- data %>% 
  filter(!is.na(INDUCED_VERTICAL_BREAK)) %>% 
  janitor::clean_names()
var_select_bart <- wbart(x.train = dplyr::select(try,
                                                 RELEASE_SPEED, PLATE_SPEED,
                                                 SPIN_RATE, PLATE_Z,
                                                 RELEASE_HEIGHT,
                                                 HORIZONTAL_APPROACH_ANGLE),
                         y.train = pull(try, INDUCED_VERTICAL_BREAK),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000)

var_select_bart <- wbart(x.train = dplyr::select(try, release_speed, plate_speed,
                                                 spin_rate, plate_z,
                                                 release_height,
                                                 horizontal_approach_angle),
                         y.train = pull(try, induced_vertical_break),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000)
var_select_bart <- wbart(x.train = dplyr::select(try, release_speed, 
                                                 plate_speed),
                         y.train = pull(try, induced_vertical_break),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000)
var_select_bart <- wbart(x.train = dplyr::select(data,
                                                 RELEASE_SPEED, PLATE_SPEED),
                         y.train = pull(data, ),
                         sparse = TRUE,
                         nskip = 2000,
                         ndpost = 5000)


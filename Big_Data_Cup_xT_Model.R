#Big Data Cup xTT Model
#Avery Ellis and Matt Hurley
#2/16/2021

#Quick Notes on xTT
#From Soccer: Singh [2019]
#Location-based Markov model that tries to describe value of any point on the pitch [this time: ice]
#Assumes 3 possible actions from any location: move, pass, shoot
#Uses naive location-based value for shot
#Uses Markov transition matrix to iteratively determine value of pass/move based on chances of moving to more valuable location
#Sportlogiq [2020]: what I'm calling xTT
#Same as soccer model, but added in term for turnover
#Giveaway is simply the inverse; i.e a giveaway's negative is equivalent to the gain an opponent would get by being there
#So for given (i, j) on the rink:
#xTT(i,j) = (p(Score at i,j) * p(Shoot from i,j))+ 
#p(skate or pass from i, j) * sum over all other zones of (p(passing or skating to a zone) * xTT(that zone)) -
#p(lose the puck from i, j) * sum over all other zones of (p(failing a pass to a zone) * xTT(that zone))
#A player's xTT is defined as the sum of all xT they create in individual actions (positive and negative)

#Why Is This Useful?
#Already done for NHL (SportLogiq), so why implement it here?
#Evaluate where prospects are above-average in creating threat--i.e. do they pass to valuable places? do they move to valuable places?
#While certainly not expected in NHL immediately (for most), can be a useful gauge relative to NHL team's xT
#do they create threat by making useful passes or valuable shots?
#If future data becomes available, tracking change in threat over leagues/time could be HUGE
#Attempts to quantify "hockey IQ" --i.e. how often does this player choose to make a play that adds value?
#Compare general OHL to NHL also valuable--understanding how different areas have different threat levels at different competition levels
#The model itself isn't necessarily new, but using it as a tool to evaluate prospects is
#Also--maybe use k-means to cluster threat generation?
#i.e. value added via passing, carrying, etc to characterize playstyles
#And definitely more applications--gotta keep thinking

#Maybe when finished running the model compare results to the person on Twitter running VAEP? Curious to see what results he gets
#idk even if the dataset is large enough to run ML, it's only 40 games so I don't think so but would be a useful comparison




#PART 1: SETUP
#(libraries I'll use, downloading data, subsetting data into what we'll use, etc.)

library(tidyverse)
library(ggforce)
'%nin%' <- Negate('%in%')

full_scouting_dataset <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
scouting_dataset <- full_scouting_dataset %>%
  subset({{Home.Team.Skaters == 5} & {Away.Team.Skaters == 5}})
scouting_dataset$X.Coordinate <- as.character(scouting_dataset$X.Coordinate)
scouting_dataset$Y.Coordinate <- as.character(scouting_dataset$Y.Coordinate)

model_events <- scouting_dataset %>%
  subset({Event == "Shot" | Event == "Play" | Event == "Goal" | Event == "Takeaway"})
  # subset({Event == "Play" | Event == "Incomplete Play" | Event == "Shot" | Event == "Goal" | Event == "Takeaway"})
#this pulls all the data from the scouting dataset and then subsets it to all our events we use for the xT model

#Below times how long it takes to make the carries
start_time <- Sys.time()
#Creating "Carries" --i.e. moving with the puck, based on the event data given
for (row in 1:nrow(scouting_dataset)) {
  if ({scouting_dataset[row, ]$Event %in% c("Puck Recovery", "Takeaway")} & 
      {scouting_dataset[(row + 1), ]$Event %in% c("Play", "Incomplete Play", "Dump In/Out", "Shot", "Goal")} &
      {scouting_dataset[row, ]$Player == scouting_dataset[(row + 1), ]$Player}) {
    model_events <- model_events %>%
      rbind(c(scouting_dataset[row, "game_date"], scouting_dataset[row, "Home.Team"], scouting_dataset[row, "Away.Team"],
      scouting_dataset[row, "Period"], scouting_dataset[row, "Clock"],
      scouting_dataset[row, "Home.Team.Skaters"], scouting_dataset[row, "Away.Team.Skaters"], 
      scouting_dataset[row, "Home.Team.Goals"], scouting_dataset[row, "Away.Team.Goals"], scouting_dataset[row, "Team"],
      scouting_dataset[row, "Player"], "Carry", 
      scouting_dataset[row, "X.Coordinate"], scouting_dataset[row, "Y.Coordinate"],
      "", "", "", "", "", scouting_dataset[(row + 1), "X.Coordinate"], scouting_dataset[(row + 1), "Y.Coordinate"])
      )
  }
}
end_time <- Sys.time()
print(end_time-start_time)

#these loops do the same thing--checking to see where consecutive events happen and then creating a "carry" event
#based on the location difference
#NOTE: this is pretty naive and assumes moving in a straight line
#with tracking data, could evaluate carries that aren't straight (which is almost all of them)
for (row in (1:nrow(scouting_dataset))) {
  if ({scouting_dataset[row, ]$Event == "Play"} &
      {scouting_dataset[(row + 1), ]$Event %in% c("Play", "Incomplete Play", "Takeaway", "Puck Recovery", "Dump In/Out", "Shot", "Goal")} &
      {scouting_dataset[row, ]$Player.2 == scouting_dataset[(row + 1), ]$Player}) {
    model_events <- model_events %>%
        rbind(c(scouting_dataset[row, ]$game_date, scouting_dataset[row, ]$Home.Team, scouting_dataset[row, ]$Away.Team,
        scouting_dataset[row, ]$Period, scouting_dataset[row, ]$Clock,
        scouting_dataset[row, ]$Home.Team.Skaters, scouting_dataset[row, ]$Away.Team.Skaters, 
        scouting_dataset[row, ]$Home.Team.Goals, scouting_dataset[row, ]$Away.Team.Goals, scouting_dataset[row, ]$Team,
        scouting_dataset[row, ]$Player, "Carry", 
        as.character(scouting_dataset[row, ]$X.Coordinate.2), as.character(scouting_dataset[row, ]$Y.Coordinate.2),
        "", "", "", "", "", scouting_dataset[(row + 1), ]$X.Coordinate, scouting_dataset[(row + 1), ]$Y.Coordinate)
        )
    }
}

#One more thing we gotta make based on data inferences: failed passes.
#Makes calculating the negative Markov transition matrix a lot easier.

for (row in (1:nrow(scouting_dataset))) {
  if ({scouting_dataset[row, ]$Event == "Incomplete Play"} &
      {scouting_dataset[(row + 1), ]$Event == "Puck Recovery"} &
      {scouting_dataset[row, ]$Team != scouting_dataset[(row + 1), ]$Team}) {
    model_events <- model_events %>%
      rbind(c(scouting_dataset[row, ]$game_date, scouting_dataset[row, ]$Home.Team, scouting_dataset[row, ]$Away.Team,
              scouting_dataset[row, ]$Period, scouting_dataset[row, ]$Clock,
              scouting_dataset[row, ]$Home.Team.Skaters, scouting_dataset[row, ]$Away.Team.Skaters, 
              scouting_dataset[row, ]$Home.Team.Goals, scouting_dataset[row, ]$Away.Team.Goals, scouting_dataset[row, ]$Team,
              scouting_dataset[row, ]$Player, "Failed Play", 
              scouting_dataset[row, ]$X.Coordinate, scouting_dataset[row, ]$Y.Coordinate, "", "", "", "", 
              scouting_dataset[(row + 1), ]$Player, 
              scouting_dataset[(row + 1), ]$X.Coordinate, scouting_dataset[(row + 1), ]$Y.Coordinate)
      )
  }
}

model_events$X.Coordinate <- as.numeric(model_events$X.Coordinate)
model_events$X.Coordinate.2 <- as.numeric(model_events$X.Coordinate.2)
model_events$Y.Coordinate <- as.numeric(model_events$Y.Coordinate)
model_events$Y.Coordinate.2 <- as.numeric(model_events$Y.Coordinate.2)


#PART 2: INITIAL PLOTTING
#just to visualize where everything happens

model_events_plotting <- model_events
model_events_plotting$Event = as.factor(model_events_plotting$Event)
model_events_plotting$X.Coordinate = (model_events_plotting$X.Coordinate - 100)
model_events_plotting$X.Coordinate.2 = (model_events_plotting$X.Coordinate.2 - 100)
model_events_plotting$Y.Coordinate = (model_events_plotting$Y.Coordinate - 42.5)
model_events_plotting$Y.Coordinate.2 = (model_events_plotting$Y.Coordinate.2 - 42.5)
#makes the original data look nice to plot it b/c otherwise it wouldn't fit on the rink plot for some reason

all_model_events_viz <-
  nhl_rink_plot()+
  theme_void()+
  geom_jitter(data = model_events_plotting, aes(x = X.Coordinate, y = Y.Coordinate, color = Event), alpha = 0.35, 
              fill = "#100b96")+
  scale_fill_continuous(type = "viridis")+
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0.5, face = "italic"))+
  labs(x = "", y = "", title = "Locations of Model Events", 
       caption = "Viz by Avery Ellis, @aellis0; Data from Stathletes   ")
all_model_events_viz




#PART 3: CLEANING DATA 
#goal here is to split our model events into 5x5 bins that cover the entire rink

model_events$Rounded.X.Location <- model_events$X.Coordinate %/% 5
model_events$Rounded.Y.Location <- model_events$Y.Coordinate %/% 5
#divides the rink into 680 5x5 squares to condense data since we don't have enough data to get more granular
#Future Consideration w' more data: divide smaller (3x3?)
#Singh (2019) does 12x8 bins for a soccer pitch, this seems a relatively good scaled approximation
#although maybe we can scale to make it "square" i.e. same bins lengthwise and widthwise, but this is an easy fix
#just change the 5 to something larger for the x-coordinate line
model_events <- mutate(
  model_events, Bin = ifelse(
    model_events$Rounded.X.Location > 0 & model_events$Rounded.Y.Location > 0, 
    ((17 * model_events$Rounded.X.Location) + model_events$Rounded.Y.Location),
    ifelse(
      model_events$Rounded.X.Location > 0 & model_events$Rounded.Y.Location == 0, 
      (model_events$Rounded.X.Location + (17 * model_events$Rounded.X.Location)),
      ifelse(
        model_events$Rounded.X.Location == 0 & model_events$Rounded.Y.Location > 0, model_events$Rounded.Y.Location, 0
      ))))

model_events$Rounded.X.Location.2 <- model_events$X.Coordinate.2 %/% 5
model_events$Rounded.Y.Location.2 <- model_events$Y.Coordinate.2 %/% 5
model_events <- mutate(
  model_events, Bin.2 = ifelse(
    model_events$Rounded.X.Location.2 > 0 & model_events$Rounded.Y.Location.2 > 0, 
    ((17 * model_events$Rounded.X.Location.2) + model_events$Rounded.Y.Location.2),
    ifelse(
      model_events$Rounded.X.Location.2 > 0 & model_events$Rounded.Y.Location.2 == 0, 
      (model_events$Rounded.X.Location.2 + (17 * model_events$Rounded.X.Location.2)),
      ifelse(
        model_events$Rounded.X.Location.2 == 0 & model_events$Rounded.Y.Location.2 > 0, model_events$Rounded.Y.Location.2, 0
      ))))
#and then some minor changes would be needed here
model_events %>%
  as_tibble()
#puts every event into a "bin" based on its location (697 possible bins but only 691 used)




#PART 4: WHERE ARE EVENTS CONCENTRATED?
#Finding total events per bin and then plotting to visualize where things happen
bins_df <- tibble(Possible.Bins = c(1:691))
bins_df$Freq = 0

for (possible_bin in 1:nrow(bins_df)) {
  bins_df[[possible_bin, "Freq"]] = length(which(model_events$Bin == possible_bin))
}

#Data Tidying b/c weird import
names(bins_df)[names(bins_df) == "Possible.Bins"] <- "Bin"
#renames column to bin for easier understandability

bins_df <- mutate(
  bins_df, Rounded.X.Location = Bin %/% 17
)
bins_df <- mutate(
  bins_df, Rounded.Y.Location = ifelse(
    bins_df$Rounded.X.Location == 0, Bin, (Bin - (17 * bins_df$Rounded.X.Location)))
)
bins_df$Approx.X.Location <- (bins_df$Rounded.X.Location * 5)
bins_df$Approx.Y.Location <- (bins_df$Rounded.Y.Location * 5)
#Adding in new columns with the locations of the bins; in effect working backwards to get what we had before in a new df

bins_df$NumericFreq <- as.numeric(bins_df$Freq)
bins_df$NumericFreq <- scale(bins_df$NumericFreq)
bins_df$Freq <- as.factor(bins_df$Freq)
#For ggplot2 to make plotting easier, this doesn't actually change anything about our observations.
#Results in a tibble with the rink split up into 5x5 bins, and how many events happened in each bin.

bins_df_plotting <- bins_df
bins_df_plotting$Approx.X.Location = 0.97 * (bins_df_plotting$Approx.X.Location - 97.5)
bins_df_plotting$Approx.Y.Location = (bins_df_plotting$Approx.Y.Location - 41)
#To make our new dataframe look nice when we plot it.

bins_df_viz <-
  nhl_rink_plot()+ 
  theme_void()+
  geom_point(data = bins_df_plotting, aes(x = Approx.X.Location, y = Approx.Y.Location, size = NumericFreq),
              color = "#032cfc", alpha = 0.5)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "", y = "", title = "Concentrated Locations of Passes, Shots, and Carries", 
       subtitle = "Located in Closest 5x5 Region, Scaled by Concentration",
       caption = "Viz by Avery Ellis, @aellis0; Data via Stathletes")
bins_df_viz
#This is an estimation of where things were recorded on the ice; bigger circles mean more things were recorded there.




#5: UNDERSTANDING THE MODEL SETUP
#Let's choose a random bin (in the offensive zone) and see all events that happen there.

rand_bin <- model_events %>%
  subset(Bin == 552)
rand_bin_plotting <- rand_bin
rand_bin_plotting$Event = as.factor(rand_bin_plotting$Event)
rand_bin_plotting$X.Coordinate = (rand_bin_plotting$X.Coordinate - 100)
rand_bin_plotting$X.Coordinate.2 = (rand_bin_plotting$X.Coordinate.2 - 100)
rand_bin_plotting$Y.Coordinate = (rand_bin_plotting$Y.Coordinate - 42.5)
rand_bin_plotting$Y.Coordinate.2 = (rand_bin_plotting$Y.Coordinate.2 - 42.5)

rand_bin_viz <-
  nhl_rink_plot()+ 
  theme_void()+ 
  geom_segment(data = (rand_bin_plotting %>% filter(Event %nin% c("Shot", "Goal"))), 
               aes(x = X.Coordinate, xend = X.Coordinate.2, y = Y.Coordinate, yend = Y.Coordinate.2, color = Event))+
  geom_jitter(data = (rand_bin_plotting %>% filter(Event %in% c("Shot", "Goal"))), 
             aes(x = X.Coordinate, y = Y.Coordinate, color = Event))+
  theme(plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "", y = "", title = "Events in Bin 500", subtitle = "Total Events = 35", 
       caption = "Viz by Avery Ellis, @aellis0; Data from Stathletes")
rand_bin_viz
#So we've got like 6 passes and a bunch of shots, and a singular goal.
#Our xT(sub i) at a given location is defined as having three parts:
#first, the probability of scoring a goal from that location
#second, the probability of scoring after i iterations
#third, the likelihood of a turnover in those i iterations followed by an opponent scoring.
#So let's run the first iteration, which is the probability of scoring in 1 iteration. 
#This is basically a VERY NAIVE xG model, using only location-based data, and even more naive since it's just location-based. 




#6: THE FIRST ITERATION
#Building the naive xG model.
#We do this pretty easily: take the data for all bins where the event is a shot or a goal. The probability of scoring in 1 iteration
#is thus Goals/(Goals + Shots).

iter_1_events <- model_events %>%
  filter(Event %in% c("Shot", "Goal"))
iter_1_bins <- iter_1_events$Bin %>%
  table() %>%
  as.data.frame() %>%
  as_tibble()
names(iter_1_bins)[names(iter_1_bins) == "."] <- "Bin"
names(iter_1_bins)[names(iter_1_bins) == "Freq"] <- "Total Shots"
iter_1_bins$Bin <- as.numeric(as.character(iter_1_bins$Bin))
#this is pulling all shots into a new dataframe

iter_1_goals <- model_events %>%
  filter(Event %in% c("Goal"))
iter_1_goals_bins <- iter_1_goals$Bin %>%
  table() %>%
  as.data.frame %>%
  as_tibble()
names(iter_1_goals_bins)[names(iter_1_goals_bins) == "."] <- "Bin"
names(iter_1_goals_bins)[names(iter_1_goals_bins) == "Freq"] <- "Goals"
iter_1_goals_bins$Bin <- as.numeric(as.character(iter_1_goals_bins$Bin))
#same here but with just goals, it would be nice if I knew how to write a damn function

iter_1_bins <- left_join(iter_1_bins, iter_1_goals_bins)
iter_1_bins[is.na(iter_1_bins)] = 0
#joins into 1 df and replaces all the NA with 0
iter_1_bins$xTT1 = (iter_1_bins$Goals / iter_1_bins$`Total Shots`)
#our initial xTT column, appended with 1 to indicate first iteration

xTT <- left_join(bins_df, iter_1_bins)
xTT[is.na(xTT)] = 0
#joins into the bins_df df and replaces all the NA with 0


#For ggplot2 to make plotting easier, this doesn't actually change anything about our observations.
xTT_plotting <- xTT
xTT_plotting$xTT_plotting <- scale(xTT_plotting$xTT1)
xTT_plotting$Approx.X.Location = 0.97 * (xTT_plotting$Approx.X.Location - 97.5)
xTT_plotting$Approx.Y.Location = (xTT_plotting$Approx.Y.Location - 41)
#To make our new dataframe look nice when we plot it.

iter_1_viz <-
  nhl_rink_plot()+ 
  theme_void()+
  geom_point(data = (xTT_plotting %>% filter((`Total Shots` > 1) & (xTT1 > 0))), 
             aes(x = Approx.X.Location, y = Approx.Y.Location, size = xTT1),
             color = "#032cfc", alpha = 0.5)+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.subtitle = element_text(hjust = 0.5))+
  labs(x = "", y = "", title = "Initial xT Model", 
       subtitle = "Minimum 1 Goal and 2 Shots, Located in Closest 5x5 Region, Scaled by xG",
       caption = "Viz by Avery Ellis, @aellis0; Data via Stathletes")
iter_1_viz
#Our initial xT model! Hey hey! 
#Note that we filtered to exclude the regions where there weren't any shots taken or there was only one shot.
#So what have we learned? Not a lot, cause this is a super naive model.
#But it passes the smell test, seemingly concentrated in the slot and towards the net. So I haven't messed up yet.
#This is again, in effect, the xT if only shooting is a possibility, and you can't move.




#PART 7: MORE ITERATIONS
#Now to really define xTT.
#As a reminder: our xTT in a given region is defined as:
#(chance of shooting from that region * chance of scoring from that region) + 
#(chance of moving from that region * sum over all regions of (value of that region * chance of moving to that region))-
#(chance of losing the puck in that location * xTT of that region's opposite)
#So let's create the xT of one bin and then apply that for every other bin.

#Take bin 2. Events are: Play, Carry, Carry, Carry, Carry.
#Thus: we say xT of bin 2 is equal to xT generated from shooting there (zero)
#plus xT from all the zones passed to/carried to * probability of moving to each zone
#minus the xT of that location flipped across the rink.

xTT <- xTT %>%
  add_column(xTT2 = 0,
             Positive.Events = 0,
             Positive.Move.Probability = 0,
             Negative.Events = 0,
             Negative.Event.Probability = 0)
#For calculating more xTT. Positive moves are successful carries or passes.
#Negative moves are incomplete passes or giveaways
xTT$xTT2 <- xTT$xTT1

for (row in 1:nrow(xTT)) {
  pos_df <- model_events %>%
    subset({{Bin == xTT[[row, "Bin"]]} & {Event %in% c("Carry", "Play")}})
  neg_df <- model_events %>%
    subset({{Bin == xTT[[row, "Bin"]]} & {Event %in% c("Incomplete Play", "Takeaway")}})
  xTT[row, "Positive.Events"] = nrow(pos_df)
  xTT[row, "Negative.Events"] = nrow(neg_df)
}
#Just calculating the number of positive and negative events that happen in each bin
xTT <- xTT %>%
  mutate(Positive.Move.Probability = 
           ifelse((Positive.Events >= 1), (xTT$Positive.Events / as.numeric(as.character(xTT$Freq))), 0),
         Negative.Event.Probability = 
           ifelse((Negative.Events >= 1), (xTT$Negative.Events / as.numeric(as.character(xTT$Freq))), 0)
  )
#All positive events that happen are moves (passes and carries)
#But a takeaway is an "event", so this is failed passes and takeaways.

#7A: Calculating The Positive Transition Matrix 
for (bin in 1:nrow(xTT)) {
  #iterates through every bin
  pos_df <- model_events %>%
    subset({{Bin == xTT[[bin, "Bin"]]} & {Event %in% c("Carry", "Play")}})
  #takes the model_events df and subsets it into only positive events 
  #that start in that bin
  if (nrow(pos_df) == 0) {
    next()
  }
  #if there aren't any, then skip it
  pos_df_freq <- pos_df$Bin.2 %>%
    table() %>%
    as.data.frame() %>%
    drop_na()
  #take the ENDING locations of these events and calculate the frequencies where these occur.
  #This forms the Markov transition matrix.
  names(pos_df_freq)[names(pos_df_freq) == "."] <- "Bin"
  pos_df_freq$Bin <- as.numeric(as.character(pos_df_freq$Bin))
  #this just makes the df look nicer
  pos_df_freq <- pos_df_freq %>%
    mutate(xTT1 = 0)
  #add in a column to describe that bin's initial xTT
  for (each_bin in 1:nrow(pos_df_freq)) {
    pos_df_freq$xTT1[each_bin] <- xTT[[pos_df_freq$Bin[each_bin], "xTT1"]]
  }
  #call each bin's xTT from the xTT dataframe
  pos_df_freq$Weighted.xTT1 <- (pos_df_freq$xTT1 *
                                  (pos_df_freq$Freq / sum(pos_df_freq$Freq)))
  #then weight that bin's xTT by the number of times it gets moved to
  xTT[bin, "xTT2"] <- xTT[bin, "xTT2"] + 
    (xTT[bin, "Positive.Move.Probability"] * sum(pos_df_freq$Weighted.xTT1))
  #finally, add the new "weighted xTT1" to the xTT df, creating xTT2!
}

xtt_neg_impacts <- tibble(c(1:691)) %>%
  rename(Bin = "c(1:691)") %>%
  mutate(Neg.Impact = 0)


#7B: Calculating the Negative Transition Matrix
for (bin in 1:nrow(xTT)) {
  #iterates through every bin
  neg_df <- model_events %>%
    subset({{Bin == xTT[[bin, "Bin"]]} & {Event %in% c("Failed Play", "Takeaway")}})
  #takes the model_events df and subsets it into only negative events that 
  #either occur or start in that bin
  if (nrow(neg_df) == 0) {
    next()
  }
  #if there aren't any, then skip this part of the loop
  neg_df_takeaways <- neg_df %>%
    subset(Event == "Takeaway") %>%
    as_tibble()
  neg_df_plays <- neg_df %>%
    subset(Event == "Failed Play") %>%
    as_tibble()
  #Part 7B1: Calculating the Negative Impact of Failed Passes
  if (nrow(neg_df_plays) > 0) {
    neg_df_freq <- neg_df_plays$Bin.2 %>%
      table() %>%
      as.data.frame() %>%
      drop_na()
    #take the ENDING locations of these events and calculate the frequencies where these occur.
    #This forms the Markov transition matrix.
    names(neg_df_freq)[names(neg_df_freq) == "."] <- "Bin"
    neg_df_freq$Bin <- as.numeric(as.character(neg_df_freq$Bin))
    #this just makes the df look nicer
    neg_df_freq <- neg_df_freq %>%
      mutate(xTT1 = 0)
    #add in a column to describe that bin's initial xTT
    for (each_bin in 1:nrow(neg_df_freq)) {
      neg_df_freq$xTT1[each_bin] <- xTT[[neg_df_freq$Bin[each_bin], "xTT1"]]
    }
  #call each bin's xTT from the xTT dataframe
  neg_df_freq$Weighted.xTT1 <- (neg_df_freq$xTT1 *
                                  (neg_df_freq$Freq / sum(neg_df_freq$Freq)))
  #then weight that bin's xTT by the number of times it gets moved to
  failed_passes_xTT <- sum(neg_df_freq$Weighted.xTT1)
  #finally, calculate and store the total xTT from failed passes.
  }
  #Part 7B2: Calculating the Negative Impact of Takeaways
  if (nrow(neg_df_takeaways > 0)) {
    #basically all this block does is find the flipped x-location, 
    #adjusts it to the approximate coordinates,
    #and then makes the takeaways dataframe smaller,
    #because they only happen in this bin,
    #so we only need the number that occur and the location they occur in
    neg_df_takeaways <- neg_df_takeaways %>%
      select(X.Coordinate, Y.Coordinate, Rounded.X.Location, Rounded.Y.Location, Bin) %>%
      mutate(Flipped.X.Coordinate = as.numeric((200 - as.numeric(X.Coordinate))),
             Flipped.Y.Coordinate = as.numeric((85 - as.numeric(Y.Coordinate)))) %>%
      #When you lose the puck, you effectively flip the rink.
      #O-Zone loss becomes D-Zone, and vice versa.
      mutate(Approx.Flipped.X = as.numeric(Flipped.X.Coordinate %/% 5),
             Approx.Flipped.Y = as.numeric(Flipped.Y.Coordinate %/% 5))%>%
      #These mutates() basically just recalculate the bin you'd be in
      #if you were to take the puck away in a bin.
      mutate(Flipped.Bin = ifelse(
                 Approx.Flipped.X > 0 & Approx.Flipped.Y > 0, 
                 ((17 * Approx.Flipped.X) + Approx.Flipped.Y),
                 ifelse(
                   Approx.Flipped.X > 0 & Approx.Flipped.Y == 0, 
                   (Approx.Flipped.X + (17 * Approx.Flipped.Y)),
                   ifelse(
                     Approx.Flipped.X == 0 & Approx.Flipped.Y > 0, 
                     Approx.Flipped.Y, 0
                   ))))
    #The code for this is basically the same way the bins were
    #originally generated. Just small changes.
    takeaways_xTT <- (xTT[[neg_df_takeaways$Flipped.Bin[1], "xTT1"]] /
                        nrow(neg_df_takeaways))
    #calculate the total xTT that comes from takeaways in this bin
    #weighted by the probability of losing the puck here.
  }
  xTT[bin, "xTT2"] <- xTT[bin, "xTT2"] -
    (xTT[bin, "Negative.Event.Probability"] * (failed_passes_xTT + takeaways_xTT))
  #finally, add the xTT that comes from takeaways in this bin
  #and failed passes to all other bins, multiply it by
  #the probability of a negative event occuring,
  #and subtract that all from our xTT figure, completing the second iteration
  #of our xTT
}
  
#Big Data Cup xTT Model
#Avery Ellis and Matt Hurley
#2/27/2021




#PART 1: SETUP

library(tidyverse)
library(ggforce)
'%nin%' <- Negate('%in%')

full_scouting_dataset <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
full_scouting_dataset$X.Coordinate <- as.character(full_scouting_dataset$X.Coordinate)
full_scouting_dataset$Y.Coordinate <- as.character(full_scouting_dataset$Y.Coordinate)

#Creating "Carries" --i.e. moving with the puck, based on the event data given
for (row in 1:nrow(full_scouting_dataset)) {
  if ({full_scouting_dataset[row, ]$Event %in% c("Puck Recovery", "Takeaway")} & 
      {full_scouting_dataset[(row + 1), ]$Event %in% c("Play", "Incomplete Play", "Dump In/Out", "Shot", "Goal")} &
      {full_scouting_dataset[row, ]$Player == full_scouting_dataset[(row + 1), ]$Player}) {
    full_scouting_dataset <- full_scouting_dataset %>%
      rbind(c(full_scouting_dataset[row, "game_date"], full_scouting_dataset[row, "Home.Team"], full_scouting_dataset[row, "Away.Team"],
              full_scouting_dataset[row, "Period"], full_scouting_dataset[row, "Clock"],
              full_scouting_dataset[row, "Home.Team.Skaters"], full_scouting_dataset[row, "Away.Team.Skaters"], 
              full_scouting_dataset[row, "Home.Team.Goals"], full_scouting_dataset[row, "Away.Team.Goals"], full_scouting_dataset[row, "Team"],
              full_scouting_dataset[row, "Player"], "Carry", 
              full_scouting_dataset[row, "X.Coordinate"], full_scouting_dataset[row, "Y.Coordinate"],
              "", "", "", "", "", full_scouting_dataset[(row + 1), "X.Coordinate"], full_scouting_dataset[(row + 1), "Y.Coordinate"])
      )
  }
}

#these loops do the same thing--checking to see where consecutive events happen and then creating a "carry" event
#based on the location difference
#NOTE: this is pretty naive and assumes moving in a straight line
#with tracking data, could evaluate carries that aren't straight (which is almost all of them)
for (row in (1:nrow(full_scouting_dataset))) {
  if ({full_scouting_dataset[row, ]$Event == "Play"} &
      {full_scouting_dataset[(row + 1), ]$Event %in% c("Play", "Incomplete Play", "Takeaway", "Puck Recovery", "Dump In/Out", "Shot", "Goal")} &
      {full_scouting_dataset[row, ]$Player.2 == full_scouting_dataset[(row + 1), ]$Player}) {
    full_scouting_dataset <- full_scouting_dataset %>%
      rbind(c(full_scouting_dataset[row, ]$game_date, full_scouting_dataset[row, ]$Home.Team, full_scouting_dataset[row, ]$Away.Team,
              full_scouting_dataset[row, ]$Period, full_scouting_dataset[row, ]$Clock,
              full_scouting_dataset[row, ]$Home.Team.Skaters, full_scouting_dataset[row, ]$Away.Team.Skaters, 
              full_scouting_dataset[row, ]$Home.Team.Goals, full_scouting_dataset[row, ]$Away.Team.Goals, full_scouting_dataset[row, ]$Team,
              full_scouting_dataset[row, ]$Player.2, "Carry", 
              full_scouting_dataset[row, ]$X.Coordinate.2, full_scouting_dataset[row, ]$Y.Coordinate.2,
              "", "", "", "", "", full_scouting_dataset[(row + 1), ]$X.Coordinate, full_scouting_dataset[(row + 1), ]$Y.Coordinate)
      )
  }
}

#One more thing we gotta make based on data inferences: failed passes.
#Makes calculating the negative Markov transition matrix a lot easier.

for (row in (1:nrow(full_scouting_dataset))) {
  if ({full_scouting_dataset[row, ]$Event == "Incomplete Play"} &
      {full_scouting_dataset[(row + 1), ]$Event == "Puck Recovery"} &
      {full_scouting_dataset[row, ]$Team != full_scouting_dataset[(row + 1), ]$Team}) {
    full_scouting_dataset <- full_scouting_dataset[-row, ]
    full_scouting_dataset <- full_scouting_dataset %>%
      rbind(c(full_scouting_dataset[row, ]$game_date, full_scouting_dataset[row, ]$Home.Team, full_scouting_dataset[row, ]$Away.Team,
              full_scouting_dataset[row, ]$Period, full_scouting_dataset[row, ]$Clock,
              full_scouting_dataset[row, ]$Home.Team.Skaters, full_scouting_dataset[row, ]$Away.Team.Skaters, 
              full_scouting_dataset[row, ]$Home.Team.Goals, full_scouting_dataset[row, ]$Away.Team.Goals, full_scouting_dataset[row, ]$Team,
              full_scouting_dataset[row, ]$Player, "Failed Play", 
              full_scouting_dataset[row, ]$X.Coordinate, full_scouting_dataset[row, ]$Y.Coordinate, "", "", "", "", 
              full_scouting_dataset[(row + 1), ]$Player, 
              full_scouting_dataset[(row + 1), ]$X.Coordinate, full_scouting_dataset[(row + 1), ]$Y.Coordinate)
      )
  }
}
scouting_dataset <- full_scouting_dataset %>%
  subset({{Home.Team.Skaters == 5} & {Away.Team.Skaters == 5}})


#this pulls all the data from the scouting dataset and then subsets it to all our events we use for the xT model



#convert the coordinate data to numeric type
coordcolumns = c('X.Coordinate','X.Coordinate.2','Y.Coordinate','Y.Coordinate.2')
full_scouting_dataset[, coordcolumns] <- apply(full_scouting_dataset[, coordcolumns], 2, function(x) as.numeric(as.character(x)))



#PART 2: CLEANING DATA 
#splitting our events into 5x5 bins that cover the entire rink

full_scouting_dataset <- full_scouting_dataset %>% mutate(
    Rounded.X.Location = (X.Coordinate %/% 5),
    Rounded.Y.Location = (Y.Coordinate %/% 5),
    Rounded.X.Location.2 = (X.Coordinate.2 %/% 5),
    Rounded.Y.Location.2 = (Y.Coordinate.2 %/% 5)
  )
#divides the rink into 697 5x5 squares to condense data since we don't have enough data to get more granular
#Future Consideration w' more data: divide smaller (3x3?)

full_scouting_dataset <- full_scouting_dataset %>% mutate(
  Bin = ifelse(
    Rounded.X.Location > 0 & Rounded.Y.Location > 0, 
    ((17 * Rounded.X.Location) + Rounded.Y.Location),
    ifelse(
      Rounded.X.Location > 0 & Rounded.Y.Location == 0, 
      (Rounded.X.Location + (17 * Rounded.X.Location)),
      ifelse(
        Rounded.X.Location == 0 & Rounded.Y.Location > 0, Rounded.Y.Location, 0
      ))),
  Bin.2 = ifelse(
    Rounded.X.Location.2 > 0 & Rounded.Y.Location.2 > 0, 
    ((17 * Rounded.X.Location.2) + Rounded.Y.Location.2),
    ifelse(
      Rounded.X.Location.2 > 0 & Rounded.Y.Location.2 == 0, 
      (Rounded.X.Location.2 + (17 * Rounded.X.Location.2)),
      ifelse(
        Rounded.X.Location.2 == 0 & Rounded.Y.Location.2 > 0, Rounded.Y.Location.2, 0
      )))
  )
#puts every event into a "bin" based on its location (697 possible bins, 0 being at (0,0) and 696 being at (200, 80) )




#PART 3: BIN CONCENTRATIONS
#Finding total events per bin and then plotting to visualize where things happen
model_events <- full_scouting_dataset %>%
  subset({{Home.Team.Skaters == 5} & {Away.Team.Skaters == 5}})
model_events <- model_events %>%
  subset({Event == "Shot" | Event == "Play" | Event == "Goal" | 
      Event == "Takeaway" | Event == "Carry" | Event == "Failed Play"})

bins_df <- tibble(Possible.Bins = c(0:696))
bins_df <- bins_df %>% mutate(
  Freq = 0
)

for (possible_bin in 1:nrow(bins_df)) {
  bins_df[[possible_bin, "Freq"]] = length(which(model_events$Bin == possible_bin))
}
bins_df <- bins_df %>% rename(
  Bin = Possible.Bins
)
#renames column to bin for easier understandability

bins_df <- bins_df %>% 
  mutate(Rounded.X.Location = Bin %/% 17)
bins_df <- bins_df %>% 
  mutate(Rounded.Y.Location = ifelse(
    Rounded.X.Location == 0, Bin, (Bin - (17 * Rounded.X.Location))))
bins_df <- bins_df %>%
  mutate(Approx.X.Location = (Rounded.X.Location * 5),
    Approx.Y.Location = (Rounded.Y.Location * 5)
  )
#Adding in new columns with the locations of the bins; in effect working backwards to get what we had before in a new df



#PART 4: THE FIRST ITERATION
#Building the naive xG model.
#We do this pretty easily: take the data for all bins where the event is a shot or a goal. 
#The probability of scoring in 1 iteration is thus Goals/(Goals + Shots).

iter_1_events <- model_events %>%
  filter(Event %in% c("Shot", "Goal"))
iter_1_bins <- iter_1_events$Bin %>%
  table() %>%
  as.data.frame() %>%
  as_tibble()
iter_1_bins <- iter_1_bins %>% rename(
  `Total Shots` = Freq
  )
names(iter_1_bins)[names(iter_1_bins) == "."] <- "Bin"
#it won't mutate for some reason
iter_1_bins$Bin <- as.numeric(as.character(iter_1_bins$Bin))
#this is pulling all shots into a new dataframe

iter_1_goals <- model_events %>%
  filter(Event %in% c("Goal"))
iter_1_goals_bins <- iter_1_goals$Bin %>%
  table() %>%
  as.data.frame %>%
  as_tibble()
iter_1_goals_bins <- iter_1_goals_bins %>%
  rename(Goals = Freq)
names(iter_1_goals_bins)[names(iter_1_goals_bins) == "."] <- "Bin"
iter_1_goals_bins$Bin <- as.numeric(as.character(iter_1_goals_bins$Bin))
#same here but with just goals


iter_1_bins <- left_join(iter_1_bins, iter_1_goals_bins)
iter_1_bins[is.na(iter_1_bins)] = 0
#joins into 1 df and replaces all the NA with 0
iter_1_bins <- iter_1_bins %>%
  mutate(xTT1 = (Goals / `Total Shots`))
#our initial xTT column, appended with 1 to indicate first iteration

xTT <- left_join(bins_df, iter_1_bins) %>%
  replace(is.na(.), 0)
#joins the xTT column into the bins_df and replaces all the NA with 0




#PART 5: THE SECOND ITERATION

xTT <- xTT %>%
  add_column(xTT2 = 0,
             Positive.Events = 0,
             Positive.Move.Probability = 0,
             Negative.Events = 0,
             Negative.Event.Probability = 0)
#For calculating more xTT. Positive moves are successful carries or passes.
#Negative moves are incomplete passes or giveaways

for (row in 1:nrow(xTT)) {
  pos_df <- model_events %>%
    subset({Bin == xTT[[row, "Bin"]] & Event %in% c("Carry", "Play")})
  ifelse((nrow(pos_df) > 0), 
         (xTT[[row, "Positive.Events"]] = nrow(pos_df)),
         (xTT[[row, "Positive.Events"]] = 0))
  neg_df <- model_events %>%
    subset({Bin == xTT[[row, "Bin"]] & Event %in% c("Failed Play", "Takeaway")})
  ifelse((nrow(neg_df) > 0), 
         (xTT[[row, "Negative.Events"]] = nrow(neg_df)),
         (xTT[[row, "Negative.Events"]] = 0))
}
#Just calculating the number of positive and negative events that happen in each bin

xTT <- xTT %>%
  mutate_at(c("Positive.Events", "Negative.Events"),
            funs(lead), 
            n = 1)
  #for some reason, the positive and negative events are put into the next bin.
  #The mutate_at just shifts them up 1 to account for this

#drop the NA values that result and replace them with 0
xTT <- xTT %>%
  mutate_at(c("Positive.Events", "Negative.Events"),
            ~replace(., is.na(.), 0))

xTT <- xTT %>%
  mutate(Positive.Move.Probability = 
           ifelse((Positive.Events >= 1), (xTT$Positive.Events / as.numeric(as.character(xTT$Freq))), 0),
         Negative.Event.Probability = 
           ifelse((Negative.Events >= 1), (xTT$Negative.Events / as.numeric(as.character(xTT$Freq))), 0)
  )
#All positive events that happen are moves (passes and carries)
#But a takeaway is an "event", so this is failed passes and takeaways.

#PART 5A: Calculating The Positive Transition Matrix 
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


#PART 5B: Calculating the Negative Transition Matrix
for (bin in 1:nrow(xTT)) {
  #iterates through every bin
  neg_df <- model_events %>%
    subset({{Bin == xTT[[bin, "Bin"]]} & {Event %in% c("Failed Play", "Takeaway")}})
  #takes the model_events df and subsets it into only negative events that 
  #either occur or start in that bin
  if (nrow(neg_df) == 0) {
    next()
  }
  failed_passes_xTT <- 0
  #if there aren't any, then skip this part of the loop
  neg_df_takeaways <- neg_df %>%
    subset(Event == "Takeaway") %>%
    as_tibble()
  neg_df_plays <- neg_df %>%
    subset(Event == "Failed Play") %>%
    as_tibble()
  #Part 5B1: Calculating the Negative Impact of Failed Passes
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
  failed_passes_xTT <- ifelse(failed_passes_xTT != 0, failed_passes_xTT, 0)
  #Part 5B2: Calculating the Negative Impact of Takeaways
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
      #These mutate() calls basically just recalculate the bin you'd be in
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
  #of our xTT!
}




#PART 6: MORE ITERATIONS
#Singh & Forstner both used 5 iterations.
#We're gonna try that and see if our model converges to our liking.
xTT <- xTT %>%
  mutate(xTT3 = 0,
         xTT4 = 0,
         xTT5 = 0)
#adding in new columns to calculate each new iteration of xTT.

#The majority of this loop is just copy-pasted from part 7.
#So we've added small comments on the new stuff, and took out
#the comments on stuff we've already talked about.
for (iteration in 3:5) {
  for (col in colnames(xTT)) {
    if(str_sub(col, -1, -1) == as.character(iteration - 1)) {
      prior_iteration <- colnames(xTT)[which(colnames(xTT) == col)]
    }
    if(str_sub(col, -1, -1) == as.character(iteration)) {
      current_iteration <- colnames(xTT)[which(colnames(xTT) == col)]
    }
  }
  #this loop basically uses the current iteration number to decide
  #which column is the "new" xTT column and which is the "old"
  xTT[, current_iteration] <- xTT[, prior_iteration]
  #just sets the current iteration of xTT as the prior, so it can then be adjusted.
  
  #Everything below this is the same, but with a few slight variations
  #using the current_iteration and prior_iteration variables
  #and replacing the xTT1 and xTT2 with xTT_prior and xTT_current.
  #That's it, though.
  for (bin in 1:nrow(xTT)) {
    pos_df <- model_events %>%
      subset({{Bin == xTT[[bin, "Bin"]]} & {Event %in% c("Carry", "Play")}})
    if (nrow(pos_df) == 0) {
      next()
    }
    pos_df_freq <- pos_df$Bin.2 %>%
      table() %>%
      as.data.frame() %>%
      drop_na()
    names(pos_df_freq)[names(pos_df_freq) == "."] <- "Bin"
    pos_df_freq$Bin <- as.numeric(as.character(pos_df_freq$Bin))
    pos_df_freq <- pos_df_freq %>%
      mutate(xTT_prior = 0)
    for (each_bin in 1:nrow(pos_df_freq)) {
      pos_df_freq$xTT_prior[each_bin] <- xTT[[pos_df_freq$Bin[each_bin], prior_iteration]]
    }
    pos_df_freq$Weighted.xTT_prior <- (pos_df_freq$xTT_prior *
                                    (pos_df_freq$Freq / sum(pos_df_freq$Freq)))
    xTT[bin, current_iteration] <- xTT[bin, current_iteration] + 
      (xTT[bin, "Positive.Move.Probability"] * sum(pos_df_freq$Weighted.xTT_prior))
  }
  
  for (bin in 1:nrow(xTT)) {
    neg_df <- model_events %>%
      subset({{Bin == xTT[[bin, "Bin"]]} & {Event %in% c("Failed Play", "Takeaway")}})
    if (nrow(neg_df) == 0) {
      next()
    }
    failed_passes_xTT <- 0
    neg_df_takeaways <- neg_df %>%
      subset(Event == "Takeaway") %>%
      as_tibble()
    neg_df_plays <- neg_df %>%
      subset(Event == "Failed Play") %>%
      as_tibble()
    if (nrow(neg_df_plays) > 0) {
      neg_df_freq <- neg_df_plays$Bin.2 %>%
        table() %>%
        as.data.frame() %>%
        drop_na()
      names(neg_df_freq)[names(neg_df_freq) == "."] <- "Bin"
      neg_df_freq$Bin <- as.numeric(as.character(neg_df_freq$Bin))
      neg_df_freq <- neg_df_freq %>%
        mutate(xTT_prior = 0)
      for (each_bin in 1:nrow(neg_df_freq)) {
        neg_df_freq$xTT_prior[each_bin] <- xTT[[neg_df_freq$Bin[each_bin], prior_iteration]]
      }
      neg_df_freq$Weighted.xTT_prior <- (neg_df_freq$xTT_prior *
                                      (neg_df_freq$Freq / sum(neg_df_freq$Freq)))
      failed_passes_xTT <- sum(neg_df_freq$Weighted.xTT_prior)
    }
    failed_passes_xTT <- ifelse(failed_passes_xTT != 0, failed_passes_xTT, 0)
    if (nrow(neg_df_takeaways > 0)) {
      neg_df_takeaways <- neg_df_takeaways %>%
        select(X.Coordinate, Y.Coordinate, Rounded.X.Location, Rounded.Y.Location, Bin) %>%
        mutate(Flipped.X.Coordinate = as.numeric((200 - as.numeric(X.Coordinate))),
               Flipped.Y.Coordinate = as.numeric((85 - as.numeric(Y.Coordinate)))) %>%
        mutate(Approx.Flipped.X = as.numeric(Flipped.X.Coordinate %/% 5),
               Approx.Flipped.Y = as.numeric(Flipped.Y.Coordinate %/% 5))%>%
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
      takeaways_xTT <- (xTT[[neg_df_takeaways$Flipped.Bin[1], prior_iteration]] /
                          nrow(neg_df_takeaways))
    }
    xTT[bin, current_iteration] <- xTT[bin, current_iteration] -
      (xTT[bin, "Negative.Event.Probability"] * (failed_passes_xTT + takeaways_xTT))
  }
}  

#plotting the xTT heatmap
xTT_plotting <- xTT
xTT_plotting <- xTT_plotting %>%
  mutate(
    Approx.X.Location = (0.96* (Approx.X.Location - 100)),
    Approx.Y.Location = Approx.Y.Location - 40
  )


upperlimit <- 0.33
lowerlimit <- -0.035
xTT_plotting <- xTT_plotting %>% 
  mutate(xTT5 = ifelse((xTT5 > upperlimit), (upperlimit), 
                       (ifelse((xTT5 < lowerlimit),(lowerlimit),(xTT5)))))
#sets limits of colormap

color1 = "blue" # low color
color2 = "red" # high color

#geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
#geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
#geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
#geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28))

more_iter_viz <-
  nhl_rink_plot()+ 
  theme_void()+
  geom_tile(data = (xTT_plotting %>% filter(
      Bin %nin% c())), 
      aes(x = Approx.X.Location, y = Approx.Y.Location, fill = xTT5),
      alpha = 0.55)+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))+
  scale_fill_gradient(low = color1, high = color2)+
  labs(x = "", y = "", title = "OHL Expected Total Threat (xTT)",
       subtitle = "< Defensive Zone \n Offensive Zone >",
       fill = "xTT of Zone",
       caption = "Viz by Avery Ellis and Matt Hurley; Data via Stathletes")

for (row in 1:nrow(xTT_plotting)) {
  if (xTT_plotting[row,]$Approx.X.Location^2+xTT_plotting[row,]$Approx.Y.Location^2 > 784) {
    more_iter_viz <- more_iter_viz + geom_point(aes(as.numeric(xTT_plotting[row,]$Approx.X.Location),as.numeric(xTT_plotting[row,]$Approx.Y.Location)))
  }
}

more_iter_viz
ggsave("BigDataCup/xTT_heatmap.png")

#Majority of regions are positive or (barely) negative, with a few zeroes.
#Concentration in o-zone is in slot, with the majority of the d-zone being negative.





#PART 7: Applying xTT

full_scouting_dataset <- full_scouting_dataset %>%
  mutate(xTT = 0,
         xTT.2 = 0,
         xTT.Change = 0)
#adding in columns to the scouting_dataset to describe how the xTT changes in an event

for (event in 1:nrow(full_scouting_dataset)) {
  full_scouting_dataset[[event, "xTT"]] <- xTT[[(full_scouting_dataset$Bin[event]), "xTT5"]]
  if(is.na(full_scouting_dataset$Bin.2[event])) {
    next()
  }
  full_scouting_dataset[[event, "xTT.2"]] <- xTT[[(full_scouting_dataset$Bin.2[event]), "xTT5"]]
}
#finding the xTT values before and after the event

valued_events <- full_scouting_dataset %>%
  subset(Event %in% c("Play", "Carry", "Takeaway", "Failed Play", "Incomplete Play", "Puck Recovery")) %>%
  mutate(xTT.Change = ifelse(
    (Event %in% c("Play", "Carry")), (xTT.2 - xTT),
    ifelse((Event == "Failed Play"), -(xTT.2 + xTT), 
           ifelse((Event == "Incomplete Play"), -xTT, xTT)
    )
  )
  )
#calculating the change in xTT from those values. 
#Carries and Plays get you the pure change in xTT.
#Failed Plays get you the sum of the xTT, flipped negative;
#Takeaways and Puck Recoveries give you the xTT where they occur.

plays <- valued_events %>%
  subset(Event == "Play")
carries <- valued_events %>%
  subset(Event == "Carry")
takeaways <- valued_events %>%
  subset(Event == "Takeaway")
failed_plays <- valued_events %>%
  subset(Event == "Failed Play")
incomplete_plays <- valued_events %>%
  subset(Event == "Incomplete Play")
puck_recoveries <- valued_events %>%
  subset(Event == "Puck Recovery")

total_xTT <- valued_events %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(Total.xTT = sum(xTT.Change),
            Average.xTT = Total.xTT / length(Event),
  )

plays_xTT <- plays %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Plays = sum(xTT.Change),
            Average.Play.xTT = xTT.From.Plays / length(Event),
  )

carries_xTT <- carries %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Carries = sum(xTT.Change),
            Average.Carry.xTT = xTT.From.Carries / length(Event),
  )
takeaways_xTT <- takeaways %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Takeaways = sum(xTT.Change),
            Average.Takeaway.xTT = xTT.From.Takeaways / length(Event),
  )
failed_plays_xTT <- failed_plays %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Failed_Plays = sum(xTT.Change),
            Average.Failed_Play.xTT = xTT.From.Failed_Plays / length(Event),
  )
incomplete_plays_xTT <- incomplete_plays %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Incomplete_Plays = sum(xTT.Change),
            Average.Incomplete_Play.xTT = xTT.From.Incomplete_Plays / length(Event))

puck_recoveries_xTT <- puck_recoveries %>%
  select(Team, Player, Event, xTT, xTT.2, xTT.Change) %>%
  group_by(Player, Team) %>%
  summarise(xTT.From.Puck_Recoveries = sum(xTT.Change),
            Average.Puck_Recoveries.xTT = xTT.From.Puck_Recoveries / length(Event))


players_xTT <- plyr::join_all(list(total_xTT, plays_xTT, carries_xTT, 
                                   takeaways_xTT, failed_plays_xTT, incomplete_plays_xTT,
                                   puck_recoveries_xTT)) %>%
  replace(is.na(.), 0)
#results in a df with all players in the dataset, and the xTT they generate, broken down by event type
#and also with the averages




#PART 8: Calculating xTT Chain


#Goal here is to sort by possession. But carries and failed passes were appended to the end.
#We need to get them into their correct positioning, within possessions.
#So this part works with dates and periods, etc; goal is to then use arrange()
#to sort the scouting_dataset and then we can include carries & failed passes in our possessions.
sorted_scouting_dataset <- full_scouting_dataset %>%
  mutate(game_date = lubridate::parse_date_time(game_date, "y-m-d"))
sorted_scouting_dataset <- sorted_scouting_dataset %>%
  separate(Clock, c("minutes_period", "seconds_period", "split_seconds_period"))
sorted_scouting_dataset <- sorted_scouting_dataset %>%
  mutate(minutes_period = as.numeric(minutes_period),
         seconds_period = as.numeric(seconds_period),
         split_seconds_period = as.numeric(split_seconds_period))
sorted_scouting_dataset <- sorted_scouting_dataset %>%
  select(-split_seconds_period)


sorted_scouting_dataset <- sorted_scouting_dataset %>%
  mutate(xTT.Change = ifelse(
    (Event %in% c("Play", "Carry")), (xTT.2 - xTT),
    ifelse((Event == "Failed Play"), -(xTT.2 + xTT), 
           ifelse((Event == "Incomplete Play"), -xTT, 
                  ifelse((Event %in% c("Takeaway", "Puck Recovery")), xTT, 0))
    )
  )
  )
#Just assigning our xTT values as defined earlier to the new, sorted scouting dataset.

sorted_scouting_dataset <- sorted_scouting_dataset %>%
  arrange(game_date, desc(Period), desc(minutes_period), desc(seconds_period))
#Arranging by when the event occurs. Now we've got it all in order!

sorted_scouting_dataset <- sorted_scouting_dataset %>%
  select(Home.Team, Away.Team, Period, Home.Team.Skaters, Away.Team.Skaters,
         Team, Player, Event, Player.2, xTT, xTT.2, xTT.Change)
#only select things we'll need when subsetting into possessions.



#Determine the events we're going to assign value to
xTT_events <- c("Play", "Failed Play", "Carry", "Takeaway", "Incomplete Play", "Puck Recovery")

players_xTT_chain <- players_xTT %>%
  select(Team, Player) %>%
  mutate(Personal.xTT = 0,
         Team.xTT.Chain = 0,
         xTT.Chain = 0)
#This df copies the team name and player name of each player in the dataset.
#And readies it so we can calculate xTT Chain and assign it to each player.



#CLEANING THE SORTED DATASET--odds and ends.
#We can remove penalties b/c they're always followed by a faceoff.

sorted_scouting_dataset <- sorted_scouting_dataset %>%
  filter(Event != "Penalty Taken")


double_faceoffs <- c()
for (event in 1:nrow(sorted_scouting_dataset)) {
  if ({sorted_scouting_dataset[[event, "Event"]] == "Faceoff Win" &
      sorted_scouting_dataset[[(event + 1), "Event"]] == "Faceoff Win"}) {
    double_faceoffs <- double_faceoffs %>%
      append(event)
  }
}
sorted_scouting_dataset <- sorted_scouting_dataset[-double_faceoffs, ]
#Gets rid of a few  double faceoffs that occur for some reason--probably penalties
#but unsure.

#EDGE CASES--weird points where the carries/failed passes creation messed up
sorted_scouting_dataset <- sorted_scouting_dataset[-c(21734), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:23881, 23884, 23882, 23883, 23885:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:24189, 24191, 24190, 24192:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:24267, 24269, 24268, 24270:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:25480, 25482, 25481, 25483:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:27552, 27554, 27553, 27555:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:36961, 36964, 36962, 36963, 36965:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:55893, 55896, 55894, 55895, 55897, 55899, 55900:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:56081, 56084, 56082, 56083, 56085, 56087:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:74744, 74747, 74745, 74746, 74748:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:80907, 80911, 80908, 80909, 80910, 80912:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:80897, 80901, 80898, 80899, 80900, 80902:nrow(sorted_scouting_dataset)), ]
sorted_scouting_dataset <- sorted_scouting_dataset[c(1:95744, 95747, 95745, 95746, 95748:nrow(sorted_scouting_dataset)), ]
row.names(sorted_scouting_dataset) <- c(1:114820)



#FUNCTIONS TO FIND POSSESSION VALUE

find_possession_value <- function(possession, players_xTT_chain) {
  #Arguments: a possession and the players' xTT df.
  #This function takes in a possession and from that possession does two things:
  #one, calculates personal xTT, the team xTT contribution, and the xTT Chain from it.
  #second, adds that possession into the players_xTT_chain dataframe.
  #It returns the players_xTT_chain dataframe.
  
  
  delt_xTT <- (possession[[nrow(possession), "xTT.2"]] - 
                 possession[[1, "xTT"]])
  #finds the change in xTT from the previous possession's ending to the final event of the possession. 
  passing_players_df <- possession %>%
    subset(Event == "Play") %>%
    distinct(Player)
  
  if (length(passing_players_df$Player) < 1) {
    team_contrib <- 0
    possession_xTT <- possession %>%
      select(Team, Player, xTT.Change) %>%
      group_by(Team, Player) %>%
      summarise(Personal.xTT = sum(xTT.Change),
                Team.xTT.Chain = delt_xTT) %>%
      mutate(xTT.Chain = (Personal.xTT + Team.xTT.Chain) * 0.5)
  } else {
    passing_players <- length(passing_players_df$Player)
    #finds the number of players that contributed to that possession
    team_contrib <- (delt_xTT / passing_players)
    #finds the team contribution based on that
    possession_xTT <- possession %>%
      select(Team, Player, xTT.Change) %>%
      filter(Player %in% passing_players_df$Player) %>%
      group_by(Team, Player) %>%
      summarise(Personal.xTT = sum(xTT.Change),
                Team.xTT.Chain = delt_xTT / passing_players) %>%
      mutate(xTT.Chain = ifelse((Player %in% passing_players_df$Player), 
                                ((Personal.xTT + Team.xTT.Chain) * 0.5),
                                Personal.xTT)
             )
 
  }
  
  
  for (each_player in 1:nrow(possession_xTT)) {
    player_name <- possession_xTT[[each_player, "Player"]]
    #The player's name
    player_team <- possession_xTT[[each_player, "Team"]]
    #Which team the player is playing for.
    player_row <- ifelse((length((which(players_xTT_chain$Player == player_name))) > 1), 
                         (which({players_xTT_chain$Player == player_name & players_xTT_chain$Team == player_team})), 
                         (which(players_xTT_chain$Player == player_name)))
    #There are a few players who appear twice in the dataset. Shane Bulitka, to name one.
    #This conditional is necessary because otherwise it extracts a vector of length 2.
    #If the player appears twice, then it checks the team as well, 
    #and assigns the xTT to the correct combination of player and team.
    players_xTT_chain[[player_row, "Personal.xTT"]] = (
      players_xTT_chain[[player_row, "Personal.xTT"]] + 
        possession_xTT[[each_player, "Personal.xTT"]])
    players_xTT_chain[[player_row, "Team.xTT.Chain"]] = (
      players_xTT_chain[[player_row, "Team.xTT.Chain"]] +
        possession_xTT[[each_player, "Team.xTT.Chain"]])
    players_xTT_chain[[player_row, "xTT.Chain"]] = (
      players_xTT_chain[[player_row, "xTT.Chain"]] + 
        possession_xTT[[each_player, "xTT.Chain"]])
  }
  return(players_xTT_chain)
}


single_event_value <- function(possession, players_xTT_chain) {
  #Arguments: a possession of only one event and the players' xTT df.
  #This function takes in a possession of just one event and from that possession
  #calculates personal xTT and then adds it into the players_xTT_chain dataframe.
  #It returns the players_xTT_chain dataframe.
  
  
  player_name <- possession[[1, "Player"]]
  #The player's name
  player_team <- possession[[1, "Team"]]
  #Which team the player is playing for.
  player_row <- ifelse((length((which(players_xTT_chain$Player == player_name))) > 1), 
                       (which({players_xTT_chain$Player == player_name & players_xTT_chain$Team == player_team})), 
                       (which(players_xTT_chain$Player == player_name)))
  #There are a few players who appear twice in the dataset. Shane Bulitka, to name one.
  #This conditional is necessary because otherwise it extracts a vector of length 2.
  #If the player appears twice, then it checks the team as well, 
  #and assigns the xTT to the correct combination of player and team.
  players_xTT_chain[[player_row, "Personal.xTT"]] <- 
    (players_xTT_chain[[player_row, "Personal.xTT"]] + possession[[1, "xTT.Change"]])
  return(players_xTT_chain)
}


#CALCULATING xTT CHAIN
#This is a giant for loop, so we've tried to comment it well to make
#everything pretty clear.

for (event in 1:nrow(sorted_scouting_dataset)) {
  #BASE CASE--assigning the first value
  if (event == 1) {
    team_with_possession <- sorted_scouting_dataset[[1, "Team"]]
    #Start by assigning the team with possession to the one who wins the first faceoff.
    previous_possession_end <- 1
    
  #WHEN TO SKIP EVENTS: ANYTHING THAT'S NOT 5V5  
  } else if ({sorted_scouting_dataset[[event, "Home.Team.Skaters"]] != 5 |
      sorted_scouting_dataset[[event, "Away.Team.Skaters"]] != 5}) {
    if (sorted_scouting_dataset[[event, "Team"]] != team_with_possession) {
      previous_possession_end <- event
    }
    team_with_possession <- sorted_scouting_dataset[[event, "Team"]]
    next()
    #We don't care about anything that's not 5v5 for now, so just keep on rolling.
    #However, we do track which team has possession and if a possession ended, 
    #because it'll become relevant once the penalty is expired, since possessions 
    #can carry over past the end of a penalty.
    
  #WHEN POSSESSIONS ARE GUARANTEED TO END: STOPPAGES (FACEOFFS)
  } else if ({sorted_scouting_dataset[[event, "Event"]] == "Faceoff Win" & 
      {sorted_scouting_dataset[[event, "Home.Team.Skaters"]] == 5 & 
          sorted_scouting_dataset[[event, "Away.Team.Skaters"]] == 5} }) {
    #If the event is a faceoff win 
    #then a possession is guaranteed to have ended.
    if (((event - 1) - previous_possession_end) > 0) {
      possession <- sorted_scouting_dataset[previous_possession_end:(event - 1), ]
      possession <- possession %>%
        subset(Event %in% xTT_events)
      players_xTT_chain <- find_possession_value(possession, players_xTT_chain)
      
    } else if (((event - 1) - previous_possession_end) == 0) {
      #If the possession only lasted one event,
      #then the xTT Chain of that possession is just that player's personal
      #contribution.
      possession <- sorted_scouting_dataset[(event - 1), ]
      possession <- possession %>%
        subset(Event %in% xTT_events)
      players_xTT_chain <- single_event_value(possession, players_xTT_chain) 

    }
    team_with_possession <- sorted_scouting_dataset[[event, "Team"]]
    #Assign the team with possession to the one who currently has the puck.
    previous_possession_end <- event
    #And note that the previous possession ends here.
    
  #WHEN POSSESSIONS END BETWEEN STOPPAGES  
  } else if ({sorted_scouting_dataset[[event, "Team"]] != team_with_possession & 
      sorted_scouting_dataset[[event, "Event"]] != "Faceoff Win"}){
    #if the team changes between faceoffs, then it's the end of a possession.
    if (((event - 1) - previous_possession_end) > 0) {
      #Checking if the possession lasts longer than one event.
      possession <- sorted_scouting_dataset[previous_possession_end:(event - 1), ]
      possession <- possession %>%
        subset(Event %in% xTT_events)
      players_xTT_chain <- find_possession_value(possession, players_xTT_chain)
      
    } else if (((event - 1) - previous_possession_end) == 0) {
      #If the possession only lasted one event,
      #then the xTT Chain of that possession is just that player's personal
      #contribution.
      possession <- sorted_scouting_dataset[event, ]
      possession <- possession %>%
        subset(Event %in% xTT_events)
      players_xTT_chain <- single_event_value(possession, players_xTT_chain)
      
    } else {
      team_with_possession <- sorted_scouting_dataset[[event, "Team"]]
      #Assign the team with possession to the one who currently has the puck.
      previous_possession_end <- event
    }
    team_with_possession <- sorted_scouting_dataset[[event, "Team"]]
    #Assign the team with possession to the one who currently has the puck.
    previous_possession_end <- event
    #And note that the previous possession ends here.
    
  } else if (event == nrow(sorted_scouting_dataset)) {
    #Final case. Go back to the end of the last possession
    #and say it ends at the end of the dataset.
    possession <- sorted_scouting_dataset[previous_possession_end:event, ]
    possession <- possession %>%
      subset(Event %in% xTT_events)
    
    players_xTT_chain <- find_possession_value(possession, players_xTT_chain)
  }
}




#PART 9: RESULTS OF xTT CHAIN
#Finding number of GP per team to adjust to per-game xTT Chain values
#to somewhat normalize our results.

teams_in_games <- full_scouting_dataset %>%
  distinct(game_date, .keep_all = TRUE) %>%
  select(Home.Team, Away.Team)

teams <- tibble(Team = c(union(unique(teams_in_games$Home.Team), unique(teams_in_games$Away.Team))),
                GP = 0)

for (team in 1:nrow(teams)) {
  teams[[team, "GP"]] <- length(which({teams_in_games$Home.Team == teams[[team, "Team"]] | 
      teams_in_games$Away.Team == teams[[team, "Team"]]}))
}
#Results in a df named "Teams" which lists the teams in the dataset and how many games they played.

# Below creates df team_xTT_data with normalized team data
team_xTT_data <- players_xTT_chain[0,]
team_xTT_data <- team_xTT_data %>% ungroup() %>%
  select(Team, Personal.xTT, Team.xTT.Chain, xTT.Chain)
col_names <- names(team_xTT_data)
team_df <- unique(players_xTT_chain[c("Team")])
for (row in 1:nrow(teams)) {
  team_subset <- players_xTT_chain %>% subset({Team==as.character(teams[row, "Team"])})
  team_xTT_data <- team_xTT_data %>% rbind(c(as.character(teams[row, "Team"]), 
                                             sum(team_subset$Personal.xTT)/as.numeric(teams[row,"GP"]), 
                                             sum(team_subset$Team.xTT.Chain)/as.numeric(teams[row,"GP"]), 
                                             sum(team_subset$xTT.Chain)/as.numeric(teams[row,"GP"])))
}
names(team_xTT_data) <- col_names

#convert the team data to numeric type
mycolumns = c('Personal.xTT','Team.xTT.Chain','xTT.Chain')
team_xTT_data[, mycolumns] <- apply(team_xTT_data[, mycolumns], 2, function(x) as.numeric(as.character(x)))

#Normalizing player data by games played.
players_xTT_chain <- players_xTT_chain %>%
  mutate(GP = 0) %>%
  select(Team, Player, GP, Personal.xTT, Team.xTT.Chain, xTT.Chain)

for (player in 1:nrow(players_xTT_chain)) {
  player_team <- players_xTT_chain[[player, "Team"]]
  team <- which(teams$Team == player_team)
  players_xTT_chain[[player, "GP"]] <- teams[[team, "GP"]]
}

players_xTT_chain <- players_xTT_chain %>%
  mutate(Normalized.Personal = (Personal.xTT / GP),
         Normalized.Team = (Team.xTT.Chain / GP), 
         Normalized.xTT.Chain = (xTT.Chain / GP))

larger_sampled_chain <- players_xTT_chain %>%
  subset(GP > 1) %>%
  select(Team, Player, Normalized.Personal, Normalized.Team, Normalized.xTT.Chain) %>%
  arrange(desc(Normalized.xTT.Chain))

top_10 <- larger_sampled_chain[1:10, ]
#for our results: the top 10 players in terms of xTT Chain per game played.
write.csv(top_10, file = "top_10_players.csv")
  

#larger_sampled_chain restricts to players who played more than 1 game
#with the idea that it's a small enough sample size and we can't
#draw enough conclusions from any 1 game. 40 is hard enough, but 2 is the
#best we can do.

  
mycolumns = c('Personal.xTT','Team.xTT.Chain','xTT.Chain','Normalized.Personal','Normalized.Team','Normalized.xTT.Chain')
players_xTT_chain[, mycolumns] <- apply(players_xTT_chain[, mycolumns], 2, function(x) as.numeric(as.character(x)))

team_bar_chart <- ggplot(team_xTT_data, aes(reorder(Team, -xTT.Chain), xTT.Chain)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab('Team') +
  ylab('xTT Chain')
team_bar_chart

player_scatter_plot <- ggplot(players_xTT_chain, aes(Normalized.Personal, Normalized.Team)) +
  geom_point(shape=21, alpha=0.6, color='white', fill='red', size=1.25) +
  xlab('Individual xTT') +
  ylab('Team xTT') +
  #stat_smooth(method = lm, se=FALSE, size=0.4) +
  theme(aspect.ratio=0.95/1.7) +
  coord_cartesian(xlim = c(-0.4, 1.3), ylim = c(-0.35, 0.6))
player_scatter_plot
ggsave("BigDataCup/player_scatter_plot1.png")
#colour = factor(Team) (if you want teams as different colors, but looks messy)

player_scatter_plot2 <- ggplot(players_xTT_chain, aes(Normalized.Personal, Normalized.xTT.Chain)) +
  geom_point(shape=21, alpha=0.6, color='white', fill='red', size=1) +
  xlab('Individual xTT') +
  ylab('xTT Chain') +
  stat_smooth(method = lm, se=FALSE, size=0.4) +
  theme(aspect.ratio=1.25/1.8) +
  coord_cartesian(xlim = c(-0.5, 1.3), ylim = c(-0.25, 1))
player_scatter_plot2
ggsave("BigDataCup/player_scatter_plot2.png")

#Quick checks:
sprintf("Half the total sum of personal and team: %f", 0.5*(sum(players_xTT_chain$Personal.xTT)+sum(players_xTT_chain$Team.xTT.Chain)))
sprintf("Sum of xTT Chain: %f", sum(players_xTT_chain$xTT.Chain))
sprintf("Half of the total sum of normalized personal and team: %f", 0.5*(sum(players_xTT_chain$Normalized.Personal)+sum(players_xTT_chain$Normalized.Team)))
sprintf("Normalized sum of xTT Chain: %f", sum(players_xTT_chain$Normalized.xTT.Chain))






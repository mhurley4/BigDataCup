#Big Data Cup xTT Model
#Avery Ellis and Matt Hurley
#2/22/2021




#PART 1: SETUP

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
#this pulls all the data from the scouting dataset and then subsets it to all our events we use for the xT model


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
        scouting_dataset[row, ]$X.Coordinate.2, scouting_dataset[row, ]$Y.Coordinate.2,
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

#convert the coordinate data to numeric type
coordcolumns = c('X.Coordinate','X.Coordinate.2','Y.Coordinate','Y.Coordinate.2')
model_events[, coordcolumns] <- apply(model_events[, coordcolumns], 2, function(x) as.numeric(as.character(x)))
#left the below in as comments in case you want to try to get it to work
#trying to make this work but unsure how to fix it
#model_events <- model_events %>% mutate_at(
  #vars(contains("Coordinate")), funs(as.numeric())
#)



#PART 2: CLEANING DATA 
#splitting our events into 5x5 bins that cover the entire rink

model_events <- model_events %>% mutate(
    Rounded.X.Location = (X.Coordinate %/% 5),
    Rounded.Y.Location = (Y.Coordinate %/% 5),
    Rounded.X.Location.2 = (X.Coordinate.2 %/% 5),
    Rounded.Y.Location.2 = (Y.Coordinate.2 %/% 5)
  )
#divides the rink into 697 5x5 squares to condense data since we don't have enough data to get more granular
#Future Consideration w' more data: divide smaller (3x3?)

model_events <- model_events %>% mutate(
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
  #of our xTT!
}




#PART 6: MORE ITERATIONS
#Singh used 5 iterations--we're gonna try that and see if our model converges to our liking.
xTT <- xTT %>%
  mutate(xTT3 = 0,
         xTT4 = 0,
         xTT5 = 0)
#adding in new columns to calculate each new iteration of xTT.

#The majority of this loop is just copy-pasted from part 7.
#So I've added small comments on the new stuff, and took out
#the stuff I've already talked about.
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
for (row in (1:nrow(xTT_plotting))) {
  xTT_plotting <- xTT_plotting %>% mutate(xTT5 = ifelse((xTT5 > upperlimit), (upperlimit), (xTT5)), xTT5 = ifelse((xTT5 < lowerlimit),(lowerlimit),(xTT5)))
}
#sets limits of colormap

color1 = "blue" # low color
color2 = "red" # high color
#some possible colors: turquoise, blue, mediumblue, red, sienna1

more_iter_viz <-
  nhl_rink_plot()+ 
  theme_void()+
  geom_tile(data = (xTT_plotting %>% filter(
      Bin %nin% c(0, 1, 2, 14, 15, 16, 17, 18, 32, 33, 34, 50, 646, 662, 663, 664, 678, 679, 680, 681, 682, 694, 695, 696))), 
      aes(x = Approx.X.Location, y = Approx.Y.Location, fill = xTT5),
      alpha = 0.55)+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5, face = "italic"))+
  scale_fill_gradient(low = color1, high = color2)+
  labs(x = "", y = "", title = "OHL Expected Total Threat (xTT)",
       fill = "xTT of Zone",
       caption = "Viz by Avery Ellis and Matt Hurley; Data via Stathletes, color1/color2") #can delete colors from caption later

more_iter_viz

#Majority of regions are positive or (barely) negative, with a few zeroes.
#Concentration in o-zone is in slot, with the majority of the d-zone being negative.




#PART 7: Applying xTT
model_events <- model_events %>%
  mutate(xTT = 0,
         xTT.2 = 0,
         xTT.Change = 0)
#adding in columns to model_events to describe how the xTT changes in an event

for (event in 1:nrow(model_events)) {
  model_events[[event, "xTT"]] <- xTT[[(model_events$Bin[event]), "xTT5"]]
  if(is.na(model_events$Bin.2[event])) {
    next()
  }
  model_events[[event, "xTT.2"]] <- xTT[[(model_events$Bin.2[event]), "xTT5"]]
}
#finding the xTT values before and after the event

model_events <- model_events %>%
  subset(Event %in% c("Play", "Carry", "Takeaway", "Failed Play")) %>%
  mutate(xTT.Change = ifelse(
    (Event %in% c("Play", "Carry")), (xTT.2 - xTT),
    ifelse((Event == "Failed Play"), -(xTT.2 + xTT),
           xTT))
  )
#calculating the change in xTT from those values. 
#Carries and Plays get you the pure change in xTT.
#Failed Plays get you the sum of the xTT, flipped negative;
#this is because the s
#Takeaways give you the xTT where they occur.

plays <- model_events %>%
  subset(Event == "Play")
carries <- model_events %>%
  subset(Event == "Carry")
takeaways <- model_events %>%
  subset(Event == "Takeaway")
failed_plays <- model_events %>%
  subset(Event == "Failed Play")

total_xTT <- model_events %>%
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

players_xTT <- plyr::join_all(list(total_xTT, plays_xTT, carries_xTT, takeaways_xTT, failed_plays_xTT)) %>%
  replace(is.na(.), 0)
#results in a df with all players in the dataset, and the xTT they generate, broken down by event type
#and also with the averages
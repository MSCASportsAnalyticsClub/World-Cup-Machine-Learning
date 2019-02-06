# Number of passes leading up to a shot
# Time duration of the sequence
# Binary was the pass sequence started by interception (in play or not) (interception)
# Categorical location of possession gain
# Binary whether the shot taker was under pressure  (pressure)
# Body part of the shot
# Location X,Y of where shot occurred
  #Distance from goal
  #Angle from
# Distance traveled
  #Distance from goal start - distance from goal shot
# Distinct number of players involved



#Data one row for each shot, becomes a typical classification problem
#Obs 1, 5 passes, 2 minutes, 30 height, result = goal


#Step 1 -- Pull in body part and location and create so it can be sourced and everyone has access to
          # pass_sequence csv to do their own feature engineering


#Step 3 -- Engineer above features
#Step 4 -- Build models -- Random Forest, XG Boost etc.

####################################################################################################### I ### Clinical mining----


# Will need to mofy dataframe name later when everything is merge

# Age per Gender
Demographics %>% 
  ggplot(aes(x= Sex, y=AgeAtFirstContact), fill= Sex)+
  geom_boxplot(color= c("purple3", "royalblue2")) +
  labs(x="Gender", y="Age at First Contact", title="Age repartition per gender")

# Age per Race
Demographics %>% 
  ggplot(aes(x= Race, y=AgeAtFirstContact), fill= Race)+
  geom_boxplot() +
  coord_flip() +
  labs(x="Gender", y="Age at First Contact", title="Age repartition per gender")


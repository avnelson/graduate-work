HW2data <- read.csv("/Users/avanelson/Desktop/IST 707 Data Analytics/WK2 HW/data-storyteller.csv")
35*0.75 #Establish that students are currently 26 lessons into the term.
str(HW2data)
any(is.na(HW2data)) #there are no NA values in this data 

#----------------------
#CLEANING DATA: Convert Intergers to Numeric
HW2data$Very.Ahead..5 <- as.numeric(HW2data$Very.Ahead..5)
HW2data$Middling..0 <- as.numeric(HW2data$Middling..0)
HW2data$Behind..1.5 <- as.numeric(HW2data$Behind..1.5)
HW2data$More.Behind..6.10 <- as.numeric(HW2data$More.Behind..6.10)
HW2data$Very.Behind..11 <- as.numeric(HW2data$Very.Behind..11)
HW2data$Completed <- as.numeric(HW2data$Completed)
HW2data$Section <- as.factor(HW2data$Section) #make section a factor, as it is descriptive
colnames(HW2data)[3:8] <- c("Very_Ahead", "Middling", "Behind", "More_Behind", "Very_Behind", "Completed") #rename columns

HW2data$Total_Students <- rowSums(HW2data[,3:8]) #create total # students as own column to enable further analysis
max(HW2data$Total_Students) - min(HW2data$Total_Students)

HW2data$School_Section <- paste(HW2data$School, HW2data$Section) #put school and section into own column
#----------------------

#-------CONVERT INTO PERCENTAGE----------
HW2data$pMiddling <- (HW2data$Middling/HW2data$Total_Students)*100
HW2data$pVery_Ahead <- (HW2data$Very_Ahead/HW2data$Total_Students)*100
HW2data$pBehind <- (HW2data$Behind/HW2data$Total_Students)*100
HW2data$pMore_Behind <- (HW2data$More_Behind/HW2data$Total_Students)*100
HW2data$pVery_Behind <- (HW2data$Very_Behind/HW2data$Total_Students)*100
HW2data$pCompleted <- (HW2data$Completed/HW2data$Total_Students)*100

max(HW2data$Total_Students)
mean(HW2data$Total_Students[14:25]) #mean of students in School B
HW2data$Total_Students[29] #students in School D



#--------------- SUMMARY STATISTICS -----------------
#Table of Means
SchoolKey <- c("A"="1:13", "B"="14:25", "C"="26:28", "D"="29", "E"="30")
mA <- c(mean(HW2data$pVery_Behind[1:13]), mean(HW2data$pMore_Behind[1:13]), 
             mean(HW2data$pBehind[1:13]), mean(HW2data$pMiddling[1:13]), 
             mean(HW2data$pVery_Ahead[1:13]), mean(HW2data$pCompleted[1:13]))
mB <- c(mean(HW2data$pVery_Behind[14:25]), mean(HW2data$pMore_Behind[14:25]), 
        mean(HW2data$pBehind[14:25]), mean(HW2data$pMiddling[14:25]), 
        mean(HW2data$pVery_Ahead[14:25]), mean(HW2data$pCompleted[14:25]))
mC <- c(mean(HW2data$pVery_Behind[26:28]), mean(HW2data$pMore_Behind[26:28]), 
        mean(HW2data$pBehind[26:28]), mean(HW2data$pMiddling[26:28]), 
        mean(HW2data$pVery_Ahead[26:28]), mean(HW2data$pCompleted[26:28]))
mD <- c(HW2data$pVery_Behind[29], HW2data$pMore_Behind[29], 
        HW2data$pBehind[29], HW2data$pMiddling[29], 
        HW2data$pVery_Ahead[29], HW2data$pCompleted[29])
mE <- c(HW2data$pVery_Behind[30], HW2data$pMore_Behind[30], 
        HW2data$pBehind[30], HW2data$pMiddling[30], 
        HW2data$pVery_Ahead[30], HW2data$pCompleted[30])
meanHW2 <- data.frame(mA, mB, mC, mD, mE)
colnames(meanHW2) <- c("A", "B", "C", "D", "E")
rownames(meanHW2) <- c("Very Behind", "More Behind", "Behind", "Middling", "Very Ahead", "Completed")
meanHW2 <- t(meanHW2)
School <- rownames(meanHW2)
rownames(meanHW2) <- NULL
meanHW2 <- cbind(School, meanHW2)
meanHW2 <- data.frame(meanHW2)
str(meanHW2)
#transform variables to numeric
meanHW2$Very.Behind <- as.numeric(as.character(meanHW2$Very.Behind))
meanHW2$More.Behind <- as.numeric(as.character(meanHW2$More.Behind))
meanHW2$Behind <- as.numeric(as.character(meanHW2$Behind))
meanHW2$Middling <- as.numeric(as.character(meanHW2$Middling))
meanHW2$Very.Ahead <- as.numeric(as.character(meanHW2$Very.Ahead))
meanHW2$Completed <- as.numeric(as.character(meanHW2$Completed))
meanHW2$Sum <- rowSums(meanHW2[,2:7])

OnTime <- rowSums(meanHW2[,5:7]) #percentage of students on time with coursework
OnTime
mean(OnTime)

#stacked barchar
Categorys <- c("Very Behind", "More Behind", "Behind", "Middling", "Very Ahead", "Completed")
ggplot(meanHW2, aes(x=School, y=Sum))+
  geom_point(aes(y=Very.Ahead, color="Very Ahead"))+
  geom_point(aes(y=Middling,color="Middling"))+
  geom_point(aes(y=Behind, color="Behind"))+
  geom_point(aes(y=More.Behind, color="More Behind"))+
  geom_point(aes(y=Very.Behind, color="Very Behind"))+
  geom_point(aes(y=Completed, color="Completed")) +
  ggtitle("Completion Average by School") +
  labs(x = "School",
       y = "Percentage of Students",
       color = "Level of Completion") +
  scale_color_manual(values = colorsRG)
               
  

#--------------- PLOT DATA -----------------
#--------------- FULL PLOT -----------------
colors <- c("Very Ahead"="#332B3D", "Middling"="#415977", "Behind"="#6E8A78", "More Behind"="#E6D69E", "Very Behind"="#CD6264", "Completed"="#258CA2")
ggplot(HW2data) +
  geom_point(aes(HW2data$School_Section, HW2data$Very_Ahead, color="Very Ahead", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$Middling,color="Middling", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$Behind, color="Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$More_Behind, color="More Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$Very_Behind, color="Very Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$Completed, color="Completed", shape=HW2data$School)) +
  ggtitle("Distribution of Students based on Level of Completion") +
  labs(x = "Class",
       y = "Number of Students",
       color = "Level of Completion",
       shape = "School") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = colorsRG) 

#--------------- FULL PLOT AS PERCENTAGE -----------------
colors <- c("Very Ahead"="#332B3D", "Middling"="#415977", "Behind"="#6E8A78", "More Behind"="#E6D69E", "Very Behind"="#CD6264", "Completed"="#258CA2")
ggplot(HW2data) +
  geom_point(aes(HW2data$School_Section, HW2data$pVery_Ahead, color="Very Ahead", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$pMiddling,color="Middling", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$pBehind, color="Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$pMore_Behind, color="More Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$pVery_Behind, color="Very Behind", shape=HW2data$School))+
  geom_point(aes(HW2data$School_Section, HW2data$pCompleted, color="Completed", shape=HW2data$School)) +
  ggtitle("Distribution of Students based on Level of Completion") +
  labs(x = "Class",
       y = "Percentage of Students",
       color = "Level of Completion",
       shape = "School") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = colorsRG)

#--------------- FULL PLOT AS PERCENTAGE WITH FACET -----------------
colorsRG <- c("Very Ahead"="#7cc300", "Middling"="#a9a900", "Behind"="#c98b00", "More Behind"="#df6900", "Very Behind"="#ea3e1b", "Completed"="#1ad945")
ggplot(HW2data) +
  geom_point(aes(HW2data$Section, HW2data$pVery_Ahead, color="Very Ahead"))+
  geom_point(aes(HW2data$Section, HW2data$pMiddling,color="Middling"))+
  geom_point(aes(HW2data$Section, HW2data$pBehind, color="Behind"))+
  geom_point(aes(HW2data$Section, HW2data$pMore_Behind, color="More Behind"))+
  geom_point(aes(HW2data$Section, HW2data$pVery_Behind, color="Very Behind"))+
  geom_point(aes(HW2data$Section, HW2data$pCompleted, color="Completed")) +
  facet_wrap(HW2data$School ~ ., ncol=2, scales="free") +
  scale_y_continuous(limits=c(0,100)) +
  ggtitle("Distribution of Students based on Level of Completion") +
  labs(x = "Section",
       y = "Percentage of Students",
       color = "Level of Completion") +
  scale_color_manual(values = colorsRG)




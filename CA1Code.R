# Creating vectors for the scores with and without visual aid
no_visual_aid <- c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)
with_visual_aid <- c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)

# Creating the data frame
lecture_scores <- data.frame(No_Visual_Aid = no_visual_aid, With_Visual_Aid = with_visual_aid)

print(lecture_scores)

#Loading the library for the boxplots
install.packages("ggplot2")
library(ggplot2)

#Creating the boxplots
#Without visual aid
ggplot(lecture_scores, aes(x = factor(1), y = No_Visual_Aid)) + 
  geom_boxplot(fill = "lightblue") + 
  labs(x = "Condition", y = "Scores", title = "Scores without visual aid")

#With visual aid
ggplot(lecture_scores, aes(x = factor(1), y = With_Visual_Aid)) + 
  geom_boxplot(fill = "lightgreen") + 
  labs(x = "Condition", y = "Scores", title = "Scores with visual aid")

#Installing package for skewness
install.packages("psych")

#Shapiro Wil test for normality
shapiro_test_no_visual_aid <- shapiro.test(lecture_scores$No_Visual_Aid)
shapiro_test_with_visual_aid <- shapiro.test(lecture_scores$With_Visual_Aid)

#Calculating mean, median and skewness for no visual aid
mean_no_visual_aid <- mean(lecture_scores$No_Visual_Aid)
median_no_visual_aid <- median(lecture_scores$No_Visual_Aid)
skewness_no_visual_aid <- psych::skew(lecture_scores$No_Visual_Aid)

#Calculating mean, median and skewness with visual aid
mean_with_visual_aid <- mean(lecture_scores$With_Visual_Aid)
median_with_visual_aid <- median(lecture_scores$With_Visual_Aid)
skewness_with_visual_aid <- psych::skew(lecture_scores$With_Visual_Aid)

#Printing results for no visual aid
cat("Results for scores without visual aids:\n")
cat("Shapiro-Wilk p-value:", shapiro_test_no_visual_aid$p.value, "\n")
cat("Mean:", mean_no_visual_aid, "\n")
cat("Median:", median_no_visual_aid, "\n")
cat("Skewness:", skewness_no_visual_aid, "\n\n")

#Printing results with visual aid
cat("Results for scores with visual aids:\n")
cat("Shapiro-Wilk p-value:", shapiro_test_with_visual_aid$p.value, "\n")
cat("Mean:", mean_with_visual_aid, "\n")
cat("Median:", median_with_visual_aid, "\n")
cat("Skewness:", skewness_with_visual_aid, "\n")

# Calculate deviation
sd_no_visual_aid <- sd(lecture_scores$No_Visual_Aid)
sd_with_visual_aid <- sd(lecture_scores$With_Visual_Aid)

# Calculate confidence intervals
ci_no_visual_aid <- t.test(lecture_scores$No_Visual_Aid)$conf.int
ci_with_visual_aid <- t.test(lecture_scores$With_Visual_Aid)$conf.int

# Print results
cat("Standard deviation without visual aids:", sd_no_visual_aid, "\n")
cat("95% Confidence interval without visual aids:", ci_no_visual_aid, "\n\n")

cat("Standard deviation with visual aids:", sd_with_visual_aid, "\n")
cat("95% Confidence interval with visual aids:", ci_with_visual_aid, "\n")


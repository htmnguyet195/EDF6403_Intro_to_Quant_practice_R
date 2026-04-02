# Example 1: 2 x 2 with interaction
# Interaction analysis with balanced group
# Step 0: Load library
library(psych)
library(haven)
library(car)
library(emmeans)
library(ggplot2)
library(effectsize)
library(moments)
library(scales)
library(nortest)

# Step 1: Read the data
data22int <- read_sav("data/2x2_bal_int.sav")
View(data22int)
# Labeling the data
data22int$FA <- factor(data22int$FA, levels = c(1, 2), labels = c("A1", "A2"))
data22int$FB <- factor(data22int$FB, levels = c(1, 2), labels = c("B1", "B2"))
# Descriptive statistics for the four groups
describeBy(data22int$Score, list(data22int$FA, data22int$FB))

# Step 2: Running the model
model = lm(Score ~ FA * FB, data = data22int, contrasts=list(FA=contr.sum, FB=contr.sum))
anova_model <- Anova(model, type = "III")

# Step 3: Check homogeneity assumption
levene_test_results <- leveneTest(Score ~ FA*FB, data = data22int)
levene_test_results

# Step 4: Checking normality
data22int$residuals <- residuals(model)
residuals <- data22int$residuals
# 4.1. Use skewness and kurtosis statistics
skew_val <- skew(residuals, type = 2)
kurt_val <- kurtosi(residuals, type = 2)
print(paste("Skewness:", number(skew_val, accuracy = 0.01)))
print(paste("Kurtosis:", number(kurt_val, accuracy = 0.01)))
# 4.2. A histogram with a normal curve overlayed on top of it
# 4.2.1. Calculate the statistics
m   <- mean(residuals, na.rm = TRUE)
md  <- median(residuals, na.rm = TRUE)
std <- sd(residuals, na.rm = TRUE)
# 4.2.2. Set up the histogram
h <- hist(residuals, plot=FALSE)
plot(h, col="white", border="white", main="",
     xlab="", ylab="", axes=FALSE,
     ylim=c(0, max(h$counts) * 1.2))
grid(nx=NA, ny=NULL, col="gray", lty="dotted")
par(new=TRUE)
hist(residuals, freq=TRUE,
     col = "lightblue", border = "black",
     xlab="SA3 residuals",
     main="Histogram of the residuals",
     cex.main = 0.9,
     ylim=c(0, max(h$counts) * 1.2))
# 4.2.3. Add the normal curve
scaling_factor <- length(residuals) * diff(h$breaks)[1]
curve(dnorm(x, mean=m, sd=std) * scaling_factor,
      col="darkred", lwd=2, add=TRUE, yaxt="n")
abline(v = m, col = "darkblue", lwd = 2, lty = 2)
# 4.2.4. Add a Legend
legend("topright", legend = c(
  paste("Mean =", sprintf("%.2f", m)),
  paste("SD =", sprintf("%.2f", std)),
  paste("Median =", sprintf("%.2f", md)),
  "Normal Curve"),
  col = c("darkblue", "transparent", "transparent","darkred"),
  lty = c(2, 0, 0, 1), lwd = c(2, 0, 0, 2),
  bty = "n", cex = 0.8)
box(bty = "l")
# 4.3. A QQplot
# 4.3.1. Draw the points
qqnorm(residuals, main = "Q-Q Plot of data residuals",
       xlab = "Theoretical Quantiles (Normal Distribution)",
       ylab = "Observed data residuals",
       pch = 19, col = "steelblue")
# 4.3.2. Add the reference line
qqline(residuals, col = "darkred", lwd = 2)

# 4.4. The Lilliefors-corrected KS test (n = 80 > 50)
lillie_test_result <- lillie.test(residuals)
lillie_test_result

# Step 5: ANOVA table and effect size:
anova_model
eta_squared <- eta_squared(anova_model, partial = TRUE)
eta_squared

# Step 6: Bonferroni post hoc test
emmeans_model <- emmeans(model, ~ FA * FB)
bonferroni_result1 <- contrast(emmeans_model, method = "pairwise", adjust = "bonferroni", by="FA")
bonferroni_result1
bonferroni_result2 <- contrast(emmeans_model, method = "pairwise", adjust = "bonferroni", by="FB")
bonferroni_result2

# Step 7: Graphs
emm <- emmeans(model, ~ FA * FB)
emm_df <- as.data.frame(emm)
emm_df

ggplot(emm_df, aes(x = FA, y = emmean, group = FB, color = FB)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Interaction Plot: Estimated Marginal Means",
       x = "Factor A (FA)",
       y = "Estimated Marginal Means",
       color = "Factor B (FB)") +
  theme_minimal()

## Insect traits survey data
# For insect traits database paper
#
# Kayla I Perry
# 30 June 2026


dat <- read.csv("Insect traits survey data 2025.csv")
str(dat)

library(reshape2)
library(ggplot2)
library(tidyverse)
library(dplyr)

#### Question 1 ----
# What realm is most of your work focused on? 

field <- melt(dat[,3:5])
colnames(field) <- c("Category","Response")

str(field)

summary_data_field <- field %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

summary_data_field_sorted <- summary_data_field[order(summary_data_field$Total_Count, decreasing = TRUE),]
summary_data_field_sorted$Category <- factor(summary_data_field_sorted$Category, 
                                    levels = summary_data_field_sorted$Category)

barplot(Total_Count ~ Category, data = summary_data_field, col = c("palegreen3", "burlywood4", "steelblue2"),
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 1.5, ylim = c(0,250))
barplot(Total_Count ~ Category, data = summary_data_field_sorted, col = c("palegreen3", "burlywood4", "steelblue2"),
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 1.5, ylim = c(0,250))


#### Question 2 ----
# What insect group(s) do you primarily work on?

colSums(dat[,7:19])
taxa <- melt(dat[,7:19])
colnames(taxa) <- c("Taxon","Response")

str(taxa)

summary_taxa <- taxa %>%
  group_by(Taxon) %>%
  summarise(Total_Count = sum(Response))
summary_taxa_sorted <- summary_taxa[order(summary_taxa$Total_Count, decreasing = TRUE),]
summary_taxa_sorted$Taxon <- factor(summary_taxa_sorted$Taxon, 
                             levels = summary_taxa_sorted$Taxon)

str(summary_taxa_sorted)

par(mar = c(5, 10, 1, 1))
barplot(Total_Count ~ Taxon, data = summary_taxa_sorted, horiz = TRUE, las = 1, col = "steelblue2",
        ylab = "", xlab = "Number of Responses", cex.lab = 1.5, cex.names = 1.4, cex.axis = 1.5, xlim = c(0,100))


#### Question 7 ----
# What is your relationship to insect trait data?

t.data <- melt(dat[,41:46])
colnames(t.data) <- c("Category","Response")

str(t.data)

summary_t.data <- t.data %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

summary_t.data <- summary_t.data[-c(3:4),]
summary_t.data <- droplevels(summary_t.data)

levels(summary_t.data$Category)
levels(summary_t.data$Category)[levels(summary_t.data$Category)=="Manage"] <- "Manage Database"
levels(summary_t.data$Category)[levels(summary_t.data$Category)=="Never_used"] <- "Never"
levels(summary_t.data$Category)[levels(summary_t.data$Category)=="Collect"] <- "Collect New Trait Data"
levels(summary_t.data$Category)[levels(summary_t.data$Category)=="Existing_trait_data"] <- "Use Existing Trait Data"
levels(summary_t.data$Category)

summary_t.data_sorted <- summary_t.data[order(summary_t.data$Total_Count, decreasing = TRUE),]
summary_t.data_sorted$Category <- factor(summary_t.data_sorted$Category, 
                                    levels = summary_t.data_sorted$Category)

par(mar = c(5, 10, 1, 1))
barplot(Total_Count ~ Category, data = summary_t.data_sorted, horiz = TRUE, las = 1,
        ylab = "", xlab = "Number of Responses", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 2, xlim = c(0,150))



#### Question 8 & 9 ----
# If you have used insect trait data in your work, what types of data have you used?
# What types of insect trait data would you be interested to use in future work?

# Question 8
t.data.past <- melt(dat[,48:56])
colnames(t.data.past) <- c("Category","Response")

str(t.data.past)

summary_t.data.past <- t.data.past %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

barplot(Total_Count ~ Category, data = summary_t.data.past, horiz = TRUE, las = 1,
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 2, xlim = c(0,200))

summary_t.data.past$Use <- rep(c("Past"),each = 9)


# Question 9
t.data.future <- melt(dat[,59:67])
colnames(t.data.future) <- c("Category","Response")

str(t.data.future)

summary_t.data.future <- t.data.future %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

barplot(Total_Count ~ Category, data = summary_t.data.future, horiz = TRUE, las = 1,
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 2, xlim = c(0,200))

summary_t.data.future$Use <- rep(c("Future"),each = 9)

# combine both questions
levels(summary_t.data.past$Category)
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_morphological"] <- "Morphological"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_physiological"] <- "Physiological"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_movement"] <- "Movement"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_behavioral"] <- "Behavioral"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_lifehistory"] <- "Life History"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_interactions"] <- "Interactions"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_trophicposition"] <- "Trophic Position"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_phenological"] <- "Phenological"
levels(summary_t.data.past$Category)[levels(summary_t.data.past$Category)=="used_envpreferences"] <- "Environmental Preferences"
levels(summary_t.data.past$Category)

levels(summary_t.data.future$Category)
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_morphological"] <- "Morphological"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_physiological"] <- "Physiological"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_movement"] <- "Movement"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_behavioral"] <- "Behavioral"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_lifehistory"] <- "Life History"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_interactions"] <- "Interactions"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_trophicposition"] <- "Trophic Position"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_phenological"] <- "Phenological"
levels(summary_t.data.future$Category)[levels(summary_t.data.future$Category)=="want_envpreferences"] <- "Environmental Preferences"
levels(summary_t.data.future$Category)

trait_matrix.past <- summary_t.data.past %>%
  pivot_wider(
    names_from = Category,    # The categorical variable becomes column names
    values_from = Total_Count   # The continuous variable fills the cells
  )

trait_matrix.future <- summary_t.data.future %>%
  pivot_wider(
    names_from = Category,    # The categorical variable becomes column names
    values_from = Total_Count   # The continuous variable fills the cells
  )

summary_traits <- rbind(trait_matrix.past, trait_matrix.future)

summary_traits <- summary_traits %>% column_to_rownames(var = "Use")
summary_traits <- as.matrix(summary_traits)

par(mar = c(5, 11, 1, 1))
barplot(summary_traits,
        beside = TRUE,
        horiz = TRUE,
        las = 1,
        xaxt = "n",
        xlim = c(0,175),
        legend.text = rownames(summary_traits),
        col = c("palegreen3", "steelblue2"))
axis(side = 1, at = seq(0, 175, by = 25))


#### Question 10 ----
# What do you perceive as the primary barrier to you using trait data in your future work?

t.data.barriers <- melt(dat[,70:82])
colnames(t.data.barriers) <- c("Category","Response")

str(t.data.barriers)

summary_t.data.barriers <- t.data.barriers %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

summary_t.data.barriers <- summary_t.data.barriers[-c(3:4),]
summary_t.data.barriers <- summary_t.data.barriers[-c(4:5),]
summary_t.data.barriers <- summary_t.data.barriers[-c(5:7),]
summary_t.data.barriers <- summary_t.data.barriers[-c(6),]
summary_t.data.barriers <- droplevels(summary_t.data.barriers)

str(summary_t.data.barriers)

levels(summary_t.data.barriers$Category)
levels(summary_t.data.barriers$Category)[levels(summary_t.data.barriers$Category)=="Data_availability"] <- "Availability"
levels(summary_t.data.barriers$Category)[levels(summary_t.data.barriers$Category)=="Time_intensive"] <- "Time Intensive"

summary_t.data.barriers_sorted <- summary_t.data.barriers[order(summary_t.data.barriers$Total_Count, decreasing = TRUE),]
summary_t.data.barriers_sorted$Category <- factor(summary_t.data.barriers_sorted$Category, 
                                         levels = summary_t.data.barriers_sorted$Category)

barplot(Total_Count ~ Category, data = summary_t.data.barriers_sorted, horiz = TRUE, las = 1,
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 2, xlim = c(0,125),
        col = "aquamarine3")


#### Question 14 ----
# If you are willing to share data under certain circumstances, what would reduce barriers for you to share trait data?

t.data.share <- melt(dat[,88:91])
colnames(t.data.share) <- c("Category","Response")

str(t.data.share)

summary_t.data.share <- t.data.share %>%
  group_by(Category) %>%
  summarise(Total_Count = sum(Response))

levels(summary_t.data.share$Category)
levels(summary_t.data.share$Category)[levels(summary_t.data.share$Category)=="Standardized_protocol"] <- "Standardized Protocol"
levels(summary_t.data.share$Category)[levels(summary_t.data.share$Category)=="Online_repository"] <- "Online Repository"
levels(summary_t.data.share$Category)[levels(summary_t.data.share$Category)=="Coauthorship"] <- "Co-authorship"
levels(summary_t.data.share$Category)[levels(summary_t.data.share$Category)=="Education_activity"] <- "Educational Materials"
levels(summary_t.data.share$Category)

summary_t.data.share_sorted <- summary_t.data.share[order(summary_t.data.share$Total_Count, decreasing = TRUE),]
summary_t.data.share_sorted$Category <- factor(summary_t.data.share_sorted$Category, 
                                                  levels = summary_t.data.share_sorted$Category)

par(mar = c(5, 10, 1, 1))
barplot(Total_Count ~ Category, data = summary_t.data.share_sorted, horiz = TRUE, las = 1,
        ylab = "", xlab = "", main = "", cex.main = 2.5, cex.lab = 2, cex.axis = 2, xlim = c(0,160))



#### Panel figure ----

png("traits_survey_responses v3.png", width = 2600, height = 3000, pointsize = 30)

par(mfrow=c(3,2))
# c(bottom, left, top, right)
par(mar = c(5, 14, 2, 4))

par(lwd = 3)
barplot(Total_Count ~ Category,
        data = summary_data_field_sorted,
        horiz = TRUE,
        las = 1,
        col = c("gray88", "gray55", "black"),
        ylab = "",
        xlab = "",
        cex.lab = 2,
        cex.names = 1.5,
        cex.axis = 1.5,
        border = "black",
        xlim = c(0,200))
title("Realm working in", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
text(185,3.55, "A", pos = 4, font = 2, cex = 1.8)

par(lwd = 2)
barplot(Total_Count ~ Taxon,
        data = summary_taxa_sorted,
        horiz = TRUE,
        las = 1,
        ylab = "",
        xlab = "",
        cex.lab = 1.5,
        cex.names = 1.5,
        cex.axis = 1.5,
        border = "black",
        xlim = c(0,100))
title("Insect taxa working on", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
text(92,15.35, "B", pos = 4, font = 2, cex = 1.8)

par(lwd = 2)
barplot(Total_Count ~ Category,
        data = summary_t.data_sorted,
        horiz = TRUE,
        las = 1,
        ylab = "",
        xlab = "",
        cex.lab = 2,
        cex.names = 1.5,
        cex.axis = 1.5,
        xaxt = "n",
        border = "black",
        xlim = c(0,140))
axis(side = 1, at = seq(0, 140, by = 20), cex.axis = 1.5)
title("Relationship to trait data", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
text(130,4.7, "C", pos = 4, font = 2, cex = 1.8)

par(lwd = 2)
barplot(summary_traits,
        beside = TRUE,
        horiz = TRUE,
        las = 1,
        ylab = "",
        xlab = "",
        cex.lab = 2,
        cex.names = 1.5,
        xaxt = "n",
        xlim = c(0,175),
        border = "black",
        col = c("gray", "black"))
axis(side = 1, at = seq(0, 175, by = 25), cex.axis = 1.5)
legend(x = 142, y = 26,
       legend = c("Past", "Future"),
       col = c("gray", "black"),
       cex = 1.5,
       pch = 15,
       pt.cex = 4,
       bty = "n",
       x.intersp = 1.25,
       y.intersp = 1.5)
title("Trait data types used and of interest", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
text(162,26.7, "D", pos = 4, font = 2, cex = 1.8)

par(lwd = 2)
barplot(Total_Count ~ Category,
        data = summary_t.data.barriers_sorted,
        horiz = TRUE,
        las = 1,
        ylab = "",
        xlab = "Number of Responses",
        cex.lab = 2,
        cex.names = 1.5,
        cex.axis = 1.5,
        xaxt = "n",
        border = "black",
        xlim = c(0,120))
axis(side = 1, at = seq(0, 120, by = 20), cex.axis = 1.5)
title("Perceived barriers", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
text(111.5,5.85, "E", pos = 4, font = 2, cex = 1.8)

par(lwd = 2)
barplot(Total_Count ~ Category,
        data = summary_t.data.share_sorted,
        horiz = TRUE,
        las = 1,
        ylab = "",
        xlab = "Number of Responses",
        cex.lab = 2,
        cex.names = 1.5,
        cex.axis = 1.5,
        xaxt = "n",
        border = "black",
        xlim = c(0,160))
title("Preferred means of barrier reduction", cex.main = 2, line = 0.5, adj = 0)
box(col = "black", lwd = 1, lty = "solid")
axis(side = 1, at = seq(0, 160, by = 20), cex.axis = 1.5)
text(148.5,4.7, "F", pos = 4, font = 2, cex = 1.8)

dev.off()

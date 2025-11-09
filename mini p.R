library(tidyverse)

file_path <- "C:/Users/SURAJ/Desktop/DMA/Peer Relationships and Availability of Advisors - Student Survey.csv"

survey_data <- read_csv(file_path)

data_clean <- survey_data %>%
  rename(
    gender = `Gender`,
    peer_collab = `How often do you study or collaborate with peers on academic work?`,
    peer_relationship = `How would you describe your overall relationship with peers in your academic environment?`,
    ask_peers = `Do you feel comfortable asking peers for academic help?`,
    peer_challenges = `Have you ever faced challenges in building or maintaining peer relationships? (Select all that apply)`,
    seek_advisor = `How often do you seek advice from academic advisors?`,
    advisor_access = `How accessible are your academic advisors when you need guidance?`,
    advisor_contact = `Which methods do you usually use to contact academic advisors? (Select all that apply)`,
    advisor_difficulty = `What is the biggest difficulty you face in approaching academic advisors?`,
    satisfaction_score = `On a scale of 1 to 10, how would you rate your overall satisfaction with academic support (both from peers and advisors)?`
  )

relationship_levels <- c("Very unsupportive", "Unsupportive", "Neutral", "Supportive", "Very supportive")
access_levels <- c("Very inaccessible", "Somewhat inaccessible", "Neutral", "Somewhat accessible", "Very accessible")
frequency_levels <- c("Never", "Rarely", "Monthly", "Weekly", "Daily")

data_clean <- data_clean %>%
  mutate(
    peer_relationship = factor(peer_relationship, levels = relationship_levels),
    advisor_access = factor(advisor_access, levels = access_levels),
    peer_collab = factor(peer_collab, levels = frequency_levels),
    seek_advisor = factor(seek_advisor, levels = frequency_levels)
  )

glimpse(data_clean)

plot1 <- ggplot(data_clean, aes(x = peer_relationship)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  labs(
    title = "Overall Relationship with Peers",
    x = "Peer Relationship",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot1)

plot2 <- ggplot(data_clean, aes(x = satisfaction_score)) +
  geom_bar(fill = "darkgreen", alpha = 0.8) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Overall Satisfaction with Academic Support",
    x = "Satisfaction Score (1-10)",
    y = "Number of Students"
  ) +
  theme_minimal()

print(plot2)

plot3 <- ggplot(data_clean, aes(x = advisor_access, y = satisfaction_score)) +
  geom_boxplot(fill = "goldenrod", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Satisfaction Score by Advisor Accessibility",
    x = "Advisor Accessibility",
    y = "Satisfaction Score (1-10)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot3)

challenges_summary <- data_clean %>%
  filter(!is.na(peer_challenges)) %>%
  separate_rows(peer_challenges, sep = ";") %>%
  mutate(peer_challenges = str_trim(peer_challenges)) %>%
  count(peer_challenges, sort = TRUE) %>%
  mutate(peer_challenges = fct_reorder(peer_challenges, n))

plot4 <- ggplot(challenges_summary, aes(x = peer_challenges, y = n)) +
  geom_col(fill = "firebrick", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Common Challenges in Peer Relationships",
    x = "Challenge",
    y = "Number of Mentions"
  ) +
  theme_minimal()

print(plot4)

# --- Added More Plots ---

# Plot 5: Gender Distribution (Pie Chart)
gender_summary <- data_clean %>%
  count(gender) %>%
  mutate(
    percentage = n / sum(n),
    label = scales::percent(percentage, accuracy = 0.1)
  )

plot5 <- ggplot(gender_summary, aes(x = "", y = percentage, fill = gender)) +
  geom_col(width = 1, alpha = 0.8) +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            fontface = "bold",
            size = 4) +
  labs(
    title = "Gender Distribution of Respondents",
    fill = "Gender",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.position = "right")

print(plot5)

# Plot 6: Advisor Contact Methods (Select all that apply)
contact_summary <- data_clean %>%
  filter(!is.na(advisor_contact)) %>%
  separate_rows(advisor_contact, sep = ";") %>%
  mutate(advisor_contact = str_trim(advisor_contact)) %>%
  count(advisor_contact, sort = TRUE) %>%
  mutate(advisor_contact = fct_reorder(advisor_contact, n))

plot6 <- ggplot(contact_summary, aes(x = advisor_contact, y = n)) +
  geom_col(fill = "#404080", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Common Advisor Contact Methods",
    x = "Contact Method",
    y = "Number of Mentions"
  ) +
  theme_minimal()

print(plot6)

# Plot 7: Biggest Difficulty Approaching Advisors
difficulty_summary <- data_clean %>%
  filter(!is.na(advisor_difficulty)) %>%
  count(advisor_difficulty, sort = TRUE) %>%
  mutate(advisor_difficulty = fct_reorder(advisor_difficulty, n))

plot7 <- ggplot(difficulty_summary, aes(x = advisor_difficulty, y = n)) +
  geom_col(fill = "#E69F00", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Biggest Difficulty Approaching Advisors",
    x = "Difficulty",
    y = "Number of Students"
  ) +
  theme_minimal()

print(plot7)

# Plot 8: Satisfaction Score by Gender
plot8 <- ggplot(data_clean, aes(x = gender, y = satisfaction_score)) +
  geom_boxplot(fill = "#56B4E9", alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.4) +
  labs(
    title = "Satisfaction Score by Gender",
    x = "Gender",
    y = "Satisfaction Score (1-10)"
  ) +
  theme_minimal()

print(plot8)



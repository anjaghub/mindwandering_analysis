# Installing all necessary packages
  install.packages("tidyverse")
  install.packages("hrbrthemes")
  install.packages("plotly")
  install.packages("ggplot2")
  install.packages("viridis")
  install.packages("dplyr")
  library(tidyverse)
  library(hrbrthemes)
  library(plotly)
  library(ggplot2)
  library(viridis)
  library(dplyr)

# Loading the data and renaming the columns
  mw_data <- read.csv("/Users/anja/Documents/mw_data.csv")
  colnames(mw_data) <- c("timestamp",
                         "id",
                         "sampling_method",
                         "length",
                         "social",
                         "interaction_type",
                         "felt_purpose",
                         "emotions",
                         "emotion_valence",
                         "emotion_intensity",
                         "agency_start_end",
                         "agency_meanwhile",
                         "stepping_out",
                         "environment_awareness",
                         "mood",
                         "context",
                         "content",
                         "comment")

# Change datatypes
  mw_data["id"] <- as.character(mw_data[,"id"])
  
# sample size
  sample_size = mw_data %>% group_by(id) %>% summarize(num=n())

# Boxplots  
# Create plot - How easily could you have stepped out of mind wandering?
  mw_data %>%
      left_join(sample_size) %>%
      mutate(myaxis = paste0(id, "\n", "n=", num)) %>%
      ggplot( aes(x=myaxis, y=stepping_out, fill=id)) +
      geom_violin(width=1.4) +
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_fill_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("How easily could you have stepped out of mind wandering?") +
      xlab("") 
  
# Create plot - environment awareness
  mw_data %>%
    left_join(sample_size) %>%
    mutate(myaxis = paste0(id, "\n", "n=", num)) %>%
    ggplot( aes(x=myaxis, y=environment_awareness, fill=id)) +
    geom_violin(width=1.4) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("How aware were you about your environment?") +
    xlab("") 
    

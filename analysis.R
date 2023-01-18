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
  mw_data <- read.csv("/Users/anja/Documents/GitHub/mindwandering_analysis/mw_data.csv")
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
    
# Are social mind wanderings more difficult to leave? 
# Create vector with IDs to run through loop
  ids <- unique(mw_data[,"id"])
  
# Create empty list to store plots
  plot_list = list()

# Loop to create plot per person  
  for(i in ids){
    new_data <- mw_data[mw_data$id == i, ] 
    sample_size = new_data %>% group_by(social) %>% summarize(num=n())
    
    p <- new_data %>% 
      left_join(sample_size) %>%
      mutate(myaxis = paste0(social, "\n", "n=", num)) %>%
      ggplot( aes(x=myaxis, y=stepping_out, fill=social)) +
      geom_violin(width=0.8) +
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_fill_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("How easily could you have stepped out a social vs. a non social mind wandering?") +
      xlab("") 
    
    plot_list[[i]] = p
  }

# Loop to store each visualization locally
  for (i in ids) {
    file_name = paste("social_plot_", i, ".tiff", sep="")
    tiff(file_name)
    print(plot_list[[i]])
    dev.off()
  }  
  
# Are emotional mind wanderings more difficult to leave? 
  
# Create empty list to store plots
  plot_list = list()
  
# Loop to create plot per person  
  for(i in ids){
    new_data <- mw_data[mw_data$id == i, ] 
    sample_size = new_data %>% group_by(emotions) %>% summarize(num=n())
    
    p <- new_data %>% 
      left_join(sample_size) %>%
      mutate(myaxis = paste0(emotions, "\n", "n=", num)) %>%
      ggplot( aes(x=myaxis, y=stepping_out, fill=emotions)) +
      geom_violin(width=0.8) +
      geom_boxplot(width=0.1, color="grey", alpha=0.2) +
      scale_fill_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position="none",
        plot.title = element_text(size=11)
      ) +
      ggtitle("How easily could you have stepped out an emotional vs. a non emotional mind wandering?") +
      xlab("") 
    
    plot_list[[i]] = p
  }
  
  # Loop to store each visualization locally
  for (i in ids) {
    file_name = paste("emotional_plot_", i, ".tiff", sep="")
    tiff(file_name)
    print(plot_list[[i]])
    dev.off()
  }  

# Are emotionally intense mind wanderings deeper?
# Filter for only low and intense mind wanderings
  target <- c(1,2,6,7)
  new_data <- filter(mw_data, emotion_intensity %in% target)
  
# Create new grouping variable for emotion intensity
  new_data$emotion_intensity_group <- with(new_data, ifelse(emotion_intensity > 2, 'intense', 'low'))

# Create boxplot per emotion intensity group
  sample_size = new_data %>% group_by(emotion_intensity_group) %>% summarize(num=n())
  
  new_data %>% 
    left_join(sample_size) %>%
    mutate(myaxis = paste0(emotion_intensity_group, "\n", "n=", num)) %>%
    ggplot( aes(x=myaxis, y=stepping_out, fill=emotion_intensity_group)) +
    geom_violin(width=0.8) +
    geom_boxplot(width=0.1, color="grey", alpha=0.2) +
    scale_fill_viridis(discrete = TRUE) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("How easily could you have stepped out a low vs. high emotional intense mind wandering?") +
    xlab("") 
  
 
  
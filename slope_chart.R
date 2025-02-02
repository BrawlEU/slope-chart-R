# Libaries --------------------------------------------------------------------

library(ggplot2)
library(scales)
theme_set(theme_classic())
library(ggrepel)

# Prepares the data ------------------------------------------------------------
# Create the dataframe to use for the slope chart, based on a FIRST value and a SECOND value


# Regions
regions  = c("Northern Ireland", "Wales", "Yorkshire & H", "Scotland",
              "South West", "North West", "London", "East of England", "South East")


# Values
First = c(23,38,61,302,435,91,350,225,907)
Last  = c(14,15,116,163,229,164,451,223,606)

# Put Regions, First and Last vectors into dataframe (df)
df  = data.frame(regions, First, Last)

left_label = paste(df$regions, round(df$`First`),sep=", ")
right_label = paste(df$regions, round(df$`Last`),sep=", ")
df$class = ifelse((df$`Last` - df$`First`) < 0, "red", "green")

# Plotting the chart ---------------------------------------------------------------------
p = ggplot(df) + geom_segment(aes(x=1, xend=2, y=`First`, yend=`Last`, col=class), size=0.8, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#EDC073", "red"="#20244C")) +  # color of lines
  #labs(x="", y="GERD") +  # Axis labels
  xlim(.5, 2.5) + 
  ylim(0,(1.1*(max(df$`First`, df$`Last`)))) +  # X and Y axis limits
  theme_void()

# Add text labels to the chart using geom_text_repel. This has arrows in
p = p + geom_text_repel(segment.size = 0.1,arrow = arrow(length = unit(0.01, "npc"),angle =20, type = "open", ends = "last"),
                         label=left_label, y=df$`First`, x=rep(1, NROW(df)), nudge_x = -0.05, hjust='right', direction = 'y', size=2.6)
p = p + geom_text_repel(segment.size = 0.1, arrow = arrow(length = unit(0.01, "npc"),angle =20, type = "open", ends = "first"),
                         label=right_label, y=df$`Last`, x=rep(2, NROW(df)), nudge_y = 1.17,hjust=-0.1,direction = 'y',size=2.6)

p = p + geom_text(label="2011 (£m)", x=1, y=1.1*(max(df$`First`, df$`Last`)), hjust=1.2, size=4)  # title
p = p + geom_text(label="2016 (£m)", x=2, y=1.1*(max(df$`First`, df$`Last`)), hjust=-0.1, size=4)  # title

# Minify theme
p + theme(panel.background = element_blank(), 
          panel.grid = element_blank(),
          #axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(),
          plot.margin = unit(c(1,2,1,2), "cm"))

# When saving the chart as a pdf, this is the ration and height figures used
aspect_ratio = 0.9
height = 7

# Save the chart to your desktop
ggsave("D:/Users/NHoo01/Desktop/slope.pdf",height = 7 , width = 7 * aspect_ratio)

# Close the file
dev.off()

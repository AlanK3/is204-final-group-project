library(RCurl) #to read from url 
library(ggpubr)  #to add tables in plot space
library(gridExtra)


# load dataset
x <- getURL("https://raw.githubusercontent.com/AlanK3/is204-final-group-project/refs/heads/main/data/2024ChicagoTrafficCrashes.csv")
crash_data <- read.csv(text = x)

# add a new column for street blocks (grouping by the same 100 block range)
crash_data <- crash_data %>%
  mutate(STREET_BLOCK = floor(STREET_NO / 100) * 100)

# summarize crash frequency by street and direction
street_summary <- crash_data %>%
  group_by(STREET_NAME, STREET_DIRECTION) %>%
  summarize(TOTAL_CRASHES = n(), .groups = "drop") %>%
  arrange(desc(TOTAL_CRASHES))  # sort by most crashes


# summarize crash frequency by street, direction, and block
block_summary <- crash_data %>%
  group_by(STREET_NAME, STREET_DIRECTION, STREET_BLOCK) %>%
  summarize(TOTAL_CRASHES = n(), .groups = "drop") %>%
  arrange(desc(TOTAL_CRASHES))  # sort by most crashes

# top 10 streets by total crashes
top_streets <- head(street_summary, 10)

# top 10 blocks by total crashes
top_blocks <- head(block_summary, 10)

x <- data.frame(row.names = paste("", 1:10))  # prefix numbers
x[, 1] <- top_streets$STREET_NAME          # street name
x[, 2] <- top_streets$STREET_DIRECTION     # street direction
x[, 3] <- top_streets$TOTAL_CRASHES        # total crashes
colnames(x) <- c("Street Name", "Street Direction", "Total Crashes")

#needs gridExtra
tableStreets <- tableGrob(x)

y <- data.frame(row.names = paste("", 1:10))  # prefix numbers
y[, 1] <- top_blocks$STREET_NAME          # street name
y[, 2] <- top_blocks$STREET_DIRECTION     # street direction
y[, 3] <- paste(top_blocks$STREET_BLOCK, "block")  # block range (ex. "1200 block")
y[, 4] <- top_blocks$TOTAL_CRASHES        # total crashes
colnames(y) <- c("Street Name", "Street Direction", "Block", "Total Crashes")

#needs gridExtra
tableBlocks <- tableGrob(y)

grid.arrange(
  tableStreets, 
  tableBlocks,
  top = NULL,
  bottom = NULL,
  layout_matrix = rbind(c(1), c(2)),
  heights = c(1, 1.2)
)

# send to plot land
grid.arrange(
  arrangeGrob(tableStreets, top = ("Top 10 Streets by Crash Frequency")),
  arrangeGrob(tableBlocks,top = ("Top 10 Blocks by Crash Frequency")),
  ncol = 2
)

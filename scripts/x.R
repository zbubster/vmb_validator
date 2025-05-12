install.packages("networkD3")

library(networkD3)

# Define nodes
nodes <- data.frame(name = c("Source A", "Source B", "Target X", "Target Y"))

# Define links (source and target use 0-based index from nodes)
links <- data.frame(
  source = c(0, 1),     # Source A, Source B
  target = c(3, 2),     # Target X, Target Y
  value = c(10, 20)
)

# Draw Sankey
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 12, nodeWidth = 30)


install.packages("ggalluvial")

library(ggplot2)

library(ggalluvial)

data <- data.frame(
  source = c("A", "A", "B", "B"),
  target = c("X", "Y", "X", "Y"),
  value = c(10, 50, 15, 5)
)

ggplot(data,
       aes(axis1 = source, axis2 = target, y = value)) +
  geom_alluvium(aes(fill = source)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Source", "Target"), expand = c(.1, .1)) +
  theme_minimal()

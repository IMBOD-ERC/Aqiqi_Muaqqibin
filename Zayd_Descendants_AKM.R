#Extracting all descendants of Zayd both male and female
# Load required library

library(tidyverse)
library(dplyr)
#load AKM General
AKM <- read.csv("~/desktop/ACADEMIA/IMBOD/AKM_R/data/data_raw/Aqiqi_Muaqqibin_AKM.csv")

#define search strings
search_Zayd <- c("Zayd", "AKM_0144_IMAM4", "AKM_0003_IMAM3", "AKM_0001_IMAM1")

#select H_Nasab columns
Zayd_Nasab <- grep("^(H_Nasab_|W_Nasab_)", names(AKM), value = TRUE)

# Filter rows where any of the selected columns contain Zayd
Zayd_Descendants <- AKM %>%
  filter(apply(AKM[Zayd_Nasab], 1, function(row) {
    all(sapply(search_Zayd, function(pattern) {
      any(grepl(pattern, row, ignore.case = TRUE))
    }))
  }) | ID_Husband == "AKM_0148")

#Check if it works
print(Zayd_Descendants)

#save Zayd_Descendants as csv
write.csv(Zayd_Descendants, "data/data_processed/Zayd_Descendants_AKM.csv")

library(igraph)

# Now, create the edge list ensuring all relationships point back to Zayd
edges <- data.frame(
  from = c(Zayd_Descendants$ID_Husband, Zayd_Descendants$ID_Husband, Zayd_Descendants$ID_Wife),
  to = c(Zayd_Descendants$ID_Child, Zayd_Descendants$ID_Wife, Zayd_Descendants$ID_Child)
)


# Check if edges were created correctly
if (nrow(edges) == 0) {
  stop("Edge list is empty. No valid relationships were found.")
} else {
  print("Edge list created successfully.")
}

# Remove duplicate edges where the source and target are the same (self-loops)
edges <- edges[edges$from != edges$to, ]

# Optional: Add weights to the edges if necessary (default 1 for simplicity)
edges$weight <- 1

#create graph
g <- graph_from_data_frame(edges, directed = TRUE)


# Customize the appearance (you can adjust based on your preferences)
#plot(g, 
     #vertex.size = 10, 
     #vertex.label.cex = 0.7, 
     #edge.arrow.size = 0.5, 
     #edge.width = E(g)$weight,  # Edge width based on weight
    # main = "Weighted Directed Network of Relationships",
    # layout= layout_as_tree(g))

library(visNetwork)

# Create the mapping from IDs to Arabic names
id_to_arabic <- c(
  setNames(Zayd_Descendants$Husband_Ar, Zayd_Descendants$ID_Husband),  # Husband names
  setNames(Zayd_Descendants$Wife_Ar, Zayd_Descendants$ID_Wife),        # Wife names
  setNames(Zayd_Descendants$Child_Ar, Zayd_Descendants$ID_Child)        # Children names
)

# Check if the mapping is created successfully
head(id_to_arabic)

# Create Nodes
nodes <- data.frame(id = V(g)$name, label = V(g)$name)

# Replace IDs with Arabic names in the label column using the id_to_arabic mapping
nodes$label <- id_to_arabic[nodes$id]

# Assign colors based on roles
nodes$color <- ifelse(nodes$id %in% Zayd_Descendants$ID_Husband, "green",
                      ifelse(nodes$id %in% Zayd_Descendants$ID_Wife, "orange",
                             ifelse(nodes$id %in% Zayd_Descendants$ID_Child, "green", "gray")))

# Create Edges
edges <- data.frame(
  from = c(Zayd_Descendants$ID_Husband, Zayd_Descendants$ID_Husband, Zayd_Descendants$ID_Wife),
  to = c(Zayd_Descendants$ID_Child, Zayd_Descendants$ID_Wife, Zayd_Descendants$ID_Child)
)


# Check if edges were created correctly
if (nrow(edges) == 0) {
  stop("Edge list is empty. No valid relationships were found.")
} else {
  print("Edge list created successfully.")
}

# Remove duplicate edges where the source and target are the same (self-loops)
edges <- edges[edges$from != edges$to, ]

# Optional: Add weights to the edges if necessary (default 1 for simplicity)
edges$weight <- 1

# Identify marriage edges and remove their directionality
marriage_edges <- edges %>%
  filter(from %in% Zayd_Descendants$ID_Husband & to %in% Zayd_Descendants$ID_Wife)

# Remove directionality for marriage edges by creating symmetric edges
marriage_edges <- rbind(marriage_edges, marriage_edges %>% rename(from = to, to = from))

# Combine marriage edges with other edges
non_marriage_edges <- edges %>%
  filter(!(from %in% Zayd_Descendants$ID_Husband & to %in% Zayd_Descendants$ID_Wife))

# Final combined edges
edges <- rbind(non_marriage_edges, marriage_edges)



visNetwork(nodes, edges) %>%
  visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),  # Directed edges for non-marriages
           smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE) %>%
  visLayout(improvedLayout = TRUE) %>%
  visPhysics(stabilization = TRUE, solver = "repulsion", repulsion = list(
    centralGravity = 0.2,
    springLength = 200,  # Increase spacing
    springConstant = 0.05
  )) %>% 
  visNodes(
    size = 10, 
    font = list(size = 12),
    label = nodes$label,
    labelHighlightBold = TRUE,
    color = list(
      background = nodes$color,  # Use the color column for node background
      border = "black"           # Set border color for nodes
    )
  )


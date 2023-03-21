    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 4.2.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tidyr)
    library(ggplot2)
    library(ggraph)

    ## Warning: package 'ggraph' was built under R version 4.2.3

    library(igraph)

    ## Warning: package 'igraph' was built under R version 4.2.2

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

    library(stringr)

    ### Define a matrix with the seat arrangement
    seat_matrix <- matrix(c(NA, "5", NA, NA, 
                            "6", "D", "3", "4", 
                            NA, "B", "C", NA, 
                            NA, "A", NA, NA, 
                            NA, "2", NA, NA, 
                            NA, "1", NA, NA), nrow = 6, byrow = TRUE)

    ### Convert the matrix to a dataframe
    seat_df <- data.frame(seat_matrix)

    ### Rename the column titles
    colnames(seat_df) <- c("1", "2", "3", "4")

    ### Add row names
    seat_df$row <- 1:6

    # Rename the row column to x
    seat_df <- seat_df %>% rename(x = row)

    ### Convert the dataframe to long format
    seat_df_long <- gather(seat_df, key = "y", value = "value", "1":"4", na.rm = TRUE)

    # Convert the column variable to a numeric value
    seat_df_long$y <- as.numeric(seat_df_long$y)

    ### Map the status to "occupied" or "available"
    seat_df_long$status <- ifelse(seat_df_long$value %in% c("1", "2", "3", "4", "5", "6"), 
                                  "occupied", "available")

    ### Create the plot
    ggplot(seat_df_long, aes(x = y, y = x, fill = status)) +
      geom_tile(color = "white", size = 1) +
      geom_text(aes(label = value), color = "black") +
      scale_fill_manual(values = c("green", "red"), na.value = "gray") +
      labs(title = "Seat Arrangement", x = "", y = "") +
      theme_void() +
      theme(legend.position = "bottom") +
      coord_flip()

![](Exercise-2_files/figure-markdown_strict/Creating%20and%20Visualizing%20the%20Data-1.png)

    ### Define a function to get adjacent values
    get_adjacent <- function(x, y) {
      
      ### Calculate the x and y values for the adjacent seats
      x1 <- x - 1
      x2 <- x + 1
      y1 <- y - 1
      y2 <- y + 1
      
      ### Get the adjacent values if they exist
      adj1 <- ifelse(x1 >= 1 & y1 >= 1, 
                     seat_df_long$value[seat_df_long$x == x1 & seat_df_long$y == y1], NA)
      adj2 <- ifelse(y1 >= 1, 
                     seat_df_long$value[seat_df_long$x == x & seat_df_long$y == y1], NA)
      adj3 <- ifelse(x2 <= max(seat_df_long$x) & y1 >= 1, 
                     seat_df_long$value[seat_df_long$x == x2 & seat_df_long$y == y1], NA)
      adj4 <- ifelse(x1 >= 1, 
                     seat_df_long$value[seat_df_long$x == x1 & seat_df_long$y == y], NA)
      adj5 <- ifelse(x2 <= max(seat_df_long$x), 
                     seat_df_long$value[seat_df_long$x == x2 & seat_df_long$y == y], NA)
      adj6 <- ifelse(x1 >= 1 & y2 <= max(seat_df_long$y), 
                     seat_df_long$value[seat_df_long$x == x1 & seat_df_long$y == y2], NA)
      adj7 <- ifelse(y2 <= max(seat_df_long$y), 
                     seat_df_long$value[seat_df_long$x == x & seat_df_long$y == y2], NA)
      adj8 <- ifelse(x2 <= max(seat_df_long$x) & y2 <= max(seat_df_long$y), 
                     seat_df_long$value[seat_df_long$x == x2 & seat_df_long$y == y2], NA)
      
      ### Return the adjacent values as a named list
      return(list(adj1 = adj1, adj2 = adj2, adj3 = adj3, adj4 = adj4, adj5 = adj5, 
                  adj6 = adj6, adj7 = adj7, adj8 = adj8))
    }

    ### Create a new dataframe for adjacent values
    adjacent_df <- data.frame(value = seat_df_long$value)

    ### Loop through each seat and get its adjacent values
    for (i in 1:nrow(seat_df_long)) {
      # Get the x and y values for the seat
      x <- seat_df_long$x[i]
      y <- seat_df_long$y[i]
      
      ### Get the adjacent values for the seat
      adj_values <- get_adjacent(x, y)
      
      ### Add the adjacent values to the adjacent_df dataframe
      adjacent_df[i, c("adj1", "adj2", "adj3", "adj4", "adj5", "adj6", "adj7", "adj8")] <- adj_values
    }

    ### Edges: Convert the adjacent_df to a long format
    adjacent_long <- adjacent_df %>%
      pivot_longer(cols = starts_with("adj"), names_to = "adjacent_num", 
                   values_to = "adjacent_value") %>%
      filter(!is.na(adjacent_value))

    ### Edges: Group by the value and create a comma-separated list of adjacent values
    adjacent_summary <- adjacent_long %>%
      group_by(value) %>%
      summarize(adjacent_values = paste(adjacent_value, collapse = ",")) %>%
      ungroup()

    ### Edges: Create the final dataframe by repeating values if there are multiple adjacents
    edges <- adjacent_summary %>%
      separate_rows(adjacent_values, sep = ",") %>%
      mutate(value = rep(value, times = str_count(adjacent_values, ",") + 1)) %>%
      select(value, adjacent_values)

    ### Nodes
    nodes <- data.frame(value = unique(adjacent_df$value))

    ### Calculate Degree, Closeness, Betweenness
    graph <- graph_from_data_frame(d = edges,
                                   vertices = nodes,
                                   directed = TRUE)

    degree_centrality <- degree(graph, mode="in")

    closeness_centrality <- closeness(graph, mode="all")

    betweenness_centrality <- betweenness(graph)/2

    ### Plot
    V(graph)$label <- paste("ID:", V(graph)$name, "\nBet:", 
                            round(betweenness_centrality,2), "\nClo:", 
                            round(closeness_centrality,3), "\nDeg:", degree_centrality)

    plot(graph, vertex.label=V(graph)$label, vertex.label.cex = 0.7, 
         vertex.label.color = "black")

![](Exercise-2_files/figure-markdown_strict/Plot-1.png)

    ### If the goal is to develop informal connections with coworkers on the bus, meaning, you are connected to the most seats, I should sit in seat B. This is also assuming that all the other seats on the bus fill up. Due to its high degree centrality, seat B provides an optimal opportunity to connect with 5 different people whereas other seats provide less. When compared to other seats which also provide access to 5 different people, seat B has the highest betweenness as well, meaning, seat B has a big influence on the flow of information on this bus as there are other short paths that pass through it, therefore, it may act as a bridge between other paths in the network. 

    ### Although seat B may allow me to develop informal connections, this choice may not be the most beneficial when attempting to develop stronger connections. By sitting next to less people, we may be given the opportunity to get to know them in a more intimate way as we are more focused on solely them. 

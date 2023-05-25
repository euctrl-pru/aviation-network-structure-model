##### Script for running analysis ####
rm(list = ls())

##### Load librairies ####
library(data.table)
library(tidyverse)
library(reticulate)
library(foreach)
library(fdm2id)
library(pruatlas)
set.seed(42)
setwd(
  "C:/Users/juulk/Vrije Universiteit Brussel/FacPE-PhD Juul Vossen - Documenten/Stage Eurocontrol/data/graphwave"
)
graphwave <- import(module = "graphwave")
setwd(
  "C:/Users/juulk/Vrije Universiteit Brussel/FacPE-PhD Juul Vossen - Documenten/Stage Eurocontrol/final_data"
)

np <- import("numpy")
nx <- import("networkx")
pd <- import("pandas")
sk <- import("sklearn")


##### Import Data #####
weeks <- 26:31

# Create sample data
net2019 <- fread("df_2019.csv") %>%
  filter(week %in% weeks) %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2020 <- fread("df_2020.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2021 <- fread("df_2021.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

net2022 <- fread("df_2022.csv") %>%
  filter(week %in% weeks)  %>%
  group_by(origin, destination, week, market_segment,type.origin,type.destination,from_member_status,to_member_status)%>%
  reframe(weight = sum(weight))

fwrite(net2019 ,file = "sample_2019.csv")
fwrite(net2020 ,file = "sample_2020.csv")
fwrite(net2021 ,file = "sample_2021.csv")
fwrite(net2022 ,file = "sample_2022.csv")



# First we select the focal airports of our analysis. Note that we have only flights arriving/departing
# from airports in Europe. Hence, the clustering is only meaningful for European airports, as we
# miss non-EU fligths for non-EU airports.

extract_airports <- function(net) {
  airports_to <- net %>%
    filter(
      type.destination %in% c("medium_airport", "large_airport") &
        !to_member_status == ""
    ) %>%
    select(destination) %>% unique() %>% unlist()
  airports_from <- net %>%
    filter(type.origin %in% c("medium_airport", "large_airport") &
             !from_member_status == "") %>%
    select(origin) %>% unique() %>% unlist()
  airports <- c(airports_from, airports_to) %>% unique()
  return(airports)
}

airports19 <- extract_airports(net2019)
airports20 <- extract_airports(net2020)
airports21 <- extract_airports(net2021)
airports22 <- extract_airports(net2022)



# Remove self loops and connections that contain AFIL (Flight plans received in flight) or
# ZZZZ (Unidentified) as they can create artifiical conenctions between otherwise
# unconnected airports
preprocess_network <-
  function(net,
           remove_self_loops = T,
           remove_AFIL_ZZZZ = T) {
    if (remove_self_loops == T) {
      net <- net %>% filter(origin != destination)
    }
    
    if (remove_AFIL_ZZZZ == T) {
      net <- net %>% filter(!origin %in% c("AFIL", "ZZZZ") &
                              !destination %in% c("AFIL", "ZZZZ"))
    }
    
    return(net)
  }

net2019 <- preprocess_network(net2019)
net2020 <- preprocess_network(net2020)
net2021 <- preprocess_network(net2021)
net2022 <- preprocess_network(net2022)

n_weeks <- length(weeks)

##### GraphWave analysis #####

create_nx_graph <- function(net, weighted = T) {
  net <-
    net %>% select(origin, destination, weight) %>% as.data.frame() %>%
    rename(source = origin,
           target = destination)
  
  net <- pd$DataFrame(net)
  if (weighted == T) {
    g <-
      nx$from_pandas_edgelist(net,
                              edge_attr = T,
                              create_using = nx$DiGraph)
    
  } else {
    g <- nx$from_pandas_edgelist(net[, 1:2], create_using = nx$DiGraph)
    
  }
  
  return(g)
}
nodes_to_string <- function(nodes) {
  nodes <- toString(nodes)
  nodes <- substr(nodes, 3, nchar(nodes) - 2)
  nodes <- stringr::str_remove_all(nodes, "'")
  nodes <- stringr::str_split(nodes, ", ")[[1]]
  return(nodes)
}

# First the 2019 network is processed. Taus is set to 'auto' to allow the algorithm to find
# the optimal upper limit and lower limit for the signal. As the 2019 network serves as the baseline,
# these taus will be used to recreate the same signal in the corresponding weeks of 2020-2022

embeddings_2019 <- list()
taus_2019 <- list()

for (wk in weeks) {
  print(paste("Processing week", wk))
  el <- create_nx_graph(net2019 %>% filter(week == wk))
  emb <-
    graphwave$graphwave_alg(el,
                            seq(0, 100, length.out = 25),
                            taus = "auto",
                            verbose = T)
  embedding <- emb[[1]] %>% as.data.frame()
  embedding$name <- nodes_to_string(el$nodes)
  embeddings_2019[[wk]] <- embedding
  taus_2019[[wk]] <- emb[[3]]
}

embeddings_2020 <- list()
for (wk in weeks) {
  print(paste("Processing week", wk))
  el <- create_nx_graph(net2020 %>% filter(week == wk))
  emb <-
    graphwave$graphwave_alg(el,
                            seq(0, 100, length.out = 25),
                            taus = taus_2019[[wk]],
                            verbose = F)
  embedding <- emb[[1]] %>% as.data.frame()
  embedding$name <- nodes_to_string(el$nodes)
  embeddings_2020[[wk]] <- embedding
}

embeddings_2021 <- list()
for (wk in weeks) {
  print(paste("Processing week", wk))
  el <- create_nx_graph(net2021 %>% filter(week == wk))
  emb <-
    graphwave$graphwave_alg(el,
                            seq(0, 100, length.out = 25),
                            taus = taus_2019[[wk]],
                            verbose = F)
  embedding <- emb[[1]] %>% as.data.frame()
  embedding$name <- nodes_to_string(el$nodes)
  embeddings_2021[[wk]] <- embedding
}

embeddings_2022 <- list()
for (wk in weeks) {
  print(paste("Processing week", wk))
  el <- create_nx_graph(net2022 %>% filter(week == wk))
  emb <-
    graphwave$graphwave_alg(el,
                            seq(0, 100, length.out = 25),
                            taus = taus_2019[[wk]],
                            verbose = F)
  embedding <- emb[[1]] %>% as.data.frame()
  embedding$name <- nodes_to_string(el$nodes)
  embeddings_2022[[wk]] <- embedding
}

#### Run K-means clustering on embeddings ####

create_scree_plot <- function(data, k_centers) {
  wcss <- list()
  ch_index <- list()

  # fit the k-means model for different values of the number of clusters
  for (i in 1:k_centers) {
    kmeans_fit <- kmeans(data, centers = i, iter.max = 50)
    wcss[[i]] <- kmeans_fit$tot.withinss
    
    if (i > 1) {
      ch_index[[i]] <-
        sk$metrics$calinski_harabasz_score(data, kmeans_fit$cluster)
    }
  }
  
  # plot the WCSS values against the number of clusters
  plot(
    1:k_centers,
    unlist(wcss),
    type = "b",
    main = "Elbow Method",
    xlab = "Number of clusters",
    ylab = "Within-cluster sum of squares"
  )
  
  return(list(
    wcss = unlist(wcss),
    ch_index = unlist(ch_index)
  ))
}

clustering_index <- list()
for (i in weeks) {
  clustering_index[[i]] <-
    create_scree_plot(embeddings_2019[[i]][, 1:100], k_centers = 20)
}

clust <- do.call(rbind, clustering_index)

data.frame(
  ch_index = do.call(c, clust[, 2]),
  n_clust = rep(2:20, n_weeks),
  week = foreach(i = weeks, .combine = c) %do% {
    rep(i, 19)
  }
) %>%
  group_by(n_clust) %>%
  mutate(group_mean = mean(ch_index)) %>%
  group_by(week) %>%
  ggplot(aes(x = n_clust, group = week)) +
  geom_line(aes(y = ch_index), alpha = .2) +
  scale_x_continuous(breaks = 2:20) +
  geom_line(aes(y = group_mean), lwd = 1.5) +
  theme_minimal() +
  xlab("# of cluster centers") +
  ylab("Calinski-Harabasz  Index")

# Even these 6 weeks show variability in the optimal number of clusters
# To find the optimal number of clusters for this timeperiod we rank each clustering solution
# based on its ch index score and average each rank score across the weeks.

data.frame(
  ch_index = do.call(c, clust[, 2]),
  n_clust = rep(2:20, n_weeks),
  week = foreach(i = weeks, .combine = c) %do% {
    rep(i, 19)
  }
) %>%
  group_by(week) %>%
  reframe(rank = rank(ch_index),
          ch_index = ch_index,
          n_clust = n_clust) %>%
  group_by(n_clust) %>%
  summarise(rank = mean(rank)) %>%
  arrange(desc(rank))

##### Create K-means models ####

clust6_kmeans_models <- list()
for (i in weeks) {
  clust6_kmeans_models[[i]] <-
    kmeans(embeddings_2019[[i]][, 1:100], 6)
}

generate_clusters <- function(data, kmeans_model) {
  # Kmeans predictions have no sense of ordinality, meaning that the largest airports
  # do not always end up in the cluster with the highest value.
  data$cluster <- predict(kmeans_model, data[, 1:100])
  pca1 <- prcomp(data[, 1:100])$x[, 1]
  pca2 <- prcomp(data[, 1:100])$x[, 2]
  
  # To circumvent this problem, we use a heuristic using PCA to reduce our signals to 2 dimensions
  # and fix the axes as such that the largest airport is in the ("LFTM") is in the
  # top right corner, as such that increase in pca dimension 1 relates to airports with
  # increasing levels of connectivity. Clusters are then renamed based on their occurrence
  # on the pca dimension 1,as such that the least connective cluster get assigned 1, and the most
  # connective cluster 6. This approach works for six clusters, as the change in connectivity is nicely
  # captured by PCA dimension 1. With overlapping clusters, a different approach should be tried.
  
  if (pca1[data$name == "LTFM"] < 0) {
    pca1 <- -pca1
  }
  if (pca2[data$name == "LTFM"] < 0) {
    pca2 <- -pca2
  }
  
  plot(pca1, pca2, col = data$cluster)
  
  clustering <- data[, 101:102] %>% left_join(
    data.frame(pca1, pca2, data$cluster) %>%
      arrange(pca1) %>% select(data.cluster) %>% unique() %>%
      mutate(clust = unique(1:n())) %>%
      rename(cluster = data.cluster)
  ) %>%
    select(name, clust)
  
  return(clustering)
}

net2019_clust_list <- list()
net2020_clust_list <- list()
net2021_clust_list <- list()
net2022_clust_list <- list()

for (i in weeks) {
  net2019_clust <-
    generate_clusters(embeddings_2019[[i]], clust6_kmeans_models[[i]])
  net2020_clust <-
    generate_clusters(embeddings_2020[[i]], clust6_kmeans_models[[i]])
  net2021_clust <-
    generate_clusters(embeddings_2021[[i]], clust6_kmeans_models[[i]])
  net2022_clust <-
    generate_clusters(embeddings_2022[[i]], clust6_kmeans_models[[i]])
  
  net2019_clust$week <- i
  net2020_clust$week <- i
  net2021_clust$week <- i
  net2022_clust$week <- i
  
  net2019_clust$year <- 2019
  net2020_clust$year <- 2020
  net2021_clust$year <- 2021
  net2022_clust$year <- 2022
  
  net2019_clust_list[[i]] <-
    net2019_clust
  net2020_clust_list[[i]] <-
    net2020_clust
  net2021_clust_list[[i]] <-
    net2021_clust
  net2022_clust_list[[i]] <-
    net2022_clust
}

net2019_clust <- do.call(rbind, net2019_clust_list)
net2020_clust <- do.call(rbind, net2020_clust_list)
net2021_clust <- do.call(rbind, net2021_clust_list)
net2022_clust <- do.call(rbind, net2022_clust_list)

##### Finding cluster differences ####

# Next we can test for differences in clustering between our target years compared to
# the baseline network.

# Comment function

find_clust_diff <- function(net1, net2, what, type = "sum") {

  net1_full <-
    data.frame(name = sort(rep(net1$name %>% unique(), n_weeks)),
               week = rep(weeks, net1$name %>% unique() %>% length()))
  net2_full <-
    data.frame(name = sort(rep(net2$name %>% unique(), n_weeks)),
               week = rep(weeks, net2$name %>% unique() %>% length()))
  tab_base <-
    net1_full %>% left_join(net1[, c("name", "clust", "week")]) %>%
    replace(is.na(.), 0) %>%
    select(name, clust) %>%
    table()
  tab_new <-
    net2_full %>% left_join(net2[, c("name", "clust", "week")]) %>%
    replace(is.na(.), 0) %>%
    select(name, clust) %>%
    table()
  if (what == "region") {
    res <- tab_base %>% as.data.frame() %>%
      rename(Freq1 = Freq) %>%
      full_join(tab_new %>% as.data.frame() %>% rename(Freq2 = Freq)) %>%
      replace(is.na(.), 0) %>%
      mutate(clust = as.numeric(clust) - 1) %>%
      mutate(sum1 = as.numeric(clust) * Freq1,
             sum2 = as.numeric(clust) * Freq2) %>%
      group_by(name) %>%
      summarise(avg_role_base = sum(sum1) / n_weeks,
                avg_role_new = sum(sum2) / n_weeks) %>%
      mutate(diff = avg_role_new - avg_role_base) %>%
      mutate(region = substr(name, 1, 2))
    if (type == "absolute") {
      res <- res %>% group_by(region) %>%
        summarise(diff = sum(abs(diff))) %>% # Find regions with most class mobility
        arrange(desc(diff))
    } else if (type == "mean") {
      res <- res %>% group_by(region) %>%
        summarise(diff = mean(diff)) %>% # Find regions with greatest increase/decrease in class representation
        arrange(desc(diff))
    } else if (type == "median") {
      res <- res %>% group_by(region) %>%
        summarise(diff = median(diff)) %>% # Find regions with greatest increase/decrease in class representation
        arrange(desc(diff))
    } else if (type == "sum") {
      res <- res %>% group_by(region) %>%
        summarise(diff = sum(diff)) %>% # Find regions with greatest increase/decrease in class representation
        arrange(desc(diff))
    }
  }
  if (what == "airport") {
    res <- tab_base %>% as.data.frame() %>%
      rename(Freq1 = Freq) %>%
      full_join(tab_new %>% as.data.frame() %>% rename(Freq2 = Freq)) %>%
      replace(is.na(.), 0) %>%
      mutate(clust = as.numeric(clust) - 1) %>%
      mutate(sum1 = as.numeric(clust) * Freq1,
             sum2 = as.numeric(clust) * Freq2) %>%
      group_by(name) %>%
      summarise(avg_role_base = sum(sum1) / n_weeks,
                avg_role_new = sum(sum2) / n_weeks) %>%
      mutate(diff = avg_role_new - avg_role_base) %>%
      arrange(desc(diff))
  }
  return(res)
}

airport_diff <- find_clust_diff(
  net2019_clust %>% filter(name %in% airports19),
  net2022_clust %>% filter(name %in% airports22),
  "airport",
  type = "sum"
) %>%
  arrange(desc(diff))


regional_diff <- find_clust_diff(
  net2019_clust %>% filter(name %in% airports19),
  net2022_clust %>% filter(name %in% airports22),
  "region",
  type = "sum"
) %>%
  arrange(diff)


# Plot used in paper
clust_change_plot <- find_clust_diff(net2019_clust%>%filter(name %in% airports19),
                                     net2022_clust%>%filter(name %in% airports22),
                                     "airport",type = "sum")%>%
  mutate(name = substr(name,1,2))%>%
  filter(name %in% c("LI","LF","LR","EG","LT","EF"))%>%
  mutate(name = ifelse(name == "LI","Italy",
                       ifelse(name == "LF","France",
                              ifelse(name == "LR","Romania",
                                     ifelse(name=="EG","United Kingdom",
                                            ifelse(name == "LT","Turkey","Finland"))))))%>%
  mutate(region = factor(name,levels = c("Italy","France","Romania",
                                         "United Kingdom","Finland","Turkey")))%>%
  ggplot(aes(x=avg_role_base,y=avg_role_new,label=name,color=region))+
  geom_abline(intercept = 0,slope=1,linewidth=.75)+facet_wrap(~region)+geom_point()+
  scale_x_continuous(breaks=0:6,minor_breaks = 0:6)+scale_y_continuous(breaks=0:6,minor_breaks = 0:6)+
  xlab("Average role in 2019")+ylab("Average role in 2022")+theme_minimal()+
  theme(legend.position = "none")

clust_change_plot

top3_incr <- regional_diff[order(regional_diff$diff,decreasing = T),]$region[1:3]
top3_decr <- regional_diff$region[2:4] #excluding Ukraine

find_clust_diff(net2019_clust%>%filter(name %in% airports19),
                net2022_clust%>%filter(name %in% airports22),
                "airport",type = "sum")%>%
  mutate(name = substr(name,1,2))%>%
  filter(name %in% c(top3_incr,top3_decr))%>%
  mutate(region = factor(name,levels = c("LI","LP","LG",
                                         "ES","LT","LF")))%>%
  ggplot(aes(x=avg_role_base,y=avg_role_new,label=name,color=region))+
  geom_abline(intercept = 0,slope=1,linewidth=.75)+facet_wrap(~region)+geom_point()+
  scale_x_continuous(breaks=0:6,minor_breaks = 0:6)+scale_y_continuous(breaks=0:6,minor_breaks = 0:6)+
  xlab("Average role in 2019")+ylab("Average role in 2022")+theme_minimal()+
  theme(legend.position = "none")



#### Change in cluster performance ####

# Code used to compute the N-hop neighborhood indices for each airport
neighborhod_indices_airport <-
  function(nodes, net, full, full_ms, hop = 1) {
    create_subgraph <- function(el, nodes, hop) {
      subset_el <- function(el, nodes) {
        sub_el <- el %>% filter(from %in% nodes |
                                  to %in% nodes)
        rest <- el %>% filter(!from %in% nodes &
                                !to %in% nodes)
        return(list(sub_el,
                    rest))
      }
      if (sum(names(el) %in% "n") > 0) {
        el <- el %>% rename(weight = n)
      }
      if (sum(names(el) %in% "origin") > 0) {
        el <- el %>% rename(from = origin,
                            to = destination)
      }
      net <- el
      if (hop >= 1) {
        hop_1 <- subset_el(net, nodes)
      }
      if (hop >= 2) {
        hop_1_nodes <- c(hop_1[[1]]$to, hop_1[[1]]$from) %>% unique()
        hop_1_nodes <-
          hop_1_nodes[!hop_1_nodes %in% c("ZZZZ", "AFIL")]
        hop_2 <- subset_el(hop_1[[2]], hop_1_nodes)
      }
      if (hop == 3) {
        hop_2_nodes <- c(hop_2[[1]]$to, hop_2[[1]]$from) %>% unique()
        hop_2_nodes <-
          hop_2_nodes[!hop_2_nodes %in% c("ZZZZ", "AFIL")]
        hop_3 <- subset_el(hop_2[[2]], hop_2_nodes)
      }
      if (hop == 1) {
        return(hop_1[[1]])
      } else if (hop == 2) {
        return(rbind(hop_1[[1]], hop_2[[1]]))
      } else {
        return(rbind(hop_1[[1]], hop_2[[1]], hop_3[[1]]))
      }
    }
    if (sum(names(net) %in% "n") > 0) {
      net <- net %>% rename(weight = n)
    }
    if (sum(names(net) %in% "origin") > 0) {
      net <- net %>% rename(from = origin,
                            to = destination)
    }
    
    # "Scheduled" is no longer used from 2019 onward. Data contained handfull of instances of Scheduled
    net <- net %>% filter(market_segment != "Scheduled")
    nb <- create_subgraph(net, nodes, hop)
    sub <- nb %>% group_by(week) %>%
      summarise(
        v = c(from, to) %>% unique() %>% length(),
        e = paste(from, to) %>% unique() %>% length(),
        w = sum(weight),
        avg_weights = round(w / e, 2)
      )
    sub <- left_join(sub, full)
    sub <- sub %>%
      mutate(
        perc_edges = round(e / e_full, 4),
        perc_vertices = round(v / v_full, 4),
        perc_weights = round(w / w_full, 4)
      ) %>%
      select(week,
             v,
             e,
             w,
             avg_weights,
             perc_edges,
             perc_vertices,
             perc_weights)
    sub$market_segment <- "All-flights"
    sub_ms <- nb %>% group_by(week, market_segment) %>%
      summarise(
        v = c(from, to) %>% unique() %>% length(),
        e = paste(from, to) %>% unique() %>% length(),
        w = sum(weight),
        avg_weights = round(w / e, 2)
      )
    sub_ms <-
      left_join(sub_ms, full_ms, by = c("week", "market_segment"))
    sub_ms <- sub_ms %>%
      mutate(
        perc_edges = round(e / e_full, 4),
        perc_vertices = round(v / v_full, 4),
        perc_weights = round(w / w_full, 4)
      ) %>%
      select(week,
             market_segment,
             v,
             e,
             w,
             avg_weights,
             perc_edges,
             perc_vertices,
             perc_weights)
    result <- rbind(sub, sub_ms)
    result$name = nodes
    return(result)
  }

full <- net2019 %>%
  rename(from = origin,
         to = destination) %>%
  group_by(week) %>%
  summarise(
    v_full = c(from, to) %>% unique() %>% length(),
    e_full = paste(from, to) %>% unique() %>% length(),
    w_full = sum(weight)
  )
full_ms <- net2019 %>%
  rename(from = origin,
         to = destination) %>% group_by(week, market_segment) %>%
  summarise(
    v_full = c(from, to) %>% unique() %>% length(),
    e_full = paste(from, to) %>% unique() %>% length(),
    w_full = sum(weight)
  )

neighborhod_indices_airport("EHAM", net2019, full, full_ms, hop = 1)
neighborhod_indices_airport("EHAM", net2019, full, full_ms, hop = 2)
neighborhod_indices_airport("EHAM", net2019, full, full_ms, hop = 3)


net2019_nb_airport <- fread("net2019_nb_airports.csv") %>%
  filter(week %in% weeks)
net2020_nb_airport <- fread("net2020_nb_airports.csv") %>%
  filter(week %in% weeks)
net2021_nb_airport <- fread("net2021_nb_airports.csv") %>%
  filter(week %in% weeks)
net2022_nb_airport <- fread("net2022_nb_airports.csv") %>%
  filter(week %in% weeks)

net2022_nb_airport$year <- 2022

# fwrite(net2019_nb_airport,"sample_2019_nb.csv")
# fwrite(net2020_nb_airport,"sample_2020_nb.csv")
# fwrite(net2021_nb_airport,"sample_2021_nb.csv")
# fwrite(net2022_nb_airport,"sample_2022_nb.csv")

net2019_nb_airport <-
  net2019_nb_airport %>% left_join(net2019_clust) %>% filter(name %in% airports19 &
                                                               !is.na(clust6))
net2020_nb_airport <-
  net2020_nb_airport %>% left_join(net2020_clust) %>% filter(name %in% airports20 &
                                                               !is.na(clust6))
net2021_nb_airport <-
  net2021_nb_airport %>% left_join(net2021_clust) %>% filter(name %in% airports21 &
                                                               !is.na(clust6))
net2022_nb_airport <-
  net2022_nb_airport %>% left_join(net2022_clust) %>% filter(name %in% airports22 &
                                                               !is.na(clust6))



library("Partiallyoverlapping")

compare_overlapped_distributions <- function(base, new, outcome) {
  sample1 <- new %>% mutate(name = paste(name, week))
  sample2 <- base %>% mutate(name = paste(name, week))
  unpaired.s1 <- sample1 %>% filter(!name %in% sample2$name)
  unpaired.s2 <- sample2 %>% filter(!name %in% sample1$name)
  paired.s1 <- sample1 %>% filter(name %in% sample2$name)
  paired.s2 <- sample2 %>% filter(name %in% sample1$name)
  
  res <-
    Partover.test(
      x1 = unpaired.s1 %>% select(all_of(outcome)) %>% unlist(),
      x2 = unpaired.s2 %>% select(all_of(outcome)) %>% unlist(),
      x3 = paired.s1 %>% select(all_of(outcome)) %>% unlist(),
      x4 = paired.s2 %>% select(all_of(outcome)) %>% unlist(),
      conf.level = .95
    )
  
  
  
  
  return(
    data.frame(
      mean_new = sample1 %>% select(all_of(outcome)) %>% unlist() %>% mean(),
      mean_base = sample2 %>% select(all_of(outcome)) %>% unlist() %>%
        mean(),
      estimate = res$estimate,
      statistic = res$statistic,
      df = res$parameter,
      p.value = res$p.value,
      conf.ll = res$conf.int[1],
      conf.ul = res$conf.int[2]
    )
  )
  
}

create_ttest_table <- function(base, new, outcome, clust, weeks) {
  names(base)[names(base) == clust] <- "clust"
  names(new)[names(new) == clust] <- "clust"
  
  if (outcome %>% length() > 1) {
    tab <- foreach(out = outcome, .combine = rbind) %do% {
      tab1 <- foreach(h = 1:3, .combine = rbind) %:%
        foreach(ci = 1:max(base$clust), .combine = rbind) %:%
        foreach(i = weeks, .combine = rbind) %do% {
          res <- compare_overlapped_distributions(
            base %>% filter(clust %in% ci & hop == h & week == i),
            new %>% filter(clust %in% ci & hop == h & week == i),
            out
          )
          res <- cbind(
            week = i,
            cluster = ci,
            hop = h,
            round(res, 4)
          )
          res
        }
      cbind(outcome = out, tab1)
      
    }
  } else {
    tab <- foreach(h = 1:3, .combine = rbind) %:%
      foreach(ci = 1:max(base$clust), .combine = rbind) %:%
      foreach(i = weeks, .combine = rbind) %do% {
        res <- compare_overlapped_distributions(
          base %>% filter(clust %in% ci & hop == h & week == i),
          new %>% filter(clust %in% ci & hop == h & week == i),
          outcome
        )
        res <- cbind(week = i,
                     cluster = ci,
                     hop = h,
                     round(res, 4))
        res
        
      }
    
  }
  
  
  return(tab)
  
}


ttest_19_20 <- create_ttest_table(
  net2019_nb_airport %>%
    filter(name %in% airports19 &
             market_segment == "All-flights"),
  net2020_nb_airport %>%
    filter(name %in% airports20 &
             market_segment == "All-flights"),
  c("perc_vertices", "perc_edges", "perc_weights"),
  "clust6",
  weeks
)

ttest_19_21 <- create_ttest_table(
  net2019_nb_airport %>%
    filter(name %in% airports19 &
             market_segment == "All-flights"),
  net2021_nb_airport %>%
    filter(name %in% airports21 &
             market_segment == "All-flights"),
  c("perc_vertices", "perc_edges", "perc_weights"),
  "clust6",
  weeks
)
ttest_19_22 <- create_ttest_table(
  net2019_nb_airport %>%
    filter(name %in% airports19 &
             market_segment == "All-flights"),
  net2022_nb_airport %>%
    filter(name %in% airports22 &
             market_segment == "All-flights"),
  c("perc_vertices", "perc_edges", "perc_weights"),
  "clust6",
  weeks
)

ttest_19_20 %>%
  ggplot(aes(
    x = factor(week),
    y = estimate,
    group = hop,
    color = factor(hop)
  )) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ul,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ll,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line() +
  facet_wrap( ~ outcome + cluster, ncol = 6) +
  theme_minimal() + theme(legend.position = "top")

ttest_19_21 %>%
  ggplot(aes(
    x = factor(week),
    y = estimate,
    group = hop,
    color = factor(hop)
  )) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ul,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ll,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line() +
  facet_wrap( ~ outcome + cluster, ncol = 6) +
  theme_minimal() + theme(legend.position = "top")

ttest_19_22 %>%
  ggplot(aes(
    x = factor(week),
    y = estimate,
    group = hop,
    color = factor(hop)
  )) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ul,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line(
    aes(
      x = factor(week),
      y = conf.ll,
      group = hop,
      color = factor(hop)
    ),
    alpha = .5,
    linetype = "dashed"
  ) +
  geom_line() +
  facet_wrap( ~ outcome + cluster, ncol = 6) +
  theme_minimal() + theme(legend.position = "top")



##### Market change plots #####

region_by_market_segment19 <- net2019 %>%
  mutate(origin = substr(origin, 1, 2),
         destination = substr(destination, 1, 2)) %>%
  group_by(origin, destination, market_segment, week) %>%
  reframe(weight = sum(weight))

region_by_market_segment20 <- net2020 %>%
  mutate(origin = substr(origin, 1, 2),
         destination = substr(destination, 1, 2)) %>%
  group_by(origin, destination, market_segment, week) %>%
  reframe(weight = sum(weight))

region_by_market_segment21 <- net2021 %>%
  mutate(origin = substr(origin, 1, 2),
         destination = substr(destination, 1, 2)) %>%
  group_by(origin, destination, market_segment, week) %>%
  reframe(weight = sum(weight))

region_by_market_segment22 <- net2022 %>%
  mutate(origin = substr(origin, 1, 2),
         destination = substr(destination, 1, 2)) %>%
  group_by(origin, destination, market_segment, week) %>%
  reframe(weight = sum(weight))


#### Plot region ####
airports <-
  fread(
    "C:/Users/juulk/Vrije Universiteit Brussel/FacPE-PhD Juul Vossen - Documenten/Stage Eurocontrol/full dataset/airports_20221025.csv"
  )
node.coords <- airports[,
                        c("ident", "latitude_deg", "longitude_deg", "type")]

data20 <-
  find_clust_diff(
    net2019_clust %>% filter(name %in% airports19),
    net2020_clust %>% filter(name %in% airports20),
    "airport",
    "clust6",
    type = "abs"
  ) %>%
  left_join(node.coords, by = c("name" = "ident"))
data21 <-
  find_clust_diff(
    net2019_clust %>% filter(name %in% airports19),
    net2021_clust %>% filter(name %in% airports21),
    "airport",
    "clust6",
    type = "abs"
  ) %>%
  left_join(node.coords, by = c("name" = "ident"))
data22 <-
  find_clust_diff(
    net2019_clust %>% filter(name %in% airports19),
    net2022_clust %>% filter(name %in% airports22),
    "airport",
    "clust6",
    type = "abs"
  ) %>%
  left_join(node.coords, by = c("name" = "ident"))
data_full <- rbind(cbind(data20, year = 2020),
                   cbind(data21, year = 2021),
                   cbind(data22, year = 2022))

library("rnaturalearth")
plot_region <- function(region, country_name, data, years = 2020:2022) {
  colour_breaks <- c(-3, -2,-1,-.5, 0, .5, 1)
  
  colours <-
    c("red4",
      "red3",
      "red2",
      "tomato3",
      "white",
      "dodgerblue2",
      "dodgerblue4")
  
  ggplot(ne_countries(
    country = country_name,
    returnclass = "sf",
    scale = "large"
  )) +
    geom_sf(fill = "grey99") +
    geom_jitter(
      aes(
        y = latitude_deg,
        x = longitude_deg,
        fill = diff,
        shape = factor(ifelse(
          round(sum1) <= 2,
          "<=2", as.character(round(sum1))
        ))
      ),
      data %>% filter(substr(name, 1, 2) == region) %>% filter(year %in% years),
      size = 2.5,
      alpha = .9
    ) +
    scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
    scale_fill_gradientn(
      limits  = range(data[substr(data$name, 1, 2) == region, ]$diff),
      colours = colours[c(1, seq_along(colours), length(colours))],
      values  = c(0, scales::rescale(colour_breaks,
                                     from = range(data[substr(data$name, 1, 2) == region, ]$diff)), 1),
    ) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    labs(shape = "") +
    facet_wrap( ~ year)
}
plot_region("LI", "Italy", data_full) + theme(legend.position = "top") +
  ylab("") + xlab("")

plot_region("EG", "United Kingdom", data_full) + theme(legend.position = "top") +
  ylab("") + xlab("") + coord_sf(xlim = c(-12, 3))

plot_region("EG", "United Kingdom", data_full)

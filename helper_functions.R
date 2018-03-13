plot_coords <- function(ches, liberalism, populism) {
  set.seed(2018)
  liberalism <- enquo(liberalism)
  populism <- enquo(populism)
  ches_cluster <- ches %>% 
    select(party_cntry, !!liberalism, !!populism) %>% 
    na.omit() %>% 
    as.data.frame()
  
  row_names <- ches_cluster$party_cntry
  
  ches_cluster %<>% 
    select(-party_cntry) %>% 
    mutate_all(scale) 
  
  rownames(ches_cluster) <- row_names
  
  
  k3 <- kmeans(ches_cluster, centers = 4, nstart = 25)
  
  ches %<>%
    select(party_cntry, !!liberalism, !!populism) %>% 
    na.omit() %>% 
    mutate(cluster = k3$cluster,
           state = row.names(ches_cluster)) %>% 
    mutate(cluster = factor(cluster))
  
  ches %>% 
    ggplot(aes_(liberalism, 
                populism, 
                color = quo(cluster))) +
    geom_point() +
    geom_text_repel(aes_(liberalism, 
                         populism, 
                         label = quo(party_cntry)), 
                    show.legend = F) +
    guides(text = F) +
    ggthemes::theme_hc() +
    ggthemes::scale_color_gdocs() +
    xlab("<<Progressivism - Traditionalism>>") + 
    ylab("<<Establishment - Anti-Establishment>>") +
    guides(color = guide_legend("Cluster")) +
    ggtitle("Classification of European Parties") +
    labs(subtitle = "Based on K-Nearest Neighbour Clustering",
         caption = "Source: CHES Data 1999 - 2014") +
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15), 
          legend.title = element_text(size = 15), 
          title = element_text(size = 17))
}
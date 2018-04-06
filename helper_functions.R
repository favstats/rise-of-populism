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


my_stat_compare_means  <- function (mapping = NULL, data = NULL, method = NULL, paired = FALSE, 
                                    method.args = list(), ref.group = NULL, comparisons = NULL, 
                                    hide.ns = FALSE, label.sep = ", ", label = NULL, label.x.npc = "left", 
                                    label.y.npc = "top", label.x = NULL, label.y = NULL, tip.length = 0.03, 
                                    symnum.args = list(), geom = "text", position = "identity", 
                                    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
{
  if (!is.null(comparisons)) {
    method.info <- ggpubr:::.method_info(method)
    method <- method.info$method
    method.args <- ggpubr:::.add_item(method.args, paired = paired)
    if (method == "wilcox.test") 
      method.args$exact <- FALSE
    pms <- list(...)
    size <- ifelse(is.null(pms$size), 0.3, pms$size)
    color <- ifelse(is.null(pms$color), "black", pms$color)
    map_signif_level <- FALSE
    if (is.null(label)) 
      label <- "p.format"
    if (ggpubr:::.is_p.signif_in_mapping(mapping) | (label %in% "p.signif")) {
      if (ggpubr:::.is_empty(symnum.args)) {
        map_signif_level <- c(`p < 0.001` = 0.0001, `p < 0.001` = 0.001, 
                              `p < 0.01` = 0.01, `p < 0.05` = 0.05, ns = 1)
      } else {
        map_signif_level <- symnum.args
      } 
      if (hide.ns) 
        names(map_signif_level)[5] <- " "
    }
    step_increase <- ifelse(is.null(label.y), 0.12, 0)
    ggsignif::geom_signif(comparisons = comparisons, y_position = label.y, 
                          test = method, test.args = method.args, step_increase = step_increase, 
                          size = size, color = color, map_signif_level = map_signif_level, 
                          tip_length = tip.length, data = data)
  } else {
    mapping <- ggpubr:::.update_mapping(mapping, label)
    layer(stat = StatCompareMeans, data = data, mapping = mapping, 
          geom = geom, position = position, show.legend = show.legend, 
          inherit.aes = inherit.aes, params = list(label.x.npc = label.x.npc, 
                                                   label.y.npc = label.y.npc, label.x = label.x, 
                                                   label.y = label.y, label.sep = label.sep, method = method, 
                                                   method.args = method.args, paired = paired, ref.group = ref.group, 
                                                   symnum.args = symnum.args, hide.ns = hide.ns, 
                                                   na.rm = na.rm, ...))
  }
}


colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}
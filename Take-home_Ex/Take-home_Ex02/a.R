
# ---------- 全局样式配置 ----------
STYLES <- list(
  font_family = "Roboto Condensed",
  primary_color = "blue",
  primary_light_color = "white",
  secondary_color = "darkorange",
  secondary_light_color = "white",
  muted_color = "grey50",
  title_color = "grey25",
  normal_text_color = "grey50",
  node_size = 7.5,
  node_border_color = "grey50",
  node_border_stroke = 0.5,
  node_emphasized_border_color = "black",
  node_emphasized_border_stroke = 1,
  arrow_margin = 3.2,
  arrow_style = grid::arrow(type = "closed", length = unit(2, "pt")),
  base_edge_thickness = 0.2,
  panel_border_color = "grey50",
  panel_border_thickness = 0.5,
  node_label_size = 2,
  node_label_dark = "black",
  node_label_light = "white",
  default_caption = "Hover on the nodes to see more details.",
  tooltip_css = paste0(
    "background-color:black;color:white;",
    "font-family:Roboto Condensed;font-size:10pt;",
    "padding:4px;text-align:center;"
  ),
  svg_width = 6,
  svg_height = 6 * 0.618
)

CONFIGS <- list(
  default_seed = 1234  # 可调整为你想要的随机种子
)

MAPPINGS <- list(
  node_supertype_to_shape = c(
    "Person" = 24,
    "Organization" = 21,
    "Vessel" = 22,
    "Group" = 23,
    "Location" = 25,
    "Event" = 8
  ),
  node_subtype_to_color = c(
    "Person" = "#44AA99",
    "Organization" = "#117733",
    "Vessel" = "#88CCEE",
    "Group" = "#DDCC77",
    "Location" = "#CC6677",
    "Monitoring" = "#AA4499"
  ),
  edge_relationship_subtype_to_color = c(
    "AccessPermission" = "#1E88E5",
    "Operates" = "#004D40",
    "Colleagues" = "#FFC107",
    "Suspicious" = "#D81B60",
    "Reports" = "#6A1B9A",
    "Jurisdiction" = "#43A047",
    "Unfriendly" = "#E53935",
    "Friends" = "#F4511E"
  )
)


library(tidygraph)
library(ggraph)
library(igraph)
library(ggiraph)
library(dplyr)

plot_entity_ego_network <- function(graph,
                                    target_entity,
                                    nodes_table,
                                    layout = "fr",
                                    order = 2,
                                    mode = "all",
                                    metric = "pagerank",  # 可选值："pagerank" 或 "betweenness"
                                    title_prefix = "Influence Network") {
  # ✅ Step 1: 找到目标节点 ID
  target_id <- nodes_table %>%
    filter(label == target_entity) %>%
    pull(id)
  
  if (length(target_id) == 0) {
    stop(paste("Entity", target_entity, "not found in nodes table!"))
  }
  
  # ✅ Step 2: 转换为 igraph 对象
  ig_graph <- as.igraph(graph)
  
  # ✅ Step 3: 提取 ego 网络并计算中心性
  ego_net_tbl <- make_ego_graph(ig_graph, order = order, nodes = which(nodes_table$id == target_id), mode = mode)[[1]] %>%
    as_tbl_graph() %>%
    mutate(
      degree = centrality_degree(),
      betweenness = centrality_betweenness(),
      pagerank = centrality_pagerank()
    )
  
  # ✅ Step 4: 确定绘图用的指标
  if (!metric %in% c("pagerank", "betweenness")) {
    stop("Invalid metric. Choose 'pagerank' or 'betweenness'.")
  }
  
  # ✅ Step 5: 自动加缺省列
  ego_net_tbl <- ego_net_tbl %>%
    mutate(
      type = ifelse("type" %in% colnames(.N()), type, "Node"),
      pagerank = ifelse(is.na(pagerank), 1, pagerank)
    ) %>%
    activate(edges) %>%
    mutate(subtype = ifelse("subtype" %in% colnames(.N()), subtype, "default"))
  
  # ✅ Step 6: 绘制交互图
  g <- ggraph(ego_net_tbl, layout = layout) +
    ggiraph::geom_node_point_interactive(
      aes(
        tooltip = paste0("Node: ", label, "<br>Type: ", type, "<br>", metric, ": ", round(.data[[metric]], 3)),
        data_id = label,
        color = .data[[metric]],
        size = .data[[metric]]
      ),
      show.legend = TRUE
    ) +
    ggiraph::geom_edge_link_interactive(
      aes(
        tooltip = paste0("Edge: ", from, " → ", to, "<br>Subtype: ", subtype),
        data_id = paste0(from, "-", to),
        color = subtype
      ),
      alpha = 0.5,
      arrow = grid::arrow(type = "closed", length = unit(2, "mm")),
      end_cap = circle(3, "mm")
    ) +
    scale_color_viridis_c() +
    labs(
      title = paste(title_prefix, "of", target_entity),
      subtitle = paste("Node size & color reflect", metric)
    ) +
    theme_void()
  
  girafe(
    ggobj = g,
    width_svg = 8,
    height_svg = 6,
    options = list(
      opts_hover(css = "stroke:black;stroke-width:2px;"),
      opts_tooltip(css = "background-color:black;color:white;padding:5px;border-radius:3px;")
    )
  )
}
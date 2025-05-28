

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




plot_relationship_graph_interactive <- function(graph,
                                                emphasize_nodes = c(),
                                                layout = "fr",
                                                circular = FALSE,
                                                title = NULL,
                                                subtitle = NULL,
                                                node_size = 6,
                                                arrow_margin = 3) {
  nodes <- as_data_frame(graph, what = "vertices")
  edges <- as_data_frame(graph, what = "edges")
  
  # ⚠️ 如果 edges 没有 subtype，加一列默认值
  if (!"subtype" %in% colnames(edges)) {
    edges$subtype <- "default"
    graph <- graph %>% activate(edges) %>% mutate(subtype = "default")
  }
  
  # ⚠️ 如果 nodes 没有 pagerank，加一列默认值
  if (!"pagerank" %in% colnames(nodes)) {
    nodes$pagerank <- 1
    graph <- graph %>% activate(nodes) %>% mutate(pagerank = 1)
  }
  
  # ⚠️ 如果 nodes 没有 type，加一列默认值
  if (!"type" %in% colnames(nodes)) {
    nodes$type <- "Node"
    graph <- graph %>% activate(nodes) %>% mutate(type = "Node")
  }
  
  g <- ggraph(graph, layout = layout, circular = circular) +
    ggiraph::geom_node_point_interactive(
      aes(
        tooltip = paste0("Node: ", name, "<br>Type: ", type, "<br>PageRank: ", round(pagerank, 3)),
        data_id = name,
        color = type,
        shape = type,
        size = pagerank
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
      end_cap = circle(arrow_margin, "mm")
    ) +
    theme_graph(base_family = "Arial") +
    labs(title = title, subtitle = subtitle)
  
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
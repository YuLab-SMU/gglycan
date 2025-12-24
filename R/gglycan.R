#' Plot Glycan Structure
#'
#' Visualizes a glycan structure using ggplot2 and ggtangle.
#'
#' @param data A glycan graph object (from `read_glycan`) or an IUPAC string.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param layout The layout algorithm to use. Default is `ggtangle::layout_fishbone`. Can be a string ("tree", "nicely") or a function.
#' @param direction The direction of the main chain growth ("left", "right", "up", "down"). Default is "left".
#' @param length The distance between nodes. Default is 1.
#' @param angle_sep The angle separation for branches in degrees. Default is 30.
#' @param motif A motif string (IUPAC format) to highlight in the structure. Default is NULL.
#' @param ... Additional arguments passed to `ggplot`.
#' @return A ggplot object.
#' @import ggplot2
#' @import ggtangle
#' @import ggstar
#' @importFrom igraph as.igraph
#' @export
gglycan <- function(data, mapping = aes(), layout = ggtangle::layout_fishbone, direction = "left", length = 1, angle_sep = 30, motif = NULL, ...) {
  # Handle input type: string -> graph
  if (is.character(data)) {
    data <- read_glycan(data)
  }
  
  # Apply style mapping if not present
  data <- match_snfg_style(data)
  
  # Highlight motif if provided
  # This adds 'alpha' attribute to nodes and edges
  data <- highlight_motif(data, motif)
  
  # Convert to igraph for ggtangle dispatch
  if (inherits(data, "igraph")) {
    ig <- data
  } else {
    ig <- igraph::as.igraph(data)
  }
  
  # Format edge labels (a/b -> alpha/beta)
  # Check if edges have labels
  if ("label" %in% igraph::edge_attr_names(ig)) {
    new_labels <- format_linkage_label(igraph::edge_attr(ig, "label"))
    ig <- igraph::set_edge_attr(ig, "label", value = new_labels)
    
    # Add label_vjust for smart placement
    # Linkage 6/4 -> Above (-0.5)
    # Linkage 3/2 -> Below (1.5)
    vj <- rep(-0.5, length(new_labels))
    vj[grepl("3|2", new_labels)] <- 1.5 
    ig <- igraph::set_edge_attr(ig, "label_vjust", value = vj)
  } else {
    ig <- igraph::set_edge_attr(ig, "label_vjust", value = -0.5)
  }

  # Create a layout wrapper if using fishbone, to pass custom args
  is_fishbone <- identical(layout, ggtangle::layout_fishbone)
  layout_fun <- layout
  
  if (is_fishbone) {
    layout_fun <- function(g, ...) {
       ggtangle::layout_fishbone(g, direction = direction, length = length, angle_sep = angle_sep, ...)
    }
  }
  
  p <- ggplot(ig, mapping = mapping, layout = layout_fun, ...) +
    theme_void() +
    coord_fixed()
    
  if (direction %in% c("left", "right")) {
     p <- p + scale_y_continuous(expand = expansion(mult = 0.2))
  } else {
     p <- p + scale_x_continuous(expand = expansion(mult = 0.2))
  }

  if (!is_fishbone && is.character(layout) && layout == "tree") {
     p <- p + coord_flip() + scale_x_reverse()
  }
  
  return(p)
}

#' Glycan Geometry Layer
#'
#' A convenient wrapper to add edges, edge labels, and SNFG nodes to a glycan plot.
#'
#' @param edge_color Color of the edges.
#' @param edge_width Width of the edges.
#' @param arrow_length Length of the edge arrows.
#' @param node_size Size of the node symbols.
#' @param node_label Logical. Whether to show node labels (monosaccharide names). Default is FALSE.
#' @param label_size Size of the node labels.
#' @param edge_label_size Size of the edge labels (linkage).
#' @param ... Additional arguments (currently unused).
#' @import ggplot2
#' @import ggtangle
#' @import ggstar
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang sym
#' @export
geom_glycan <- function(edge_color = "black", 
                        edge_width = 0.5, 
                        arrow_length = unit(2, "mm"),
                        node_size = 5,
                        node_label = FALSE,
                        label_size = 3.88, # Default geom_text size
                        edge_label_size = 3.88,
                        ...) {
  # Prepare manual scales from SNFG map
  # We assume snfg_map is available in the package environment
  fill_vals <- stats::setNames(snfg_map$color, snfg_map$monosaccharide)
  shape_vals <- stats::setNames(snfg_map$starshape, snfg_map$monosaccharide)

  layers <- list(
    # Edge line
    geom_edge(
      aes(alpha = I(alpha)),
      arrow = NULL, # Default undirected (no arrow)
      color = edge_color,
      linewidth = edge_width
    ),
    # Edge label (Linkage)
    geom_edge_text(
      aes(label = !!sym("label"), vjust = !!sym("label_vjust"), alpha = I(alpha)), 
      angle_calc = 'along', 
      size = edge_label_size
    ),
    # Node symbol (SNFG)
    geom_star(
      aes(x = !!sym("x"), y = !!sym("y"), starshape = !!sym("base_type"), fill = !!sym("base_type"), alpha = I(alpha)), 
      size = node_size, 
      color = "black"
    ),
    # Manual scales for legend
    scale_fill_manual(values = fill_vals, na.value = "white", name = "Monosaccharide"),
    scale_starshape_manual(values = shape_vals, na.value = 15, name = "Monosaccharide")
  )
  
  if (node_label) {
    layers <- c(layers, list(
      # Node label (Monosaccharide name)
      geom_text_repel(
        aes(label = !!sym("label")), 
        nudge_y = 0.2,
        size = label_size
      )
    ))
  }
  
  return(layers)
}

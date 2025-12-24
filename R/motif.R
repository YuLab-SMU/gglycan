#' Identify Motif in Glycan Graph
#'
#' Finds occurrences of a motif subsequence in the main glycan graph and marks them.
#'
#' @param graph The main glycan graph (igraph/tbl_graph).
#' @param motif_str The motif string (IUPAC format).
#' @return A graph with an added `alpha` vertex attribute (1 for motif, 0.4 for others).
#' @importFrom igraph subgraph_isomorphisms vertex_attr edge_attr V E
#' @importFrom tidygraph as_tbl_graph activate mutate
#' @export
highlight_motif <- function(graph, motif_str) {
  if (is.null(motif_str)) {
     # If no motif, everything is alpha 1
     graph <- igraph::set_vertex_attr(graph, "alpha", value = 1)
     graph <- igraph::set_edge_attr(graph, "alpha", value = 1)
     return(graph)
  }
  
  # Parse motif
  motif_graph <- read_glycan(motif_str)
  
  # Convert to igraph
  g <- igraph::as.igraph(graph)
  p <- igraph::as.igraph(motif_graph)
  
  # Ensure graphs are valid
  if (igraph::vcount(p) > igraph::vcount(g)) {
     warning("Motif is larger than target graph. No match found.")
     graph <- igraph::set_vertex_attr(graph, "alpha", value = 0.6)
     graph <- igraph::set_edge_attr(graph, "alpha", value = 0.6)
     return(graph)
  }
  
  # Use LAD algorithm with domains for node matching
  # This is more robust than VF2 with color vectors in some R igraph versions
  
  g_labels <- igraph::V(g)$label
  p_labels <- igraph::V(p)$label
  
  if (is.null(g_labels) || is.null(p_labels)) {
     stop("Graph nodes must have 'label' attribute for motif matching.")
  }
  
  # Construct domains: list of compatible target nodes for each pattern node
  domains <- vector("list", length(p_labels))
  for (i in seq_along(p_labels)) {
      # Find all nodes in g with matching label
      # Using strict equality
      domains[[i]] <- which(g_labels == p_labels[i])
  }
  
  # If any pattern node has no matches, then motif cannot exist
  if (any(sapply(domains, length) == 0)) {
      # No match possible
      matches <- list()
  } else {
      # Find structural isomorphisms compatible with domains
      matches <- igraph::subgraph_isomorphisms(pattern = p, target = g, method = "lad", domains = domains)
  }
  
  # Filter matches by edge labels (linkages)
  valid_matches <- list()
  
  if (length(matches) > 0) {
      # Normalize linkages function
      normalize <- function(x) {
         if (is.null(x)) return(x)
         x <- gsub("^a", "\u03b1", x)
         x <- gsub("^b", "\u03b2", x)
         return(x)
      }
      
      g_edges_attr <- normalize(igraph::edge_attr(g, "label"))
      p_edges_attr <- normalize(igraph::edge_attr(p, "label"))
      
      # If motif has no edges, all structural matches are valid
      if (igraph::gsize(p) == 0) {
          valid_matches <- matches
      } else {
          p_edgelist <- igraph::as_edgelist(p, names = FALSE)
          
          for (m in matches) {
              is_valid <- TRUE
              # Check all edges in pattern
              for (k in 1:nrow(p_edgelist)) {
                  u <- p_edgelist[k, 1]
                  v <- p_edgelist[k, 2]
                  
                  target_u <- as.numeric(m[u])
                  target_v <- as.numeric(m[v])
                  
                  # Find edge ID in target
                  eid <- igraph::get_edge_ids(g, c(target_u, target_v))
                  
                  if (eid == 0) {
                      # Should not happen if structural match is correct, but safety check
                      is_valid <- FALSE
                      break
                  }
                  
                  # Check label
                  # p edge index? p is simple tree?
                  # We need to find the edge index in p to get its label
                  # Or iterate over E(p) directly?
                  # Let's get label for u->v in p
                  p_eid <- igraph::get_edge_ids(p, c(u, v))
                  
                  l_p <- p_edges_attr[p_eid]
                  l_g <- g_edges_attr[eid]
                  
                  if (l_p != l_g) {
                      is_valid <- FALSE
                      break
                  }
              }
              
              if (is_valid) {
                  valid_matches[[length(valid_matches) + 1]] <- m
              }
          }
      }
  }
  
  # Mark nodes
  is_motif <- rep(FALSE, length(igraph::V(g)))
  
  if (length(valid_matches) > 0) {
     for (m in valid_matches) {
        is_motif[m] <- TRUE
     }
  }
  
  # Assign alpha
  node_alpha <- ifelse(is_motif, 1, get_hl_alpha())
  g <- igraph::set_vertex_attr(g, "alpha", value = node_alpha)
  
  # Identify matched edges for alpha
  is_motif_edge <- rep(FALSE, length(igraph::E(g)))
  
  if (length(valid_matches) > 0 && igraph::gsize(p) > 0) {
      p_edgelist <- igraph::as_edgelist(p, names = FALSE)
      
      for (m in valid_matches) {
          for (k in 1:nrow(p_edgelist)) {
              u <- p_edgelist[k, 1]
              v <- p_edgelist[k, 2]
              
              target_u <- as.numeric(m[u])
              target_v <- as.numeric(m[v])
              
              eid <- igraph::get_edge_ids(g, c(target_u, target_v))
              if (eid != 0) is_motif_edge[eid] <- TRUE
          }
      }
  }
  
  edge_alpha <- ifelse(is_motif_edge, 1, get_hl_alpha())
  g <- igraph::set_edge_attr(g, "alpha", value = edge_alpha)
  
  return(g)
}

#' Set Highlight Alpha
#'
#' Sets the alpha value for motif highlighting.
#'
#' @param value Numeric value for alpha (default 0.4).
#' @importFrom yulab.utils update_cache_item
#' @export
set_hl_alpha <- function(value = 0.4) {
   update_cache_item('gglycan', list(highlight_motif_alpha = value))
}

#' Get Highlight Alpha
#'
#' Gets the current alpha value for motif highlighting.
#'
#' @param default Default value if not set (0.4).
#' @return Numeric alpha value.
#' @importFrom yulab.utils get_cache_item
#' @export
get_hl_alpha <- function(default = 0.4) {
   x <- get_cache_item('gglycan')[["highlight_motif_alpha"]]
   if (is.null(x)) {
      x <- default
   }
   return(x)
}

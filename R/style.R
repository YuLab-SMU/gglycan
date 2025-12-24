#' Match SNFG Style to Graph Nodes
#'
#' Maps glycan node labels to SNFG shapes and colors.
#'
#' @param graph A `tbl_graph` or `igraph` object.
#' @return A graph object with additional columns: `snfg_shape` (numeric starshape) and `snfg_fill` (color).
#' @importFrom dplyr mutate case_when
#' @importFrom tidygraph as_tbl_graph activate
#' @export
match_snfg_style <- function(graph) {
  graph <- tidygraph::as_tbl_graph(graph)
  
  graph <- graph |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(
      base_type = case_when(
        grepl("GlcNAc", label) ~ "GlcNAc",
        grepl("GalNAc", label) ~ "GalNAc",
        grepl("ManNAc", label) ~ "ManNAc",
        grepl("Neu5Ac", label) ~ "Neu5Ac",
        grepl("Neu5Gc", label) ~ "Neu5Gc",
        grepl("Kdn", label) ~ "Kdn",
        grepl("GlcA", label) ~ "GlcA",
        grepl("GalA", label) ~ "GalA",
        grepl("ManA", label) ~ "ManA",
        grepl("IdoA", label) ~ "IdoA",
        grepl("Glc", label) ~ "Glc",
        grepl("Gal", label) ~ "Gal",
        grepl("Man", label) ~ "Man",
        grepl("Fuc", label) ~ "Fuc",
        grepl("Rha", label) ~ "Rha",
        grepl("Xyl", label) ~ "Xyl",
        grepl("Rib", label) ~ "Rib",
        grepl("Ara", label) ~ "Ara",
        TRUE ~ "Unknown"
      ),
      # ggstar shapes:
      # 15: Circle (Glc, Gal, Man)
      # 13: Square (GlcNAc, GalNAc, ManNAc)
      # 12: Diamond/4-point Star (Neu5Ac, etc)
      # 11: Triangle (Fuc, Rha)
      # 1: Star (Xyl, Rib, Ara)
      snfg_starshape = case_when(
        base_type %in% c("Glc", "Gal", "Man") ~ 15,
        base_type %in% c("GlcNAc", "GalNAc", "ManNAc") ~ 13,
        base_type %in% c("Neu5Ac", "Neu5Gc", "Kdn", "GlcA", "GalA", "ManA", "IdoA") ~ 12,
        base_type %in% c("Fuc", "Rha") ~ 11,
        base_type %in% c("Xyl", "Rib", "Ara") ~ 1,
        TRUE ~ 15 # Default circle
      ),
      snfg_fill = case_when(
        base_type == "Glc" ~ "#0090BC",
        base_type == "Gal" ~ "#FFD400",
        base_type == "Man" ~ "#00A651",
        base_type == "GlcNAc" ~ "#0090BC",
        base_type == "GalNAc" ~ "#FFD400",
        base_type == "ManNAc" ~ "#00A651",
        base_type == "Neu5Ac" ~ "#A54399",
        base_type == "Neu5Gc" ~ "#EBF0FC",
        base_type == "Kdn" ~ "#EBF0FC",
        base_type == "Fuc" ~ "#ED1C24",
        base_type == "Rha" ~ "#FFFFFF",
        base_type == "Xyl" ~ "#F47920",
        base_type == "Rib" ~ "#FFFFFF",
        base_type == "Ara" ~ "#FFFFFF",
        base_type == "GlcA" ~ "#0090BC",
        base_type == "GalA" ~ "#FFD400",
        base_type == "ManA" ~ "#00A651",
        base_type == "IdoA" ~ "#FFFFFF",
        TRUE ~ "white"
      )
    )
  
  return(graph)
}

#' Format Linkage Labels
#'
#' Converts 'a'/'b' to Greek letters alpha/beta.
#'
#' @param labels A character vector of linkage labels.
#' @return A character vector with Greek letters.
#' @export
format_linkage_label <- function(labels) {
  if (is.null(labels)) return(labels)
  labels <- sub("^a\\d-", " \u03b1", labels)
  labels <- sub("^b\\d-", " \u03b2", labels)
  return(labels)
}

#' SNFG Symbol Mapping
#'
#' A dataset containing the shapes and colors for SNFG symbols.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{monosaccharide}{The name of the monosaccharide (e.g., Glc, Gal, Man)}
#'   \item{shape}{The shape of the symbol (e.g., circle, square)}
#'   \item{color}{The fill color of the symbol (hex code)}
#'   \item{starshape}{The ggstar shape ID}
#' }
#' @export
snfg_map <- data.frame(
  monosaccharide = c("Glc", "Gal", "Man", 
                     "GlcNAc", "GalNAc", "ManNAc", 
                     "Neu5Ac", "Neu5Gc", "Kdn",
                     "Fuc", "Rha", 
                     "Xyl", "Rib", "Ara", 
                     "GlcA", "GalA", "ManA", "IdoA"),
  shape = c("circle", "circle", "circle", 
            "square", "square", "square", 
            "diamond", "diamond", "diamond",
            "triangle", "triangle", 
            "star", "star", "star", 
            "diamond", "diamond", "diamond", "diamond"),
  # Mapping to ggstar shapes:
  # 15: Circle (or similar)
  # 13: Square
  # 12: Diamond (4-pointed star looks like diamond, or use 23/28 if available)
  # 11: Triangle (3-pointed star)
  # 1: Star (5-pointed)
  # 27: Anise star (could be used for something else?)
  # Let's use 12 for Diamond for now.
  starshape = c(15, 15, 15, 
                13, 13, 13, 
                12, 12, 12, 
                11, 11, 
                1, 1, 1, 
                12, 12, 12, 12),
  color = c("#0090BC", "#FFD400", "#00A651", # Glc, Gal, Man
            "#0090BC", "#FFD400", "#00A651", # GlcNAc, GalNAc, ManNAc
            "#A54399", "#EBF0FC", "#EBF0FC", # Neu5Ac (Purple), Neu5Gc (Light Blue), Kdn (White?)
            "#ED1C24", "#FFFFFF",            # Fuc (Red), Rha (White?)
            "#F47920", "#FFFFFF", "#FFFFFF", # Xyl (Orange), Rib, Ara
            "#0090BC", "#FFD400", "#00A651", "#FFFFFF"), # GlcA, GalA, ManA, IdoA
  stringsAsFactors = FALSE
)

# Function to get mapping
get_snfg_style <- function(mono) {
  res <- snfg_map[snfg_map$monosaccharide == mono, ]
  if (nrow(res) == 0) {
    return(list(shape = "hexagon", color = "white", starshape = 27)) # Default for unknown
  }
  return(list(shape = res$shape, color = res$color, starshape = res$starshape))
}

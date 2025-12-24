
#' Read Glycan Data
#'
#' Parses a glycan string (IUPAC condensed format) and returns a graph object.
#'
#' @param x A character string representing the glycan structure.
#' @param format The format of the string. Currently only "iupac" is supported.
#' @return A `tbl_graph` object representing the glycan structure.
#' @importFrom tidygraph tbl_graph
#' @export
read_glycan <- function(x, format = "iupac") {
  format <- match.arg(format, c("iupac"))
  
  if (format == "iupac") {
    parsed <- parse_iupac_string(x)
    gr <- tidygraph::tbl_graph(nodes = parsed$nodes, edges = parsed$edges)
    return(gr)
  }
}

#' Parse IUPAC Condensed String
#'
#' Internal function to parse IUPAC string.
#'
#' @param iupac_string A string in IUPAC condensed format.
#' @return A list containing `nodes` and `edges` dataframes.
#' @noRd
parse_iupac_string <- function(iupac_string) {
  # Remove newlines or spaces
  iupac_string <- gsub("\\s+", "", iupac_string)
  
  # Tokenize
  s <- iupac_string
  s <- gsub("(\\[)", " \\1 ", s)
  s <- gsub("(\\])", " \\1 ", s)
  s <- gsub("(\\()", " \\1", s) # Start of link
  s <- gsub("(\\))", "\\1 ", s) # End of link
  
  tokens <- unlist(strsplit(s, "\\s+"))
  tokens <- tokens[tokens != ""]
  
  # Reverse tokens for processing from Reducing End
  tokens <- rev(tokens)
  
  nodes <- data.frame(id = integer(), label = character(), stringsAsFactors = FALSE)
  edges <- data.frame(from = integer(), to = integer(), label = character(), stringsAsFactors = FALSE)
  
  node_counter <- 1
  current_parent <- NA
  parent_stack <- list()
  
  i <- 1
  while (i <= length(tokens)) {
    tok <- tokens[i]
    
    if (tok == "]") {
      # Entering a branch
      if (is.na(current_parent)) {
        stop("Error: Found ']' without a valid parent node.")
      }
      parent_stack <- c(parent_stack, current_parent)
      i <- i + 1
      
    } else if (tok == "[") {
      # Exiting a branch
      if (length(parent_stack) == 0) {
        stop("Error: Unbalanced brackets.")
      }
      current_parent <- parent_stack[[length(parent_stack)]]
      parent_stack <- parent_stack[-length(parent_stack)]
      i <- i + 1
      
    } else if (grepl("^\\(", tok)) {
      # Linkage
      linkage <- gsub("^\\(|\\)$", "", tok)
      
      # Next token must be the node
      i <- i + 1
      if (i > length(tokens)) break
      node_tok <- tokens[i]
      
      if (node_tok == "]") {
         stop("Error: Linkage followed by ']'")
      }
      
      child_id <- node_counter
      node_counter <- node_counter + 1
      nodes <- rbind(nodes, data.frame(id = child_id, label = node_tok))
      
      edges <- rbind(edges, data.frame(from = child_id, to = current_parent, label = linkage))
      
      current_parent <- child_id
      i <- i + 1
      
    } else {
       # Node without linkage (Root or implicit)
       node_id <- node_counter
       node_counter <- node_counter + 1
       nodes <- rbind(nodes, data.frame(id = node_id, label = tok))
       
       if (!is.na(current_parent)) {
         # Implicit linkage
         edges <- rbind(edges, data.frame(from = node_id, to = current_parent, label = "?"))
       }
       
       current_parent <- node_id
       i <- i + 1
    }
  }
  
  return(list(nodes = nodes, edges = edges))
}

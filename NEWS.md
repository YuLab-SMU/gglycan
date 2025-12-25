# gglycan 0.0.2 (2025-12-25, Thu)

- Remove `tidygraph` dependency. 
- Use `igraph` for internal graph data structure and manipulation.

# gglycan 0.0.1 (2025-12-24, Wed)

- `gglycan()` to create a ggplot2 object for glycan plotting. 
- `geom_glycan()` to plot glycans using ggplot2.
- `read_glycan()` to parse IUPAC condensed strings into graph objects.
- `highlight_motif()` to highlight specific glycan substructures (motifs).
- `match_snfg_style()` to automatically map monosaccharides to SNFG shapes and colors.
- `snfg_map` dataset containing SNFG symbol definitions.
- Support for custom layouts via `ggtangle` and standard SNFG symbols via `ggstar`.
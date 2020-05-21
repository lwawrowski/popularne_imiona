# https://data-steve.github.io/d3-r-chord-diagram-of-white-house-petitions-data/

curl::curl_download(
  "https://github.com/yoni/r_we_the_people/blob/master/data/petitions.RData?raw=true"
  , destfile="petitions.RData" )
load("petitions.RData")

# recover tag names and ids
p <- petitions   # save some typing
ids_names <- rbind(    
  p[, c("issues1.id", "issues1.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues2.id", "issues2.name")] %>% setNames(c("ids", "names"))
  , p[, c("issues3.id", "issues3.name")]%>% setNames(c("ids", "names"))
) %>%
  unique() %>% na.omit()


# get only petitions with multi-tags
tag_count <- p %>%              
  select(id, issues1.id, issues2.id, issues3.id) %>%
  tidyr::gather(order, cats, -id) %>%
  filter(!is.na(cats)) %>%
  mutate(order = parse_number(order)) %>%
  left_join(ids_names, by=c("cats"="ids"))

xtab_tag <- tag_count %>%
  count(names) %>%
  arrange(desc(n))


tags <- sort(unique(ids_names$names))

# matrix to hold counts
mat <- matrix(0,nrow=nrow(tag_count),ncol=length(tags))
colnames(mat) <- tags


# get columns with tags from dataframe
p_id_nam <- p %>%
  select(contains(".name")) %>%
  mutate(issues1.name= ifelse(is.na(issues1.name), issues.name, issues1.name)) %>%
  mutate_all(funs(ifelse(is.na(.), "", .)))

# make matrix
for (i in seq_along(tags)) {
  for (j in c(1,2,3)){ # 1,2,3 are columns I want
    mat[,i] <- as.numeric(tags[i]==p_id_nam[[j]]) +  mat[,i]
    is.na(mat[,i]) <- 0
  }
}

adjmat <- t(mat) %*% mat

colorCount <- length(tags)

# makes function to create palette
getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

remove_diags <- function(mat, rm.lower = TRUE, ...) {
  diag(mat) <- 0
  if (isTRUE(rm.lower)) mat[lower.tri(mat)] <- 0
  mat
}

# ## order plot layering by smallest to largest so larges are on top
ord <- order(rowSums(remove_diags(adjmat, FALSE)))

chorddiag::chorddiag(adjmat[ord, ord], margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8  # have to shrink font for web viewing
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors = getPalette(colorCount)    
)

chorddiag::chorddiag(remove_diags(adjmat[ord, ord], FALSE), margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8       
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors =getPalette(colorCount)) 

adjmat_diag <- adjmat[-diag(adjmat)]

chorddiag::chorddiag(adjmat[-diag(adjmat)], margin = 150, showTicks =FALSE
                     , groupnameFontsize = 8       
                     , groupnamePadding = 5
                     , groupThickness = .05
                     , chordedgeColor = "gray90"
                     , groupColors =getPalette(colorCount)) 
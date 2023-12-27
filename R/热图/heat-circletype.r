set.seed(123)
nr1 = 4; nr2 = 8; nr3 = 6; nr = nr1 + nr2 + nr3
nc1 = 6; nc2 = 8; nc3 = 10; nc = nc1 + nc2 + nc3
mat = cbind(rbind(matrix(rnorm(nr1*nc1, mean = 1,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc1, mean = 0,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc1, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc2, mean = 0,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc2, mean = 1,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc2, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc3, mean = 0.5, sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc3, mean = 0.5, sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc3, mean = 1,   sd = 0.5), nr = nr3))
   )
mat = mat[sample(nr, nr), sample(nc, nc)] # random shuffle rows and columns
rownames(mat) = paste0("row", seq_len(nr))
colnames(mat) = paste0("column", seq_len(nc))
dim(mat)
pacman::p_load("lmbio","ComplexHeatmap","stringr","dplyr","circlize")
small_mat = mat[1:9, 1:9]
b = mat[1:9, 1:9]
a <- pheatmap(small_mat,scale = "row")
small_mat <- as.data.frame(a@matrix)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
ht_list <- Heatmap(small_mat, name = "mat", cluster_columns = FALSE, col = col_fun,rect_gp = gpar(type = "none"),
    cell_fun = function(j, i, x, y, width, height, fill) {
        grid.circle(x = x, y = y, r =abs(small_mat[i, j])/5 * min(unit.c(width, height)), 
                gp = gpar(fill = col_fun(small_mat[i, j]), col = NA))
})

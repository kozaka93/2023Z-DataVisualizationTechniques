library(dplyr)
library(plotly)
library(htmlwidgets)


expand_df <- function(df) {
        result_df <- df[0, ]
        for (i in 1:nrow(df)) {
                replicate_times <- max(df$f) - df$f[i] + 1

                for (j in 0:(replicate_times - 1)) {
                        replicated_row <- df[i, ]
                        replicated_row$f <- df$f[i] + j
                        result_df <- rbind(result_df, replicated_row)
                }
        }
        
        row.names(result_df) <- NULL
        return(result_df)
}

add_frame <- function(df) {
        result_df <- df %>% filter(f == max(df$f))
        result_df$f <- max(df$f) + 1
        return(rbind(df, result_df))
}


make_df <- function(n) {
        phi <- runif(n) * 2 * pi * 10
        r <- abs(2 * pi * 10 - phi)
        
        z <- (1 - r) * 10 + rnorm(n, 0, 0.2 * (max(r) - min(r))) * (r/(max(r) - min(r)))
        x <- r * cos(phi) + rnorm(n, 0, 0.1) * r
        y <- r * sin(phi) + rnorm(n, 0, 0.1) * r
        df <- data.frame(x = x, y = y, z = z, f = ceiling(z / min(z) * (10)))
        
        return(df)
}

n <- 10000
df <- make_df(n)
df <- expand_df(df)

# add star and baubles
df <- add_frame(df)
df[nrow(df) + 1, ] <- c(0, 0, 20, max(df$f))

m <- 100
ornaments <- make_df(m)
ornaments$f <- max(df$f)

df <- rbind(df, ornaments)

size <- c(rep(2, nrow(df) - m - 1), 20, rep(10, m))
color <- c(rep("rgb(37, 115, 52)", nrow(df) - m - 1), "rgb(230, 200, 9)", rep("rgb(219, 33, 33)", m))
symbol <- c(rep("circle", nrow(df) - m - 1), "diamond", rep("circle", m))

fig <- plot_ly(df, x=~x, y=~y, z=~z, frame = ~f) %>%
        add_trace(type = "scatter3d", mode = "markers",  opacity = 1,
                  marker=list(size=size, color=color, symbol=symbol,
                              line=list(color=color)))%>% 
        layout(showlegend = FALSE,
               scene = list(
                xaxis = list(title = '', range = c(-60, 60), showticklabels = FALSE, zeroline = FALSE), 
                yaxis = list(title = '', range = c(-60, 60), showticklabels = FALSE, zeroline = FALSE),
                zaxis = list(title = '', range = c(-600, 20), showticklabels = FALSE, zeroline = FALSE))) %>% 
        animation_opts(
                frame = 250,
                redraw = TRUE)
fig

saveWidget(fig, "christmass_tree.html", selfcontained = TRUE)

setwd("C:\\Users\\Dennis\\school\\thesis\\projects\\wasm-superoptimizer\\benchmarks")
library("ggpubr")

# Taken from StackOverflow
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

find_speedup <- function( a, b ) {
  res <- t.test( a, b, conf.int = T )

  if ( res$p.value >= 0.05 ) { # No difference between samples
    1
  } else {
    a.speed <- mean(a[!is.na(a)])
    b.speed <- mean(b[!is.na(b)])

    if ( b.speed < a.speed ) { # It got faster
      ( a.speed - min( res$conf.int ) ) / a.speed
    } else { # It got slower
      ( a.speed + min( res$conf.int ) ) / a.speed
    }
  }
}

build_results <- function( bmarks, rows ) {
  res <- c()
  for ( row in rows ) {
    name <- bmarks[row,1]
    # print( name )
    prog1 <- remove_outliers( as.numeric( bmarks[row,2:length(bmarks[row,])] ) )
    prog2 <- remove_outliers( as.numeric( bmarks[row+1,2:length(bmarks[row+1,])] ) )
    speedup <- find_speedup( prog1, prog2 )
  
    res.append <- c( speedup )
    names(res.append)[1] <- name

    res <- c( res, res.append )
  }
  res
} 


bmarks_native  <- read.csv(file = 'benchmarks_native.csv', header = FALSE)
bmarks_chrome  <- read.csv(file = 'benchmarks_chrome.csv', header = FALSE)
bmarks_firefox <- read.csv(file = 'benchmarks_firefox.csv', header = FALSE)

bmarks_native[,1]

rows = c(1,3,5,7,9)
res_native  <- build_results( bmarks_native, rows )
res_chrome  <- build_results( bmarks_chrome, rows )
res_firefox <- build_results( bmarks_firefox, rows )


res_native
res_chrome
res_firefox

row = 9
bmarks_native[,1]
hist( as.numeric( bmarks_native[row,2:length(bmarks_native[row,])] ) )

t = matrix( c(res_native, res_chrome, res_firefox), nrow=3, byrow = TRUE )

colnames(t) = names(res_native)

bp <- barplot(t, beside=TRUE, ylim=c(0.0,1.4), legend=TRUE) # main="reduced execution speeds"
legend("topright", 
       legend = c("wasmer", "chrome", "firefox"), 
       fill = c("#4D4D4D", "#AEAEAE", "#E6E6E6"),
	 ncol = 1,
       cex = 1.1)
abline(h = 1)
res <- c(rbind(res_native, res_chrome,res_firefox))


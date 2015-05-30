#' Draw pairwise venn diagram
#'
#' @param a       size of set A
#' @param b       size of set B
#' @param ab      size of shared part between A & B
#' @param name    names of A & B
#' @param border  color of circle's border
#' @param col     colors of A & B
#' @param main    main title of the diagram
#' @param padding padding size of the diagram, for display names
#' @param cex     change font size
#'
#' @examples
#' venn2(7, 10, 3)
venn2 <- function(a, b, ab,
                  name = c("", ""),
                  border = "black",
                  col = c(rgb(1, 0, 0, .2), rgb(0, 0, 1, .2)),
                  main = "",
				  padding = c(0, .15, .3, .15),
				  cex = 1)
{
	circle <- function(x, y, r, ...)
	{
		polygon(x + r * sin(2 * pi * (1:100) / 100),
				y + r * cos(2 * pi * (1:100) / 100), ...)
	}

    stopifnot(length(a) == 1 && length(b) == 1 && length(c) == 1)
    stopifnot(a >= ab && b >= ab)
    plot.new()
	width <- 1 - padding[2] - padding[4]
	height <- 1 - padding[1] - padding[3]
    plot.window(c(-padding[2] / width, 1 + padding[4] / width),
				c(-padding[1] / height, 1 + padding[3] / height))

    max_ratio <- 10

    ra <- sqrt(a)
    rb <- sqrt(b)
	rab <- sqrt(ab)
    if (ra > rb * max_ratio) {
        ra <- rb * max_ratio
    } else if (ra * max_ratio < rb) {
        rb <- ra * max_ratio
    }

    w <- 2 * (ra + rb - rab)
    circle(ra / w, .5, ra / w, border = border, col = col[1])
    circle(1 - rb / w, .5, rb / w, border = border, col = col[2])

    xa <- (ra - rab) / w
    xb <- 1 - (rb - rab) / w
    xab <- (2 * ra - rab) / w
    text(0, .6, name[1], adj = c(1, 0), cex = 1.2 * cex)
    text(1, .6, name[2], adj = c(0, 0), cex = 1.2 * cex)

    if (a - ab > 0) {
		if (ra - rab < w / 10) {
			text(0, .45, a - ab, adj = c(1, 1), cex = cex)
		} else {
			text(xa, .5, a - ab, adj = c(.5, .5), cex = cex)
		}
	}
    if (ab > 0) {
		text(xab, .5, ab, adj = c(.5, .5), cex = cex)
	}
    if (b - ab > 0) {
		if (rb - rab < w / 10) {
			text(1, .45, b - ab, adj = c(0, 1), cex = cex)
		} else {
			text(xb, .5, b - ab, adj = c(.5, .5), cex = cex)
		}
	}

    if (!is.na(main)) {
		text(.5, 1 + padding[3] / height / 2,
			 label = main, adj = c(.5, .5), cex = 1.5 * cex)
	}

	NULL
}

picto_scale <- function(aesthetic, values = NULL, ...) {
    
    values <- if (is_missing(values)) "circle" else force(values)
    
    pal <- function(n) {
        vapply(
            if (n > length(values)) rep(values[[1]], n) else values,
            function(.x) .fa_unicode[.fa_unicode[["name"]] == .x, "unicode"],
            character(1),
            USE.NAMES = FALSE
        )
    }
    
    discrete_scale(aesthetic, "manual", pal, ...)
}

#' Used with geom_pictogram() to map Font Awesome fonts to labels
#'
#' @param ... dots
#' @param values values
#' @param aesthetics aesthetics
#' @export
scale_label_pictogram <- function(..., values, aesthetics = "label") {
    picto_scale(aesthetics, values, ...)
}

#' Legend builder for pictograms
#'
#' @param data,params,size legend key things
#' @keywords internal
#' @export
draw_key_pictogram <- function(data, params, size) {
    
    # msg("==> draw_key_pictogram()")
    #
    # print(str(data, 1))
    # print(str(params, 1))
    
    if (is.null(data$label)) data$label <- "a"
    
    textGrob(
        label = data$label,
        x = 0.5, y = 0.5,
        rot = data$angle %||% 0,
        hjust = data$hjust %||% 0,
        vjust = data$vjust %||% 0.5,
        gp = gpar(
            col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
            fontfamily = data$family %||% "",
            fontface = data$fontface %||% 1,
            fontsize = (data$size %||% 3.88) * .pt,
            lineheight = 1.5
        )
    )
}

#' Pictogram Geom
#'
#' There are two special/critical `aes()` mappings:
#' - `label` (so the geom knows which column to map the glyphs to)
#' - `values` (which column you're mapping the filling for the squares with)
#'
#' @md
#' @param mapping Set of aesthetic mappings created by `aes()` or
#'   `aes_()`. If specified and `inherit.aes = TRUE` (the
#'   default), it is combined with the default mapping at the top level of the
#'   plot. You must supply `mapping` if there is no plot mapping.
#' @param n_rows how many rows should there be in the waffle chart? default is 10
#' @param flip If `TRUE`, flip x and y coords. n_rows then becomes n_cols.
#'     Useful to achieve waffle column chart effect. Defaults is `FALSE`.
#' @param make_proportional compute proportions from the raw values? (i.e. each
#'        value `n` will be replaced with `n`/`sum(n)`); default is `FALSE`.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to `ggplot()`.
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    `fortify()` for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame.`, and
#'    will be used as the layer data.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. `borders()`.
#' @param ... other arguments passed on to `layer()`. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `color = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#' @export
geom_pictogram <- function(mapping = NULL, data = NULL,
                           n_rows = 10, make_proportional = FALSE, flip = FALSE,
                           ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    
    layer(
        data = data,
        mapping = mapping,
        stat = "waffle",
        geom = "pictogram",
        position = "identity",
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            n_rows = n_rows,
            make_proportional = make_proportional,
            flip = flip,
            ...
        )
    )
}

#' @rdname geom_pictogram
#' @export
GeomPictogram <- ggplot2::ggproto(
    `_class` = "GeomPictogram",
    `_inherit` = GeomText,
    
    #  required_aes = c("x", "y", "label", "colour"),
    
    default_aes = aes(
        fill = NA, alpha = NA, colour = "black",
        size = 9, angle = 0, hjust = 0.5, vjust = 0.5,
        family = "FontAwesome5Free-Solid", fontface = 1, lineheight = 1
    ),
    
    
    draw_group = function(self, data, panel_params, coord,
                          n_rows = 10, make_proportional = FALSE, flip = FALSE,
                          radius = grid::unit(0, "npc")) {
        
        # msg("Called => GeomPictogram::draw_group()")
        
        coord <- ggplot2::coord_equal()
        grobs <- GeomText$draw_panel(data, panel_params, coord, parse = FALSE, check_overlap = FALSE)
        
        # msg("Done With => GeomPictogram::draw_group()")
        
        ggname("geom_pictogram", grid::grobTree(children = grobs))
        
    },
    
    
    draw_panel = function(self, data, panel_params, coord,
                          n_rows = 10, make_proportional = FALSE, flip = FALSE, ...) {
        
        # msg("Called => GeomPictogram::draw_panel()")
        # print(str(data, 1))
        
        coord <- ggplot2::coord_equal()
        grobs <- GeomText$draw_panel(data, panel_params, coord, parse = FALSE, check_overlap = FALSE)
        
        # msg("Done With => GeomPictogram::draw_panel()")
        
        ggname("geom_pictogram", grid::grobTree(children = grobs))
        
    },
    
    draw_key = draw_key_pictogram
    
)


#' @rdname geom_waffle
#' @export
stat_waffle <- function(mapping = NULL, data = NULL, geom = "waffle",
                        n_rows = 10, make_proportional = FALSE, flip = FALSE,
                        radius = grid::unit(0, "npc"),
                        na.rm = NA, show.legend = NA,
                        inherit.aes = TRUE, ...) {
    
    # msg("Called => stat_waffle::stat_waffle()")
    # msg("Done With => stat_waffle::stat_waffle()")
    
    layer(
        stat = StatWaffle,
        data = data,
        mapping = mapping,
        geom = geom,
        position = "identity",
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        check.param = FALSE,
        params = list(
            na.rm = na.rm,
            n_rows = n_rows,
            make_proportional = make_proportional,
            flip = flip,
            radius = radius,
            ...
        )
    )
}

#' @rdname geom_waffle
#' @export
StatWaffle <- ggplot2::ggproto(
    
    `_class` = "StatWaffle",
    `_inherit` = ggplot2::Stat,
    
    extra_params = c("na.rm", "n_rows", "make_proportional", "flip", "radius"),
    
    required_aes = c("fill", "values", "colour", "label"),
    
    setup_params = function(data, params) {
        # msg("Called => StatWaffle::setup_params()")
        # msg("Done With => StatWaffle::setup_params()")
        params
    },
    
    setup_data = function(data, params) {
        
        # msg("Called => StatWaffle::setup_data()")
        #
        # print(str(data, 1))
        # print(str(params, 1))
        
        use <- if ("label" %in% names(data)) "label" else "fill"
        
        if (inherits(data[[use]], "factor")) {
            flvls <- levels(data[[use]])
        } else {
            flvls <- levels(factor(data[[use]]))
        }
        
        if (inherits(data[["colour"]], "factor")) {
            clvls <- levels(data[["colour"]])
        } else {
            clvls <- levels(factor(data[["colour"]]))
        }
        
        if (!("colour" %in% names(data))) {
            if ("colour" %in% names(params)) {
                data[["colour"]] <- params[["colour"]]
            } else {
                data[["colour"]] <- "white"
            }
            clvls <- levels(factor(data[["colour"]]))
        } else {
            if (any(is.na(as.character(data[["colour"]])))) {
                data[["colour"]] <- "white"
                clvls <- levels(factor(data[["colour"]]))
            } else {
                data[["colour"]] <- as.character(data[["colour"]])
            }
        }
        
        # msg("       => StatWaffle::setup_data() : colour")
        # print(str(data, 1))
        
        p <- split(data, data$PANEL)
        
        lapply(p, function(.x) {
            
            if (params[["make_proportional"]]) {
                .x[["values"]] <- .x[["values"]] / sum(.x[["values"]])
                .x[["values"]] <- round_preserve_sum(.x[["values"]], digits = 2)
                .x[["values"]] <- as.integer(.x[["values"]] * 100)
            }
            
            parts_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
                rep(as.character(.x[[use]][i]), .x[["values"]][i])
            }))
            
            pgrp_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
                rep(.x[["group"]], .x[["values"]][i])
            }))
            
            # print(str(.x, 1))
            
            colour_vec <- unlist(sapply(1:length(.x[[use]]), function(i) {
                rep(.x[["colour"]][i], .x[["values"]][i])
            }))
            
            expand.grid(
                y = 1:params$n_rows,
                x = seq_len((ceiling(sum(.x[["values"]]) / params$n_rows)))#,
                # stringsAsFactors = FALSE
            ) -> tdf
            
            parts_vec <- c(parts_vec, rep(NA, nrow(tdf)-length(parts_vec)))
            colour_vec <- c(colour_vec, rep(NA, nrow(tdf)-length(colour_vec)))
            
            # tdf$parts <- parts_vec
            tdf[["values"]] <- NA
            tdf[["colour"]] <- colour_vec
            tdf[[use]] <- parts_vec
            tdf[["PANEL"]] <- .x[["PANEL"]][1]
            tdf[["group"]] <- 1:nrow(tdf)
            
            tdf <- tdf[sapply(tdf[[use]], function(x) !is.na(x)),]
            
        }) -> p
        
        p <- plyr::rbind.fill(p)
        p[[use]] <- factor(p[[use]], levels=flvls)
        p[["colour"]] <- factor(p[["colour"]], levels = clvls)
        
        # print(str(p, 1))
        #
        # msg("Done With => StatWaffle::setup_data()")
        # data
        
        wdat <- p
        
        if (params$flip) {
            x_temp <- wdat$x
            wdat$x <- wdat$y
            wdat$y <- x_temp
            x_temp <- NULL
        }
        
        wdat$width <- wdat$width %||% params$width %||% ggplot2::resolution(wdat$x, FALSE)
        wdat$height <- wdat$height %||% params$height %||% ggplot2::resolution(wdat$y, FALSE)
        
        transform(
            wdat,
            xmin = x - width / 2,
            xmax = x + width / 2,
            width = NULL,
            ymin = y - height / 2,
            ymax = y + height / 2,
            height = NULL
        ) -> p
        
        p
        
    },
    
    compute_layer = function(self, data, params, layout) {
        # msg("Called => StatWaffle::compute_layer()")
        # print(str(data, 1))
        # print(str(params, 1))
        # msg("Done With => StatWaffle::compute_layer()")
        data
    },
    
    finish_layer = function(self, data, params) {
        # msg("Called => StatWaffle::finish_layer()")
        # msg("Done With => StatWaffle::finish_layer()")
        data
    },
    
    compute_panel = function(self, data, scales, ...) {
        # msg("Called => StatWaffle::compute_panel()")
        # msg("Done With => StatWaffle::compute_panel()")
        data
    }
    
)

round_preserve_sum <- function(x, digits = 0) {
    up <- 10^digits
    x <- x * up
    y <- floor(x)
    indices <- tail(order(x - y), round(sum(x)) - sum(y))
    y[indices] <- y[indices] + 1
    y / up
}


is_missing_arg <- function(x) identical(x, quote(expr = ))
is_missing <- function(x) identical(x, quote(expr = ))


# VIA: http://stackoverflow.com/q/13294952/1457051

rbind_gtable_max <- function(...) {
    
    gtl <- list(...)
    
    stopifnot(all(sapply(gtl, is.gtable)))
    
    bind2 <- function (x, y) {
        
        stopifnot(ncol(x) == ncol(y))
        
        if (nrow(x) == 0) return(y)
        if (nrow(y) == 0) return(x)
        
        y$layout$t <- y$layout$t + nrow(x)
        y$layout$b <- y$layout$b + nrow(x)
        x$layout <- rbind(x$layout, y$layout)
        
        x$heights <- insert_unit(x$heights, y$heights)
        x$rownames <- c(x$rownames, y$rownames)
        x$widths <- unit.pmax(x$widths, y$widths)
        x$grobs <- append(x$grobs, y$grobs)
        
        x
        
    }
    Reduce(bind2, gtl)
    
}

cbind_gtable_max <- function(...) {
    
    gtl <- list(...)
    
    stopifnot(all(sapply(gtl, is.gtable)))
    
    bind2 <- function (x, y) {
        
        stopifnot(nrow(x) == nrow(y))
        
        if (ncol(x) == 0) return(y)
        if (ncol(y) == 0) return(x)
        
        y$layout$l <- y$layout$l + ncol(x)
        y$layout$r <- y$layout$r + ncol(x)
        x$layout <- rbind(x$layout, y$layout)
        
        x$widths <- insert_unit(x$widths, y$widths)
        x$colnames <- c(x$colnames, y$colnames)
        x$heights <- unit.pmax(x$heights, y$heights)
        x$grobs <- append(x$grobs, y$grobs)
        
        x
        
    }
    
    Reduce(bind2, gtl)
    
}

insert_unit <- function (x, values, after = length(x)) {
    
    lengx <- length(x)
    
    if (lengx == 0) return(values)
    if (length(values) == 0) return(x)
    
    if (after <= 0) {
        unit.c(values, x)
    } else if (after >= lengx) {
        unit.c(x, values)
    } else {
        unit.c(x[1L:after], values, x[(after + 1L):lengx])
    }
    
}

# Name ggplot grid object
# Convenience function to name grid objects
#
# @keyword internal
ggname <- function(prefix, grob) {
    grob$name <- grid::grobName(grob, prefix)
    grob
}

"%||%" <- function(a, b) { if (!is.null(a)) a else b }
"%l0%" <- function(a, b) { if (length(a)) a else b }

.pt <- ggplot2::.pt

#' Waffle chart theme cruft remover that can be used with any other theme
#'
#' Removes:
#'
#' - panel grid
#' - all axis text
#' - all axis ticks
#' - all axis titles
#'
#' @md
#' @export
theme_enhance_waffle<- function() {
    
    ret <- theme(panel.grid = element_blank())
    ret <- ret + theme(axis.text = element_blank())
    ret <- ret + theme(axis.text.x = element_blank())
    ret <- ret + theme(axis.text.y = element_blank())
    ret <- ret + theme(axis.title = element_blank())
    ret <- ret + theme(axis.title.x = element_blank())
    ret <- ret + theme(axis.title.x.top = element_blank())
    ret <- ret + theme(axis.title.x.bottom = element_blank())
    ret <- ret + theme(axis.title.y = element_blank())
    ret <- ret + theme(axis.title.y.left = element_blank())
    ret <- ret + theme(axis.title.y.right = element_blank())
    
    ret
    
}

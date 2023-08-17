home.38_06_06_130655 <- path.expand("~")
hs.home.expand <- function(fp1, q = 0) {
    for (i in seq_along(fp1)) {
        a <- fp1[i]
        if ((a == "~") || (substr(a, 1, 2) == "~/")) 
            fp1[i] <- paste0(home.38_06_06_130655, substring(a, 2))
    }
    if (q) 
        fp1 <- shQuote(fp1)
    fp1
}
hs.machine.isLocal <- function() {
    file.exists("~/wk/db/这里是本地")
}
wk.dm.wjj <- switch(Sys.info()["nodename"], node9 = "/dev/shm/wk/dm/r", hs.home.expand("~/org/dm/r"))
hs.getTextFile <- function(wj1) {
    wj1 <- hs.home.expand(wj1)
    if (file.exists(wj1)) 
        return(wj1)
    wj2 <- paste0(wj1, ".xz")
    if (file.exists(wj2)) 
        return(wj2)
    wj2 <- paste0(wj1, ".gz")
    if (file.exists(wj2)) 
        return(wj2)
}
hs.readLines.robust <- function(wj) {
    con1 <- gzfile(hs.getTextFile(wj))
    on.exit(close(con1))
    unlist(strsplit(readChar(con1, 1e+07), "[\r\n]+"))
}
hs.source.robust <- function(wj, env = .GlobalEnv) {
    eval(expr = parse(text = hs.readLines.robust(wj = hs.getTextFile(wj1 = wj))), envir = env)
}
hs.gre <-
structure(function (s1, p1, r1 = {
}, match = 0, perl = 1, ignore.case = 1, useBytes = 0, fixed = 0, no_match = {
}, ...) 
{
    m1 <- gregexpr(p1, s1, perl = as.logical(perl), ignore.case = as.logical(ignore.case), fixed = as.logical(fixed), useBytes = as.logical(useBytes))
    if (match) 
        return(m1)
    0L
    if (length(r1)) {
        hs.regmatches(s1, m1, no_match = no_match, replacement = r1, ...)
        s1
    }
    else hs.regmatches(s1, m1, no_match = no_match, ...)
}, usage = "【功能】提取或替换模式. s1: string. p1: pattern. r1: replacement. match: if true, return regmatches.")
hs.grep <-
function (x, pattern, fixed = FALSE, perl = !fixed, ...) 
grep(pattern, x, ..., perl = perl, fixed = fixed)
hs.grepl <-
function (x, pattern, fixed = FALSE, perl = !fixed, ...) 
grepl(pattern, x, ..., perl = perl, fixed = fixed)
hs.grepV <-
function (..., hs0 = {
}) 
{
    if (length(hs0)) {
        xi <- match("x", ...names(), nomatch = 1)
        x <- hs0(...elt(xi))
        L <- list(...)
        L[[xi]] <- x
        i <- do.call(hs.grep, L)
        ...elt(xi)[i]
    }
    else hs.grep(..., v = 1)
}
hs.gsub <-
function (x, pattern, replacement = "", fixed = FALSE, perl = !fixed, ...) 
gsub(pattern, x, replacement = replacement, ..., perl = perl, fixed = fixed)
hs.re <-
structure(function (s1, p1, r1 = {
}, match = 0, perl = 1, ignore.case = 1, useBytes = 0, fixed = 0, no_match = {
}, ..., after = FALSE) 
{
    m1 <- regexpr(p1, s1, perl = as.logical(perl), ignore.case = as.logical(ignore.case), fixed = as.logical(fixed), useBytes = as.logical(useBytes))
    if (match) 
        return(m1)
    if (after) 
        return(substring(s1, m1 + attr(m1, "match.length")))
    if (length(r1)) {
        hs.regmatches(s1, m1, no_match = no_match, replacement = r1, ...)
        s1
    }
    else hs.regmatches(s1, m1, no_match = no_match, ...)
}, usage = "【功能】提取或替换模式. s1: string. p1: pattern. r1: replacement. match: if true, return regmatches.")
hs.sub <-
function (x, pattern, replacement = "", fixed = FALSE, perl = !fixed, ...) 
sub(pattern, x, replacement = replacement, ..., perl = perl, fixed = fixed)
hs.colAlls <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colAlls"(x, na.rm = na.rm, ...)
}
hs.colAnys <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colAnys"(x, na.rm = na.rm, ...)
}
hs.colCumsums <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colCumsums"(x, na.rm = na.rm, ...)
}
hs.colCvs <-
function (x) 
{
    Mean <- hs.colMeans(x)
    Sd <- hs.colSds(x)
    Sd/Mean
}
hs.colMads <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colMads"(x, na.rm = na.rm, ...)
}
hs.colMaxs <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colMaxs"(x, na.rm = na.rm, ...)
}
hs.colMeans <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colMeans2"(x, na.rm = na.rm, ...)
}
hs.colMedians <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colMedians"(x, na.rm = na.rm, ...)
}
hs.colMins <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colMins"(x, na.rm = na.rm, ...)
}
hs.colProds <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colProds"(x, na.rm = na.rm, ...)
}
hs.colSds <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colSds"(x, na.rm = na.rm, ...)
}
hs.colSums <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colSums2"(x, na.rm = na.rm, ...)
}
hs.colVars <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"colVars"(x, na.rm = na.rm, ...)
}
hs.rowAlls <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowAlls"(x, na.rm = na.rm, ...)
}
hs.rowAnys <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowAnys"(x, na.rm = na.rm, ...)
}
hs.rowCumsums <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowCumsums"(x, na.rm = na.rm, ...)
}
hs.rowCvs <-
function (x) 
{
    Mean <- hs.rowMeans(x)
    Sd <- hs.rowSds(x)
    Sd/Mean
}
hs.rowMads <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowMads"(x, na.rm = na.rm, ...)
}
hs.rowMaxs <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowMaxs"(x, na.rm = na.rm, ...)
}
hs.rowMeans <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowMeans2"(x, na.rm = na.rm, ...)
}
hs.rowMedians <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowMedians"(x, na.rm = na.rm, ...)
}
hs.rowMins <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowMins"(x, na.rm = na.rm, ...)
}
hs.rowProds <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowProds"(x, na.rm = na.rm, ...)
}
hs.rowSds <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowSds"(x, na.rm = na.rm, ...)
}
hs.rowSums <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowSums2"(x, na.rm = na.rm, ...)
}
hs.rowVars <-
function (x, na.rm = TRUE, ...) 
{
    if (!inherits(x, c("matrix", "Matrix"))) 
        x <- as.matrix(x)
    MatrixGenerics::"rowVars"(x, na.rm = na.rm, ...)
}
hs.grepV <- function(..., hs0 = {
}) {
    if (length(hs0)) {
        xi <- match("x", ...names(), nomatch = 1)
        x <- hs0(...elt(xi))
        L <- list(...)
        L[[xi]] <- x
        i <- do.call(hs.grep, L)
        ...elt(xi)[i]
    }
    else hs.grep(..., v = 1)
}
hs.grep_sub <- function(v, p, r = "", ...) {
    a <- hs.grepV(v, p, ...)
    hs.sub(a, p, r, ...)
}
hs.re.capture <- local({
    match.hs.capture <- function(m1, c1) {
        m2 <- attr(m1, "capture.start")[, c1]
        attr(m2, "match.length") <- attr(m1, "capture.length")[, c1]
        m2
    }
    function(s1, p1, c1 = {
    }, all = 0) {
        m1 <- (if (all) 
            gregexpr
        else regexpr)(p1, s1, perl = TRUE)
        if (is.list(m1)) 
            "capture.start" %in% names(attributes(m1[[1]]))
        cn.db <- attr(if (all) m1[[1]] else m1, "capture.names")
        c1 <- if (length(c1)) {
            if (is.character(c1)) {
                if (any(c1 %not.in% cn.db)) 
                  hs.browser("37.04.15.182833")
                match(c1, cn.db)
            }
            else if (is.numeric(c1)) {
                if (any(c1 %not.in% seq_along(cn.db))) 
                  hs.browser("37.04.15.184825")
                c1
            }
            else hs.browser("37.04.15.182809")
        }
        else seq_along(cn.db)
        jg <- lapply(c1, function(c1) {
            cm <- if (all) 
                lapply(m1, match.hs.capture, c1)
            else match.hs.capture(m1, c1)
            regmatches(s1, cm)
        })
        names(jg) <- cn.db
        jg
    }
})
hs.has_x <- function(E) {
    if (length(E) > 1) {
        any(sapply(E, hs.has_x))
    }
    else identical(E, quote(x))
}
"%=>%" <- local({
    hs_blank <- function(x, ...) {
    }
    add_x <- function(E) if (E[[1]] == quote(`[`)) {
        E[[2]] <- add_x(E[[2]])
        E
    }
    else as.call(append(as.list(E), quote(x), after = 1))
    function(x, e, env = parent.frame(), assign = 0, substitute = !assign, out = 0) {
        if (substitute) {
            e <- substitute(e)
            x <- substitute(x)
        }
        if (is.symbol(x) && (x == quote(.))) {
            if (is.symbol(e) && is.function(e)) {
                e
            }
            else {
                if (!hs.has_x(e)) 
                  e <- add_x(e)
                body(hs_blank) <- bquote({
                  .(e)
                })
                environment(hs_blank) <- env
                hs_blank
            }
        }
        else {
            if (is.symbol(e) && do.call(is.function, list(e), envir = env) || e[[1]] == quote(hs.hsgly)) {
                e <- bquote(.(e)(x))
            }
            else if (inherits(e, c("{", "("))) {
            }
            else if (!hs.has_x(e)) 
                e <- add_x(e)
            if (assign) {
                hs1 <- if (out) 
                  `<<-`
                else `<-`
                eval(bquote(.(hs1)(.(x), with(list(x = .(x)), .(e)))), envir = env)
            }
            else eval(bquote(with(list(x = .(x)), .(e))), envir = env)
        }
    }
})
"%<=>%" <- function(x, e, env = parent.frame()) {
    `%=>%`(x = substitute(x), e = substitute(e), env = env, assign = 1)
}
hs.with <- function(..., env.2023_07_21_174408 = parent.frame()) {
    A <- substitute(list(...))
    data <- head(A, -1)
    if (is.null(names(data))) {
        if (length(data) == 2) {
            names(data)[2] <- "x"
        }
        else if (length(data) > 2) {
            names(data)[2:length(data)] <- paste0("x", seq(length(data) - 1))
        }
    }
    e1 <- A[[length(A)]]
    if ((length(data) == 2) && (length(e1) == 1) && is.symbol(e1) && eval(bquote(is.function(.(e1))), envir = env.2023_07_21_174408)) 
        e1 <- bquote(.(e1)(x))
    e1 <- bquote(with(data = .(data), expr = .(e1)))
    eval(e1, envir = env.2023_07_21_174408)
}
hs.pipe1 <- function(..., env.2023_07_24_205312 = parent.frame()) {
    A <- substitute(list(...))
    hs.switch(...length(), 1, ..1, 0, {
    }, {
        x <- ..1
        for (i1 in seq(3, length(A))) x <- do.call("%=>%", list(x, A[[i1]]), envir = env.2023_07_24_205312)
        x
    })
}
hs.update <- function(x, e, hs.if = function(x) TRUE, env = parent.frame(), n = (length(x) == 1) && inherits(x, "character"), out = FALSE) {
    hs1 <- substitute(hs.if)
    target <- substitute(x)
    env
    do <- if ((is.name(hs1) && is.function(hs.if)) || (paste(as.list(hs1)[[1]]) == "function")) {
        eval(bquote(.(hs.if)(.(if (is.symbol(target)) 
            target
        else x))), envir = env)
    }
    else {
        eval(bquote(with(list(x = .(x)), .(hs1))), envir = env)
    }
    if (do) {
        exp <- substitute(e)
        if (is.name(exp) && is.function(e)) {
            exp <- bquote(.(exp)(x))
        }
        else if (hs.has_x(exp)) 
            exp <- bquote(with(list(x = .(target)), .(exp)))
        e1 <- bquote(.(if (out) 
            `<<-`
        else `<-`)(.(target), .(exp)))
        eval(e1, envir = env)
        return(TRUE)
    }
    return(FALSE)
    eval(bquote(.(hs1)(.(x))), envir = env)
    target <- substitute(x)
    eval(bquote(.(hs1)(.(target))), envir = env)
    if (identical(hs.if, missing)) {
        target <- substitute(x)
        eval(bquote(missing(.(target))), envir = env)
    }
    else {
        if (inherits(x, "character")) 
            x <- as.symbol(x)
        target <- substitute(x)
        if (!hs1(eval(target, envir = env))) 
            return("没更新")
    }
    exp <- substitute(e)
    if (is.name(exp) && is.function(e)) 
        exp <- bquote(.(exp)(x))
    e1 <- bquote(.(if (out) 
        `<<-`
    else `<-`)(.(target), .(if (is.symbol(target) && (!exists(paste(target), envir = env))) {
        substitute(exp)
    }
    else substitute(with(list(x = target), exp)))))
    invisible(eval(e1, envir = env))
}
"%<<=>%" <- function(x, e, env = parent.frame()) `%=>%`(x = substitute(x), e = substitute(e), env = env, assign = 1, out = 1)
..x <- function(up = 1) {
    env1 <- parent.frame()
    while (up) {
        env1 <- parent.env(env1)
        up <- up - 1
    }
    get("x", envir = env1)
}
hs.libPaths <- function(fp) {
    .libPaths(unique(c(fp, .libPaths())))
}
wk.true <- !0
wk.false <- !1
wk.null <- {
}
wk.t <- "\t"
wk.n <- "\n"
hs.havePackages <- function(...) {
    all(c(...) %in% .packages(1, .libPaths()))
}
hs.browser <- wk.browser <- function(text = "", condition = NULL, expr = TRUE, skipCalls = 0L, env = parent.frame(), debug = 0) {
    eval(bquote({
        if (.(if (debug) 
            debug4browse.38_09_01_174554
        else 1)) {
            message(rep("\n", 3), "browse at ", .(text), "\n")
            browser(.(text), .(condition), .(expr), .(skipCalls))
        }
    }), env)
}
mirror_db <- c(同济 = "https://mirrors.tongji.edu.cn/CRAN/", 上海交大 = "https://mirrors.sjtug.sjtu.edu.cn/cran/", 南大 = "https://mirrors.nju.edu.cn/CRAN/", 清华 = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/", 兰大 = "https://mirror.lzu.edu.cn/CRAN/", `0-Cloud-East-Asia` = "https://cran.asia/")
wk.lib <- file.path(R.home(), "library")
wk.home <- path.expand("~")
wk.lib.mine <- file.path(wk.home, ifelse(length(grep("(?i)virtualbox", Sys.info()["nodename"])), "rj2/lib/R", "rj/lib/R"))
wk_node_local <- c("wk-VirtualBox", "wk-mbp-2017-13.local", "kai-PC", "wk-NBLK-WAX9X", "calvin_mpb", "wk-ThinkPad-X220", "wk-MS-7A21", "wk-HP-EliteBook-Revolve-810-G2")
hs.wj.is.Symlink <- function(wj1) {
    wj1 %=>% sub("/*$", "", x) %=>% {
        file.exists(x) && (path.expand(x) != normalizePath(x))
    }
}
hs.with_wd <- function(wd = ".", exp1, skip_if_wd_not_exist = 0, local = 1, env1 = parent.frame(), temp = 0, temp.rm = 1) {
    if (skip_if_wd_not_exist && (!file.exists(wd))) 
        return()
    if (!dir.exists(wd)) 
        wd <- dirname(wd)
    if (temp) {
        wd %<=>% hs.wjj(.muid4machine())
        if (temp.rm) 
            on.exit(hs.wj.rm(wd, 1), add = TRUE)
    }
    if (!identical(normalizePath("."), normalizePath(wd))) {
        wjj.37_11_13_163320 <- getwd()
        dir.create(wd, recursive = TRUE)
        setwd(wd)
        on.exit(setwd(wjj.37_11_13_163320), add = TRUE)
    }
    eval(substitute(exp1), envir = if (local) 
        new.env(parent = env1)
    else env1)
}
hs.with_wd_temp <- function(wjj0 = ".", ...) {
    if (skip_if_wd_not_exist && (!file.exists(wd))) 
        return()
    if (!dir.exists(wd)) 
        wd <- dirname(wd)
    if (!identical(normalizePath("."), normalizePath(wd))) {
        wjj.37_11_13_163320 <- getwd()
        dir.create(wd, recursive = TRUE)
        setwd(wd)
        on.exit(setwd(wjj.37_11_13_163320))
    }
    eval(substitute(exp1), envir = if (local) 
        new.env(parent = env1)
    else env1)
}
hs.c <- function(name, value) {
    if (missing(value)) {
        value <- name
        a <- substitute(name)
        name <- as.character(a)
        if (class(a) == "call") 
            name <- name[-1]
    }
    names(value) <- name
    value
}
hs.c1 <- function(..., recursive = FALSE, use.names = FALSE) {
    a <- list(...)
    hs.c(names(a), hs.unlist(a, recursive = recursive, use.names = use.names))
}
hs.db <- function(x, inv = FALSE, ot = "nv") {
    if (ot != "nv") 
        hs.browser("2023.07.29.113830", debug = 0)
    x_ <- substitute(x)
    if (inherits(x_, "call")) 
        x <- as.list(substitute(x)[-1])
    if (!length(names(x))) 
        x <- if (inherits(x, "list")) 
            hs.c(unlist(x[seq(1, length(x), by = 2)]), unlist(x[seq(2, length(x), by = 2)]))
        else hs.c(x[seq(1, length(x), by = 2)], x[seq(2, length(x), by = 2)])
    if (inv) 
        x <- hs.c(x, names(x))
    x
}
hs.normalizePath <- function(fp) {
    suppressWarnings(normalizePath(fp))
}
hs.wjm.home <- local({
    h1 <- sprintf("^(%s$|%s(?=/))", home.38_06_06_130655, home.38_06_06_130655)
    Map <- c("~/org", "~/xt", "~/swxx", "~/1", "~") %=>% hs.c(normalizePath(x), x)
    need_abbreviation <- function(x, before) {
        y <- hs.normalizePath(x)
        L <- nchar(before)
        if ((nchar(y) == L) && (y == before)) 
            return(TRUE)
        if ((nchar(y) > L) && (substr(y, 1, L + 1) == paste0(before, "/"))) 
            return(TRUE)
        return(FALSE)
    }
    abbreviate <- function(x, before, after) paste0(after, substring(hs.normalizePath(x), nchar(before) + 1))
    function(wj1) {
        for (h in seq_along(wj1)) {
            x <- wj1[h]
            for (i in seq_along(Map)) {
                before <- names(Map)[i]
                if (need_abbreviation(x, before = before)) {
                  wj1[h] <- abbreviate(x, before = before, after = Map[i])
                  break
                }
            }
        }
        wj1
    }
})
hs.fp.components <- function(fp1) {
    jg <- rep(NA, length(gregexpr("/", fp1, fixed = TRUE)[[1]]) + 1)
    i1 <- length(jg)
    hs.browser("38.04.28.225512")
    repeat {
        if (grepl("^~/*$", fp1, perl = TRUE)) {
            jg[i1] <- "~"
            break
        }
        if (fp1 == "/") {
            jg[i1] <- ""
            break
        }
        bn1 <- basename(fp1)
        if (nzchar(bn1)) {
            jg[i1] <- bn1
            fp1 <- substr(fp1, 1, nchar(fp1) - nchar(bn1))
            basename("~/a/\\/")
        }
        else break
        i1 <- i1 - 1
        if (i1 < 0) 
            hs.browser("38.04.28.221919")
    }
    if (i1 > 1) 
        tail(jg, 1 - i1)
    else jg
}
hs.wj.symlink <- function(wj1, wjj1 = hs.home.expand("~/1/常用"), name = basename(wj1), R = 0, F = 0, ot = "") {
    hs.with_wd(wjj1, {
        wj2 <- file.path(wjj1, name)
        if (F) {
            unlink(wj2)
        }
        if (R) {
            a1 <- strsplit(c(wj1, wjj1), "/")
            depth.min <- min(sapply(a1, length))
            i1 <- 0
            repeat {
                i1 <- i1 + 1
                if (hs.any(i1 > depth.min, a1[[1]][i1] != a1[[2]][i1])) {
                  i1 <- i1 - 1
                  break
                }
            }
            wj1 <- paste(c(rep("..", length(a1[[2]]) - i1), tail(a1[[1]], -i1)), collapse = "/")
        }
        switch(ot, wj1 = wj1, file.symlink(wj1, wj2))
    })
}
file.is_symlink <- function(wj1) {
    nzchar(Sys.readlink(wj1), keepNA = TRUE)
}
hs.symlink.target <- function(wj1) {
}
hs.wj.symlink.force <- function(wj1, wj2) {
    if (file.exists(wj2)) {
        if (hs.normalizePath(wj1) != hs.normalizePath(wj2)) {
            unlink(wj2)
        }
    }
    if (file.exists(wj2)) {
    }
}
hs.wjj.create <- function(wj, force = 0) {
    if (!dir.exists(wj)) {
        if (file.exists(wj)) {
            file.rename(wj, paste0(wj, ".bf"))
        }
        dir.create(wj, recursive = TRUE)
    }
    wj
}
hs.seqForward <- function(from, to, ...) {
    if (to >= from) 
        seq.int(from = from, to = to, ...)
}
hs.all <- function(...) {
    for (i in hs.seqForward(1, ...length())) if (!...elt(i)) 
        return(FALSE)
    return(TRUE)
}
hs.any <- function(...) {
    for (i in hs.seqForward(1, ...length())) if (...elt(i)) 
        return(TRUE)
    return(FALSE)
}
hs.cat <- function(..., file = "", sep = if (less) "\n" else "", end = if (sep == "\n") "" else "\n", append = file.exists(hs.home.expand(file)), intern = FALSE, less = 0) {
    file <- hs.home.expand(file)
    if (intern) {
        capture.output(cat(..., sep = sep))
    }
    else {
        arg <- list(..., sep = sep, append = as.logical(append), file = file)
        if (less) 
            arg[["file"]] <- "| less -Ni"
        do.call("cat", arg)
        if (less) 
            return()
        if (nzchar(end)) 
            cat(end, append = TRUE, file = file)
        if (nzchar(file)) 
            return(file)
    }
}
hs.catn <- function(...) hs.cat(..., sep = "\n")
hs.say <- function(..., sep = " ") {
    hs.cat(..., sep = sep)
}
hs.info <- function(..., call. = TRUE, domain = NULL, appendLF = TRUE, stop = 0) {
    A <- list("\n\t", ..., "\n\n")
    A[["domain"]] <- domain
    if (stop) {
        hs <- "stop"
        A[["call."]] <- call.
    }
    else {
        hs <- "message"
        A[["appendLF"]] <- appendLF
    }
    do.call(hs, A)
}
.try <- function(expr, silent = TRUE) {
    op <- options(warn = -1)
    on.exit(options(op))
    try(expr, silent)
}
.try1 <- function(expr, value.on.error = NA, silent = TRUE) {
    on.exit(options(op))
    op <- options(warn = -1)
    r <- try(expr, silent)
    if (class(r) == "try-error") 
        value.on.error
    else r
}
hs.is_try_error <- function(x1) {
    is(x1, "try-error")
}
hs.load <- function(file, keep.name = FALSE, env = new.env(), mc = 0) {
    if (inherits(file, "connection")) {
        jg <- .try(load(file, envir = env))
        if (!hs.is_try_error(jg)) {
            if (keep.name || (length(jg) > 1)) {
                return(mget(jg, envir = env))
            }
            else return(env[[jg]])
        }
        hs.browser("2023.07.03.150920", debug = 0)
    }
    if (is.character(file)) 
        file %<=>% hs.home.expand
    if (hs.is_try_error(.try(infoRDS(file)))) {
        if (mc) {
            con1 <- .sys("pigz", "-cd", file)
            con1 %<=>% pipe(x, "rb")
            on.exit(close(con1), add = TRUE)
            b <- load(con1, envir = env)
        }
        else b <- load(file, envir = env)
        if (keep.name || (length(b) > 1)) {
            mget(b, envir = env)
        }
        else env[[b]]
    }
    else readRDS(file)
}
hs.source.part <- function(wj, env = if (local) parent.frame() else .GlobalEnv, local = 0, sep = "## ka 34.09.27.190116", nr = {
}) {
    if (!grepl("^\\s*#", sep)) 
        stop("需用注释行作为分割行")
    if (!missing(wj)) {
        wj %<=>% hs.home.expand
        nr %<=>% hs.readLines.robust
    }
    if (length(nr)) 
        eval(parse(text = head(nr, match(sep, nr, length(nr))), encoding = "UTF-8"), env)
    if (0) {
        i1 <- 15692
        eval(parse(text = append(head(nr, match(sep, nr, length(nr))), deparse(quote(message("37_01_25_105517"))), after = i1), encoding = "UTF-8"), env)
        cat(append(head(nr, match(sep, nr, length(nr))), deparse(quote(message("37_01_25_105517"))), after = 1103)[seq.int(1100, 1200)], sep = "\n")
        cat(head(nr, match(sep, nr, length(nr)))[seq.int(1110, 1130)], sep = "\n")
        wk.less(head(nr, match(sep, nr, length(nr))), start = i1)
    }
}
hs.source.rd <- function(wj, env = .GlobalEnv, text) {
    if (missing(text)) 
        text <- hs.load(hs.home.expand(wj))
    hs.source.part(nr = text, env = env)
}
hs.source.wj <- function(wj, local = 0, up = parent.frame(), env = {
}) {
    wj %<=>% hs.home.expand
    rd <- grepl("(?i)\\.rd$", wj, perl = !0)
    hs.update(env, if (local) 
        up
    else .GlobalEnv, hs.if = is.null)
    if (rd) {
        hs.source.rd(wj, env = env)
    }
    else hs.source.part(wj, env = env)
}
hs.source.mtime <- local({
    mt <- list()
    function(wj, smart = 1) {
        for (wj1 in wj) {
            wj1 %<=>% normalizePath(hs.home.expand(x))
            if (smart && length(mt[[wj1]]) && (wj1 %=>% (mt[[x]] >= file.mtime(x)))) {
            }
            else {
                mt[[wj1]] <<- file.mtime(wj1)
                hs.source.wj(wj1)
            }
        }
    }
})
hs.r.2rd <- function(wj, wj.rd) {
    wj %<=>% hs.home.expand
    if (missing(wj.rd)) {
        wj.rd <- sub("[^\\.]+$", "rd", wj)
        for (i1 in seq_along(wj)) {
            line <- hs.readLines.robust(wj)
            save(line, file = wj.rd[i1])
        }
        wj.rd
    }
    else {
        wj.rd <- hs.home.expand(wj.rd)
        line <- unlist(lapply(wj, hs.readLines.robust))
        save(line, file = wj.rd)
        line
    }
}
hs.install.packages <- function(bao, reinstall = 0, lib = wk.lib[1], destdir = hs.wjj("~/rj/src/r包"), ...) {
    for (b1 in bao) {
        e1 <- 1
        if (reinstall) 
            unlink(system.file(package = b1), recursive = TRUE)
        if (file.exists(b1)) {
            description.tar_fp <- .wk("cat_wk", .sys.fpGood(b1), "2>/dev/null | head -n 20 | tar -tf - 2>/dev/null | grep -m1 ", .q("DESCRIPTION$"), intern = 1)
            description <- .wk("cat_wk", .sys.fpGood(b1), "| tar -xvf -", .q(description.tar_fp), "--to-stdout 2>/dev/null", intern = 1)
            b1.name <- hs.re(hs.grepV(description, "^(?i)package:"), "[^ ]*$")
            b1.version <- hs.re(hs.grepV(description, "^(?i)version:"), "[^ ]*$")
            b1.fp_lib <- hs.fp(.libPaths()[1], b1.name)
            if (file.exists(b1.fp_lib)) {
                b1.version_lib <- hs.re(hs.grepV(readLines(hs.fp(b1.fp_lib, "DESCRIPTION")), "^(?i)version:"), "[^ ]*$")
                if (package_version(b1.version) > package_version(b1.version_lib)) {
                }
                else {
                  message("[message@37_12_17_134422]", .q(b1.name), " installed already for ", .sys.fpGood(b1))
                  next
                }
            }
            install.packages(b1, lib = lib, destdir = destdir, ...)
        }
        if (hs.any(!hs.havePackages(b1), reinstall)) {
            if (hs.grepl(b1, "/")) {
                devtools::install_github(b1, lib = lib, destdir = destdir, ...)
                if (0) {
                  devtools::install_github
                  install_github <- function(repo, ref = "HEAD", subdir = NULL, auth_token = remotes:::github_pat(quiet), host = "api.github.com", dependencies = NA, upgrade = c("default", "ask", "always", "never"), force = FALSE, quiet = FALSE, build = TRUE, build_opts = c("--no-resave-data", "--no-manual", "--no-build-vignettes"), build_manual = FALSE, build_vignettes = FALSE, repos = getOption("repos"), type = getOption("pkgType"), ...) {
                    pkgbuild::with_build_tools({
                      ellipsis::check_dots_used(action = getOption("devtools.ellipsis_action", rlang::warn))
                      {
                        remotes <- lapply(repo, remotes::github_remote, ref = ref, subdir = subdir, auth_token = auth_token, host = host)
                        install_remotes(remotes, auth_token = auth_token, host = host, dependencies = dependencies, upgrade = upgrade, force = force, quiet = quiet, build = build, build_opts = build_opts, build_manual = build_manual, build_vignettes = build_vignettes, repos = repos, type = type, ...)
                      }
                    }, required = FALSE)
                  }
                  install_github("MRCIEU/TwoSampleMR")
                  .libPaths()[1] %=>% .bash
                  remotes:::install_remotes
                  install_remotes <- function(remotes, ...) {
                    res <- character(length(remotes))
                    for (i in seq_along(remotes)) {
                      tryCatch(res[[i]] <- install_remote(remotes[[i]], ...), error = function(e) {
                        stop(remotes:::remote_install_error(remotes[[i]], e))
                      })
                    }
                    invisible(res)
                  }
                  remotes::install_remote
                  install_remote <- function(remote, dependencies, upgrade, force, quiet, build, build_opts, build_manual, build_vignettes, repos, type, ...) {
                    stopifnot(remotes:::is.remote(remote))
                    package_name <- remotes::remote_package_name(remote)
                    local_sha <- remotes:::local_sha(package_name)
                    remote_sha <- remotes::remote_sha(remote, local_sha)
                    if (!isTRUE(force) && !remotes:::different_sha(remote_sha = remote_sha, local_sha = local_sha)) {
                      if (!quiet) {
                        message("Skipping install of '", package_name, "' from a ", sub("_remote", "", class(remote)[1L]), " remote,", " the SHA1 (", substr(remote_sha, 1L, 8L), ") has not changed since last install.\n", "  Use `force = TRUE` to force installation")
                      }
                      return(invisible(package_name))
                    }
                    if (inherits(remote, "cran_remote")) {
                      hs.browser("2023.07.22.172435", debug = 0)
                      install_packages(package_name, repos = remote$repos, type = remote$pkg_type, dependencies = dependencies, quiet = quiet, destdir = "~/rj/src/r包", ...)
                      return(invisible(package_name))
                    }
                    res <- try(bundle <- remotes::remote_download(remote, quiet = quiet), silent = quiet)
                    remotes::remote_download
                    UseMethod("remote_download")
                    remotes::remote_download
                    getS3method("remote_download", class = "github_remote")
                    getAnywhere("remote_download")
                    findMethods("remote_download", class = class(remote))
                    findMethods("remote_download", class = "github_remote")
                    remotes:::remote_download.github_remote
                    remotes:::download
                    remotes:::base_download
                    remotes:::download_method()
                    remotes:::base_download_headers
                    utils::download.file
                    if (inherits(res, "try-error")) {
                      return(NA_character_)
                    }
                    hs.browser("2023.07.22.172347", debug = 0)
                    on.exit(unlink(bundle), add = TRUE)
                    source <- remotes:::source_pkg(bundle, subdir = remote$subdir)
                    on.exit(unlink(source, recursive = TRUE), add = TRUE)
                    update_submodules(source, remote$subdir, quiet)
                    add_metadata(source, remotes::remote_metadata(remote, bundle, source, remote_sha))
                    remotes:::clear_description_md5(source)
                    remotes:::install(source, dependencies = dependencies, upgrade = upgrade, force = force, quiet = quiet, build = build, build_opts = build_opts, build_manual = build_manual, build_vignettes = build_vignettes, repos = repos, type = type, ...)
                  }
                }
            }
            else {
                (if ("repos" %in% ...names()) 
                  install.packages
                else BiocManager::install)(b1, lib = lib, destdir = destdir, update = FALSE, ...)
            }
            e1 <- 1
        }
    }
}
hs.require <- function(bao, install = reinstall, reinstall = 0, lib = .libPaths()[1], load = 0, ...) {
    bao %<=>% unique
    r <- data.frame(installed = rep(TRUE, length(bao)))
    rownames(r) <- bao
    for (b1 in bao) {
        if (grepl("@", b1)) 
            hs.browser("39.05.27.221613", debug = 0)
        b2 <- basename(b1)
        b2 %<=>% hs.sub("-.*")
        e1 <- 1
        if (reinstall) 
            unlink(system.file(package = b2), recursive = TRUE)
        if (hs.any(!hs.havePackages(b2), reinstall)) {
            e1 <- 0
            r[b1, "installed"] <- FALSE
            if (install) {
                hs.switch(b2, "Rfast", if (system("type gsl-config")) 
                  .wk("sudo apt install libgsl-dev"), "data.table", {
                  if (system("type pkgconf")) {
                    system("sudo apt install pkgconf")
                  }
                  if (system("pkg-config --libs zlib")) {
                    system("sudo apt install zlib1g-dev")
                  }
                }, "scRNAtoolVis", hs.require(c("junjunlab/jjAnno", "sajuukLyu/ggunchull", "ComplexHeatmap", "junjunlab/scRNAtoolVis"), 1))
                wj1 <- "~/.Rprofile"
                wj2 <- paste0(wj1, ".备份")
                if (file.exists(wj1)) 
                  file.rename(wj1, wj2)
                hs.install.packages(b1, lib = lib, ...)
                if (!file.exists(wj1)) 
                  file.rename(wj2, wj1)
                e1 <- 1
                r[b1, "installed"] <- 1
            }
        }
        if (e1 && load) 
            suppressMessages(do.call("require", list(b2, quietly = 1)))
    }
    if (!all(r)) 
        stop("[38_01_08_114620]缺包 ", paste(rownames(r)[!r], collapse = ", "))
}
hs.source2wj <- function(hs1, wjj2 = "~/tmp") {
    on <- paste(substitute(hs1))
    if ((length(on) > 1) && hs.grepl(on[1], "::+")) 
        on %<=>% paste(x[-1], collapse = "_")
    wj2 <- hs.fp(wjj2, paste0(on, ".r"))
    if (!file.exists(wj2)) {
        code <- capture.output(hs1)
        hs.update(code[length(code)], paste("##", x), hs.grepl(x, "^\\<environment: .*\\>$"))
        hs.update(code[length(code) - 1], paste("##", x), hs.grepl(x, "^\\<bytecode: .*\\>$"))
        wk.dt.fwrite(code, wj2)
    }
    wj2
}
hs.init <- function(wjj = wk.dm.wjj, smart = 1) {
    wjj %<=>% hs.home.expand
    ..debug <- 0
    wj <- sapply(file.path(wjj, paste0(c("internal.r", "string.r", "wj.r", "data.table.r", "xm.r", "function.common.r", "swxx.r", "fig.r", "math.r", if (interactive()) c("zim.r", "org1.r"), if (hs.machine.isLocal()) c("my.r")), "")), hs.getTextFile) %=>% unlist
    sapply(wj, . %=>% {
        if (file.exists(x)) {
            if (hs.machine.isLocal()) 
                message("加载", x)
            hs.source.robust(x)
        }
    })
    return()
    identical(Sys.getenv()[["USERID"]], "38_06_28_092642")
    if (!interactive()) {
        sapply(wj, function(wj1) {
            source(wj1)
        })
        return()
    }
    wj.rd <- file.path(wjj, "all.r.rd")
    if (1) {
        convert <- (!file.exists(wj.rd)) || (length(wj) && (max(file.mtime(c(wj, hs.getTextFile(file.path(wjj, "init.r"))))) > file.mtime(wj.rd)))
        if (..debug) 
            convert <- 1
        if (convert) {
            line <- hs.r.2rd(wj, wj.rd)
            hs.source.rd(text = line)
        }
    }
    if (..debug) 
        hs.browser("36.02.07.203804")
    hs.source.mtime(wj.rd, smart = smart)
    if (0) 
        if (hs.machine.isLocal()) 
            sapply(hs.wj(wk.dm.wjj, paste0(c("my.r"), "")), function(wj1) {
                hs.source.mtime(wj1, smart = smart)
            })
}
hs.init1 <- function() {
    .s()
    if (!exists("wk.ssh", envir = .GlobalEnv)) 
        assign("wk.ssh", {
        }, envir = .GlobalEnv)
    if (!exists("wk.rsync.wj.wj", envir = .GlobalEnv)) 
        assign("wk.rsync.wj.wj", hs.wj.portable(hs.wj(.mfp("36.02.15.203034.txt.gz"))), envir = .GlobalEnv)
    {
        environment(hs.hsgly)[["wj.wj"]] <- eval(parse(.wk("ag -g 文件/找/find.39_01_08_150201", hs.home.expand("~/org"), intern = 1))[[1]][[2]][["wj2"]])
        evalq({
            wj.wj <- eval(parse(.wk("ag -g 文件/找/find.39_01_08_150201", hs.home.expand("~/org"), intern = 1))[[1]][[2]][["wj2"]])
            uid.wj <- uid.wj.wj.hs()
            if (!exists(env.lt, envir = .GlobalEnv)) 
                assign(env.lt, new.env(), envir = .GlobalEnv)
        }, envir = environment(hs.hsgly))
    }
    for (wj1 in c("org1.r", "my.r")) hs.fp(wk.dm.wjj, wj1) %=>% hs.getTextFile %=>% hs.source.robust
}
my.r <- function() {
    wj1 <- hs.getTextFile(hs.wj(wk.dm.wjj, "my.r"))
    hs.source.robust(wj1)
}
hs.gc_here <- function(env = parent.frame()) {
    evalq(rm(list = ls()), envir = env)
}
.First <- function() if (interactive()) loadhistory(hs.home.expand("~/.Rhistory"))
hs.save.history <- function(keep.incomplete_line = 1, size = Sys.getenv("HISTSIZE")) {
    Sys.setenv(R_HISTSIZE = size)
    try({
        wj1 <- hs.home.expand("~/.Rhistory")
        wj2 <- paste0(wj1, ".tmp")
        on.exit(hs.wj.rm(wj2), add = TRUE)
        savehistory(wj2)
        h0 <- rev(readLines(wj1))
        h1 <- rev(readLines(wj2))
        if (!keep.incomplete_line) 
            h1 <- unlist(sapply(h1, function(h2) {
                r1 <- .try(parse(text = h2))
                if (!input.is.incomplete(r1)) 
                  h2
            }))
        cat(rev(h1), sep = "\n", file = wj1)
        wj1
    })
}
.Last <- function() {
    if (hs.machine.isLocal()) 
        local({
            if (0) {
                wj1 <- "~/db/软件库/.config/vivaldi/Default"
                wj2 <- "~/.config/vivaldi/Default"
                if (!dir.exists(wj2)) {
                  dir.create(dirname(wj2), recursive = TRUE)
                  file.symlink(wj1, wj2)
                }
                wjj1 <- "~/db/软件库/.config/vivaldi/"
                wjj2 <- "~/.config/vivaldi/"
                if (length(.wk("find", hs.fpGood(wjj2), "-maxdepth 1", "-iname", .q("singleton*"), intern = 1))) 
                  hs.browser("2023.07.11.105504|vivaldi在运行", debug = 0)
                hs1 <- function(wjj) hs.hsgly("列出部分文件.2023_07_11_102948")(wjj, exclude = "Default") %=>% file.mtime %=>% max(na.rm = 1)
                time1 <- hs1(wjj1)
                time2 <- hs1(wjj2)
                if (time1 != time2) {
                  if (time1 < time2) 
                    hs.swap(wjj1, wjj2)
                  .wk("rsync -ahPvkK --delete", "-f", .q("- /Default/"), hs.fpGood(wjj1), hs.fpGood(wjj2))
                }
            }
            if (0) {
                ln_wjj2_db <- hs.home.expand("~/db/软件库")
                wjj_pair <- list(c("~/rjk/ln-s/.ssh/", "~/.ssh/"), c(paste0(ln_wjj2_db, "/.config/opera/"), "~/.config/opera/"))
                for (i1 in seq_along(wjj_pair)) {
                  p1 <- wjj_pair[[i1]]
                  wjj1 <- hs.home.expand(p1[1])
                  wjj2 <- hs.home.expand(p1[2])
                  if (hs.wj.is.Symlink(wjj2)) 
                    wjj2 %=>% hs.sub("/*$") %=>% file.rename(x, paste0(x, ".bf"))
                  t2 <- hs.wjj.ls(wjj2, R = 1) %=>% file.mtime %=>% max(na.rm = 1)
                  t1 <- hs.wjj.ls(wjj1, R = 1) %=>% file.mtime %=>% max(na.rm = 1)
                  if ((paste(t2) != "NA") && (t1 != t2)) {
                    if (as.difftime(t2 - t1, units = "secs") > 0.1) 
                      hs.swap(wjj1, wjj2)
                    .wk(wj_rsync, "-ahPvz", "--delete", "-f", .q(paste("-", .q("X[0-9]"))), wjj1, wjj2)
                  }
                  "~/.ssh" %=>% c(x, hs.wjj.ls(x)) %=>% hs.fp.make_personal
                }
            }
        })
    if (interactive()) {
        hs.save.history()
        for (o1 in c("hs.org.db.tbl")) if (exists(o1)) 
            get(o1)(save.and.exit = 1)
    }
}
hs.ls <- function(..., all.names = TRUE, sorted = FALSE) {
    if (inherits(..1, "character") && (length(..1) == 1)) {
        return(getNamespaceExports(..1) %=>% x[order(tolower(x))])
    }
    ls(..., all.names = all.names, sorted = sorted)
}
hs.str <- function(x) {
    wj_temp <- "/dev/shm/2023_08_15_182030.txt"
    on.exit(hs.wj.rm(wj_temp), add = TRUE)
    sink(wj_temp)
    str(x)
    sink()
    return(.wk("less -Ni", wj_temp))
}
hs.pkg.depend <- function(pkg) {
    system.file("DESCRIPTION", package = pkg) %=>% read.dcf(fields = "Depends") %=>% hs.gsub("\\([^\\(\\)]*\\)") %=>% strsplit(",") %=>% unlist %=>% trim
}
hs.pkg.exported <- function(pkg) {
    getNamespaceExports(pkg) %=>% x[order(tolower(x))]
}
hs.pkg.get <- function(pkg, what, include_depend = 1, re = FALSE) {
    e <- getNamespaceExports(pkg)
    if (re) {
        alike <- hs.grepV(e, what)
        if (length(alike)) 
            return(hs.lapply(alike, . %=>% do.call("::", list(pkg, x))))
    }
    else if (what %in% e) {
        return(do.call("::", list(pkg, what)))
    }
    if (include_depend) {
        depend <- hs.pkg.depend(pkg) %rm% c("R", NA)
        for (d1 in depend) {
            a <- hs.pkg.get(d1, what, include_depend = 0)
            if (length(a)) 
                return(a)
        }
    }
}
hs.lvl <- base::levels
hs.nc <- base::nchar
hs.p <- base::paste
hs.p0 <- base::paste0
hs.nm <- base::names
`hs.nm<-` <- base::`names<-`
hs.ev <- base::eval
hs.bq <- base::bquote
hs.env <- base::environment
`hs.env<-` <- base::`environment<-`
hs.ss <- function(..., perl = TRUE) {
    strsplit(..., perl = perl)
}
hs.df <- function(..., rn = {
}, check.names = FALSE, stringsAsFactors = FALSE) {
    .. <- list(..., check.names = check.names, stringsAsFactors = stringsAsFactors)
    if (length(rn)) 
        ..[["row.names"]] <- rn
    hs.call...(E = data.frame(...), .. = ..)
}
hs.na <- function(x) {
    a <- x[1]
    a[1] <- NA
    a
}
hs.sample <- function(x, size = hs.switch(length(dim(x)), 0, length(x), 2, dim(x)[1], hs.browser("2023.08.02.142428", debug = 0)), replace = hs.switch(length(dim(x)), 0, if ((length(x) == 1) && inherits(x, "numeric") && (x >= 1)) size > x else size > length(x), 2, size > nrow(x), hs.browser("2023.08.02.142705", debug = 0)), prob = {
}) {
    hs.switch(length(dim(x)), 0, sample(x = x, size = size, replace = replace, prob = prob), 2, hs.ij(x, sample(x = nrow(x), size = size, replace = replace, prob = prob)))
}
hs.list <- function(..., env.36_11_30_195548 = parent.frame()) {
    a1 <- substitute(alist(...))
    name.L <- lapply(a1[seq(2, length(a1), by = 2)], eval, env.36_11_30_195548)
    i1 <- sapply(name.L, length) == 1
    if (0) 
        hs.c(lapply(a1[seq(2, length(a1), by = 2)], eval, env.36_11_30_195548), lapply(a1[seq(3, length(a1), by = 2)], eval, env.36_11_30_195548))
    if (any(i1)) {
        hs.c(unlist(name.L[i1]), lapply(a1[seq(3, length(a1), by = 2)[i1]], function(x1) {
            "language" %in% class(x1)
            if (is.language(x1)) {
                eval(x1, env.36_11_30_195548)
            }
            else x1
        }))
    }
}
hs.runCmd <- function(..., intern = FALSE) {
    wj_cmd <- tempfile()
    on.exit(unlink(wj_cmd), add = TRUE)
    cmd <- paste(c(...), collapse = " ")
    cat(cmd, file = wj_cmd)
    system(paste("bash", wj_cmd), intern = intern)
}
hs.methods <- function(O) {
    for (c1 in base::class(O)) {
        hs.say(base::sprintf("Methods for %s:", c1))
    }
}
hs.filter <- function(..., out = "i") {
    of <- list(...)
    if (length(of)%%2) 
        stop("37_02_01_215621 the argument number should be even")
    oi <- seq(1, length(of), by = 2)
    n0 <- unique(sapply(oi, function(oi) length(of[[oi]])))
    if (length(n0) > 1) 
        stop("37_01_12_102345")
    i1 <- seq_len(n0)
    for (o1i in oi) {
        if (length(i1) < 1) 
            break
        hs1 <- of[[o1i + 1]]
        i2 <- hs1(of[[o1i]][i1])
        if (is.logical(i2)) 
            i2 <- which(i2)
        i1 <- i1[i2]
    }
    if (length(of) > 2) 
        out <- "i"
    switch(out, i = i1)
}
hs.filter.1 <- function(X, ..., out = "v", x1 = 1) {
    i1 <- seq_along(X)
    hs <- list(...)
    for (hs1.i in seq_along(hs)) {
        if (length(i1) < 1) {
            break
        }
        hs1 <- hs[[hs1.i]]
        i1 <- i1[which(hs1(X[i1]))]
    }
    switch(out, i = i1, v = X[i1])
}
hs.attr.slice <- function(a1, i1, null = {
}) {
    if (!length(i1)) 
        return(null)
    r1 <- a1[i1]
    for (n1 in names(attributes(a1))) {
        attr(r1, n1) <- if (length(attr(a1, n1)) == length(a1)) {
            attr(a1, n1)[i1]
        }
        else attr(a1, n1)
    }
    r1
}
hs.match <- function(..., fail = FALSE) {
    r1 <- match(...)
    if (is.na(r1)) {
    }
    else r1
}
hs.skip <- function(v, v1) {
    i <- match(v1, v, nomatch = 0)
    if (i) 
        tail(v1, -i)
    else v
}
v2.hs_chain.v1 <- function(x1, ...) {
    chain <- list(...)
    for (c1 in chain) x1 %<=>% c1[[2]][match(x, c1[[1]])]
    x1
}
hs.eval <- function(E, env1 = parent.frame(), local = 0) {
    if (local) 
        env1 <- new.env(parent = env1)
    eval(E, env1)
}
hs.local <- function(E, restoreWd = 1, env1 = parent.frame()) {
    if (restoreWd) {
        wjj0 <- getwd()
        on.exit(setwd(wjj0), add = TRUE)
    }
    hs.eval(E, env1, local = 1)
}
hs01.expression2language <- function(e1) switch(paste(length(e1)), `0` = {
}, `1` = e1[[1]], as.call(c(as.symbol("{"), as.list(e1))))
hs.evalLazy <- function(exp1, uid, root = formals(.mfp.swxx)[["dataDir"]], fp = hs.home.expand(hs.wjm(.mfp(uid, dtt = dtt), add = ext)), ext = if ("TxDb" %in% class) "sql" else "rd", lazy = 1, ow = 1, env = parent.frame(), fp.only = 0, dtt = "handy", save.jg = 1, want = "jg", hs4save = if (hs.grepl(fp, "(?i)\\.[ct]sv(\\.gz)?$")) {
    function(x, fp) wk.dt.fwrite(x, fp)
} else if (hs.grepl(fp, "(?i)\\.xlsx?$")) {
    function(x, fp) wk.xls.write(x, fp)
} else if (hs.grepl(fp, "(?i)\\.rd$")) {
    function(x, fp) hs.save(x, file = fp)
} else {
    function(x, fp) fp
}, hs4load = if (hs.grepl(fp, "(?i)\\.[ct]sv(\\.gz)?$")) {
    wk.fread
} else if (hs.grepl(fp, "(?i)\\.xlsx?$")) {
    function(fp) wk.xls.read(fp, if (xls.all.sheet) 
        ""
    else 1)
} else if (hs.grepl(fp, "(?i)\\.rd$")) {
    hs.load
} else identity, load = 1, xls.all.sheet = 0, lib = {
}, class = {
}, local = 1) {
    if (fp.only) 
        return(fp)
    if (file.exists(fp) && lazy) 
        return(if (load) hs4load(fp))
    if (length(lib)) 
        hs.require(lib)
    R <- eval(substitute(exp1), envir = if (local) 
        new.env(parent = env)
    else env)
    hs4save(R, hs.wj(fp))
    if (load) 
        R
}
hs.nonempty <- function(L, length = 1, nrow = 0, ot = "v") {
    hs <- eval({
        if (length && nrow) {
            bquote(function(a) (length(a) >= .(length)) && (nrow(a) >= .(nrow)))
        }
        else if (length) {
            bquote(function(a) length(a) >= .(length))
        }
        else if (nrow) {
            bquote(function(a) nrow(a) >= .(nrow))
        }
    })
    if (length(L)) {
        i1 <- sapply(L, hs)
        switch(ot, v = L[i1], i = which(i1), l = i1)
    }
}
hs.is_empty <- function(x1, co1 = 0) {
    if (is(x1, "data.table")) 
        return(x1[, .N <= co1])
    if (is(x1, "character")) 
        return((length(x1) == 0) || ((length(x1) == 1) && (nchar(x1) <= co1)))
    length(x1) <= co1
}
hs.graph.node.path <- function(N, Z, P = {
}) {
    P <- c(P, N)
    p1 <- Z[N, ]
    down <- names(p1)[p1 > 0] %not% P
    self <- sys.function()
    if (length(down)) {
        lapply(down, function(n2) {
            self(n2, Z, P)
        })
    }
    else P
}
hs0.seq.eql <- function(s1, s2) {
    (length(s1) == length(s2)) && all(s1 == s2)
}
hs.exists <- wk.exists <- ieiwk.exists <- local.exists <- function(x, env1 = parent.frame(), inherits = FALSE, ...) {
    if (length(x)) {
        sapply(x, function(x) {
            exists(x, inherits = inherits, envir = env1, ...)
        })
    }
    else FALSE
}
hs.make.connection <- function(wj, open = {
}) {
    if (hs.len1(wj)) {
        if ("connection" %not.in% class(wj)) {
            if (is.character(wj)) {
                wj <- {
                  if (file.exists(hs.home.expand(wj))) {
                    gz <- hs.grepl(wj, "\\.gz$")
                    args <- list(description = wj)
                    if (length(open)) {
                      args[["open"]] <- open
                    }
                    do.call(if (gz) 
                      gzfile
                    else file, args)
                  }
                  else {
                    pipe(wj)
                  }
                }
            }
            else {
                stop("what is the connection? 34.10.05.070316 @ ~/org/dm/r/function.common.R")
            }
        }
        if (!isOpen(wj, "r")) {
            if (isOpen(wj, "w")) {
                stop("hs.make.connection: use only for reading. 34.10.05.071940 @ ~/org/dm/r/internal.r")
            }
            open(wj)
        }
        wj
    }
    else {
        hs.browser("38.06.06.145143")
    }
}
hs.readLines <- function(con, iv = {
}, toEnd = 0, n = -1, skip = {
}, ok = TRUE, warn = TRUE, encoding = "UTF-8", close = n < 1) {
    con %<=>% hs.make.connection
    if (close) 
        on.exit(close(con), add = TRUE)
    skip.chunk.size <- 1000L
    skip %=>% if (length(x)) {
        j <- x
        while (j > 0) {
            readLines(con, n = min(skip.chunk.size, j), ok = ok, warn = warn)
            j <- j - skip.chunk.size
        }
    }
    if (length(iv)) {
        iv_2 <- unique(sort(iv))
        skip <- diff(c(0, iv_2)) - 1
        r <- vector("character", length(skip))
        for (i in seq_along(skip)) {
            j <- skip[i]
            while (j > 0) {
                readLines(con, n = min(skip.chunk.size, j), ok = ok, warn = warn)
                j <- j - skip.chunk.size
            }
            r[i] <- readLines(con, n = 1, ok = ok, warn = warn)
        }
        if (toEnd) {
            c(r[rank(iv)], readLines(con, ok = ok, warn = warn))
        }
        else r[rank(iv)]
    }
    else readLines(con, n = n, ok = ok, warn = warn, encoding = encoding)
}
hs.source <- ieiwk.source <- function(f.in, pattern, ignore.case = TRUE, fixed = FALSE, perl = FALSE, local = FALSE, encoding = "UTF-8", ...) {
    f.in %<=>% hs.home.expand
    flag <- 0
    uid <- {
        a <- readLines(f.in, 10)
        hs.trim(re("[^ ]*", grep(" # file uid$", a, v = 1)), "\"")
    }
    if (!length(apropos(uid))) 
        flag <- 1
    if (!flag) {
        mtime <- file.info(f.in)[["mtime"]]
        if (Sys.time() - mtime < 24) 
            flag <- 1
    }
    if (flag) {
        if (!missing(pattern)) {
            e <- parse(f.in)
            env <- {
                if (is.logical(local)) {
                  if (local) {
                    .GlobalEnv
                  }
                  else parent.frame()
                }
                else local
            }
            e.s <- sapply(seq_along(e), function(a.i) {
                capture.output(e[a.i])
            })
            e.i <- unlist(sapply(pattern, function(p1) {
                for (a.i in seq_along(e)) if (any(grepl(p1, e.s[[a.i]], fixed = fixed))) 
                  return(a.i)
            }))
            sapply(seq_along(e.i), function(i) {
                j <- e.i[i]
                .ss(sprintf("%s # %s sourced.", f.in, names(e.i)[i]))
                eval(e[[j]], env)
            })
            invisible()
        }
        else {
            .ss(sprintf("%s sourced.", f.in))
            source(f.in, encoding = encoding, local = local, ...)
            source(f.in, encoding = encoding, local = FALSE, ...)
        }
    }
}
hs.isOdd <- function(x) as.logical(x%%2)
hs.isEven <- function(x) !(x%%2)
hs.recycle <- function(i, l) {
    if (is.logical(i)) 
        i <- which(i)
    j <- i%%l
    if (0 %in% j) 
        j[j %in% 0] <- l
    j
}
hs.ifelse <- function(test, yes, no, envUp = parent.frame()) {
    if (!(identical(environment(yes), envUp) && identical(environment(no), envUp))) 
        stop("[38_01_01_122923]")
    jg <- rep(NA, length(test))
    i1 <- test
    if (!is.logical(i1)) 
        i1 <- as.logical(i1)
    if (any(i1)) 
        jg[i1] <- yes(i1)
    if (FALSE %in% i1) {
        i1 <- !i1
        jg[i1] <- no(i1)
    }
    jg
}
hs.attr <- function(o1, nm, vl, simplify = 1) {
    if (missing(nm)) {
        names(attributes(o1))
    }
    else {
        if (missing(vl)) {
            r <- wk.sapply(nm, function(n1) {
                attr(o1, n1)
            }, nc = 1)
            if (hs.len1(r) && simplify) {
                r[[1]]
            }
            else r
        }
        else {
            if (length(nm) > 1) {
                for (i in seq_along(nm)) attr(o1, nm[i]) <- vl[[hs.recycle(i, length(vl))]]
            }
            else attr(o1, nm) <- vl
            o1
        }
    }
}
expandingSequence <- function(capacity = 10, named = 0, type = "list") {
    buffer <- vector(switch(type, list = "list", vector = "logical", type), capacity)
    if (named) 
        names <- character(capacity)
    length <- 0
    methods <- list()
    {
        a1 <- function() {
        }
        on.exit(rm(list = "a1"), add = TRUE)
        body(a1) <- bquote({
            buffer <<- c(buffer, vector(.(switch(type, list = "list", vector = "logical", type)), capacity))
            capacity <<- capacity * 2
            .(if (named) 
                quote(names <<- c(names, character(capacity))))
        })
        methods[["double.size"]] <- a1
    }
    {
        a1 <- if (named) 
            function(name, val = NULL) {
            }
        else function(val = NULL) {
        }
        body(a1) <- bquote({
            if (length == capacity) 
                methods$double.size()
            length <<- length + 1
            buffer[length] <<- .(if (type == "list") {
                quote(list(val))
            }
            else quote(val))
            .(if (named) 
                quote(names[length] <<- name))
        })
        methods[["add"]] <- a1
    }
    {
        a1 <- function(i = {
        }, ignore_out_of_range = 1) {
        }
        body(a1) <- bquote({
            if (length(i)) {
                if (max(i) > length) {
                  if (ignore_out_of_range) {
                    warning("out of range @33.07.17.204126@")
                    i <- i[i <= length]
                  }
                  else stop("out of range @33.07.17.203755@")
                }
                .(if (named) {
                  quote(hs.c(names[i], buffer[i]))
                }
                else quote(buffer[i]))
            }
            else .(if (named) {
                quote(hs.c(head(names, length), head(buffer, length)))
            }
            else quote(head(buffer, length)))
        })
        methods[["get"]] <- a1
    }
    methods
}
linkedList <- function() {
    head <- list(0)
    length <- 0
    methods <- list()
    methods$add <- function(val) {
        length <<- length + 1
        head <<- list(head, val)
    }
    methods$get <- function() {
        b <- vector("list", length)
        if (length) 
            for (i in length:1) {
                b[[i]] <- head[[2]]
                head <- head[[1]]
            }
        b
    }
    methods
}
hs.linkedList <- function() {
    head <- list(0)
    lengthTotal <- length <- 0
    methods <- list()
    methods$add <- function(val) {
        length <<- length + 1
        head <<- list(head, val)
    }
    R <- expandingList()
    methods$get <- function() {
        if (length) {
            if (get("length", environment(R$get)) == 0) 
                R <<- expandingList(length)
            for (i in length:1) {
                R$add(head[[2]])
                head <- head[[1]]
            }
            length <<- 0
        }
        R$get()
    }
    methods
}
expandingList <- function(...) {
    expandingSequence(..., named = 0, type = "list")
}
namedExpandingList <- function(...) {
    expandingSequence(..., named = 1, type = "list")
}
expandingVector <- function(..., type = "vector") expandingSequence(..., named = 0, type = type)
namedExpandingVector <- function(capacity = 10) {
    buffer <- vector(length = capacity)
    length <- 0
    methods <- list()
    methods$double.size <- function() {
        buffer <<- c(buffer, vector(length = capacity))
        capacity <<- capacity * 2
    }
    methods$add <- function(val) {
        end <- length + length(val)
        while (end >= capacity) methods$double.size()
        buffer[seq(length + 1, end)] <<- val
        length <<- end
    }
    methods$get <- function(i, rm = 0) {
        if (missing(i)) {
            R <- head(buffer, length)
            if (rm) {
                buffer <<- vector(length = capacity)
                length <<- 0
            }
        }
        else {
            R <- buffer[i]
            if (rm) {
                i <- sort(unique(i))
                buffer <<- buffer[-i]
                length <<- length - length(i)
            }
        }
        R
    }
    methods
}
hs.acc <- wk.acc <- function(once = 0, type = "list") {
    env1 <- new.env()
    nL <- switch(type, vector = expandingVector, list = expandingList)()
    N <- 0
    methods <- list()
    methods$add <- {
        hs1 <- function(n, v) {
        }
        b1 <- quote({
            if (missing(v)) {
                v <- n
                n <- N + 1
                N <<- n
            }
            n <- as.character(n)
        })
        b2 <- quote({
            if (!methods$has(n)) {
                assign(n, v, envir = env1)
                nL$add(n)
            }
        })
        if (!once) {
            b2 <- b2[[2]][[3]]
        }
        b1 <- c(`{`, append(as.list(b1[-1]), as.list(b2[-1])))
        body(hs1) <- as.call(b1)
        hs1
    }
    methods$get <- function() {
        a <- nL$get() %and% ls(envir = env1)
        if (length(a)) {
            mget(a, envir = env1)
        }
    }
    methods$has <- function(n) {
        exists(as.character(n), envir = env1, inherits = FALSE)
    }
    methods$rm <- function(n) {
        a <- as.character(n)
        if (methods$has(a)) {
            rm(a, envir = env1)
        }
    }
    methods$n <- function() {
        nL$get() %and% ls(envir = env1)
    }
    methods
}
wk.flat <- function(x, flag = "wk.flat.me") {
}
hs.list.flatten <- function(L, hs1 = identity, D = 2, hs2 = rbindlist) {
    a1 <- expandingList()
    do.call("lapply", c(list(L), unname(sapply(rep("lapply", D - 1), as.symbol)), function(.) {
        a1[["add"]](hs1(.))
    }))
    rbindlist(a1[["get"]]())
}
hs.list.merge <- function(L) {
    a1 <- tapply(seq_along(L), names(L), list)
    i1 <- which(sapply(a1, length) > 1)
    if (length(i1)) {
        for (i1.1 in i1) {
            i2 <- a1[[i1.1]]
            L[[i2[1]]] <- L[[i2[1]]] %or% unlist(L[i2[-1]])
        }
        L[unlist(lapply(a1[i1], "[", -1))] <- {
        }
    }
    L
}
hs.abline <- function() abline(0, 1, v = 0, h = 0, col = Mycol2rgb("black", 0.25))
hs.check.list <- function(L) {
    lapply(L, function(.) {
        r1 <- table(.)
        a1 <- length(r1)
        if (a1 < (length(.)/2)) 
            r1
    })
}
hs.unique <- function(a1) {
    i1 <- which(duplicated(a1))
    if (length(i1)) {
        a1[-i1]
    }
    else a1
}
hs.duplicated <- function(x, cn = {
}, inv = FALSE, ot = "v") {
    hs.cond(is.atomic(x), {
        i <- x %in% x[duplicated(x)]
        if (inv) 
            i <- !i
        switch(ot, v = x[i], i = which(i))
    }, inherits(x, "data.table"), {
        if (inv) 
            hs.browser("39.03.15.234353", debug = 0)
        i <- if (length(cn)) 
            x[, duplicated(.SD), .SDcols = cn]
        else duplicated(x)
        x[i]
    })
}
hs.v.start_with <- function(v1, e1, ot = "v") {
    i1 <- match(e1, v1)
    switch(ot, v = v1[i1:length(v1)], i = i1:length(v1))
}
hs.v.within <- function(v1, e1, e2, ot = "v") {
    i1 <- match(c(e1, e2), v1)
    i1 <- seq(i1[1], i1[2])
    switch(ot, v = v1[i1], i = i1)
}
hs.unlist <- function(x, recursive = TRUE, use.names = FALSE) unlist(x = x, recursive = recursive, use.names = use.names)
intersection_string.hs.list <- function(L) {
    names(L) <- seq_along(L)
    L1 <- inverseList(L)
    names(L1)[sapply(L1, length) == length(L)]
}
hs.table <- function(..., order = "smart", M = 0, prop = type == "p", type = "c", percentage = 0, margin = NULL) {
    sj <- list(...)
    jg <- table(..., useNA = "ifany")
    if (M) {
        jg <- rbind(jg, colSums(jg))
        jg <- cbind(jg, rowSums(jg))
    }
    if (order == "smart") {
        dimname <- lapply(seq_along(sj), function(d1) {
            a1 <- rle(as.vector(sj[[d1]]))[["values"]]
            a2 <- attr(jg, "dimnames")[[d1]]
            if (anyDuplicated(a1)) {
                seq_along(a2)
            }
            else match(a1, a2)
        })
        jg <- do.call("[", c(list(jg), dimname, drop = FALSE))
    }
    switch(type, c = jg, p = prop.table(jg, margin = margin) * ifelse(percentage, 100, 1), b = jg %=>% list(x, prop.table(x, margin = margin) * ifelse(percentage, 100, 1)) %=>% hs.c(c("count", ifelse(percentage, "pct", "prop.")), x) %=>% do.call(cbind, x))
}
hs.tapply <- function(..., simplify = 0) tapply(..., simplify = as.logical(simplify))
hs.timestamp <- function(quiet = FALSE) {
    timestamp(stamp = Sys.time(), quiet = quiet)
}
hs.cat.percentage <- local({
    def_in <- 1
    db1 <- seq(0, 100, def_in)[-1]
    function(n1, def = 1, new = 0) {
        if (new) {
            db1 <<- seq(0, 100, def_in)
            return()
        }
        if (def != def_in) {
            def_in <<- def
            db1 <<- seq(0, 100, by = def)
        }
        n2 <- ceiling(n1 * 100)
        if (n2 %in% db1) {
            cat("**** ", n2, "%", " ****", "\n", sep = "")
            db1 <<- db1 %not% n2
        }
    }
})
hs.not <- `!`
hs.some_false <- hs.not_all <- function(..., na.rm = FALSE) !all(..., na.rm = na.rm)
hs.none <- function(..., na.rm = FALSE) {
    !any(..., na.rm = na.rm)
}
hs.which <- function(x, na = 1, inv = 0) {
    a <- which(if (inv) {
        !x
    }
    else as.logical(x))
    if (na) {
        i1 <- which(is.na(x))
        if (length(i1)) 
            a <- sort(a %or% i1)
    }
    a
}
hs.updatePackages <- ieiwk.update.packages <- function(lib.loc = wk.path[["lib"]][1], ask = 0) {
    update.packages(lib.loc, ask = ask)
}
hs.removePackages <- ieiwk.remove.packages <- function(pkgs, lib = wk.path[["lib"]][1]) {
    remove.packages(pkgs, lib)
}
hs.lines_comment <- function(wj1, opt = if (plus > 0) "l" else "i", comment = "#", skip = 1, plus = 0) {
    wj1 <- hs.home.expand(wj1)
    con1 <- hs.file(wj1)
    on.exit(close(con1), add = TRUE)
    stepSize <- 10
    comments <- {
    }
    while (length(L <- readLines(con1, stepSize))) {
        i1 <- hs.which(substr(L, 1, nchar(comment)) %not.in% c(comment, ""))
        if (length(i1)) {
            if (i1[1] > 1) 
                comments <- c(comments, head(L, i1[1] - 1))
            break
        }
        else comments <- c(comments, L)
    }
    if (plus > 0) {
        comments <- c(comments, readLines(con1, plus))
    }
    switch(opt, i = length(comments), l = comments, h = unlist(strsplit(comments[length(comments) - plus], "\t")), c = {
        con2 <- hs.file(wj1)
        if (length(comments)) readLines(con2, length(comments) - 1)
        con2
    })
}
hs.makeBatch <- function(V, N = wk.xcs, K = {
}, name = 1, qtl = 1) {
    if (length(V) == 0) 
        return()
    if (length(V) == 1) 
        return(list(V))
    if (!length(K)) {
        K <- length(V)/N
    }
    f1 <- if (length(V)%%K) {
        breaks <- seq(1, length(V), by = K) %or% length(V)
        cut(V, breaks, include.lowest = TRUE)
    }
    else {
        as.integer(base::gl(length(V)/K + K, K, length(V)))
    }
    R <- tapply(V, f1, list)
    if (name) {
        R
    }
    else unname(R)
}
hs.doBatch <- function(V, hs = identity, n = wk.xcs, k = {
}, name = 1, reduce = 0, hs1 = hs) {
    H <- hs.makeBatch(V, N = n, K = k)
    R <- wk.sapply(H, hs, nc = n)
    if (reduce) 
        R <- do.call(hs1, unname(R))
    R
}
hs.is_empty_arg <- function(x) is(x, "name") && identical(paste(x), "")
hs.switch <- function(o1, ..., env1 = parent.frame()) {
    n1 <- ...length()
    lang <- substitute(c(...))
    matched <- FALSE
    hs.eq <- if (is.list(o1)) {
        function(x1, x2) {
            (length(x1) == length(x2)) && {
                i <- seq_along(x1)
                all(sapply(i, function(i1) {
                  a1 <- x1[[i1]]
                  a2 <- x2[[i1]]
                  (length(a1) == length(a2)) && all(a1 == a2)
                }))
            }
        }
    }
    else function(x1, x2) {
        (length(x1) == length(x2)) && all(x1 == x2)
    }
    for (i in hs.seqForward(1, n1 - 1, by = 2)) {
        if (matched) 
            if (!hs.is_empty_arg(lang[[i + 2]])) 
                return(...elt(i + 1))
            else next
        if (hs.eq(o1, ...elt(i))) {
            matched <- TRUE
            if (!hs.is_empty_arg(lang[[i + 2]])) 
                return(...elt(i + 1))
        }
    }
    if (n1%%2) 
        ...elt(n1)
}
hs.switch.hs <- function(AL, ..., no_match = NULL, env1 = parent.frame()) {
    if (!is.list(AL)) 
        AL <- list(AL)
    n1 <- ...length()
    lang <- substitute(c(...))
    matched <- FALSE
    for (i in seq(1, n1 - 1, by = 2)) {
        if (do.call(...elt(i), AL)) 
            matched <- TRUE
        if (hs.all(matched, !hs.is_empty_arg(lang[[i + 2]]))) 
            return(...elt(i + 1))
    }
    if (n1%%2) 
        ...elt(n1)
}
hs.case <- hs.cond <- function(...) {
    n1 <- ...length()
    lang <- substitute(c(...))
    matched <- FALSE
    for (i in seq(1, n1 - 1, by = 2)) {
        if (...elt(i)) 
            return(...elt(i + 1))
    }
    if (n1%%2) 
        ...elt(n1)
}
hs.if <- `if`
hs.repeat <- function(exp1, env1 = parent.frame()) {
    eval(substitute(repeat exp1), envir = env1)
}
hs.on.exit <- function(..., env1 = parent.frame()) {
    evalq(substitute(on.exit(...)), envir = env1)
    eval(bquote(.(substitute(on.exit(...)))), envir = env1)
    evalq(bquote(.(substitute(on.exit(...)))), envir = env1)
    do.call(evalq, list(bquote(.(substitute(on.exit(...))))))
}
hs.case.over <- function(over, hs, R = {
}, no_match = NULL, env1 = parent.frame(), env2 = if (local) new.env(parent = env1) else env1, local = 1) {
    if (!is.list(hs)) 
        hs <- list(hs)
    N <- max(length(over), length(hs), length(R))
    for (i1 in seq_len(N)) {
        r1 <- hs[[hs.recycle(i1, length(hs))]](over[[hs.recycle(i1, length(over))]])
        if (r1) 
            return(if (length(R)) R[[hs.recycle(i1, length(R))]] else r1)
    }
    no_match
}
hs.for0 <- function(..., .env = parent.frame()) {
    lang <- substitute(hs.for(...))
    for (i in hs.seqForward(2, length(lang))) {
        if (is(lang[[i]], "name")) {
            do.call("for", list(lang[[i]], lang[[i + 1]], lang[-seq(2, i + 1)]), envir = .env)
            break
        }
        else ...elt(i - 1)
    }
}
hs.for <- function(..., .env = parent.frame(), .named = {
}, .local = 0) {
    if (.local) 
        .env <- new.env(parent = .env)
    if (length(.named)) {
        e1 <- substitute(hs.catch(hs.for0(...), named = .named))
        eval(e1, envir = .env)
    }
    else hs.for0(..., .env = .env)
}
hs.screen.clear <- function(n1 = 9) {
    cat(rep("", n1), sep = "\n")
}
hs.deInf <- function(v1) {
    na.i <- is.na(v1)
    for (fx1 in c("+", "-")) {
        switch(fx1, `+` = {
            i1 <- which(is.infinite(v1) & (v1 > 0))
            if (length(i1)) {
                a1 <- max(v1[-i1], na.rm = 1)
                v1[i1] <- a1
            }
        }, `-` = {
            i1 <- which(is.infinite(v1) & (v1 < 0))
            if (length(i1)) {
                a1 <- min(v1[-i1], na.rm = 1)
                v1[i1] <- a1
            }
        })
    }
    v1
}
order.hs <- function(..., order = -1, na.last = 1) {
    tbl <- as.data.table(list(...))
    tbl[, `:=`("i.36_03_11_203708", 1:.N)]
    setorderv(tbl, names(tbl) %rm% "i.36_03_11_203708", order = order, na.last = as.logical(na.last))
    tbl[, i.36_03_11_203708]
}
set.clean.hs <- function(V) V %not% c("", NA)
hs.clean <- function(V, U = 0, dirty = c("", NA, NaN), dirty.extra = {
}, dirty.pattern = {
}, ot = if (is.list(V)) "l" else "v", inv = 0, how_many = "any") {
    if (length(dirty.extra)) 
        dirty <- dirty %or% dirty.extra
    if (is.list(V)) {
        jg <- lapply(V, hs.clean, dirty = dirty, dirty.extra = dirty.extra, ot = "l", inv = inv)
        cleanN <- hs.rowSums(jg)
        jg <- switch(how_many, any = cleanN > 0, all = cleanN == length(V))
        return(if (inv) !jg else jg)
    }
    if (U) 
        V <- unique(V)
    is_clean <- V %not.in% dirty
    for (p1 in dirty.pattern) {
        i <- which(is_clean)
        if (!length(i)) 
            break
        i1 <- hs.grep(V[i], p1)
        is_clean[i[i1]] <- FALSE
    }
    switch(ot, v = if (inv) V[!is_clean] else V[is_clean], i = which(if (inv) !is_clean else is_clean), l = if (inv) !is_clean else is_clean, n = sum(if (inv) !is_clean else is_clean), t = hs.table(if (inv) !is_clean else is_clean))
}
hs.dirty <- function(...) {
    hs.clean(..., inv = 1)
}
hs.qp <- function() cat(rep("", 30), sep = "\n")
hs.matrix_to_list <- function(m1, side = "column", name = 0) {
    jg <- switch(side, column = lapply(1:ncol(m1), function(i1) m1[, i1]), row = lapply(1:nrow(m1), function(i1) m1[i1, ]))
    if (name) {
        switch(side, column = {
            if (length(colnames(m1))) names(jg) <- colnames(m1)
        }, row = {
            if (length(rownames(m1))) names(jg) <- rownames(m1)
        })
    }
    else {
        names(jg) <- {
        }
    }
    jg
}
hs.ij <- hs.matrix_ij <- local({
    hs.flag <- function(i, m, side = 1) {
        (!length(i)) || isTRUE(i) || {
            if (is.logical(i)) {
                all(i)
            }
            else {
                N <- if (side == 1) {
                  nrow(m)
                }
                else if (side == 2) {
                  ncol(m)
                }
                if (length(i) != N) 
                  return(FALSE)
                if (is.numeric(i)) {
                  !is.unsorted(i)
                }
                else if (inherits(i, "character")) {
                  all(i == hs.switch(side, 1, rownames(m), 2, colnames(m)))
                }
            }
        }
    }
    function(m, i = {
    }, j = {
    }, drop = FALSE, ...) {
        flag_i <- hs.flag(i = i, m = m, side = 1)
        flag_j <- hs.flag(i = j, m = m, side = 2)
        if (inherits(m, "data.table")) {
            if (...length()) {
                hs.browser("39.01.17.131158", debug = 0)
                A <- list(...)
                exp_l
                wk.dt.ij(m, exp_l = exp_l)
                hs.switch(c(flag_i, flag_j), c(0, 1), m[i], c(1, 0), m[, .SD, .SDcols = c(j)], c(0, 0), m[i, .SD, .SDcols = c(j)], m)
                wk.dt.ij(m, ...)
            }
            else hs.switch(c(flag_i, flag_j), c(0, 1), m[i], c(1, 0), m[, .SD, .SDcols = c(j)], c(0, 0), m[i, .SD, .SDcols = c(j)], m)
        }
        else if (inherits(m, c("matrix", "data.frame"))) {
            drop <- as.logical(drop)
            hs.switch(c(flag_i, flag_j), c(0, 1), m[i, , drop = drop], c(1, 0), m[, j, drop = drop], c(0, 0), m[i, j, drop = drop], m)
        }
        else hs.switch(c(flag_i, flag_j), c(0, 1), m[i, ], c(1, 0), m[, j], c(0, 0), m[i, j], m)
    }
})
hs.i <- function(v, i, vt = if (inherits(v, "character") && (length(v) == 1) && (!length(names(v))) && inherits(i, "character") && (length(i) == 1) && (!length(names(i)))) "pkg") {
    if (missing(i) && inherits(v, "character") && (length(v) == 1) && (!length(names(v)))) {
        return(packageDescription(v))
    }
    if (length(vt) && (vt == "pkg")) 
        return(do.call(`::`, list(v, i)))
    v_len <- if (length(dim(v)) == 2) 
        dim(v)[2]
    else length(v)
    if (inherits(i, "character")) {
        i <- match(i, names(v))
    }
    else {
        ii <- which(i < 0)
        if (length(ii)) 
            i[ii] <- v_len + (i[ii] + 1)
        ii <- which(i > v_len)
        if (length(ii)) {
            r <- i[ii]%%v_len
            i[ii] <- ifelse(r > 0, r, v_len)
        }
    }
    hs.switch(length(dim(v)), 0, , 1, v[i], 2, hs.ij(v, , i), hs.browser("2023.08.02.141804", debug = 0))
}
hs.put2end <- function(x, i) {
    db <- seq_along(x)
    x[c(db[-i], db[i])]
}
popular.hs <- function(L, N = length(L) - D, D = 0, exclude = c("", NA)) {
    if (hs.len1(L)) 
        if (N == 1) 
            return(unlist(L))
    L2 <- inverseList(L)
    names(L2)[sapply(L2, length) >= N] %not% exclude
}
hs.getter <- function(o1, name = {
}, name.pattern = {
}, always.list = 0) {
    if (inherits(o1, "function")) 
        o1 <- environment(o1)
    if (inherits(o1, "environment")) {
        if (length(name.pattern)) 
            name <- ls(envir = o1, pattern = name.pattern)
        if (length(name)) {
            if ((length(name) == 1) && inherits(name, "character")) 
                return(o1[[name]])
            if (name == -1) {
                mget(ls(o1), o1)
            }
            else mget(name, o1)
        }
        else ls(o1)
    }
    else {
        if (length(name.pattern)) 
            name <- hs.grepV(names(o1), name.pattern)
        if (length(name)) {
            if (identical(name, -1)) {
                o1
            }
            else o1[name]
        }
        else names(o1)
    }
}
hs.matrix.merge <- function(m1, rng = {
}, cng = {
}) {
    if (length(rng)) {
        a1 <- lapply(rng, function(.) {
            colSums(m1[., , drop = 0])
        })
        jg <- do.call("rbind", a1)
        rownames(jg) <- local({
            . <- rng
            if (length(names(.))) {
                names(.)
            }
            else {
                sapply(., function(.) {
                  a1 <- if (is.numeric(.)) {
                    rownames(m1)[.]
                  }
                  else .
                  paste(a1, collapse = "+")
                })
            }
        })
        m1 <- jg
    }
    if (length(cng)) {
        m1 <- t(hs.matrix.merge(t(m1), cng))
    }
    m1
}
hs.cut <- function(x1, ..., lo = include.lowest, include.lowest = TRUE, hi = right, right = !include.lowest) {
    i1 <- hs.clean(x1, ot = "i")
    if (length(i1)) {
        if (length(i1) < length(x1)) {
            x1[i1] <- cut(x1[-i1], ..., include.lowest = lo, right = hi)
            return(x1)
        }
        else return(cut(x1, ..., include.lowest = lo, right = hi))
    }
    else return(x1)
}
hs.cut_fine <- function(x1, breaks) {
    if (any(hs.grepl(breaks, "Inf"))) 
        hs.browser("38.01.02.091035")
    breaks <- unique(breaks)
    N <- as.numeric(hs.sub(breaks, "[\\(\\)\\[\\]]"))
    jg <- rep(NA, length(x1))
    i1 <- order(N)
    if (is.unsorted(i1)) {
        breaks <- breaks[i1]
        N <- N[i1]
    }
    boundary <- hs.re(breaks, "[\\(\\)\\[\\]]")
    L <- rep("", length(breaks) + 1)
    b2 <- boundary[1]
    n2 <- N[1]
    if (b2 %in% c("(", "]")) {
        L[1] <- paste0("<=", n2)
        i1 <- x1 <= n2
    }
    else {
        L[1] <- paste0("<", n2)
        i1 <- x1 < n2
    }
    if (any(i1)) 
        jg[i1] <- L[1]
    for (i1 in hs.seqForward(2, length(breaks))) {
        n1 <- n2
        b1 <- b2
        b2 <- boundary[i1]
        n2 <- N[i1]
        bb <- paste0(b1, b2)
        if (0) {
            hs1 <- function(p1) {
                strsplit(p1, "")[[1]] %=>% lapply(. %=>% {
                  c(x, switch(x, `(` = "]", `[` = ")", `)` = "[", `]` = "("))
                }) %=>% {
                  do.call(expand.grid, x)
                } %=>% apply(1, paste, collapse = "") %=>% .q %=>% paste(collapse = ", ") %=>% {
                  paste0("c( ", x, ")")
                } %=>% .ss
            }
            null <- sapply(c("()", "[)", "[]", "(]"), hs1)
        }
        nn <- paste(n1 %or% n2, collapse = ", ")
        if (bb %in% c("()", "])", "([", "][")) {
            i2 <- (x1 > n1) & (x1 < n2)
            L[i1] <- paste0("(", nn, ")")
        }
        else if (bb %in% c("[)", "))", "[[", ")[")) {
            i2 <- (x1 >= n1) & (x1 < n2)
            L[i1] <- paste0("[", nn, ")")
        }
        else if (bb %in% c("[]", ")]", "[(", ")(")) {
            i2 <- (x1 >= n1) & (x1 <= n2)
            L[i1] <- paste0("[", nn, "]")
        }
        else if (bb %in% c("(]", "]]", "((", "](")) {
            i2 <- (x1 > n1) & (x1 <= n2)
            L[i1] <- paste0("(", nn, "]")
        }
        if (any(i2)) 
            jg[i2] <- L[i1]
    }
    b1 <- boundary[length(breaks)]
    n1 <- N[length(N)]
    if (b1 %in% c("[", ")")) {
        L[length(L)] <- paste0(">=", n1)
        i1 <- x1 >= n1
    }
    else {
        L[length(L)] <- paste0(">", n1)
        i1 <- x1 > n2
    }
    if (any(i1)) 
        jg[i1] <- L[length(L)]
    hs.factor(jg, L %and% jg, N = 0)
}
hs.top <- function(x1, N, end = "left", ot = "v", sort = 1) {
    l1 <- length(x1)
    if (N < 1) 
        N <- floor(l1 * N)
    N <- as.integer(N)
    if (sort) {
        i1 <- head(order(x1, decreasing = switch(end, left = FALSE, right = TRUE)), N)
    }
    else {
        i1 <- switch(end, left = seq.int(N), right = {
            seq.int(l1 - N + 1, l1)
        })
    }
    switch(ot, v = x1[i1], i = i1)
}
hs.hs <- function(x1, ...) {
    lapply(list(...), function(hs1) hs1(x1))
}
hs.factor <- function(v1, levels = as.character(unique(v1)), N = 1, nl = 1, ref = {
}, sort = 0, ot = "xxx", ...) {
    if (ot == "str") 
        return(levels(v1)[v1])
    if (length(ref)) 
        levels <- ref %or% levels
    if (is.factor(v1)) {
        levels <- levels %and% levels(v1)
        tbl <- hs.table(v1)
        if (0 %in% tbl) {
            level2drop <- names(tbl)[tbl == 0]
            levels <- levels[levels %not.in% level2drop]
        }
    }
    else {
        levels <- levels[levels %in% v1]
    }
    if (!(is.factor(v1) && all(levels(v1) == levels))) {
        v1 <- factor(v1, levels, ...)
    }
    if (N) {
        v.pct.hs(v1, nl = nl)
    }
    else v1
}
eval. <- function(o1, ..., env1 = parent.frame()) {
    o1.exp <- substitute(o1)
    eval(bquote({
        eval(substitute(.(substitute(...)), list(. = quote(.(o1.exp)))))
    }), env1)
}
isTrue.hs.on <- function(on1, ..., env1 = parent.frame()) {
    L <- list(...)
    if (on1 %in% names(L)) {
        as.logical(L[[on1]])
    }
    else exists(on1, envir = env1, inherits = FALSE) && get(on1, envir = env1)
}
hs.intersection <- function(...) {
    sj <- list(...)
    jg <- sj[[1]]
    if (length(sj) > 1) {
        for (sj1.i in 2:length(sj)) {
            if (!length(jg)) 
                break
            jg <- jg %and% sj[[sj1.i]]
        }
    }
    jg
}
hs.list.common <- function(L) {
    names(L) <- seq(length(L))
    L2 <- inverseList(L)
    names(L2)[sapply(L2, length) == length(L)]
}
hs.as.numeric <- function(x1) {
    switch(as.character(length(dim(x1))), `0` = {
        jg <- as.numeric(x1, ...)
        if (length(names(x1))) names(jg) <- names(x1)
        jg
    }, `2` = {
        matrix(as.numeric(x1), ncol = ncol(x1), dimnames = dimnames(x1))
    })
}
isList <- function(x) {
    (typeof(x) == "list") && (length(dim(x)) < 2)
}
chk <- local({
    chk.nonList <- function(x, howMany = 3) {
        a <- dim(x)
        if (!length(a)) 
            a <- length(x)
        do.call("[", append(list(x), append(lapply(a, function(n) {
            1:min(n, howMany)
        }), list(drop = FALSE))))
    }
    function(x, nlines = 9, width = 125, dim.n = 3) {
        if (substitute(x) %=>% {
            inherits(x, "character") && (length(x) == 1)
        }) {
            return(help(package = (x)))
        }
        if (dim.n != 3) {
            chk.nonList <<- function(x, howMany = dim.n) {
                a <- dim(x)
                do.call("[", c(list(x), sapply(a, function(n) {
                  1:min(n, howMany)
                }), list(drop = FALSE)))
            }
        }
        f <- c("length", "dim", "names", "typeof", "class")
        sapply(f, function(f) {
            .ss(sprintf("\t**** %s ****", f))
            y <- f %(% list(x)
            .s(y, max.print = 9)
        })
        flag <- 0
        if (isList(x)) {
            y <- sapply(x, length)
            .ss("length of element: ")
            .s(y, max.print = 9)
            .ss("sum of numbers of element: ")
            .s(sum(y), max.print = 9)
            flag <- listPrintLinesCounts(x)
        }
        if (flag < 999) {
            .ss("\t**** contents ****")
            if (is.list(x) & !is.data.frame(x)) {
                x <- rapply(x, chk.nonList, how = "list")
            }
            else x <- chk.nonList(x, dim.n)
            zz <- textConnection(capture.output(.s(head(x, nlines))))
            cat(sapply(readLines(zz, nlines), toString, width), sep = "\n")
            close(zz)
        }
    }
})
chk2 <- function(M, Nline = 20, width = 128) {
    dm <- dim(M)
    if (is.null(dm)) 
        ieiwk.cat.line(c("length: ", length(M)))
    else ieiwk.cat.line(c("dim: ", dim(M)))
    ieiwk.cat.line(c("type: ", typeof(M)))
    ieiwk.cat.line(c("class: ", class(M)))
    if (length(M) > 9 | (class(M) %in% c("matrix", "data.frame"))) {
        a <- capture.output(.s(head(M, max(10, Nline))))
    }
    else {
        a <- capture.output(.str(M))
    }
    i <- as.integer({
        width - 3
    }/2)
    a <- sapply(a, function(x) {
        if (nchar(x) > width) {
            i <- min(as.integer({
                nchar(x) - 3
            }/2), i)
            c(substr(x, 1, i), substring(x, nchar(x) - i)) %,% "..."
        }
        else x
    })
    .ss(head(a, Nline))
}
replicate.hs.group <- function(lv, sep = "") {
    . <- lv
    . <- tapply(., ., function(.) {
        paste0(., sep, seq_along(.))
    })
    unlist(unname(.))
}
file.maker <- function(target, dependency, hs1) {
    if (FALSE %in% file.exists(target)) {
        hs1(target, dependency)
    }
    target
}
obj.maker <- function(..., env1 = parent.frame(), env2 = new.env(parent = env1)) {
    oh <- list(...)
    if (length(oh)) {
        if (FALSE %in% sapply(oh[[1]], hs.exists, env1)) {
            if (length(oh) > 2) 
                do.call(obj.maker, append(oh[-c(1, 2)], env1 = env1))
            jg <- oh[[2]]()
            hs.browser("37.01.18.234903")
        }
    }
}
hs.order <- order10 <- function(..., decreasing = fx < 0, fx = -1, use = {
}) {
    A <- list(...)
    if (inherits(A[[1]], "matrix")) 
        A[[1]] <- hs.df(A[[1]])
    if (inherits(A[[1]], c("list", "data.frame"))) 
        A <- c(as.list(unname(hs.ij(A[[1]], , j = use))), A[-1])
    do.call(order, c(A, list(decreasing = as.logical(decreasing))))
}
hs.sort <- sort10 <- function(..., decreasing = fx < 0, fx = -1, use = {
}) {
    if (inherits(..1, "data.table")) {
        use <- if (length(use)) {
            if (is.numeric(use)) {
                names(..1)[use]
            }
        }
        else names(..1)
        return(wk.dt.setorderv(..1, cols = use, order = fx))
    }
    i <- hs.order(..., decreasing = decreasing, use = use)
    xi <- match("x", ...names(), nomatch = 1)
    x <- ...elt(xi)
    if (length(dim(x)) == 2) 
        return(hs.ij(x, i))
    return(x[i])
}
hs.apply <- function(m1, side, hs1, ...) {
    hs.switch(side, 1, lapply(seq_len(nrow(m1)), function(i1) hs1(m1[i1, ], ...)), 2, lapply(seq_len(ncol(m1)), function(i1) hs1(m1[, i1], ...)), no_match = lapply(asplit(m1, side), hs1, ...))
}
hs.match.from_tail <- function(v1, t1, n1 = 10000L) {
    t2 <- rev(tail(t1, n1))
    m1 <- length(t2) - match(v1, t2) + 1L
    if (length(t2) < length(t1)) 
        m1 <- length(t1) - length(t2) + m1
    m1
}
hs.rename <- function(x1, n1, n2, hs1 = names) {
    i1 <- match(n1, hs1(x1))
    eval(bquote(.(substitute(hs1))(x1)[i1] <- n2))
    x1
}
hs.version_newest <- function(...) {
    ver0 <- unique(c(...))
    max_i <- 1
    ver <- hs.re(ver0, "\\d.*")
    for (i1 in hs.seqForward(2, length(ver0))) if (utils::compareVersion(ver[max_i], ver[i1]) < 0) 
        max_i <- i1
    ver0[max_i]
}
hs.catch <- local({
    env.38_01_04_223523 <- new.env()
    evalq(hs.throw <- function(...) {
        acc.38_01_04_221205[["add"]](...)
        return()
    }, env.38_01_04_223523)
    function(exp1, named = 0, envUp = parent.frame()) {
        on.exit(evalq(rm(list = ls(all.names = TRUE, sorted = FALSE) %not% "hs.throw"), env.38_01_04_223523), add = TRUE)
        if (!identical(parent.env(env.38_01_04_223523), envUp)) 
            parent.env(env.38_01_04_223523) <- envUp
        env.38_01_04_223523[["acc.38_01_04_221205"]] <- if (named) 
            namedExpandingList()
        else expandingList()
        eval(substitute(exp1), envir = env.38_01_04_223523)
        env.38_01_04_223523[["acc.38_01_04_221205"]][["get"]]()
    }
})
hs.replace <- function(x, old, new = {
}) {
    if (!length(new)) {
        i <- seq(2, length(old), by = 2)
        new <- old[i]
        old <- old[-i]
    }
    if (inherits(x, "factor")) {
        i <- which(old %in% levels(x))
        if (length(i)) {
            if (length(i) < length(old)) {
                old <- old[i]
                new <- new[i]
            }
            old <- match(old, levels(x))
            levels(x) <- replace(levels(x), old, new)
        }
        x
    }
    else {
        paste(hs.replace(factor(x), old, new))
    }
}
hs.merge <- function(x, y = {
}, sort = FALSE, all.x = TRUE, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...) {
    if (length(y)) {
        if (length(names(by)) && missing(by.x) && missing(by.y)) {
            by.x <- names(by)
            by.y <- by
        }
        merge(x = x, y = y, sort = sort, all.x = all.x, by.x = by.x, by.y = by.y, by = by, ...)
    }
    else {
        if (inherits(x, "list") && (!inherits(x, "data.frame"))) {
            return(hs.list.merge(x))
        }
        else hs.browser("2023.07.25.162653", debug = 0)
    }
}
"%which.not.in%" <- function(x1, x2) {
    which(x1 %not.in% x2)
}
"%which.in%" <- function(x1, x2) {
    which(x1 %in% x2)
}
"%rm%" <- function(x, y) {
    if (any(typeof(y) %in% c("language", "expression", "symbol"))) {
        x[eval(as.cmd(sprintf(ifelse(length(grep("%s", y[[1]])), y[[1]], paste("%s", y[[1]])), "x")))]
        x[!tfs(c(deparse(substitute(x)), y[[1]]) %,% "")]
    }
    else {
        x[x %not.in% y]
    }
}
"%not.in%" <- function(x, y) !(x %in% y)
"%all.in%" <- function(x, y) all(x %in% y)
"%not.all.in%" <- function(x, y) !all(x %in% y)
"%none.in%" <- function(x, y) !any(x %in% y)
"%any.in%" <- function(x, y) any(x %in% y)
"%keep%" <- function(x, y) {
    if (any(typeof(y) %in% c("language", "expression"))) {
        x[eval(as.cmd(sprintf(ifelse(length(grep("%s", y[[1]])), y[[1]], paste("%s", y[[1]])), "x")))]
    }
    else {
        x[x %in% y]
    }
}
hs.len1 <- function(x, y = 1L) {
    length(x) == y
}
hs.of_length <- hs.len1
merge_list <- function(old, new = {
}, update = 1) {
    nm <- names(old) %and% names(new)
    if (length(nm)) {
        if (anyDuplicated(names(old)[names(old) %in% nm])) 
            stop("[37_04_25_165259] the \"old\" list has duplicated names")
        if (anyDuplicated(names(new)[names(new) %in% nm])) 
            stop("[37_04_25_165504] the \"new\" list has duplicated names")
    }
    if (length(nm) && update) {
        old[nm] <- new[nm]
    }
    c(old, if (length(nm)) new[names(new) %not.in% nm] else new)
}
"%set==%" <- function(v1, v2) {
    (v1 %all.in% v2) && (v2 %all.in% v1)
}
"%not%" <- setdiff
"%and%" <- intersect
"%or%" <- union
hs.xor <- function(v1, v2, ot = "l") {
    switch(ot, l = {
        a <- list(v1 %not% v2, v2 %not% v1)
        if (all(sapply(a, length) == 0)) a <- {
        }
        a
    }, n = c(sum(v1 %not.in% v2), sum(v2 %not.in% v1)))
}
hs.msg <- function(msg = "", ln = 0, lw = 10) {
    message(sprintf(paste0("%-", lw, "s"), if (ln) 
        paste(rep("*", ln), collapse = "")
    else ""), "[", Sys.time(), "] ", msg)
}
.muid4machine <- .m4m <- function(names, n = 1, interval = 0.01, precision = 1e+09, sep = if (precision > 0) "." else "_", sep1 = "_", hz = {
}, time_pattern = paste(c("%Y", "%m", "%d", "%H%M%S"), collapse = sep)) {
    birth.time <- "2010-09-15 22:26:51 CST"
    if (!missing(names)) 
        n <- length(names)
    r <- {
    }
    while ({
        i <- length(r)
    } < n) {
        n1 <- n - i + 1
        r[seq(i + 1, n)] <- sapply(seq(i + 1, n), function(i) {
            a <- Sys.time()
            y1 <- as.integer(re("\\d+", a)) - 1984
            paste(c(sub("\\d+", y1, format(a, time_pattern)), if (precision > 0) c(sep1, as.numeric(difftime(Sys.time(), birth.time, units = "secs") * precision))), collapse = "")
        })
        r <- unique(r)
    }
    if (!missing(names)) 
        names(r) <- names
    r
}
hs.uid.fp <- function(base_nyr = 1) {
    a <- Sys.time()
    y1 <- as.integer(re("\\d+", a)) - 1984
    f1 <- if (base_nyr) 
        "aaaa/%m/%d/aaaa_%m_%d_%H%M%S"
    else "aaaa/%m/%d/%H%M%S"
    hs.gsub(format(a, f1), "aaaa", y1)
}
hs.seed_generate <- function() {
    a1 <- hs.sub(.muid4machine(), "_\\d+$")
    a1 <- strsplit(a1, "\\.")[[1]]
    as.integer(hs.pastec0(a1[-1]))
}
today_file_path <- function(ext1 = "org", root = "") {
    jg <- hs.fp(as.integer(format(Sys.Date(), "%Y")) - 1984L, format(Sys.Date(), paste0("%m/%d.", ext1)))
    if (nzchar(root)) 
        hs.fp(root, jg)
    else jg
}
osType <- function() {
    Sys.info()["sysname"]
}
wj_rsync <- switch(osType(), Linux = "rsync", Darwin = "/usr/local/bin/rsync")
.sys <- function(..., sep = " ", wait = 1, intern = 1, chk = 1, dz = {
}, pipe = {
}, fifo = 0, report = 0, file, file.append = TRUE, use.tmpfile = 0) {
    cmd <- {
        A <- c(...)
        if (inherits(A, "list")) {
            A <- unlist(A)
        }
        a <- paste(A, collapse = sep)
        a <- hs.gsub(a, sprintf("%s\n%s", sep, sep), "\n")
        if (hs.grepl(a, "^cmd")) 
            a <- hs.gsub(a, "\\s*&&\\s*", "&&")
        a
    }
    if (!missing(file)) {
        .cat(cmd, file = file, append = file.append)
        return(invisible())
    }
    if (length(pipe)) {
        con <- pipe(cmd, pipe)
        return(con)
    }
    if (fifo) 
        return(c("<(", cmd, ")"))
    if (chk) {
        if (intern) {
            cmd
        }
        else message(cmd)
    }
    else {
        if (report) {
            r <- system(cmd, wait = TRUE, intern = as.logical(intern))
            .r()
            if (intern) 
                r
        }
        if (use.tmpfile) {
            f1 <- hs.wj.temp()
            on.exit(hs.rm.wj(f1), add = TRUE)
            cat(cmd, file = f1, sep = "\n")
            aaa <- system2("/bin/bash", f1, wait = as.logical(wait), stdout = if (intern) 
                TRUE
            else "")
        }
        else {
            aaa <- .try(system(cmd, wait = as.logical(wait), intern = as.logical(intern)))
        }
        aaa
    }
}
.sys.io.2wj <- function(wj1, cmd = "", Z = grepl("\\.gz$", wj1), append = 0) {
    pipe(.sys(cmd, if (Z) 
        c(if (cmd != "") "|", "gzip"), ifelse(append, ">>", ">"), hs.fpGood(wj1)), "w")
}
wk.io.lines.2wj <- function(L, wj1, ...) {
    io1 <- .sys.io.2wj(wj1, ...)
    on.exit(close(io1))
    cat(L, file = io1, sep = "\n", ...)
    wj1
}
.r <- ieiwk.email.job.is.done <- function(title = Sys.time(), title.postfix = if ("difftime" %in% class(title)) "in total" else ", done at", ps.pattern, nodes = cluster.all(), chk.ps = 0, update.time = 0, time.diff = {
}, time.diff.2 = {
}, content = "bbb") {
    if (.hostname() %in% c("wk-NBLK-WAX9X", "wk-MS-7A21")) {
        .wk("wps", wk.wjlj(paste0(title, title.postfix)), "&")
        return()
    }
    if (0) 
        if (!length(time.diff)) 
            time.diff <- local({
                wj1 <- .mfp("33.06.06.230513.rd")
                if (file.exists(wj1) && !update.time) 
                  hs.load(wj1)
                else {
                  n1 <- "192.115.110.254"
                  x0 <- ieiwk.eval(quote(list(readLines("https://time.is"), Sys.time())), intern = 1, n1 = n1)
                  x <- x0[[1]]
                  x1 <- x[wk.re(x, "Exact time now:", "which") + 1]
                  a <- wk.re(x1, "twd", "sub")
                  i <- wk.re(a, ">", "locate") + 1
                  t1 <- substr(a, i, i + 7)
                  d1 <- do.call(".j", c(wk.re(wk.re(x1, "title", "sub", sub.len = 50), "\\d+", all = 1), collapse = "-"))
                  dt1 <- .jc(d1, t1, sep = " ")
                  dt2 <- x0[[2]]
                  r <- difftime(dt1, dt2)
                  ieiwk.save("r", wj1)
                  r
                }
            })
    if (0) 
        if (!length(time.diff.2)) 
            time.diff.2 <- local({
                wj1 <- .mfp("33.06.06.231803.rd")
                if (file.exists(wj1) && !update.time) 
                  hs.load(wj1)
                else {
                  n1 <- "log01"
                  t1 <- ieiwk.eval(quote(Sys.time()), n1 = n1, intern = 1)
                  t2 <- Sys.time()
                  r <- difftime(t1, t2)
                  ieiwk.save("r", wj1)
                  r
                }
            })
    if (chk.ps) {
        while (wdjc.search(ps.pattern, nodes)) {
        }
    }
    title <- paste0(title, title.postfix)
    hn <- Sys.info()["nodename"]
    n1 <- ieiwk("host.with.internet")
    cmd1 <- bquote({
        .wk("echo", .q(.(content)), "| mutt -s", .q(.(title)), "wengkai_report@163.com")
    })
    if (hn == n1) {
        eval(cmd1)
        return()
    }
    ieiwk.eval(cmd1, bg = 0, n1 = n1, via.file.result = 0, email = 0)
}
.wk <- .sd <- .sys.do <- function(..., intern = 0, wait = 1, log = FALSE, log.append = TRUE, log.cmd = TRUE, target, ssh = Sys.info()["nodename"], use.tmpfile = 1, fifo = 0, fifo.4r = 0, report.via.email = 0, email = report.via.email, redo = 0) {
    if (is.logical(log) && log) 
        log <- hs.wjm(dn = hs.home.expand("~/log"), .muid4machine(), ext = "log")
    if (is.character(log) && file.exists(log) && {
        !redo
    }) 
        return()
    args <- {
        a <- lapply(substitute(list(...)), function(x) c(x))[-1]
        if (is.null(names(a))) {
            a1 <- a
            a2 <- list()
        }
        else {
            a1 <- a[names(a) == ""]
            a2 <- a[names(a) != ""]
        }
        env1 <- parent.frame()
        append(lapply(unlist(a1), eval, env1), a2)
    }
    args[["intern"]] <- intern
    if (osType() == "Windows") 
        args <- c("cmd /C", args)
    if (is.character(log)) {
        a <- ieiwk.file.path(log)
        .cat.time(file = log, append = log.append)
        if (log.cmd) {
            cmd <- local({
                args["intern"] <- 1
                do.call(.sys, args)
            })
            hs.say(cmd, file = log)
            hs.cat("run on ", if (nchar(ssh)) 
                ssh
            else Sys.info()["nodename"], file = log)
        }
    }
    if (fifo) {
        f.in <- ieiwk.tempfile()
        .sys.do("mkfifo", f.in)
        args <- append(args, list(">", unname(.sys.fpGood(f.in)), "&"))
        do.call(.sys, c(args, chk = 0, use.tmpfile = use.tmpfile))
        if (fifo.4r) 
            return(.sys("cat", unname(.sys.fpGood(f.in)), pipe = 1))
        else return(f.in)
    }
    r <- do.call(.sys, c(args, chk = 0, use.tmpfile = unname(ifelse(osType() == "Windows", 0, use.tmpfile)), wait = wait))
    if (is.character(log)) {
        if (intern || {
            !intern && !r
        }) 
            .cat.time(file = log)
    }
    if (!missing(target)) 
        cat("done", file = target)
    if (email && !wait) 
        ieiwk.email.job.is.done()
    r
}
.sys.less <- function(...) {
    args <- .sys(...)
    args <- {
        if (file.exists(args)) {
            c("less -N", args)
        }
        else c(args, "| less -N")
    }
    .sys.do(args)
}
.sys.junction <- function(s, t, p = 1, b = "D:/wk/my/rj/Junction/junction.exe") {
    if (p) 
        tp <- t
    tp <- ifelse(p, t, dirname(t))
    t <- file.path(tp, basename(s))
    .sys.do(path.expand(b), .sys.fpGood(t), .sys.fpGood(s))
}
.sys.str <- function(...) {
    .sys(..., intern = 1)
}
.sys.bsub <- function(...) {
    .sys("bsub", shQuote(.sys.str(...)))
}
.wk.bsub.2mem <- function(e1, J = paste0("wk.job", ".", wk.re(wk.re(as.character(Sys.time()), "\\s+", "replace", replacement = ",", all = 1), ":", "replace", replacement = "-", all = 1)), n = 8, R = shQuote(paste0("span[ptile=", min(n, 10), "]"), "cmd"), o = paste0(J, "_output"), e = paste0(J, "_errput"), q = "mem", m = "fat01") {
    wj1 <- ieiwk.tempfile()
    .ss(capture.output(e1), file = wj1)
    wj2 <- ieiwk.tempfile()
    .ss("#!/bin/bash", .sys("Rscript", wj1), .sys("unlink", wj1), .sys("unlink", wj2), file = wj2)
    .wk("bsub", sapply(sort(unlist(strsplit("JnoemqR", ""))), function(x) {
        paste0("-", x, " ", get(x))
    }), "<", wj2)
    c(J, o, e)
}
.sys.bsub.do <- function(...) {
    .sys.do("bsub", shQuote(.sys.str(...)))
}
.sys.man <- function(bin) {
    .ss(.sys("man", bin, intern = 1, chk = 0))
}
.sys.fpGood <- function(wj, home.expand = 1, qn = 1) {
    if (home.expand) 
        wj <- hs.home.expand(wj)
    a1 <- hs.str_split(wj, sep = "/+")
    jg <- sapply(a1, function(a1) {
        a1 <- sapply(a1, function(a1) {
            if (a1 %in% c(".", "..", "~", "")) 
                a1
            else .q(a1, n = qn)
        })
        paste(a1, collapse = "/")
    })
    return(jg)
    if (0) {
        if (hs.grepl(wj, "[ <>]")) 
            wj <- hs.gsub(wj, "([ <>]+)", "'\\1'")
    }
    wj %=>% sapply(. %=>% {
        if (hs.grepl(x, "^((\\.*|~)?/)")) {
            a1 <- strsplit(c("../../boot", "~"), "/")
            a <- sapply(a1, function(a1) {
                a1 <- sapply(a1, function(a1) {
                  if (a1 %in% c(".", "..", "~")) 
                    a1
                  else .q(a1)
                })
                paste(a1, collapse = "/")
            })
        }
        if (substr(x, 1, 2) == "~/") {
            a <- paste0("~/", .q(substring(x, 3)))
            x <- if (qn > 1) 
                .q(a)
            else a
        }
        else if (substr(x, 1, 1) == "/") {
            a <- .q(x)
            x <- if (qn > 1) 
                .q(a)
            else a
        }
        else if (hs.grepl(x, " ")) {
            x <- .q(x)
        }
        x
    })
}
.sys.fpGood.file <- function(f1) {
    if (!length(system(paste("file", f1), intern = TRUE))) 
        f1 <- shQuote(f1)
    f1
}
.sys.view <- function(f) {
    f2 <- .sys.fpGood(f)
    if (file.info(f)$isdir) {
        .sys.do("nautilus --browser", f2, "&")
        return()
    }
    else {
        if (length(grep("\\.pdf$", f))) {
            .sys.do("evince", f2, "&")
        }
    }
}
.sys.l <- function(fp = "") {
    .sys.do("ls -ahl", fp)
}
.sys.nl <- function(fp) {
    a <- sapply(fp, function(fp) .sys("wc -l", fp, intern = 1))
    r <- as.numeric(re("\\d+", a))
    names(r) <- fp
    r
}
.sys.cp <- function(f1, f2, d = 0) {
    .sys.do("cp", f1, f2)
    if (d) 
        .sys.do("rm -r", f1)
    f2
}
.q <- ieiwk.shQuote <- function(s1, n = 1, type = "sh") {
    r <- s1
    while (n) {
        r <- shQuote(r, type = type)
        n <- n - 1
    }
    r
}
.qq <- function(...) {
    .q(..., type = "cmd")
}
ieiwk.fp.cygwin2win <- function(f) {
    a <- tail(strsplit(f, "/")[[1]], -2)
    a[1] <- sprintf("%s:", a[1])
    a %,% "/"
}
ieiwk.fp.win2cygwin <- function(f) {
    p <- re("^[^:]+", f)
    sub("^[^:]+:", sprintf("/cygdrive/%s", p), f)
}
Ncpu <- function(hostname) {
    a <- ieiwk.mpstat()
    c(ceiling({
        as.numeric(a["all", "%idle"])/100
    } * {
        nrow(a) - 1
    } - 0.5), {
        nrow(a) - 1
    })
}
hs.NC <- function() {
    max(parallel::detectCores()/2 - 1, 1)
}
ieiwk.rm <- function(fp, r = 0) {
    system(sprintf("rm %s %s", ifelse(r, "-r", ""), fp))
}
wk.find.wj.2tmp <- function(wjj1 = ".", wj = 1, wjj = 0, wjj.empty = 1, wj2exclude = {
}, follow.symlink = 0, recursive = 0, maxdepth = 1, find.extra = list(), wj.tmp = {
}) {
    if (!length(wj.tmp)) 
        stop("must supply wj.tmp")
    find.par <- c(if (follow.symlink) "-L", .q(wjj1), if (length(maxdepth)) c("-maxdepth", maxdepth))
    r <- {
    }
    if (wj) 
        r <- c(r, .wk("find", find.par, "-type f", intern = 1))
    if (wjj.empty) 
        r <- c(r, .wk("find", find.par, "-type d", "-empty", intern = 1))
    if (wjj) 
        r <- c(r, wjj)
    .ss(r %not% wj2exclude, file = wj.tmp, append = 1)
    if (recursive) {
        folders <- .wk("find", find.par, "-type d", intern = 1)[-1] %not% wj2exclude
        for (folder1 in folders) wk.find.wj.2tmp(folder1, wj2exclude = wj2exclude, follow.symlink = follow.symlink, recursive = recursive, maxdepth = maxdepth, wj.tmp = wj.tmp)
    }
    {
    }
}
wk.list.wj <- function(wjj1 = ".", ms = {
}, ms.bn = {
}, invert = 0, all = 1, wj = 1, wjj = !wj, size.min, qm = 1, recursive = 0, ignore.case = 1, no.. = 1) {
    if (hs.len1(wjj1)) {
        if (0) 
            for (o1 in c("invert", "all", "wj", "wjj", "qm", "recursive", "ignore.case", "no..")) assign(o1, as.logical(get(o1)))
        r <- {
            if (!wj) 
                list.dirs(wjj1, full.names = qm, recursive = recursive)
            else list.files(wjj1, all.files = all, full.names = qm, recursive = recursive, ignore.case = ignore.case, include.dirs = wjj, no.. = no..)
        }
        if (!wjj && !recursive) 
            r <- r[!file.info(r)[, "isdir"]]
        {
            if (length(ms.bn) && length(r)) {
                i <- wk.re(basename(r), ms.bn, "detect")
                r <- r[if (invert) 
                  !i
                else i]
            }
            if (length(ms) && length(r)) {
                i <- wk.re(r, ms, "detect")
                r <- r[if (invert) 
                  !i
                else i]
            }
        }
        r
    }
    else sapply(wjj1, wk.list.wj, ms = ms, ms.bn = ms.bn, invert = invert, all = all, wj = wj, wjj = wjj, size.min = size.min, qm = qm, recursive = recursive, ignore.case = ignore.case, no.. = no..)
}
ieiwk.list.files <- function(path = ".", pattern = NULL, invert = FALSE, all.files = TRUE, only.file = TRUE, only.dir = !only.file, size.min, full.names = TRUE, recursive = FALSE, ignore.case = TRUE, include.dirs = only.dir, no.. = TRUE) {
    a <- list.files(path, pattern, all.files, full.names, recursive, ignore.case, include.dirs, no..)
    b <- list.files(path, pattern, all.files, full.names = TRUE, recursive, ignore.case, include.dirs, no..)
    names(a) <- basename(a)
    if (only.dir) 
        return(a[hs.isDir(b)])
    if (only.file) 
        a <- a[!file.info(b)[, "isdir"]]
    if (!missing(size.min) && is.numeric(size.min)) 
        a <- a[file.info(a)[["size"]] > size.min]
    a
}
ieiwk.list.dirs <- function(wjj = ".", ...) {
    ieiwk.list.files(wjj, ..., only.dir = 1)
}
ieiwk.file.rename <- function(from, to.dir) {
    sapply(from, function(x) file.rename(x, file.path(to.dir, basename(x))))
}
wk.cp.wj2wjj <- function(wj, wjj) {
    for (i1 in seq_along(wj)) {
        file.copy(wj[i1], wjj, )
    }
}
ieiwk.file.copy <- function(from, to.dir, to, copy.time = TRUE, copy.mode = TRUE, ...) {
    sapply(from, function(x) {
        from <- ""
        fp <- list(from = from, to = to, copy.mode = copy.mode, copy.date = copy.time)
        switch(osType(), Linux = {
            file.copy %(% fp[c("from", "to")]
            if (copy.time) {
                Sys.setFileTime(to, ieiwk.mtime(from))
            }
        }, Windows = {
            file.copy %(% fp[c("from", "to", "copy.date")]
        })
    })
}
ieiwk.list.files.bak <- function(path = ".", pattern = NULL, all.files = FALSE, only.file = 0, only.dir = 0, full.names = 1, recursive = 0, ignore.case = 1) {
    browser()
    full.names <- as.logical(full.names)
    recursive <- as.logical(recursive)
    ignore.case <- as.logical(ignore.case)
    r <- list.files(path, pattern, all.files, full.names, recursive, ignore.case, include.dirs = 1)
    if (full.names) 
        a <- ieiwk.fp.norm(a)
    r <- file.path(path, a)
    if (full.names) 
        r <- ieiwk.fp.norm(r)
    names(r) <- a
    if (only.dir) 
        a <- a[file.info(r)[, "isdir"]]
    if (only.file) 
        a <- a[!sapply(r, function(x) {
            file.info(x)$isdir
        })]
    a
}
.sys.ls <- function(d = ".", full = 1, dz = {
}) {
    a <- .sys.do("ls", d, intern = 1, dz = dz)
    r <- file.path(d, a)
    names(r) <- a
    r
}
.sys.scp <- function(fp1, fp2 = fp1, fp1.prefix = .sys.do("echo ~", intern = 1), fp2.prefix = .sys.do("echo ~", intern = 1), l, r = "hh", mb = "r", type.fp = "file", ow = 0) {
    .ss(fp1)
    l.info <- ieiwk.ssh.db(l)
    r.info <- ieiwk.ssh.db(r)
    d1 <- ieiwk.dirname.4home(fp1)
    d2 <- ieiwk.dirname.4home(fp2)
    switch(mb, l = {
        .sys.mkdir(d1)
        if (.sys.file.exists(ieiwk.fp.4sys(fp1))) {
            if (!ow) {
                stop("File exists!")
            }
        }
        .sys.do("scp -r -C -p -P", r.info["port"], sprintf("%s@%s:%s", r.info["user"], r.info["dz"], fp2), fp1)
    }, r = {
        .sys.mkdir(d2, dz = r)
        if (.sys.file.exists(ieiwk.fp.4sys(fp2), dz = r)) {
            if (!ow) {
                stop("File exists!")
            }
        }
        .sys.do("scp -r -C -p -P", l.info["port"], fp1, sprintf("%s@%s:%s", r.info["user"], r.info["dz"], fp2))
    })
}
.sys.scp.2newer <- function(fp1, fp2 = fp1, l, r, type.fp = "file", ow = 0) {
    r.info <- ieiwk.ssh.db(r)
    fn <- switch(judge, modify = .sys.stat.modify, size = .sys.stat.size)
    l.fp.modify <- do.call(fn, list(fp = fp1))
    r.fp.modify <- do.call(fn, list(fp = fp2, dz = dz))
    if (l.fp.modify == r.fp.modify) {
        ieiwk.cat.line("Of the same modification time; no need of transfer.")
        return()
    }
    if (l.fp.modify > r.fp.modify) 
        d <- "r"
    if (l.fp.modify < r.fp.modify) 
        d <- "l"
    .sys.scp(fp1, l = l, r = r, mb = d, ow = 1)
}
.sys.scp.2larger <- function(fp1, fp2 = fp1, l, r, type.fp = "file", ow = 0) {
    r.info <- ieiwk.ssh.db(r)
    fn <- .sys.stat.size
    l.fp.stat <- do.call(fn, list(fp = fp1))
    r.fp.stat <- do.call(fn, list(fp = fp2, dz = r))
    if (l.fp.stat == r.fp.stat) {
        ieiwk.cat.line("Of the same state; no need of transfer.")
        return()
    }
    d <- ifelse(l.fp.stat > r.fp.stat, "r", "l")
    .sys.scp(fp1, fp2, l = l, r = r, mb = d, ow = 1)
}
ieiwk.find.file <- function(pat.txt = c(), dir = ieiwk("meta.dir.code"), ext = "[r|R]", pat.fn = sprintf("\\.%s$", ext), arg.ls = "", recursive = 1, ignore.case = 1, fixed = 0) {
    a <- unlist(sapply(dir, ieiwk.list.files, pat.fn, recursive = 1))
    a <- grep(sprintf("\\.%s$", ext), a, val = 1)
    a <- grep(pat.fn, a, val = 1)
    if (!is.null(pat.txt)) {
        sapply(a, function(x) {
            b <- readLines(x)
            line <- grep(pat.txt, b, fixed = fixed)
            if (length(line)) {
                cat(x, "\n", sep = "")
                apply(cbind(line, ": ", b[line], "\n"), 1, cat, sep = "")
            }
        })
        cat(c())
    }
    else a
}
ieiwk.cpDirTree <- function(from, to) {
    from <- "~/apps/src"
    to <- "~/test"
    if (!file.exists(to)) 
        dir.create(to)
    a <- list.dirs(from, recursive = 1)
    b <- sprintf("%s%s", to, substring(a[-1], nchar(a[1]) + 1))
    sapply(b, dir.create)
    {
    }
}
.sys.backup <- function(from, to, way = "1to1", db = "") {
    from <- "/home/wengkai/F_DRIVE/doc.mobile/code"
    to <- "/home/wengkai/Z_DRIVE/wengkai/backup/f_pan"
    if (file.info(from)$isdir) {
        to2 <- file.path(to, basename(from))
        dir.create(to2, recursive = 1)
    }
    switch(way, `1to1` = {
    })
    file.info(to2)
    .sys.do("file", to2)
    .get(to2, , .sys.do, list("ls"), 2)
    fl <- list.files(from, recursive = 1)
    fl.full <- file.path(from, fl)
    fm.full <- file.path(to2, fl)
    file.copy(from, to, recursive = 1, copy.date = 1)
    null <- sapply(seq(length(fl)), function(i) {
        f1 <- fl.full[i]
        f2 <- fm.full[i]
        file.exists(f2)
        file.copy(f1, f2, copy.date = 1, copy.mode = 1, recursive = 1)
    })
}
alarm <- function(time = 25, time2 = time * 60, text = sprintf("%s minutes past", time)) {
    .sys.do("sleep", time2, "; gedit", text)
}
.cut <- function(f1, col = 1, sep = "\t", sep.out = "\t", o, intern = 0, chk = !intern) {
    .wk("cut", "-d", shQuote(sep), "-f", paste(col, collapse = ","), "--output-delimiter", shQuote(sep.out), if (!missing(o)) 
        o, f1, if (chk) 
        "| head")
}
hs.uniq <- function(v1, which = {
}) {
    switch(as.character(length(v1)), `0` = v1, `1` = {
        if (length(which)) {
            1
        } else v1
    }, {
        a1 <- rle(v1)
        a2 <- a1[["lengths"]]
        if (length(which)) {
            unlist(tapply(seq_along(v1), rep(seq_along(a2), a2), switch(which, first = "head", last = "tail"), 1))
        } else {
            a1[["values"]]
        }
    })
}
wk.wj.read.some.columns <- function(wj1, cn1 = {
}, cn2 = {
}, header = 1) {
    cn <- names(fread(cmd = .sys.cat.skip(wj1, N = 1), header = TRUE))
    if (hs.len1(cn1, 0) && hs.len1(cn2, 0)) 
        return(cn)
    ci <- sort(c(if (length(cn1)) {
        match(cn1, cn)
    }, cn2))
    fread(cmd = .sys(.sys.cat.skip(wj1), "| cut", paste0("-f", wk.integerRange.4cut(ci))), header = as.logical(header))
}
.grep <- function(p1, f1, fix = 0, max, line.number = 0, o = "", intern = 0, head = !intern, head.n = 6) {
    .sys.do("grep", if (fix) 
        "-F", if (!missing(max)) 
        c("-m", max), if (line.number) 
        "-n", o, shQuote(p1), .sys.fpGood(f1), if (head) 
        c("|head -n", head.n), intern = intern)
}
.pwd <- function(intern = 1) .wk("pwd", intern = intern)
wk.ls.path <- switch(Sys.info()["sysname"], Linux = "ls", Darwin = "gls")
.ls <- function(fp = ".", p = "AhlF", p1 = "", less = 0) {
    fp <- unlist(fp)
    .wk(wk.ls.path, sprintf("-%s", p), "--color", "--group-directories-first", "--time-style=long-iso", "-I", .q("*[#~]"), "--full-time", "--time-style=long-iso", if (nchar(p1)) 
        sprintf("-%s", p1), .sys.fpGood(fp), if (less) 
        "| less -N")
}
.ls1 <- function(wj = ".", p = "Ahlpg", less = 0) {
    .ls(dirname(wj[1]))
}
.cp <- function(f1, f2, ...) .wk("cp", hs.fpGood(f1), hs.fpGood(f2), ...)
.wc <- function(f, p = "l", intern = 1, z = grepl("\\.gz$", f)) {
    R <- .wk(.sys.cat.text(f), hs.paste("| wc -", p), intern = intern)
    if (intern) 
        as.numeric(R)
    else R
}
.cd <- function(d = "~") {
    if (!file.info(d)[["isdir"]]) 
        d <- dirname(d)
    setwd(d)
}
.less <- function(f, r = 1, skip = 0) {
    less <- osType() %=>% hs.switch("Darwin", "/usr/local/bin/less", "less")
    .wk(.sys.cat.skip(f, skip = 0), if (skip) 
        c("| perl -e", .q(paste0("foreach ( 1..", skip, ") { <>}; while( <>) { print}"))), "|", less, " -NfiF", if (r) 
        "-R")
}
.head <- function(wj1, n = 10, intern = 0, z = hs.file.extension(wj1) %in% c("gz")) {
    .wk("cat", .sys.fpGood(wj1), if (z) 
        c(.pipe(), "gzip -cd"), .pipe(), "head -n", format(n, scientific = FALSE), intern = intern)
}
.cat <- function(wj, gz = hs.grepl(wj, "\\.gz$")) {
    .wk("cat", wj, if (gz) 
        "| gzip -cd")
}
.pipe <- function(no.err = 1) {
    c(if (no.err) "2>/dev/null", "|")
}
.sys.pipe <- function(x, ..., no.err = 0) {
    .sys(x, .pipe(no.err), ...)
}
.perl <- function(..., perl = "perl", w = 1) {
    .sys(perl, if (w) 
        "-w", ...)
}
.perl.e <- function(..., l = 1, a = 0, n = 1, p = 0, F = "\\t", c = 0, args = {
}, intern = 0, sys = 0, pipe = 0, begin = {
}) {
    cmd <- .sys("perl", if (c) 
        "-c", if (n) 
        "-n", if (p) 
        "-p", if (l) 
        "-l", if (a) 
        "-a", if (length(F)) 
        paste0("-F", .q(F)), paste0("-e ", c(if (length(begin)) .q(paste0(c("BEGIN{", paste0(begin, ";"), "}"), collapse = "")), .q(paste0(c(...), ";")))), if (length(args)) 
        args)
    if (pipe) {
        pipe(cmd, "rt")
    }
    else if (sys) {
        cmd
    }
    else {
        .wk(cmd, intern = intern)
    }
}
wj_row.hs.v_wj__perl <- function(v1, wj1, wj1.cn2use, mn = -1, ot = "dt", cn = mn != 1) {
    wj_temp <- hs.wj.temp()
    on.exit(hs.wj.rm(wj_temp), add = TRUE)
    cat(v1, sep = "\n", file = wj_temp)
    pattern1 <- paste0("^", paste(rep("[^\\t]*\\t", wj1.cn2use - 1), collapse = ""), "([^\\t\\n]*)")
    cmd1 <- .sys("perl -e", .q(hs.pastec0("\nopen( in1, \"cat ", wj_temp, " |\");\nwhile( <in1>) {\n  chomp;\n  $dict1{ $_} = 1;\n}\nclose( in1);\nopen( in1, \"", .sys.cat.text(wj1), " |\");\nwhile( <in1>) {\n  /", pattern1, "/;\n  if( exists( $dict1{ $1})) {\n    print;\n", if (mn > 0) {
        c("\n    $found = $found + 1;\n    if( $found == ", mn, ") {\n      exit;\n    }\n")
    }, " }\n}\nclose( in1);\n")))
    jg <- .wk(cmd1, intern = 1)
    switch(ot, row = jg, dt = {
        jg <- fread(text = jg)
        setnames(jg, cn.hs.wj(wj1))
    })
}
.chk <- function(wj1, n = 1) .wk("head -n", format(n, scientific = FALSE), .sys.fpGood(wj1))
.tail <- function(f, n = 10) {
    .wk("cat_wk", hs.fpGood(f), "| perl -e", .q(paste0("\n$n = ", n, ";\n$i = 0;\nwhile( <>) {\n  $i = $. % 10;\n  $a[ $i] = $_;\n}\nif( $i < $n) {\n  foreach $j ( ( $i + 1) .. ( $n - 1)) {\n    print( $a[ $j]);\n  }\n}\nforeach $j ( 0 .. $i) {\n  print( $a[ $j]);\n}\n")))
}
.man <- function(x) .wk("man", x)
.top <- function() .wk("top")
.htop <- function(u = .whoami()) .wk("htop -u", u)
.df <- function() .wk("df -h")
.rm <- function(f) .wk("rm", .sys.fpGood(f))
.hostname <- function() .wk("hostname", intern = 1)
.sleep <- function(t = 3) .wk("sleep", t)
.uuidgen <- function(append) paste(c(.sys.do("uuidgen", intern = 1), if (!missing(append)) append), collapse = "_")
.whoami <- function() Sys.getenv("USER")
.Rscript <- function(..., ssh, nohup = FALSE, slave = TRUE, quiet = TRUE, no.save = TRUE) {
    dian <- c(...)
    .sys.do(if (!missing(ssh)) 
        c("ssh", ssh), if (nohup) 
        "nohup", "Rscript", if (quiet) 
        "--quiet", if (no.save) 
        "--no-save", dian, if (nohup && !length(grep("&", dian))) 
        "&")
    .sys(if (!missing(ssh) && ssh != .hostname()) 
        c("ssh", ssh), if (nohup) 
        "nohup", "Rscript", if (quiet) 
        "--quiet", if (no.save) 
        "--no-save", dian, if (nohup && !length(grep("&", dian))) 
        "&")
}
wk.zip2gz <- function(wj1) {
    wj2 <- hs.wjm(wj1, ext = "gz")
    if (!file.exists(wj2)) 
        .wk("unzip", "-p", wj1, "|", "gzip", "-c", ">", wj2)
}
hs.wj.rm <- hs.rm.wj <- wk.rm.wj <- function(wj, recursive = 0) {
    unlink(wj, recursive)
}
ieiwk.dirname.1 <- function(f1) {
    repeat {
        d1 <- dirname(f1)
        if (d1 == ".") 
            return(basename(f1))
        f1 <- d1
    }
}
ieiwk.split.file <- function(file, parts = 5, size, mb = dirname(file)) {
    c1 <- file(file, "rb")
    if (missing(size)) {
        s0 <- file.info(file)[["size"]]
        size <- ceiling(s0/parts)
    }
    part <- 1
    while (length(chunk <- readBin(c1, "raw", size))) {
        f.out <- file(ieiwk.filename(file, add = sprintf("part%s", part), dn = mb), "")
        `?`(file)
        writeBin(chunk, f.out)
        part <- part + 1
    }
    close(c1)
    part - 1
}
ieiwk.comine.files <- function(file1) {
    files <- ieiwk.list.files(dirname(file1), sub("\\d+$", "", basename(file1)))
    f.out <- ieiwk.filename(file1)
    r <- unlist(lapply(files, function(file) {
        c1 <- file(file, "rb")
        chunk <- readBin(c1, "raw", file.info(files)[["size"]])
        close(c1)
        chunk
    }), use.names = FALSE)
    writeBin(r, f.out)
    f.out
}
.sys.ssh.file.path <- function(..., fsep = .Platform$file.sep, recursive = TRUE, type = "file", sys = FALSE, expand = 0, normalize = 1, v.only = 0) {
    a <- file.path(...)
    if (v.only) 
        return(a)
    switch(type, file = {
        a.p <- dirname(a)
        if (!file.exists(a.p)) dir.create(a.p, recursive = TRUE, showWarnings = FALSE)
    }, dir = {
        a.len <- nchar(a)
        if (substr(a, a.len, a.len) != "/") a <- sprintf("%s/", a)
        if (!file.exists(a)) dir.create(a, recursive = recursive)
    })
    r <- {
        if (sys) {
            r <- gsub("\\\\ ", " ", a)
            gsub(" ", "\\\\ ", a)
        }
        else {
            a
        }
    }
    if (expand) 
        r <- path.expand(r)
    if (normalize) 
        r <- ieiwk.fp.norm(r)
    r
}
.sys.file.exists <- function(fp, dz = {
}) {
    ifelse(length(grep("No such file or directory)$", .sys.do("file", .q(ieiwk.fp.4sys(fp)), dz = dz, intern = 1))), FALSE, TRUE)
}
.sys.ssh <- function(..., dz = "sg", p = 22, sep = " ", wait = 1, intern = 0, chk = 0) {
    dz.db <- c(sg = "172.18.202.8", hp = "172.18.202.7", g1 = "gate1.picb.ac.cn", g2 = "gate2.picb.ac.cn")
    p.db <- c(sg = 53641, hp = 53641, g1 = 22, g2 = 22)
    if (dz %in% names(dz.db)) {
        p <- p.db[dz]
        dz <- dz.db[dz]
    }
    cmd <- .sys(..., intern = 1)
    if (chk) {
        ieiwk.cat.line(cmd)
    }
    else {
        .sys("ssh -p", p, dz, shQuote(cmd), sep = sep, wait = wait, intern = intern, chk = 0)
    }
}
.sys.ssh.stat <- function(fp, dz, p = 22) {
    .sys.do("ssh -p", p, dz, shQuote(.sys("stat", fp, intern = 1)), intern = 1)
}
.sys.ssh.stat.modify <- function(fp, dz, p = 22) {
    a <- .sys.ssh.stat(fp, dz, p)
    ifelse(length(a), sapply(strsplit(grep("Modify:", a, v = 1), "[ ]+")[[1]][2:3], function(x) head(strsplit(x, "[-\\:\\.]")[[1]], 3)) %,% "", 0)
}
.sys.stat <- function(fp, dz = {
}) {
    .sys.do("stat", fp, intern = 1, dz = dz)
}
.sys.stat.modify <- function(fp, dz = {
}) {
    a <- .sys.stat(fp, dz = dz)
    ifelse(length(a), sapply(strsplit(grep("Modify:", a, v = 1), "[ ]+")[[1]][2:3], function(x) head(strsplit(x, "[-\\:\\.]")[[1]], 3)) %,% "", 0)
}
.sys.stat.size <- function(fp, dz = {
}) {
    a <- .sys.stat(fp, dz = dz)
    ifelse(length(a), as.numeric(re("\\d+", grep("Size:", a, v = 1))), 0)
}
.sys.mkdir <- function(d, dz = {
}) {
    .sys.do("mkdir -p", d, dz = dz)
}
.wget <- function(dz, P = ".", T = 1, t = 9999, nc = 0, d = 0) {
    wjj0 <- getwd()
    on.exit(setwd(wjj0), add = TRUE)
    setwd(P)
    repeat {
        a1 <- .wk("wget -c", if (d) 
            "-d", "-t", t, "-T", T, dz, "2>&1", intern = 1)
        if (length(hs.grep(a1, "\\s+The file is already fully retrieved; nothing to do."))) 
            break
    }
    a <- .wk("wget -c", if (d) 
        "-d", "-nc", "-t", t, "-T", T, dz, "2>&1", intern = 1)
    a <- hs.grep(a, "^File .(.*). already there; not retrieving\\.$", v = 1)
    normalizePath(hs.re(a, "(?<=^File .).*(?=. already there; not retrieving\\.$)"))
}
ieiwk.cp.dir_str <- function(d1, d2) {
    fp.v <- ieiwk.list.files(d1, only.dir = 1, recursive = 1)
    sapply(file.path(d2, gsub(path.expand(d1), "", fp.v)), dir.create, recursive = 1)
}
ieiwk.fp.norm <- function(fp) {
    for (i in seq_along(fp)) {
        a <- fp[i]
        if (!file.exists(a)) 
            next
        if (file.info(a)$isdir && substring(a, nchar(a)) != .Platform$file.sep) {
            fp[i] <- file.path(a, "")
        }
    }
    fp
}
ieiwk.dirname.4home <- function(fp) {
    a <- head(strsplit(fp, .Platform$file.sep)[[1]], -1) %,% .Platform$file.sep
    sub("^~", "$HOME", a)
}
ieiwk.fp.4sys <- function(fp) {
    if (length(grep("/~", fp))) 
        stop(sprintf("Please check the file path: %s, in which ~ is preceeded by /.", fp))
    sub("~", "$HOME", fp)
}
ieiwk.fp.4r <- function(fp, dz = {
}) {
    sub(.sys.do("echo ~", dz = dz, intern = 1), "~", fp)
}
assign.zjjg <- function(..., extN, env = parent.frame()) {
    vn <- paste(substitute(c(...))[-1])
    for (v1 in paste(substitute(c(...))[-1])) {
        assign(paste(v1, "zjjg", sep = "."), ieiwk.filename(get(v1, env = env), zjjg = TRUE), envir = env)
    }
}
ieiwk.filename.append <- function(f1, append = "", dn = TRUE) {
    paste(ieiwk.filename(f1, dn = dn), append, ".", ieiwk.file.extension(f1), sep = "")
}
ieiwk.file.extension.change <- function(f, alt, bn = 0) {
    if (bn) 
        f <- basename(f)
    sub("([^.]+)$", alt, f)
}
ieiwk.file.size <- function(f1, co = 1024^2, bj = 0) {
    s1 <- file.info(f1)[["size"]]
    if (bj) {
        s1 >= co
    }
    else s1
}
.sys.file.fifo <- function(f1, act = c("read", "write"), gzip = "gzip", compression.level = 6, named.file, append = 0) {
    act <- match.arg(act)
    f2 <- sprintf("%s %s )", switch(act, read = paste("<(", sapply(ieiwk.file.extension(f1), switch, gz = "pigz -dc", "cat")), write = paste(">(", if (length(gzip)) sprintf("%s -c -%d >%s", gzip, compression.level, ifelse(append, ">", "")))), f1)
    if (missing(named.file)) {
        if (length(f1) > 1) 
            c("<( cat", f2, ")")
        else f2
    }
    else {
        if (is.logical(named.file) && named.file) {
            named.file <- hs.wjm(f1[1], append = .uuidgen("fifo"))
        }
        .wk("mkfifo", named.file)
        switch(act, read = .wk("cat", f2, ">", named.file, "&"), write = .wk("cat", named.file, ">", f2, "&"))
        named.file
    }
}
.sys.grep <- local({
    bin <- switch(osType(), Darwin = "ggrep", "grep")
    function(p1, inv = 0, perl = 1, m = {
    }, n = {
    }, ic = 1, start = 0, end = 0) {
        if (start) 
            p1 <- paste0("^", p1)
        if (end) 
            p1 <- paste0(p1, "$")
        .sys("LC_ALL=C", bin, if (ic) 
            "-i", if (inv) 
            "-v", if (perl) 
            "-P", if (length(m)) 
            c("-m", m), if (length(n)) 
            "-n", .q(p1))
    }
})
.sys.cat.skip <- function(wj1, comment = "#", cn = 1, header = cn, only = {
}, inv = 0, pipe = {
}, N = 0, skip = 1, plus = 0) {
    wj2 <- tolower(hs.re(wj1, "[^\\.]*$")) %=>% {
        if (x %in% c("7z")) 
            hs.browser("2023.07.04.162016", debug = 0)
        db <- environment(.sys.cat.text)[["ext.cat"]]
        if (x %in% names(db)) 
            db[x]
        else "cat"
    } %=>% .sys(.sys.fpGood(wj1))
    if (skip) {
        i1 <- hs.lines_comment(wj1, opt = "i", comment = comment) + 1
        i1 <- i1 + plus
        if (length(only)) 
            return(switch(only, i = i1, "pipe"))
        if (inv) {
            .sys(wj2, "|", .sys.head(i1 - 1, pipe = 0), if (N) 
                c("| head -n", N), pipe = pipe)
        }
        else {
            i2 <- i1 + ifelse(header, -1, 0)
            .sys(wj2, if (i2 > 1) 
                .sys("| perl -e", .q(paste0("$n = ", i2, "; while( $n) { <>; $n--;}; while( <>) { print;}"))), if (N) 
                c("| head -n", N), pipe = pipe)
        }
    }
    else wj2
}
.sys.uniq <- function(sort = 1) {
    c("| uniq", if (sort) "| sort | uniq")
}
.sys.wget <- function(wj1, q = 1, bg = 1, v = !q, c = 1, root = getwd(), o = hs.wjm(wj1, add = "wget.log", dn = root)) {
    c("wget", if (c) "-c", if (q) "-q", if (v) "-v", str_c("--directory-prefix=", wk.wjlj(root)), wj1, if (length(o)) c("-a", o), if (bg) "&")
}
.tabix <- function(w1, r1 = {
}, tabix = hs.home.expand("~/rj/tabix-0.2.6/tabix"), p = "vcf", cn = 1, start = 2, end = 3, name = 1) {
    if (file.size(w1) < 10000) 
        stop("【34.01.19.193142】file size is too small")
    .tabix.index(w1, tabix, start = start, end = end, name = name)
    if (length(r1)) {
        wj.tmp <- tempfile(tmpdir = "/dev/shm")
        on.exit(hs.rm.wj(wj.tmp), add = TRUE)
        if (hs.len1(dim(r1), 0)) {
            r2 <- do.call("rbind", strsplit(r1, "[:-]"))
            r1 <- as.data.table(r2)
        }
        if ((hs.len1(dim(r1), 2)) && (ncol(r1) >= 3)) {
            if (!is.data.table(r1)) 
                r1 <- as.data.table(r1)[, .SD, .SDcols = 1:3]
            if (length(r1) > 3) 
                r1[, `:=`(c(tail(names(r1), -3)), {
                })]
            r1[[2]] <- as.numeric(r1[[2]]) - 1
            wk.fwrite(r1, wj.tmp, append = 0, col.names = 0)
            r1 <- wj.tmp
        }
        R <- wk.fread(cmd = .sys(tabix, "-B", w1, r1), cn = 0)
        if (nrow(R) && cn) 
            setnames(R, names(wk.fread(w1, N = 1)))
        R
    }
}
.tabix.sort <- function(w1, name = 1, start = 2, end = 3) {
    w1_tmp <- hs.wjm(w1, append = "33.12.29.180245")
    comments.i <- hs.lines_comment(w1, opt = "i")
    cmd.cat.sort <- if (comments.i) {
        .sys("(", "zcat", w1, "|", "head -n", comments.i, ";", "zcat", w1, "|", "tail", "-n", paste0("+", comments.i + 1), "|", "sort", paste0("-k", name, ",", name), paste0("-k", start, ",", start, "n"), paste0("-k", end, ",", end, "n"), ")")
    }
    else .sys("zcat", w1, "|", "sort", paste0("-k", name, ",", name), paste0("-k", start, ",", start, "n"), paste0("-k", end, ",", end, "n"), ")")
    .wk(cmd.cat.sort, "|", "pbgzip", "-c", ">", w1_tmp)
    if (0) {
        if (diff(file.size(c(w1, w1_tmp))) < 0) 
            warning("size check failed; check here: 34.01.19.164703")
    }
    local({
        a <- file.size(c(w1_tmp, w1))
        a <- (a[1]/a[2]) < 0.9
        if (a) 
            stop("new file's size too small; check 34.01.30.201935")
    })
    file.rename(w1_tmp, w1)
    w1
}
.tabix.index <- function(w1, tabix = hs.home.expand("~/rj/tabix-0.2.6/tabix"), co.ratio = 0.01, start = 2, end = 3, name = 1) {
    flag <- 0
    flag.sort <- 0
    wj_tbi <- hs.wjm(w1, add = "tbi")
    if (file.exists(wj_tbi)) {
        fi <- file.info(c(wj_tbi, w1))
        if (fi[, "mtime"] %=>% diff %=>% {
            x >= 1
        }) {
            flag <- 1
        }
        if (0) {
            if (fi[, "size"] %=>% {
                x[1]/x[2] < co.ratio
            }) 
                flag.sort <- 1
        }
        if ((fi[, "size"] %=>% {
            x[1]/x[2]
        } < co.ratio) && (fi[1, "size"] < 1000)) {
            flag.sort <- 1
        }
    }
    else {
        flag.sort <- 1
    }
    if (flag.sort) {
        .tabix.sort(w1, start = start, end = end, name = name)
        flag <- 1
    }
    a <- 0
    if (flag) {
        a <- .wk(tabix, "-s", name, "-b", start, "-e", end, "-f", .q(normalizePath(w1)))
        if (a) {
            if (flag.sort) {
                hs.browser("34.07.04.112349")
            }
            else {
                .tabix.sort(w1)
                .tabix.index(w1, p, tabix, co.ratio)
            }
        }
    }
}
.sys.tabix.index <- function(wj, bin = hs.home.expand("~/rj/tabix-0.2.6/tabix"), s = 1, b = 2, e = 3, S = 0, sort = 1) {
    wj <- wj2
    if (sort) {
        if (str_sub(wj, -3, -1) == ".gz") {
            wjj1 <- hs.wjj(tempfile(tmpdir = "/dev/shm"))
            on.exit(unlink(wjj1, recursive = TRUE), add = TRUE)
        }
        else {
            wjj1 <- dirname(wj)
        }
        args <- list(wj, dn = wjj1)
        if (str_sub(wj, -3, -1) != "gz") 
            args[["add"]] <- "gz"
        wj2 <- hs.wjm %(% args
        .sys.do("sort", str_interp("-k$${ s},${ s}"), str_interp("-k${ b},${ b}n"), .sys.file.fifo(wj2), "|", "pbgzip", ">", wj2)
        .less(.sys.file.fifo(wj2))
    }
    else {
        if (str_sub(wj, -3, -1) != ".gz") {
            .sys.do("pbgzip", wj)
            wj <- hs.wjm(wj, add = "gz")
        }
    }
    for (o1 in unlist(str_split("s,b,e,S", ","))) {
        eval(bquote({
            .(as.symbol(o1)) <- sprintf("-%s%s", .(o1), formatSafe(.(as.symbol(o1))))
        }))
    }
    .sys.do(bin, s, b, e, S, wj)
}
.sys.sort <- function(k = {
}) {
    .sys("sort", if (length(k)) 
        str_c("-k", k))
}
.sys.find <- function(wjj = ".", iregex = "", regex = "", name = "", path = "", iname = "", ipath = "", wholename = "", iwholename = "", maxdepth = 99, regextype = "posix-extended", ...) {
    L <- list(...)
    e1 <- unlist(sapply(unlist(strsplit("iregex, regex, name, path, iname, ipath, wholename, iwholename", ",\\s+")), function(p1) {
        v1 <- get(p1)
        inv <- L[[paste0(p1, ".inv")]]
        if (!length(inv)) 
            inv <- rep(0, )
        if (nzchar(v1[1])) 
            sprintf("%s -%s %s", ifelse(inv == 1, "!", ""), p1, .q(get(p1)))
    }))
    .sys("find", wjj, "-maxdepth", maxdepth, "-regextype", regextype, e1)
}
.sys.gawk <- function(col2print = 0, sep.in = "\t", ofs = "\t") {
    .sys("gawk", "-F", .q(sep.in), "-v", sprintf("OFS=\"%s\"", ofs))
}
.sys.gawk.print <- function(col2print = 0, sep.in = "\t", sep.out = sep.in) {
    cmd <- paste("BEGIN{ OFS = \"\\t\"} { print(", paste(paste("$", col2print, sep = ""), collapse = ","), ")}", sep = "")
    .sys("gawk", "-F", .q(sep.in), .q(cmd))
}
.sys.gawk.selectLines <- function(i) {
    i <- wk.integerRange.4cut(i)
    .sys(.sys.gawk(), .q(str_c("BEGIN{ split( ", .q(i, type = "cmd"), ", i, \",\"); for( ii = 1; ii <= length( i); ii++) { split( i[ ii], i1, \"-\"); if( ii == 1) { i2 = 1} else { split( i1[ ii - 1], i0, \"-\"); i2 = i0[ length( i0)] + 1}; for( i_skip = i2; i_skip < i1[ 1]; i_skip++) getline; for( i_keep = i1[ 1]; i_keep <= i1[ length( i1)]; i_keep++) { getline a; print a}}}")))
}
.sys.cut <- function(col2print = "1-", sep.out = "\t", d = {
}, m = "f") {
    .sys("cut", sprintf("-%s", m), col2print, sprintf("--output-delimiter=\"%s\"", sep.out), if (length(d)) 
        c("-d", d))
}
.sys.less <- function(N = 1) .sys("less", if (N) "-N")
.sys.head <- function(n = 10, pipe = 0) {
    if (length(n)) 
        .sys("head", "-n", n, if (pipe) 
            "|")
    else c("", if (pipe) "|")
}
.sys.tail <- function(n = 10, from.beginning = 0) {
    n <- formatSafe(n)
    .sys("tail", "-n", if (from.beginning) 
        sprintf("+%s", n)
    else n)
}
.sys.pigz.c <- function(cl = 6, tn = 8, pipe = 0) {
    r <- sprintf("pigz -p%s -%s", tn, cl)
    if (pipe) 
        r <- c("|", r)
    r
}
.sys.tr <- function(set2 = ";", set1 = "\t") .sys("tr", .q(set1), .q(set2))
.sys.parallel <- function() hs.home.expand("~/rj/speedseq_2016.05.15/bin/parallel")
.sys.gzip <- function(args, l = 6) {
    .sys("gzip", args, if (length(l)) 
        str_c("-", l))
}
.sys.gzip.out <- function(...) .sys.gzip("-cd", ..., l = {
})
.sys.gzip.in <- function(...) .sys.gzip("-c", ...)
.sys.pigz <- function(act = "cd", N = n, n = {
}) {
    .sys("pigz", if (nchar(act)) 
        sprintf("-%s", act), if (length(N)) 
        c("-p", as.integer(N)))
}
.sys.pigz.out <- function(N = n, n = {
}) {
    .sys.pigz(N = N)
}
.sys.pigz.in <- function(N = n, n = {
}) {
    .sys.pigz("c", N = N)
}
.sys.pbgzip <- function(N = n, n = {
}, c = 1, d = 0) {
    .sys("pbgzip", if (length(N)) 
        c("-n", N), if (c) 
        "-c", if (d) 
        "-d", "2>&1")
}
.sys.pbgzip.out <- function(...) .sys.pbgzip(..., d = 1)
.sys.pbgzip.in <- function(...) .sys.pbgzip(..., d = 0)
.sys.genomes.human <- function(which = 1) c(hs.home.expand("~/swxx/sj/jyz/human/1kg/37/human_g1k_v37.fasta"), .sys.speedseq("wj.ref"))[which]
.sys.java <- function(mem.max = 750, jar = {
}) {
    .sys("java", str_interp("-Xmx${ formatSafe( mem.max)}m"), if (length(jar)) 
        c("-jar", jar))
}
ieiwk.copy.wjj.jiegou <- function(wjj1, wjj2 = dirname(wjj1[1]), qianzhui = "speedseq") {
    ieiwk.file.path(wjj2, qianzhui, type = "wjj")
}
uid_fp_source <- function(uid1 = c(), fp1 = c(), less = 0, ...) {
    if (length(uid1)) {
        uid1 <- hs.re(uid1, "\\d+[_\\.]\\d{2}[\\._]\\d{2}[\\._]\\d{6}")
        fp1 <- .wk("ag -g", uid1, hs.home.expand("~/org"), intern = 1)
    }
    if (!((hs.len1(fp1)) && nzchar(fp1))) 
        hs.browser("37.05.07.204631")
    if (less) {
        .less(fp1)
        return()
    }
    source(fp1, ...)[["value"]]
}
hs.hs <- function(hs1, env1 = new.env(parent = environment(hs1)), onExit = {
}) {
    hs.browser("34.07.05.104924")
    hs2 <- eval(bquote(local({
        env1.33.06.24.181214 <- environment()
        env2.33.06.24.181217 <- new.env()
        .(hs1)
    })))
    a <- hs.localize(hs1)
    ls(environment(a))
    ls(environment(hs1))
    typeof(hs1)
    class(function(x) {
    })
    ls(environment(hs2))
    ls(env1)
    env1 <- environment(hs1)
    apropos("parent")
    parent.env(env1)
    self <- sys.function()
    hs1 <- function() {
    }
    AN <- 0
    list(...)
    formalArgs()
    args <- head(formalArgs(hs.parent), -3)
}
wk.get <- ieiwk.get <- local.get <- function(...) get(..., inherits = FALSE)
.getl <- .geta <- function(uid, env1 = .GlobalEnv) {
    o1 <- ls(pattern = uid, all.names = TRUE, envir = env1)
    if (length(o1)) 
        eval(bquote(get(.(o1))), env1)
    else stop("No such object.")
}
hs.dmgly <- ieiwk.dmgly <- ieiwk.sinkORget.code <- function(wjm, bds, local = 0, ext = "R", wjj0 = ieiwk("wjj.dm"), env1 = .GlobalEnv, PO = 0, path.only = PO, blm.only = 0, load = 0, reload = 0, call = 0, argList = list(), rename = 0, bl = 1, parse.network = 0, less = 0, trim = 0) {
    {
        wj <- .mfp(wjm, wjj0, act = "find", uid.pos = "tail")
        wj2 <- hs.wjm(.mfp(wjm, wjj0, uid.pos = "tail"), add = ext)
        if (length(wj)) {
            if (wj != wj2) 
                if (!missing(bds)) {
                  file.rename(wj, wj2)
                  wj <- wj2
                }
        }
        else wj <- wj2
    }
    if (path.only) 
        return(wj)
    if ({
        !file.exists(wj)
    } && missing(bds)) 
        stop(paste("####", wj, "is not found. 34.07.04.195822 ####"))
    if (less) 
        return(.less(wj))
    if (!missing(bds)) {
        bds <- substitute(bds)
        bds.nr <- format(bds)
        if (trim) 
            bds.nr <- trim(bds.nr)
        cat2wj <- 1
        if (file.exists(wj)) {
            wj.nr <- readLines(wj, encoding = "UTF-8")
            if ({
                length(wj.nr) == length(bds.nr)
            } && identical(bds.nr, wj.nr)) 
                cat2wj <- 0
        }
        if (cat2wj) 
            cat(bds.nr, file = wj, sep = "\n")
    }
    {
        uid <- hs.file.extension(wjm, extN = 4)
        wj.blm <- hs.wjm(wj, dn = 0, ext = 0)
        if (wj.blm == uid) 
            stop(paste("####", wjm, "is not meaningful, 34.07.04.195811@function.common.R ####"))
        blm <- ls(env1, pattern = uid, all.names = TRUE)
        if (!length(blm)) 
            blm <- wj.blm
    }
    if (blm.only) 
        return(blm)
    {
        env.lt <- "env.loadtime.33.04.27.080101"
        if (!exists(env.lt, env1, inherits = FALSE)) 
            env1[[env.lt]] <- new.env(parent = env1)
        blm.loadtime <- env1[[env.lt]][[blm]]
    }
    if (missing(bds)) {
        if (exists(blm, env1, inherits = FALSE)) {
            if ((!length(blm.loadtime)) || (difftime(file.info(wj)[, "mtime"], blm.loadtime, units = "mins") > 1)) {
                load <- 1
            }
            else return(env1[[blm]])
        }
        else load <- 1
    }
    if (load || reload) {
        file.exists(wj) || stop("no file found # 34.07.13.054404@function.common.R")
        bds <- parse(wj, encoding = "UTF-8", keep.source = FALSE)
    }
    if (length(bds)) {
        if (local) 
            bds <- bquote(local(.(bds)))
        R <- eval(bds, env1)
        if (bl) {
            env1[[env.lt]][[blm]] <- Sys.time()
            env1[[blm]] <- R
        }
        else R
    }
}
a.33.03.17.191105 <- function(wj1) {
    a <- ieiwk.readLines(wj1, n = -1)
    .ss(sort(grep("ieiwk.dmgly\\(\\s*'.*\\.\\d+.\\d{2}.\\d{2}.\\d{6}',", a, v = 1)))
}
.getls <- function(uid, env1 = .GlobalEnv, update = 0) {
    uid1 <- ieiwk.file.extension(uid, extN = 4)
    if (update) 
        ieiwk.sinkORget.code(uid, reload = 1)
    o1 <- ls(pattern = uid1, all.names = TRUE, envir = env1)
    if (!length(o1)) {
        eval(bquote(ieiwk.dmgly(.(uid), bl = 1)), env1)
        o1 <- ls(pattern = uid1, all.names = TRUE, envir = env1)
    }
    eval(bquote(get(.(o1))), env1)
}
uid.filePath.code <- function(uid) {
    ieiwk.sinkORget.code(uid, path.only = 1)
}
ieiwk.sjgly <- function(wjm, bds, ext = "RData", wjj0 = ieiwk("wjj.sj"), env1 = .GlobalEnv, path.only = 0, blm.only = 0, load = 0, reload = 1, call = 0, argList = list(), rename = 0, bl = 0, parse.network = 0) {
    wj <- hs.wjm(.mfp(wjm, wjj0, uid.pos = "tail"), add = ext)
    if (path.only) 
        return(wj)
    if ({
        !file.exists(wj)
    } && missing(bds)) 
        stop(paste("####", wj, "is not found. ####"))
    blm <- wjm
    uid <- ieiwk.file.extension(wjm, extN = 4)
    if (blm == uid) 
        stop(paste("####", wjm, "is not meaningful ####"))
    if (blm.only) 
        return(blm)
    if (missing(bds)) {
        if (load || reload) {
            e1 <- bquote(hs.load(.(wj)))
            if (bl) {
                eval(bquote({
                  .(as.symbol(blm)) <- .(e1)
                }), env1)
            }
            else {
                eval(e1)
            }
        }
    }
    else {
        if (file.exists(wj) && reload) 
            ieiwk.sjgly(wjm)
        else {
            e1 <- bquote({
                local({
                  eval(.(bds))
                })
            })
            if (bl) {
                eval(bquote({
                  .(as.symbol(blm)) <- .(e1)
                }), env1)
            }
            blm
        }
    }
}
hs.xmgly <- function(wjm, wjj0 = hs.home.expand("~/wk/gz/db"), go = 1, gj = 1) {
    uid <- ieiwk.file.extension(wjm, extN = 4)
    wjj <- .mfp(uid, wjj0, uid.pos = "tail", fp.type = "dir")
    if (gj) 
        sapply(c("sj", "jg"), function(x) hs.wjj(wjj, x))
    if (go) 
        setwd(wjj)
    wjj
}
ieiwk.xmbm <- function(uid.xm, ms1, ms2) {
    if (missing(ms2)) 
        ms2 <- ieiwk.file.extension(uid.xm, extN = 4)
    xm.wjj <- ieiwk.xmgly(uid.xm)
    hs.wjj(xm.wjj, "sj", go = 1)
    wj1 <- ieiwk.list.files(, recursive = 1, full.names = 0)
    wj2 <- gsub(ms1, ms2, wj1)
    for (i1 in seq_along(wj1)) {
        if (wj1[i1] != wj2[i1]) {
            ieiwk.file.path(wj2[i1])
            file.rename(wj1[i1], wj2[i1])
        }
    }
    getwd()
    for (wjj1 in ieiwk.list.dirs()) {
        if (!length(ieiwk.list.files(wjj1, recursive = 1))) 
            unlink(wjj1, recursive = TRUE)
    }
}
ieiwk.xmbmhy <- function(wjm, ms21) {
    ms2 <- names(ms21)[2]
    for (ms2 in names(ms21)) {
        wj2 <- grep(ms2, wjm, v = 1)
        if (!length(wj2)) 
            next
        ms1 <- ms21[[ms2]]
        wj1 <- sub(ms2, ms1, wj2, fixed = TRUE)
        file.rename(wj2, wj1)
    }
}
ieiwk.function <- function(..., body) {
    f1 <- function(...) {
    }
    cmpfun(f1)
}
evalbq <- function(e1, env1 = parent.frame(), env2 = env1) {
    eval(substitute(eval(bquote(e1))), envir = env1)
}
evalsub <- function(exp1, map1, env1 = parent.frame()) {
    e <- evalbq(substitute(.(substitute(exp1)), .(substitute(map1))))
    eval(e, env1)
}
ieiwk.eval.save.load <- function(e1, f1, rerun = 0, lib = {
}, c1 = Sys.info()["nodename"]) {
    if (file.exists(f1) && !rerun) {
        hs.load(f1)
    }
    else {
        r <- ieiwk.eval(e1, lib = lib, c1 = c1)
        save(r, file = f1)
        r
    }
}
input.is.incomplete <- function(r) {
    (class(r) == "try-error") && (!is.null(attr(r, "condition"))) && (re("(?<=: )[^:]*$", readLines(textConnection(attr(r, "condition")[["message"]]), 1)) %in% c("unexpected end of input", "unexpected INCOMPLETE_STRING", "unexpected end of line", "意外的INCOMPLETE_STRING"))
}
wk.e1 <- function(e1, env1 = parent.frame(), td.co = 10, bg = 0) {
    if (missing(e1)) 
        e2 <- quote({
        })
    e2 <- as.call(c(as.symbol("{"), substitute(e1)))
    e3 <- bquote({
        wk.cat(Sys.info()["user"], "@", Sys.info()["nodename"], getwd())
        t1 <- Sys.time()
        r <- eval(.(e2), .(if (!bg) 
            env1))
        t2 <- Sys.time()
        td <- difftime(t2, t1, units = "secs")
        msg1 <- capture.output(as.difftime(td))
        .ss(msg1)
        if (td > .(td.co)) {
            if (wk.is.a.local.machine()) {
                .wk("kate", .q(msg1), "> /dev/null", "2> /dev/null", "&")
            }
            else {
                .r(msg1, "")
            }
        }
        .cat.time()
        r
    })
    wk.eval(e3, bg = bg, email = 0, report.if.done.cutoff.time = 99999)
}
wk. <- wk.time <- function(expr, gcFirst = 0, td.co = 10) {
    ppt <- function(y) {
        if (!is.na(y[4L])) 
            y[1L] <- y[1L] + y[4L]
        if (!is.na(y[5L])) 
            y[2L] <- y[2L] + y[5L]
        y[1L:3L]
    }
    if (!exists("proc.time")) 
        return(rep(NA_real_, 5L))
    if (gcFirst) 
        gc(FALSE)
    time <- proc.time()
    on.exit(cat("Timing stopped at:", ppt(proc.time() - time), "\n"))
    expr
    new.time <- proc.time()
    on.exit()
    .ss(paste(Sys.info()["user"], "@", Sys.info()["nodename"], getwd(), "\n", format(Sys.time(), "%Y %b %d %a %X"), " ? ", sep = ""))
    td <- new.time - time
    if (td["elapsed"] > td.co) 
        .r(.sys(td["elapsed"], "elapsed"), content = paste(capture.output(substitute(expr)), collapse = "\n"))
    structure(new.time - time, class = "proc_time")
}
fp_temp.38_05_01_195807 <- file.path(if (dir.exists("/dev/shm")) "/dev/shm" else hs.home.expand("~"), Sys.info()["user"], "38_05_01_171338.txt.gz")
ieiwk.repl <- function(env = parent.frame(), env1 = new.env(parent = env), init = {
}, ...) {
    A <- list(...)
    repeat {
        E.38_05_01_195517 <- do.call(ieiwk.parse, A)
        if (!length(E.38_05_01_195517)) 
            next
        if (hs.all(hs.len1(E.38_05_01_195517), is(E.38_05_01_195517[[1]], "name"), paste(E.38_05_01_195517[[1]]) == ".qq")) 
            return()
        loadhistory(hs.home.expand("~/.Rhistory"))
        time1.38_05_02_230459 <- Sys.time()
        cat("\033[31m****************", paste(time1.38_05_02_230459), "[表达式] ****************\033[0m\n", file = paste("| gzip >>", fp_temp.38_05_01_195807))
        cat(paste(E.38_05_01_195517), sep = "\n", file = paste("| gzip >>", fp_temp.38_05_01_195807))
        a.38_05_01_195727 <- .try(eval(E.38_05_01_195517, env))
        time_diff.38_05_03_000503 <- Sys.time() - time1.38_05_02_230459
        jg_tag.38_05_03_230943 <- paste(paste(Sys.time()), "[结果]", capture.output(time_diff.38_05_03_000503))
        jg_line.38_05_04_111217 <- paste("\033[31m****************", jg_tag.38_05_03_230943, "****************\033[0m\n")
        cat(jg_line.38_05_04_111217, file = paste("| gzip >>", fp_temp.38_05_01_195807))
        cat(jg_line.38_05_04_111217)
        units(time_diff.38_05_03_000503) <- "secs"
        if (time_diff.38_05_03_000503 > 30) {
            hs.hsgly("report.job.is.done.36.03.02.104436")(jg_tag.38_05_03_230943, body = paste(E.38_05_01_195517))
        }
        if (file.size(fp_temp.38_05_01_195807) > 1e+07) {
            L.38_05_01_195750 <- readLines(fp_temp.38_05_01_195807)
            cat(tail(L.38_05_01_195750, max(1, floor(length(L.38_05_01_195750) * 0.7))), sep = "\n", file = paste("| gzip >", fp_temp.38_05_01_195807))
        }
        local({
            con1.38_05_01_195851 <- pipe(paste("head -n 20 | gzip >>", fp_temp.38_05_01_195807), open = "w")
            on.exit(close(con1.38_05_01_195851), add = TRUE)
            capture.output(a.38_05_01_195727, file = con1.38_05_01_195851)
        })
        if (((class(E.38_05_01_195517[[1]]) %in% c("<-", "=")) && (!is(a.38_05_01_195727, "try-error"))) || is(a.38_05_01_195727, "help_files_with_topic")) {
        }
        else local({
            a <- capture.output(a.38_05_01_195727)
            if (length(a) > 20) 
                a[20] <- "..."
            cat(head(a, 20), sep = "\n")
        })
    }
}
ieiwk.parse <- local({
    hs.readline_saveHistory <- function(p1) {
        cmd1 <- readline(p1)
        e1 <- .try(parse(text = cmd1))
        if (!input.is.incomplete(e1)) 
            cat(cmd1, sep = "\n", append = TRUE, file = hs.home.expand("~/.Rhistory"))
        cmd1
    }
    function(prefix = "") {
        p1 <- paste0("\033[31m", Sys.info()["user"], "@", Sys.info()["nodename"], ":", getwd(), "\033[0m\n", format(Sys.time(), "%Y %b %d %a %X"), "\n", prefix, " R> ", sep = "")
        cmd <- hs.readline_saveHistory(p1)
        repeat {
            e1 <- .try(parse(text = cmd))
            if (input.is.incomplete(e1)) {
                cmd1 <- hs.readline_saveHistory("and? > ")
                cmd <- c(cmd, cmd1)
            }
            else {
                return(e1)
            }
        }
    }
})
ieiwk.repl.eval <- function(e1, env = .GlobalEnv) {
    length(e1[[1]])
    {
        if (1) 
            aaa
    }
    mode(e1[[1]])
    mode(e1[[1]][[2]])
    is.function(e1[[1]][[length(e1[[1]])]][[2]])
    mode(e1[[1]][[length(e1[[1]])]])
    mode(tail(e1[[1]], 1)[[1]])
    apropos("function")
    cmd <- {
    }
    repeat {
        cmd <- c(cmd, readline() %not% "")
        if (!length(cmd)) 
            next
        r <- .try(eval(parse(text = cmd), env = env))
        if (!input.is.incomplete) 
            return(r)
    }
}
ieiwk.get.function <- function(s, r = c("definition", "symbol", "name")) {
    r <- match.arg(r)
    a <- apropos(s, mode = "function")
    switch(r, definition = get(a), symbol = as.symbol(a), name = a)
}
ieiwk.load.or.rerun <- function(exp, fp, rerun, env = parent.frame()) {
    if ({
        !rerun
    } & {
        !file.exists(fp)
    }) 
        rerun <- 1
    if (rerun) 
        eval(exp, env)
    else load(fp, env)
}
ieiwk.get.callForm <- function(...) {
    match.call()[-1]
}
ieiwk.dynamicScope <- function(fn, env = parent.frame()) {
    environment(fn) <- env
    fn
}
defmacro <- function(..., expr) {
    expr <- substitute(expr)
    a <- substitute(list(...))[-1]
    nn <- names(a)
    if (is.null(nn)) 
        nn <- rep("", length(a))
    for (i in 1:length(a)) {
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], "not supplied")
            a[[i]] <- substitute(stop(foo), list(foo = msg))
        }
        if (nn[i] == "DOTS") {
            nn[i] <- "..."
            a[[i]] <- formals(function(...) {
            })[[1]]
        }
    }
    names(a) <- nn
    a <- as.list(a)
    ff <- eval(substitute(function() {
        tmp <- substitute(body)
        eval(tmp, parent.frame())
    }, list(body = expr)))
    formals(ff) <- a
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    attr(ff, "source") <- c(deparse(mm), deparse(expr))
    ff
}
strmacro <- function(..., expr, strexpr) {
    if (!missing(expr)) 
        strexpr <- deparse(substitute(expr))
    a <- substitute(list(...))[-1]
    nn <- names(a)
    if (is.null(nn)) 
        nn <- rep("", length(a))
    for (i in 1:length(a)) {
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], "not supplied")
            a[[i]] <- substitute(stop(foo), list(foo = msg))
        }
        else {
            a[[i]] <- a[[i]]
        }
    }
    names(a) <- nn
    a <- as.list(a)
    ff <- function(...) {
        reptab <- a
        reptab$... <- NULL
        args <- match.call(expand.dots = TRUE)[-1]
        for (item in names(args)) reptab[[item]] <- args[[item]]
        body <- strexpr
        for (i in 1:length(reptab)) {
            pattern <- paste("\\b", names(reptab)[i], "\\b", sep = "")
            value <- reptab[[i]]
            if (missing(value)) 
                value <- ""
            body <- gsub(pattern, value, body)
        }
        fun <- parse(text = body)
        eval(fun, parent.frame())
    }
    formals(ff) <- a
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    attr(ff, "source") <- c(deparse(mm), strexpr)
    ff
}
evalStr <- function(s, env = parent.frame()) {
    eval(parse(text = s), env)
}
evalStrArg <- function(s, env = parent.frame()) {
    s0 <- try(parse(text = s), silent = TRUE)
    switch(typeof(s0), character = {
        s1 <- strsplit(attr(s0, "condition")[[1]], "\n")[[1]]
        se <- as.integer(strsplit(re("\\d+:\\d+", s1[1]), ":")[[1]])
        evalStr(substr(s, se[1], se[2] - 1L), env)
        s <- substring(s, se[2] + 1)
        evalStrArg(s, env)
    }, eval(s0, env))
    invisible()
}
ieiwk.mp.code2str.r <- function(..., trim = 0) {
    a <- list(...)
    r <- sapply(a, function(x) {
        if (typeof(x) != "character") 
            x <- capture.output(x)
        x
    })
    if (is.list(r)) 
        r <- unlist(r)
    r <- c(r)
    if (trim) 
        r <- do.call("trim", list(r))
    r
}
wk.evalLazy <- ieiwk.evalLazy <- function(r, e, uid, root = formals(.mfp.swxx)[["dataDir"]], fp = {
}, ext = if ("TxDb" %in% class) "sql" else "rd", lazy = 1, ow = 1, env = parent.frame(), env2 = if (local) new.env() else parent.frame(), local = 1, fp.only = 0, dtt = "handy", save.jg = 1, want = "jg", hs1.save = {
}, reader = {
}, lib = {
}, class = {
}) {
    env
    env2
    if (substitute(e)[1] != substitute(expression())) 
        e <- substitute(e)
    if (!length(fp)) 
        fp <- hs.wjm(.mfp(uid, root, dtt = dtt), add = ext)
    if (fp.only) 
        return(fp)
    if (!length(reader)) 
        reader <- hs.case(hs.grepl(fp, "(?i)\\.(tsv|csv|txt)(\\.gz)?$"), fread, hs.grepl(fp, "(?i)\\.xlsx?$"), wk.xls.read, hs.grepl(fp, "(?)\\.rda?(ta)?$"), hs.load, no_match = readLines)
    if (!length(hs1.save)) 
        hs1.save <- switch(tolower(hs.file.extension(fp)), xlsx = , xls = wk.xls.write, gz = wk.dt.fwrite, rd = hs.save, function(x1, file) cat(x1, sep = "\n", file = file))
    e1 <- substitute(e)
    if (class(e1) == "{") 
        e <- e1
    if ("TxDb" %in% class) {
        hs1.save <- AnnotationDbi::saveDb
        reader <- AnnotationDbi::loadDb
    }
    if (save.jg) {
        if (file.exists(fp) && (lazy || (!ow))) {
        }
        else {
            for (lib1 in lib) library(lib1)
            jg <- eval(e, env2)
            hs1.save(jg, file = fp)
        }
    }
    switch(want, jg = {
        if (missing(r)) {
            reader(fp)
        } else {
            assign(r, reader(fp), envir = env)
        }
    }, fp = fp)
}
ieiwk.evalLazy.2 <- local(function(bds, bds2, update = 0, pre = if (!missing(uid)) .mfp(uid), uid, what = "jg") {
    if (length(pre)) 
        wj <- sapply(c("bds", "jg"), . %=>% hs.wjm(pre, add = x))
    env1 <- environment(sys.function())
    if (!missing(bds)) 
        bds2 <- as.expression(substitute(bds))
    if (missing(bds2)) {
        if (length(pre) && file.exists(wj["bds"])) 
            bds2 <- parse(wj["bds"], encoding = "UTF-8")
    }
    if (missing(bds2)) {
    }
    else {
        e1 <- quote(assign("bds", bds2, envir = env1))
        if (ieiwk.exists("bds", envir = env1)) {
            bds.old <- trim(capture.output(get("bds", envir = env1)))
            bds.new <- trim(capture.output(bds2))
            if ({
                length(bds.old) != length(bds.new)
            } || any(bds.old != bds.new)) {
                eval(e1)
                if (length(pre)) 
                  .ss(bds.new, file = wj["bds"])
                update <- 1
            }
        }
        else eval(e1)
    }
    {
        e2 <- quote(eval(quote(jg <- eval(bds)), env = env1))
        if (ieiwk.exists("jg", envir = env1)) {
            if (update) {
                eval(e2)
                if (length(pre)) 
                  save(jg, file = wj["jg"], envir = env1)
            }
        }
        else {
            if (length(pre) && file.exists(wj["jg"])) 
                eval(bquote(jg <- hs.load(.(wj["jg"]))), env1)
            else {
                eval(e2)
                if (length(pre) && file.exists(wj["jg"])) 
                  ieiwk.save("jg", wj["jg"], env1 = env1)
            }
        }
    }
    switch(what, jg = , bds = get(what, envir = env1), pre = pre, wj.jg = , wj.bds = hs.wjm(pre, add = re("[^\\.]+$", what)))
})
ieiwk.mp.assign <- function(..., v = {
}) {
    obj <- substitute(...)
    if (length(obj) > 1) {
        r <- sapply(seq(2, length(obj)), function(i) {
            x <- obj[[i]]
            b.29.12.07.032017 <- eval(x)
            substitute(a <- b, list(a = x, b = b.29.12.07.032017))
        })
        r <- sapply(r, function(x) {
            capture.output(x)
        })
    }
    else {
        r <- capture.output(substitute(a <- b, list(a = obj, b = eval(obj))))
    }
    r
}
bin.perl <- "perl"
ieiwk.mp.perl.cmd <- function(cmd, options = {
}, run = 0, intern = 1) {
    my.perl.lib <- switch(hostname, `kai-VirtualBox` = )
    cmd.default <- "use strict;\nuse warnings;\nuse lib \"/public/home/wengkai/calvin/code/perl\";\nuse subroutineCommon;\nuse feature qw(say);\n"
    cmd <- sprintf("%s%s", cmd.default, cmd)
    if (run) 
        .sys.do(bin.perl, options, "-e " %.% shQuote(strsplit(cmd, "\\n")[[1]]), intern = intern)
    else .sys(bin.perl, options, "-e " %.% shQuote(strsplit(cmd, "\\n")[[1]]), intern = intern)
}
ieiwk.eval.sysCmd <- function(cmd, wj.log) {
    ieiwk.eval(bquote({
        .sys.do(.(cmd), log = .(wj.log))
    }))
}
wk.e <- wk.eval <- ieiwk.eval <- function(e1, n1 = Sys.info()["nodename"], f1 = ieiwk.tempfile(), f2 = ieiwk.tempfile(), bg = 0, intern = !bg, ssh = n1 != Sys.info()["nodename"], via.file.code = bg || ssh || length(ssh.info) || length(ssh.name), via.file.result = ssh && intern && !bg, f2.remove = via.file.result, env = parent.frame(), use.wd = as.logical(one.sys), lib = {
}, job.uid, email = 0, report.if.done = email, time1 = Sys.time(), report.if.done.cutoff.time = 10, newR = 0, one.sys = 1, ssh.info = {
}, ssh.name = {
}) {
    e0 <- list()
    if (length(lib)) 
        e0 <- append(e0, unname(sapply(lib, function(l1) bquote(library(.(l1))))))
    if (use.wd) 
        e0 <- append(e0, bquote(setwd(.(getwd()))))
    e1 <- bquote({
        t1 <- Sys.time()
        "33.04.19.192621" <- .(switch(typeof(e1), expression = e1[[1]], language = e1))
        t2 <- Sys.time()
        td <- difftime(t2, t1, units = "secs")
        if (.(report.if.done) || td > .(report.if.done.cutoff.time)) {
            .ss(c("33.04.19.185828", paste(t2), paste(t1), difftime(t2, t1, units = "mins")), file = hs.home.expand("~/test.33.04.19.185834.txt"))
            .r(capture.output(td), "")
        }
    })
    if (via.file.code && !bg) 
        on.exit(unlink(f1), add = TRUE)
    if (f2.remove && !bg) 
        on.exit(unlink(f2), add = TRUE)
    if (via.file.code) {
        sapply(c(e0, e1), function(e1) {
            cat(capture.output(e1), file = f1, append = TRUE, sep = "\n")
        })
        if (0) {
            .less(f1)
        }
        cmd <- {
            acc1 <- paste(c("source(", shQuote(f1), ")", ";", "unlink(", shQuote(f1), ")", if (via.file.result) c(";", "save( ", .q("33.04.19.192621"), ", file = ", wk.wjlj(f2), ")")), collapse = " ")
            if (ssh) 
                acc1 <- shQuote(acc1)
            paste(c(if (ssh) c("ssh", n1), .q(c("source activate ER; R --no-save --slave -e", acc1)), if (bg) "&"), collapse = " ")
        }
        if (!one.sys) {
            wk.rsync.out(f1, hs.home.expand("~/tmp"), ssh = ssh.name)
            ssh.info <- ieiwk.ssh.db(ssh.name)
            .wk("ssh", "-p", ssh.info["port"], "-l", ssh.info["user"], ssh.info["dz"], shQuote(cmd))
            return()
        }
        r <- system(cmd)
        if (via.file.result) {
            r <- hs.load(f2)
        }
        r
    }
    else {
        if (typeof(e1) == "list") {
            for (e1.1 in head(e1, length(e1) - 1)) r <- eval(e1.1, env = env)
            eval(e1[[length(e1)]], env = env)
            r
        }
        else {
            eval(e1, env = env)
            get("33.04.19.192621", envir = env)
        }
    }
}
hs.substitute <- function(E) {
    E1 <- substitute(E)
    if ("name" %in% class(E1)) 
        return(E)
    if ("call" %in% class(E1)) 
        if (paste(E1[[1]]) %in% c("quote", "bquote", "expression")) 
            E <- E
    substitute(E)
}
ieiwk.eval.helper <- function(f1, f2 = ieiwk.tempfile()) {
    r <- eval(parse(text = readLines(f1)))
    save(r, file = f2)
    invisible(f2)
}
ieiwk.call0 <- function(f, ...) {
    a <- list(...)
    r <- as.call(c(as.symbol(tail(f, 1)), a))
    f <- head(f, -1)
    while (length(f)) {
        r <- call(tail(f, 1), r)
        f <- head(f, -1)
    }
    r
}
ieiwk.call <- function(f, ...) {
    a <- tail(as.list(match.call()), -2)
    r <- as.call(c(as.symbol(tail(f, 1)), a))
    f <- head(f, -1)
    while (length(f)) {
        r <- call(as.symbol(tail(f, 1)), r)
        f <- head(f, -1)
    }
    r
}
ieiwk.access.list <- function(p, last = c("element", "list")) {
    r <- as.symbol(p[1])
    if (length(p) > 2) 
        for (i in seq(2, length(p) - 1)) r <- bquote(.(r)[[.(p[i])]])
    if (length(p) > 1) 
        r <- switch(match.arg(last), list = bquote(.(r)[.(tail(p, 1))]), element = bquote(.(r)[[.(tail(p, 1))]]))
    r
}
wk.a <- ieiwk.access <- function(...) {
    p2 <- list(...)
    acc <- p2[[1]]
    for (p1 in tail(p2, -1)) {
        fn <- switch(class(acc), list = {
            ifelse(length(p1) > 1, `[`, `[[`)
        }, environment = `$`, S4 = `@`, `[`)
        acc <- fn(acc, p1)
    }
    acc
}
.get <- function(...) {
    p <- as.list(match.call())
    fn <- switch(eval(call("typeof", p[[2]]), parent.frame()), list = , environment = "[[", S4 = "@", "[")
    p[[3]] <- eval(p[[3]], parent.frame())
    fa <- call(fn, p[[2]], p[[3]])
    for (p1 in tail(p, -3)) {
        fn <- switch(eval(call("typeof", fa), parent.frame()), list = , environment = "[[", S4 = "@", "[")
        fa <- call(fn, fa, eval(p1, parent.frame()))
    }
    eval(fa, parent.frame())
}
.set <- function(x, value = "", envir = parent.frame()) {
    fa <- as.list(as.list(match.call())$x[-1])
    eval(call("<-", eval(as.call(c(as.symbol("ieiwk.access"), fa))), value), envir)
}
.bt <- browserText
.brs <- defmacro(a, expr = {
    .cat("browsing at ", a)
    browser(a)
})
ieiwk.env.here <- function(env = parent.frame()) env
wk.33.06.24.202656 <- function(env.33.06.24.205427 = parent.frame(), hs.parent = sys.function(1)) {
    evalq(local({
        hs.parent <- sys.function(1)
        formals(hs.33.06.24.203851) <- head(formals(hs.parent), -3)
        args <- head(formalArgs(hs.parent), -3)
        PL <- mget(args %rm% "...", inherits = TRUE)
        if ("..." %in% args) 
            PL <- append(PL, list(...))
        an <- names(PL)
        if (length(an)) 
            PL <- PL[order(c(an, rep("", length(PL) - length(an))), sapply(PL, object.size))]
        pc1 <- hs.try_with_time_limit(digest::digest(PL), 3)
        if (length(pc1)) {
            if (!hs.exists(pc1, env2.33.06.24.181217)) {
                assign(pc1, eval(bquote(do.call(hs.33.06.24.203851, PL))), env2.33.06.24.181217)
            }
            assign("jg", get(pc1, envir = env2.33.06.24.181217), envir = env1.33.06.24.181214)
        }
        else {
            .ss("(re)calculating 34.07.05.191435")
            assign("jg", do.call(hs.33.06.24.203851, PL), envir = env1.33.06.24.181214)
        }
        get("jg", envir = env1.33.06.24.181214)
    }), env.33.06.24.205427)
}
wk.33.06.24.202656.bak <- function(env.33.06.24.205427 = parent.frame(), hs.parent = sys.function(1)) {
    eval(bquote(local({
        args <- .(names(formals(hs.parent)))
        PL <- sapply(args %not% "...", dynGet)
        if ("..." %in% args) 
            PL <- c(PL, list(...))
        if (object.size(PL) > mnemonic.object_size.cutoff.33.06.24.180623) 
            mnemonic.33.06.24.180617 <- 0
        if (mnemonic.33.06.24.180617) {
            PL <- PL[order(names(PL))]
            pc1 <- digest::digest(PL)
            if (!wk.exists(pc1, env2.33.06.24.181217)) {
                assign(pc1, hs.33.06.24.203851(), envir = env2.33.06.24.181217)
                assign("jg", get(pc1, envir = env2.33.06.24.181217), envir = env1.33.06.24.181214)
            }
        }
        else {
            if (!wk.exists("jg", env1.33.06.24.181214) || ..new) 
                assign("jg", hs.33.06.24.203851(), envir = env1.33.06.24.181214)
        }
        jg
    })), env.33.06.24.205427)
}
hs.import <- wk.import <- function(e1, e2 = parent.frame(), only.hs = 0, exclude.hs = 0, what = {
}, exclude = "env2.33.06.24.181217", tolist = 0, ls = 0, replace = 0) {
    if (is.function(e1)) 
        e1 <- environment(e1)
    if (is.function(e2)) 
        e2 <- environment(e2)
    if (inherits(e1, "list")) {
        for (on1 in names(e1)) assign(on1, e1[[on1]], envir = e2)
        return()
    }
    a <- unlist(eapply(e1, base::is.function, all.names = TRUE))
    if (length(what)) 
        a <- a[base::match(what, base::names(a))]
    exclude <- exclude %and% base::names(a)
    if (length(exclude)) {
        a <- a[-base::match(exclude, base::names(a))]
    }
    if (NA %in% a) {
        message("####", base::names(a[base::is.na(a)]), "is not found.", sep = " ")
    }
    if (only.hs) 
        a <- a[a]
    if (exclude.hs) 
        a <- a[!a]
    if (length(a)) {
        if (ls) {
            return(names(a))
        }
        if (tolist) {
            mget(names(a), e1)
        }
        else {
            for (i in seq_along(a)) {
                on1 <- names(a[i])
                eval(hs.bq({
                  if ((!hs.exists(.(on1), inherits = FALSE)) || .(replace)) {
                    assign(.(on1), .(get(on1, envir = e1)))
                    if (.(a[i])) 
                      hs.env(.(as.symbol(on1))) <- hs.env()
                  }
                }), e2)
            }
        }
    }
}
exp_str.hs.v <- function(v1) {
    .q(v1) %=>% paste(collapse = ", ") %=>% paste0("c( ", x, ")")
}
hs.swap <- function(x, y, env = parent.frame()) {
    x.name <- paste(substitute(x))
    y.name <- paste(substitute(y))
    if (all(hs.exists(c(x.name, y.name), env))) {
        a <- x
        assign(x.name, y, envir = env)
        assign(y.name, a, envir = env)
    }
    else stop("[38_05_14_183507]", x.name, "和", y.name, "须同时存在于当前环境")
}
hs.formals.merge <- function(..., old = "replace") {
    fl <- list(...)
    f1 <- formals(fl[[1]])
    for (i in hs.seqForward(2, length(fl))) {
        f2 <- formals(fl[[i]])
        a <- names(f1) %and% names(f2)
        if (length(a)) {
            switch(old, keep = c(f1, f2[names(f2) %not.in% a]), replace = {
                f1[a] <- f2[a]
                f2[a] <- {
                }
            })
        }
        f1 <- c(f1, f2)
    }
    f1
}
hs.call <- function(hs.38_09_26_084109, ..., env1 = parent.frame()) {
    nv <- names(formals(hs.38_09_26_084109))
    a <- list(...)
    if (length(a)) {
        nv <- nv %not% c("...", names(a))
        n1 <- sum(names(a) == "")
        if (n1) 
            nv <- tail(nv, -n1)
    }
    if (length(nv)) 
        a <- c(a, mget(nv, envir = env1, ifnotfound = formals(hs.38_09_26_084109)[nv]))
    do.call(hs.38_09_26_084109, a)
}
hs.call.cmd <- function(f1) {
    a <- names(formals(f1))
    cmd <- paste0(substitute(f1), "( ", paste(paste0(a, " = ", a), collapse = ", "), ")")
    message(cmd)
}
hs.find_function <- function(hs1) {
    a <- findFunction(hs1)
    if (length(a)) {
        env1 <- a[[1]]
        env1[[hs1]]
    }
}
hs.semicolon2 <- function(p, f) {
    do.call("::", list(p, f))
}
hs.call... <- function(E, .., env1 = parent.frame()) {
    eval(bquote(local({
        hs1 <- function(...) {
            .(substitute(E))
        }
        do.call(hs1, as.list(.(..)))
    })), envir = env1)
}
hs.hs.reduce <- function(e1) {
    A <- as.list(e1[-1])
    hs.switch(paste(e1[[1]]), "hs.require", {
        e1[[1]] <- quote(library)
    }, "hs.fp", {
        hs.browser("2023.08.14.190031", debug = 0)
        i <- match("robust", names(A), 0)
        if (i && A[[i]]) {
            B <- body(hs.fp) %=>% as.list
            B[[3]] %=>% as.list
            A
            formals(hs.fp)
        }
        e1[[1]] <- quote(file.path)
    })
    e1
}
hs.pastec <- function(..., collapse = " ") .Internal(paste0(list(c(...)), collapse))
hs.pastec0 <- function(...) .Internal(paste0(list(c(...)), ""))
hs.pastec. <- function(...) .Internal(paste0(list(c(...)), "."))
hs.pastec_ <- function(...) .Internal(paste0(list(c(...)), "_"))
re.helper <- function(a, data) {
    att <- attributes(a)
    x <- a
    y <- a + att$match.length - 1
    r <- substring(data, x, y)
    if ("capture.names" %in% names(att)) {
        r <- list(match = r)
        r[["capture"]] <- {
            b <- sapply(seq_along(att$capture.names), function(i) {
                sapply(seq.int(nrow(att$capture.start)), function(j) {
                  start <- att$capture.start[j, i]
                  len <- att$capture.length[j, i]
                  substring(data[j], start, start + len - 1)
                })
            })
            if (hs.len1(dim(b), 2)) {
                colnames(b) <- att$capture.names
            }
            else {
                names(b) <- att$capture.names
            }
            b
        }
    }
    r
}
re <- function(pat, data, ignore.case = TRUE, perl = TRUE, fixed = FALSE) {
    a <- regexpr(pat, data, ignore.case, perl, fixed)
    re.helper(a, data)
}
gre.helper <- function(a, data) {
    att <- attributes(a)
    x <- a
    y <- a + att$match.length - 1
    r <- sapply(seq.int(length(x)), function(i) substr(data, x[i], y[i]))
    if ("capture.names" %in% names(att)) {
        r <- list(match = r)
        r[["capture"]] <- {
            b <- as.vector(sapply(seq.int(length(att$capture.names)), function(i) {
                sapply(seq.int(nrow(att$capture.start)), function(j) {
                  start <- att$capture.start[j, i]
                  len <- att$capture.length[j, i]
                  substr(data, start, start + len - 1)
                })
            }))
            b <- matrix(NA, nrow(att$capture.start), length(att$capture.names))
            for (i in seq.int(length(att$capture.names))) {
                for (j in seq.int(nrow(att$capture.start))) {
                  start <- att$capture.start[j, i]
                  len <- att$capture.length[j, i]
                  b[j, i] <- substr(data, start, start + len - 1)
                }
            }
            colnames(b) <- att$capture.names
            b
        }
    }
    r
}
gre <- function(pat, data, ignore.case = 0, perl = TRUE, fixed = 0, useBytes = FALSE) {
    a <- gregexpr(pat, data, ignore.case, perl, fixed, useBytes)
    lapply(seq_along(a), function(i) gre.helper(a[[i]], data[i]))
}
hs.strsplit.v <- function(split = sep, sep = "auto") {
    if (!is.character(x)) 
        x <- hs.str_as(x)
    if (split == "auto") {
        split <- hs.detect.sep(x, sep_set = split)
        if (!nzchar(split)) 
            split <- {
            }
    }
    jg <- if (length(split)) {
        strsplit(x = x, split = split, fixed = fixed, perl = perl, useBytes = useBytes)
    }
    else x
    unlist(jg)
}
formals(hs.strsplit.v) <- hs.formals.merge(strsplit, hs.strsplit.v, old = "replace")
hs.strsplit <- function(x, split, fixed = FALSE, perl = TRUE, useBytes = FALSE) {
    strsplit(x = x, split = split, fixed = fixed, perl = perl, useBytes = useBytes)
}
hs.substr.replace <- function(s1, i1, i2, s2) {
    sprintf("%s%s%s", substr(s1, 1, i1 - 1), s2, substring(s1, i2 + 1))
}
hs.regmatches <- function(s1, m1, invert = FALSE, no_match = "", replacement = {
}, env1 = parent.frame()) {
    if (length(replacement)) {
        eval(bquote(regmatches(.(substitute(s1)), .(m1), .(invert)) <- .(replacement)), env1)
    }
    else {
        a1 <- regmatches(s1, m1)
        if (length(no_match)) {
            a2 <- rep(no_match, length(s1))
            a2[m1 > 0] <- a1
            a2
        }
        else a1
    }
}
hs.detect.sep <- function(v, sep_set = "", ...) {
    if (!nzchar(sep_set)) 
        sep_set <- "[;\\|]"
    step_size <- 2^10
    lv <- length(v)
    i1 <- 1
    repeat {
        if (i1 > lv) 
            return("")
        i2 <- min(i1 + step_size, lv)
        v1 <- v[i1:i2]
        gi <- hs.grep(v1, sep_set, ...)
        jg <- if (length(gi)) 
            hs.re(v1[gi[1]], sep_set, ...)
        else ""
        if (nzchar(jg)) 
            return(jg)
        i1 <- i2 + 1
    }
}
hs.format.same_width <- function(V) {
    width1 <- max(nchar(V))
    sprintf(sprintf("%%%ds", width1), V)
}
hs.paste <- function(..., sep = "", cat = 0, end = "\n") {
    R <- paste(c(...), collapse = sep)
    if (cat) {
        cat(R, sep = end)
    }
    R
}
paste. <- function(...) {
    paste(..., sep = ".")
}
hs.paste. <- function(...) {
    paste.(c(...), collapse = ".")
}
pasteT <- function(...) {
    paste(..., sep = "\t")
}
hs.pasteT <- function(...) {
    paste.(c(...), collapse = "\t")
}
hs.file.extension <- function(fp, ext.pattern = "\\.[^.]+$", extN = 1, include. = FALSE) {
    if (extN) {
        hs.re(fp, sprintf("%s%s$", ifelse(include., "\\.", ""), paste(rep("[^\\./]*", extN), collapse = "\\.")))
    }
    else fp
}
hs.str_merge <- function(s1, s2 = {
}, sep = ";", sep2 = sep, clean = 0, unique = clean, ...) {
    a <- strsplit(c(s1, s2), sep, ...)
    a <- unlist(a)
    if (unique) {
        a <- unique(a)
    }
    if (clean) {
        a <- hs.clean(a, U = unique)
    }
    if (length(a)) 
        a <- a[order(nchar(a), a)]
    paste(a, collapse = sep2)
}
hs.str_subtract <- function(s1, s2, sep = ";", ...) {
    a <- strsplit(c(s1, s2), sep, ...)
    a <- a[[1]] %not% a[[2]]
    a <- hs.clean(a, U = 1)
    a <- a[order(nchar(a), a)]
    paste(a, collapse = sep)
}
hs.str_n <- function(s1, sep = ";") {
    sapply(hs.str_split(s1, sep), length)
}
hs.str_in <- function(s1, s2, sep = ";", sep2 = sep, all = 0, ic = 1) {
    s2 <- hs.clean(hs.strsplit.v(s2, split = sep2), U = 1)
    if (ic) {
        s1 <- tolower(s1)
        s2 <- tolower(s2)
    }
    hs1 <- if (all) 
        base::all
    else base::any
    sapply(strsplit(paste(s1), sep), function(x) if (length(x)) 
        hs1(x %in% s2)
    else FALSE)
}
hs.str_split <- function(s1, sep = ";", fixed = FALSE, ..., reversible = TRUE) {
    jg <- strsplit(s1, sep, fixed = fixed, ...)
    if (reversible) {
        f1 <- hs.grepl(s1, paste0(sep, "$"))
        nm <- names(jg)
        jg <- lapply(seq_along(jg), function(i1) {
            . <- jg[[i1]]
            if (is.null(.)) {
                . <- ""
            }
            else if (f1[i1]) {
                . <- c(., "")
            }
            .
        })
        names(jg) <- nm
    }
    jg
}
hs.str_flat <- function(s1, sep = ";", ot = "v") {
    a1 <- hs.str_split(s1, sep)
    switch(ot, v = unlist(a1), i = rep(seq_along(s1), sapply(a1, length)), all = list(v = unlist(a1), i = rep(seq_along(s1), sapply(a1, length))))
}
hs.str_pad <- function(str, width = max(nchar(str)), pad = " ", side = "left") {
    if (identical(pad, " ") && (side == "left")) {
        sprintf(paste0("%0", width, "s"), str)
    }
    else {
        jg <- paste(str)
        ns <- nchar(jg)
        i <- which(ns < width)
        if (length(i)) {
            w1 <- width - ns[i]
            a <- hs.gsub(sprintf(paste0("%", w1, "s"), ""), ".", pad)
            switch(side, left = {
                jg[i] <- paste0(a, jg[i])
            }, right = {
                jg[i] <- paste0(jg[i], a)
            })
        }
        jg
    }
}
hs.str_nonredundant <- function(x) {
    y <- toupper(x)
    if (uniqueN(y) == uniqueN(x)) 
        x
    else y
}
hs.str_list_merge <- function(..., toupper = 0) {
    a <- trim(unlist(list(...)))
    a <- if (toupper) 
        toupper(a)
    else hs.str_nonredundant(a)
    sort(unique(a))
}
hs.str_and <- function(v1, v2, inv = 0) {
    i <- match(tolower(v1), tolower(v2))
    if (inv) {
        v1[which(is.na(i))]
    }
    else {
        ii <- which(!is.na(i))
        if (length(ii)) 
            unique(v2[i[ii]])
    }
}
hs.str_not <- function(v1, v2) {
    hs.str_and(v1, v2, inv = 1)
}
hs.str_match <- function(v1, v2, ot = "i", ic = 1, ...) {
    if (ic) {
        v1 <- tolower(v1)
        v2 <- tolower(v2)
    }
    i <- match(v1, v2, ...)
    hs.switch(ot, "i", i, "l", !is.na(i), "v2", v2[i], "v1", v1[!is.na(i)])
}
hs.str_diversify <- function(v1, sep = "_", skip1 = 0) {
    a <- tapply(seq_along(v1), v1, function(i) {
        if (length(i) > 1) 
            i
    })
    a <- hs.nonempty(a)
    i <- unlist(a)
    v1[i] <- if (skip1) {
        paste0(v1[i], unlist(lapply(a, function(i) c("", paste0(sep, seq(length(i) - 1))))))
    }
    else paste0(v1[i], sep, unlist(lapply(a, seq_along)))
    v1
}
hs.str_as <- function(x, ...) {
    y <- as.character(x)
    i <- hs.clean(x, ot = "l", inv = 1, ...)
    if (any(i)) 
        y[i] <- ""
    y
}
hs.str_cut <- function(x, p1, all = 1, hs1 = if (all) gregexpr else regexpr, ic = TRUE, perl = !fixed, fixed = FALSE, at = "p1", at_once = 1, inclusive = at != "p1") {
    m <- hs1(p1, x, ignore.case = ic, perl = perl, fixed = fixed)
    if (at == "after") 
        for (i in seq_along(m)) {
            m1 <- m[[i]]
            if (identical(m1[[1]], -1L)) 
                next
            m1 <- m1 + attr(m1, "match.length")
            j <- m1 > nchar(x[i])
            if (any(j)) {
                j <- which(!j)
                if (length(j)) {
                  m[[i]] <- hs.attr.slice(m1, j)
                }
                else m[i] <- list({
                })
            }
            else m[[i]] <- m1
        }
    if (at != "p1") 
        for (i in seq_along(m)) {
            if (length(m[[i]])) 
                attr(m[[i]], "match.length") <- rep(0, length(m[[i]]))
        }
    if (!at_once) {
        a <- lapply(m, function(m1) {
            if (length(m1) > 1) 
                lapply(seq_along(m1), function(i) hs.attr.slice(m1, i))
            else list(m1)
        })
        m <- unlist(a, recursive = FALSE)
        x <- rep(x, sapply(a, length))
    }
    regmatches(x, m, invert = TRUE)
}
hs.str_sort <- function(s, ic = TRUE, ...) {
    s[order(if (ic) 
        tolower(s)
    else s, ...)]
}
hs.pv.format <- function(p1, digits = 3) {
    paste("pv:", format(p1, scientific = 1, digits = digits))
}
format.hs.size <- function(size1, unit = "auto") {
    class(size1) <- "object_size"
    format(size1, unit = unit)
}
hs.title <- function(string1) {
    substr(string1, 1, 1) <- toupper(substr(string1, 1, 1))
    substring(string1, 2) <- tolower(substring(string1, 2))
    string1
}
hs.str_split_fixed <- function(string, pattern = "\t", N = {
}, I = {
}, from = 1, ot = "dt", perl = !fixed, fixed = FALSE, ignore.case = TRUE) {
    m1 <- gregexpr(pattern, string, perl = perl, fixed = fixed)
    n1 <- length(m1[[1]])
    if (!length(N)) 
        N <- n1 + 1
    V <- if (length(I)) {
        I
    }
    else {
        switch(from, seq_len(N - 1), seq.int(n1 - N + 2, n1))
    }
    m1 <- lapply(m1, hs.attr.slice, V)
    jg <- regmatches(string, m1, invert = 1)
    switch(ot, l = jg, m = do.call(rbind, jg), dt = as.data.table(do.call(rbind, jg)))
}
hs.str_2num <- function(x, as.num = 1) {
    jg <- rep(hs.na(""), length(x))
    i <- hs.grep(x, "%$")
    if (length(i)) 
        jg[i] <- as.numeric(hs.sub(x[i], "%"))/100
    i <- hs.grep(x, ",")
    if (length(i)) 
        x[i] <- hs.gsub(x[i], ",")
    i <- which(is.na(jg))
    if (length(i)) 
        jg[i] <- x[i]
    if (as.num) 
        as.numeric(jg)
    else jg
}
hs.dna.reversComplement <- function(S) {
    paste(rev(sapply(unlist(strsplit(S, "")), switch, a = "t", t = "a", c = "g", g = "c", A = "T", T = "A", C = "G", G = "C", wk.browser("34.09.16.165416 @ hs.dna.reversComplement"))), collapse = "")
}
hs.fpGood <- function(fp, q = 1, type = "sh") {
    R <- hs.normalizePath(fp)
    i <- hs.grep(fp, "/$")
    if (length(i)) 
        R[i] <- paste0(R[i], "/")
    if (q) 
        R <- shQuote(R, type)
    R
}
hs.fp.make_read_only <- function(wj) {
    Mode <- as.character(file.info(wj)[, "mode"])
    i1 <- hs.grep(Mode, "[2367]")
    if (length(i1)) {
        wj <- wj[i1]
        Mode <- Mode[i1]
        p1 <- hs.gre(Mode, ".")[[1]]
        Mode2 <- sapply(hs.gre(Mode, "."), function(p1) {
            sapply(p1, . %=>% {
                intToBits(x) %=>% head(3) %=>% as.numeric %=>% {
                  x[2] <- 0
                  x
                } %=>% rev %=>% paste(collapse = "") %=>% strtoi(2)
            }) %=>% as.character %=>% paste(collapse = "")
        })
        base::Sys.chmod(wj, Mode2)
    }
    wj
}
hs.fp.make_personal <- function(wj) {
    Mode <- paste(file.info(wj)[, "mode"])
    i1 <- hs.grep(Mode, ".[^0]")
    if (length(i1)) 
        local({
            wj <- wj[i1]
            Mode <- Mode[i1]
            p1 <- hs.gre(Mode, ".")[[1]]
            Mode2 <- hs.sub(Mode, "(?<=.)..", "00")
            base::Sys.chmod(wj, Mode2)
        })
    wj
}
hs.wj.parent <- function(wj) basename(dirname(wj))
hs.fp <- function(..., e = FALSE, robust = FALSE, n = FALSE) {
    a <- file.path(...)
    if (robust) {
        if (length(a) > 1) 
            hs.browser("2023.08.04.190335", debug = 0)
        p1 <- "(?i)\\.gz$"
        a <- if (hs.grepl(a, p1)) 
            c(hs.sub(a, p1), a)
        else c(a, paste0(a, ".gz"))
        a <- a[file.exists(a)]
        if (length(a) > 1) 
            a <- a[1]
    }
    else if (e) {
        a <- a[file.exists(a)]
    }
    if (n) 
        names(a) <- hs.wjm(a, dn = 0, ext = 0)
    a
}
hs.fp1 <- function(...) {
    hs.pastec(..., collapse = .Platform$file.sep)
}
hs.wj <- ieiwk.file.path <- function(..., fsep = .Platform[["file.sep"]], recursive = 1, type = "file", sys = 0, expand = 0, normalize = FALSE, v.only = 0) {
    a <- file.path(...)
    if (v.only) 
        return(a)
    switch(type, file = {
        sapply(dirname(a), function(x) {
            if (!file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
        })
    }, wjj = , dir = {
        if (!file.exists(a)) dir.create(a, recursive = recursive)
    })
    r <- {
        if (sys) {
            r <- gsub("\\\\ ", " ", a)
            gsub(" ", "\\\\ ", a)
        }
        else a
    }
    if (expand) 
        r <- path.expand(r)
    if (normalize) 
        r <- ieiwk.fp.norm(r)
    r
}
MyMakePath <- ieiwk.file.path
hs.wjj <- wk.wjj <- wk.dir.path <- ieiwk.dir.path <- function(..., fsep = .Platform$file.sep, recursive = TRUE, type = "dir", sys = FALSE, expand = FALSE, normalize = FALSE, v.only = FALSE, go = FALSE) {
    wjj <- hs.wj(..., fsep = fsep, recursive = recursive, type = type, sys = sys, expand = expand, normalize = normalize, v.only = v.only)
    if (go) 
        setwd(wjj)
    wjj
}
hs.wjj.clean <- wk.wjj.clean <- function(...) {
    wjj <- hs.wjj(...)
    unlink(hs.fp(wjj, "*"), recursive = 1)
    wjj
}
hs.wjlj <- wk.wjlj <- function(wj1, path.expand = 1, quote = 1) {
    if (path.expand) 
        wj1 <- path.expand(wj1)
    if (quote) 
        wj1 <- .q(wj1)
    wj1
}
hs.with_wd_temp <- function(wd = hs.fp("/dev/shm", .muid4machine()), exp1) {
    wjj.37_11_28_135147 <- getwd()
    setwd(hs.wjj(wd))
    on.exit(setwd(wjj.37_11_28_135147))
    exp1
    return(wd)
}
hs.fp2name <- function(fp, extN = 1 + hs.grepl(fp, "(?i)\\.gz$")) {
    hs.wjm(fp, dn = 0, ext = 0, extN = 1 + hs.grepl(fp, "(?i)\\.gz$"))
}
hs.fp_add_name <- function(fp, ...) {
    hs.c(sapply(fp, hs.fp2name, ...), fp)
}
.bash <- function(wd = ".") {
    wd <- wd[1]
    if (!dir.exists(wd)) 
        wd <- dirname(wd)
    hs.with_wd(wd, {
        .wk("bash")
    })
}
.file <- function(f1) {
    sapply(f1, function(f1) {
        r <- .sys.do("file", .sys.fpGood(f1), intern = 1)
        sep.n <- sum(nchar(unlist(gre(":", f1))) == 1)
        for (i in seq(sep.n + 1)) {
            r <- sub("[^:]*:", "", r)
        }
        trim(r)
    })
}
symlink.target <- function(wj1) {
    Sys.readlink(wj1)
}
symlink.fix <- function(wj1) {
    wj1 <- wj1[!file.exists(wj1)]
    wj2 <- symlink.target(wj1) %=>% hs.wj.portable(static = 1)
    hs.wj.rm(wj1)
    file.symlink(wj2, wj1)
}
wk.muid.sep.pattern <- "[\\.\\-_]"
wk.muid.pattern <- {
    paste0("\\b\\d+", wk.muid.sep.pattern, "\\d{2}", wk.muid.sep.pattern, "\\d{2}", wk.muid.sep.pattern, "\\d{6}")
}
uid.hs <- function() {
    uid1 <- .wk("xsel --clipboard --output", intern = 1)
    uid1 <- hs.re(uid1, wk.muid.pattern)
    l1 <- .wk("ag -G", .q("\\.txt$"), .q(paste0("{[^{]*", uid1, "}")), hs.home.expand("~/xt"), intern = 1)
    if (hs.len1(l1)) {
        wj.line <- strsplit(l1, ":\\d+:")[[1]]
        a <- hs.re(wj.line[2], paste0("{[^{]*", uid1, "}"))
        jg <- hs.sub(a, "{") %=>% hs.sub("}$")
        soi <- hs.re(wj.line[2], paste0("{\\d+}{[^{]*", uid1, "}"))
        if (length(soi)) {
            up_level <- as.integer(hs.re(soi, "\\d+"))
            up_str <- paste(tail(strsplit(hs.sub(wj.line[1], "\\.txt$"), "/")[[1]], up_level), collapse = "/")
            jg <- hs.fp(up_str, jg)
        }
        jg <- paste0("hs.hsgly( ", .q(jg), ")")
        message(jg)
        cat(jg, file = "| xsel -ib")
    }
}
.mfp <- .muid.file.path <- function(.muid, dataDir = hs.home.expand(switch(data.type.time, handy = hs.home.expand("~/wk/gz/handy"), long = hs.home.expand("~/wk/db/long"))), fp.type = "file", split = wk.muid.sep.pattern, uid.pos = c("head", "tail"), uid.pat = "\\d+.\\d+.\\d+.\\d+$", normalize = FALSE, v.only = FALSE, include.uid = TRUE, smart = TRUE, act = c("compose", "find"), find.exclude.pat = "~$", data.type.time = dtt, dtt = "handy", fp.only = FALSE) {
    lvl <- 3
    a <- as.list(strsplit(.muid, split)[[1]])
    uid <- hs.gre(.muid, wk.muid.pattern)[[1]]
    if (!hs.len1(uid)) 
        hs.browser("36_10_20_121717")
    a <- strsplit(uid, wk.muid.sep.pattern)[[1]]
    p <- do.call("file.path", as.list(c(dataDir, head(a, lvl))))
    if (fp.only) 
        return(file.path(p, .muid))
    switch(match.arg(act), compose = {
        fp <- file.path(p, .muid)
        if (hs.all(fp.type == "file", !include.uid)) {
            flag <- 1
            if (smart) if (nchar(ieiwk.filename(sub(uid.pat, "", .muid))) == 0) flag <- 0
            if (flag) fp <- hs.sub(fp, wk.muid.pattern)
        }
    }, find = {
        fp <- hs.wjj.ls(p, hs.re(.muid, wk.muid.pattern))
        for (p1 in find.exclude.pat) {
            fp <- grep(p1, fp, inv = 1, v = 1)
        }
    })
    fp
}
.mfp.swxx <- function(..., dataDir = hs.home.expand("~/swxx/xm/db")) {
    .mfp(..., dataDir = dataDir)
}
.mfp.org <- function(..., dataDir = hs.home.expand("~/org/db")) {
    .mfp(..., dataDir = dataDir)
}
wj.head.hs <- function(wj) {
    names(fread(cmd = .sys.cat.skip(wj, N = 1)))
}
hs.wj.portable <- local({
    hs.wj.abs_pattern.rel <- function(wj) {
        c(paste0("^", normalizePath(hs.home.expand(wj))), wj)
    }
    db <- matrix(hs.wj.abs_pattern.rel(c("~/swxx", "~/in2/swxx", "~/org", "~/wk", "~")), ncol = 2)
    function(wj, static = 0) {
        wj_in <- wj
        wj %<=>% {
            unique(x) %=>% hs.c(x, x)
        }
        if (static) {
            wj <- hs.sub(wj, "^/work/wengkailab/swxx", "~/swxx")
        }
        else {
            i1 <- hs.grep(wj, "^~", inv = 1)
            wj[i1] %<=>% normalizePath
            for (db.ci in seq.int(nrow(db))) {
                if (!length(i1)) 
                  break
                db1 <- db[db.ci, ]
                i2 <- regexpr(db1[1], wj[i1], perl = TRUE)
                i2.1 <- which(i2 > 0)
                if (length(i2.1)) {
                  regmatches(wj[i1], i2) <- db1[2]
                  i1 %<=>% {
                    x %not% x[i2.1]
                  }
                }
            }
        }
        if (length(wj_in) > length(wj)) {
            wj[wj_in]
        }
        else wj
    }
})
hs.basename <- function(wj, time = 1, sep2 = "/", skip = {
}, keep = {
}) {
    if (time == 1) {
        basename(wj)
    }
    else {
        if (length(skip)) {
            skip <- skip[skip <= time]
            keep <- keep[keep <= time]
        }
        sapply(strsplit(wj, "/"), function(L) {
            a1 <- tail(L, time)
            if (length(keep)) {
                keey <- keep %not% skip
                a1 <- a1[keep]
            }
            else if (length(skip)) {
                a1 <- a1[-skip]
            }
            paste(a1, collapse = sep2)
        })
    }
}
hs.filename <- ieiwk.filename <- function(fp, ext.pattern = hs.pastec0("\\.", if (..) "+", "[^.]+$"), ignore.case = TRUE, dn = 1, dn.depth, zjjg, zjjg_append = "zjjg", zjjg_rename = if (!missing(zjjg)) !zjjg, append = if (!missing(zjjg)) {
}, append_sep = "_", insert, insert_sep = ".", zjjg_pattern = sprintf("%s%s", append_sep, zjjg_append), extN = if (length(zjjg_pattern) && length(grep(zjjg_pattern, fp[1]))) {
    length(strsplit(re(sprintf("(?<=%s\\.).+$", zjjg_pattern), fp[1]), "\\.")[[1]])
} else if (length(a <- hs.re(fp, "(?i)(\\.([tc]sv|txt|tar|fa|r|py))?\\.gz$"))) {
    sapply(hs.gre(a, "\\."), length)
} else 1, pop, pop_sep = append_sep, add = {
}, add_sep = ".", ext = {
    !missing(add)
} || {
    !missing(zjjg)
}, .. = 1) {
    fp.original <- fp
    fp.dn <- hs.case(inherits(dn, "character"), {
    }, dn > 0, dirname(fp), dn < 0, hs.basename(dirname(fp), abs(dn)))
    fp.ext <- rep("", length(fp))
    fp <- basename(fp)
    for (fp_i1 in seq_along(fp)) {
        extN1 <- hs.i(extN, fp_i1)
        for (i in seq(extN1)) {
            ext.v <- hs.re(fp[fp_i1], ext.pattern)
            if (!length(ext.v)) 
                break
            if (!length(ext.v)) 
                hs.browser("36.07.16.160155")
            fp.ext[fp_i1] %<=>% paste0(ext.v, x)
            fp[fp_i1] %<=>% sub(ext.v, "", x, fixed = TRUE)
        }
    }
    if (!missing(zjjg)) {
        if (zjjg) {
            append <- paste(c(append, zjjg_append), collapse = append_sep)
        }
        else pop <- zjjg_append
    }
    if (length(append)) 
        fp %<=>% paste(x, append, sep = append_sep)
    if (!missing(pop)) 
        fp %<=>% sub(sprintf("%s%s$", pop_sep, pop), "", x)
    if (is.logical(ext)) {
        if (ext) 
            fp %<=>% paste0(x, fp.ext)
    }
    else fp %<=>% paste(x, ext, sep = ".")
    if (is.character(dn)) {
        fp %<=>% file.path(dn, x)
    }
    else {
        if (dn && {
            !all(fp.dn %in% c("", "."))
        }) 
            fp <- file.path(fp.dn, fp)
    }
    if (length(add)) 
        fp %<=>% paste(x, add, sep = add_sep)
    if (length(zjjg_rename) && {
        !zjjg
    } && zjjg_rename) 
        file.rename(fp.original, fp)
    fp
}
hs.wjm <- function(..., append = {
}, append_sep = if (length(append)) ifelse(hs.grepl(append, "^\\\\{0,1}[_\\+\\-\\.,]"), "", "_"), add = {
}, add_sep = if (length(add)) ifelse(hs.grepl(add, "^[_\\-\\.]"), "", "."), ext = 1) {
    args <- list(...)
    args["append"] <- list(append)
    args["append_sep"] <- list(append_sep)
    args["add"] <- list(add)
    args["add_sep"] <- list(add_sep)
    if (!is.character(ext)) 
        ext <- as.logical(ext)
    args[["ext"]] <- ext
    jg <- do.call(hs.filename, args)
    hs.wjm.home(jg)
}
hs.fp.ext <- function(fp, i = 1) {
    fp <- basename(fp)
    while (i > 1) {
        fp <- hs.sub(fp, "\\.[^\\.]+$")
        i <- i - 1
    }
    hs.re(fp, "(?<=\\.)[^\\.]+$", no_match = "")
}
hs.wjj.ls <- hs.listWj <- function(wjj1 = ".", ms = {
}, R = 0, F = 1, A = 0, wj = if (all) TRUE else !wjj, wjj = if (all) TRUE else FALSE, all = FALSE, no.. = TRUE, ic = FALSE, ref_wj = {
}, time = ">", nm = 0) {
    if (wj) {
        r1 <- list.files(path = wjj1, pattern = ms, recursive = R, all.files = A, include.dirs = wjj, no.. = no.., ignore.case = ic, full.names = FALSE)
        if (!wjj) 
            r1 <- r1[!dir.exists(file.path(wjj1, r1))]
    }
    else {
        r1 <- list.dirs(wjj1, full.names = 0, recursive = R)
        r1 <- r1[dir.exists(hs.fp(wjj1, r1))]
        if (length(ms)) 
            r1 <- hs.grepV(r1, ms)
    }
    i1 <- hs.grepl(basename(r1), "^~")
    if (any(i1)) {
        r1 <- r1[!i1]
    }
    if (F) 
        r1 <- hs.fp(wjj1, r1)
    if (length(ref_wj)) {
        r1 <- r1[do.call(time, list(file.mtime(r1), file.mtime(ref_wj)))]
    }
    if (nm) {
        names(r1) <- basename(r1) %=>% {
            p1 <- "[^\\._]+[\\._]"
            repeat {
                f1 <- hs.re(x, p1, no_match = "")
                if ((uniqueN(f1) == 1) && (f1[1] != "")) {
                  x <- hs.sub(x, p1)
                }
                else {
                  break
                }
            }
            p1 <- "[\\._][^\\._]+$"
            repeat {
                f1 <- hs.re(x, p1, no_match = "")
                if ((uniqueN(f1) == 1) && (f1[1] != "")) {
                  x <- hs.sub(x, p1)
                }
                else {
                  break
                }
            }
            x
        }
    }
    r1
}
wk.find.wj <- function(wjj1 = ".", ms = "", ms.bn = "", wj = 1, wjj = 0, wj2exclude = {
}, wjm2exclude.ms = "\"*~\"", follow.symlink = 0, recursive = 0, maxdepth = 0, find.extra = list(), context = 3, less = 1, wj.tmp = hs.home.expand("~/.tmp.34.04.08.150846")) {
    find.par <- c(if (follow.symlink) "-L", wjj1 %=>% path.expand %=>% wk.re.replace("[/]*$", "/"), if (maxdepth) c("-maxdepth", maxdepth), if (wj) "-type f")
    .wk("find", find.par, "!", "-iname", wjm2exclude.ms, "|", .sys.grep(ms), "--color", ">", wj.tmp)
    .ss("--", file = wj.tmp, append = 1)
    .wk("find", find.par, "!", "-iname", wjm2exclude.ms, "-print0", "|", "xargs", "-0", .sys.grep(ms), "--color=never", "-n", paste0("-", context), "-T", ">>", wj.tmp)
    if (less) {
        .less(wj.tmp)
    }
    else {
        .wk("cat", wj.tmp)
    }
    if (0) {
        if (recursive) {
            folders <- .wk("find", find.par, "-type d", intern = 1)[-1] %not% wj2exclude
            r <- {
            }
            if (length(folders)) {
                if (wj) 
                  r <- .wk("find", find.par, "-type f", intern = 1) %not% wj2exclude
                r <- c(r, unlist(lapply(folders, wk.find.wj, wj2exclude = wj2exclude, follow.symlink = follow.symlink, recursive = recursive, maxdepth = maxdepth, wj.tmp = wj.tmp)))
            }
            else {
                if (wj) 
                  r <- c(r, .wk("find", find.par, "-type f", intern = 1))
                if (wjj) 
                  r <- c(r, wjj)
            }
            if (length(wj.tmp)) 
                .ss(folders, file = wj.tmp)
            else r
        }
        else {
            r <- {
                if (wj & wjj) {
                  .wk("find", find.par, intern = 1)[-1]
                  .wk("find", find.par, intern = 1)
                }
                else if (wj) {
                  .wk("find", find.par, "-type f", intern = 1)
                }
                else .wk("find", find.par, "-type d", intern = 1)[-1]
            }
            r %not% wj2exclude
        }
    }
}
wk.ag.wj <- function(ms = "", wjj1 = hs.home.expand("~/org"), wjm2exclude.ms = "\"*~\"", follow.symlink = 0, recursive = 0, maxdepth = 0, find.extra = list(), context = 3, less = 1, wj.tmp = hs.home.expand("~/.tmp.34.04.14.104524"), C = 3) {
    wjj1 %=>% wk.re.replace("/*$", "/") %=>% {
        .wk("find", x, "!", "-iname", wjm2exclude.ms, "-print", "| ag", .q(ms), "--pager \"less -R\"")
        .wk("ag", "-i", .q(ms), x, "--pager", .q("less -R"), "-C", "C")
    }
}
hs.wj.real <- function(wj) base::normalizePath(wj)
hs.uid.wj <- function(uid) {
    .wk("ag", "-g", uid, hs.home.expand("~/org"), intern = 1)
}
hs.wj.copy <- function(from, to, ..., copy.date = TRUE, recursive = TRUE) {
    file.copy(from = from, to = to, ..., copy.date = copy.date, recursive = recursive)
}
hs.wj.move <- function(from, to = ".", ..., copy.date = TRUE, recursive = TRUE, ow = 0) {
    if ((!length(from))) 
        return()
    if (length(to) %not.in% c(1, length(from))) 
        hs.browser("39.04.05.161937", debug = 0)
    if (length(to) > 1) 
        hs.browser("39.04.08.110341", debug = 0)
    if ((length(to) == 1) && (dir.exists(to) || (length(from) > 1))) {
        to %<=>% hs.fp(basename(from))
    }
    if (any(file.exists(to))) 
        hs.browser("39.04.09.202117", debug = 0)
    for (to1 in unique(dirname(to))) hs.wjj(to1)
    file.rename(from = from, to = to)
    i <- which(file.exists(from))
    if (length(i)) {
        hs.wj.copy(from = from[i], to = to[i], ..., copy.date = copy.date, recursive = recursive)
        hs.wj.rm(from[i], 1)
    }
    to
}
hs.dirname <- local({
    home <- normalizePath(hs.home.expand("~"))
    function(fp, time = 1, tilde.expansion = 0) {
        while (time > 0) {
            fp <- dirname(fp)
            time <- time - 1
        }
        return(hs.wjm.home(fp))
        if (!tilde.expansion) {
            i1 <- which(substr(fp, 1, 1) == "~")
            if (length(i1)) {
                i2 <- which(hs.grepl(R[i1], paste0("^", home)))
                if (length(i2)) 
                  R[i1[i2]] <- hs.sub(R[i1[i2]], paste0("^", home, "(?=/)"), "~")
            }
        }
        R
    }
})
hs.file <- function(wj, open = "r", ...) {
    gz <- hs.grepl(wj, "(?i)\\.[gx]z$")
    con1 <- (if (gz) 
        gzfile
    else file)(wj, open = open, ...)
}
hs.list.write <- function(L1, wj2) {
    con1 <- hs.file(wj2, "w")
    on.exit(close(con1), add = TRUE)
    if (!is.null(names(L1))) 
        hs.cat("#named=1", file = con1)
    for (i1 in seq_along(L1)) hs.cat(names(L1)[i1], L1[[i1]], sep = "\t", append = 1, file = con1)
    wj2
}
hs.list.read <- function(wj2) {
    nr <- readLines(wj2)
    if (nr[1] == "#named=1") {
        L1 <- strsplit(nr[-1], "\t")
        names(L1) <- sapply(L1, "[", 1)
        lapply(L1, "[", -1)
    }
    else strsplit(nr, "\t")
}
hs.wj.open <- function(wj, ..., open = "r", skip.comments = 1, comment = "#", cn = 1) {
    con1 <- gzfile(wj, open = open, ...)
    if (skip.comments) {
        skip <- hs.lines_comment(wj, comment = comment, opt = "i")
        if (skip && cn) 
            skip <- skip - 1
        if (skip) 
            readLines(con1, skip)
    }
    con1
}
hs.wj.temp <- local({
    wjj <- {
        shm <- file.exists("/dev/shm")
        wjj <- if (shm) {
            hs.wjj(tempfile(tmpdir = "/dev/shm"))
        }
        else tempdir()
    }
    function(ext = "", ...) {
        if (nchar(ext) && (!hs.grepl(ext, "^\\."))) 
            ext <- paste0(".", ext)
        hs.wj(tempfile(tmpdir = wjj, fileext = ext, ...))
    }
})
hs.wj.temp <- function(pattern = "file", fileext = "", tmpdir = hs.wjj(if (file.exists("/dev/shm")) "/dev/shm" else hs.wjj(hs.home.expand("~/tmp")), "36_10_16_121431"), tmpdir.clean = 0, old = 10) {
    if (tmpdir.clean) {
        a <- list.files(tmpdir, full = 1)
        c1 <- 0
        sapply(a, function(a1) {
            if (difftime(Sys.Date(), file.info(a1)[["mtime"]], u = "d") > old) {
                file.remove(a1)
                c1 <<- c1 + 1
            }
        })
        hs.cat(c1, "old temporary files have just been deleted.", sep = " ")
        return()
    }
    pattern <- sprintf("..%s..", pattern)
    tempfile(pattern, tmpdir, fileext)
}
hs.wj.bf <- function(wj1, wj2 = hs.wjm(wj1, add = post), post = "bf", inv = 0, mv = 1, ow = 0, ...) {
    if (inv) {
        a1 <- wj1
        wj1 <- wj2
        wj2 <- a1
    }
    if (ow && any(file.exists(wj2))) {
        hs.wj.rm(wj2[file.exists(wj2)], 1)
    }
    if (mv) {
        hs.wj.move(wj1, wj2)
    }
    else {
        hs.wj.copy(wj1, wj2, ...)
    }
    wj2
}
hs.wj.exists <- function(wj, dir = 0, wj_is = size, size = 0) {
    file.exists(wj) & {
        if (wj_is) 
            !dir.exists(wj)
        else 1
    } & {
        if (dir) 
            dir.exists(wj)
        else 1
    } & {
        if (size) 
            file.size(wj) >= size
        else 1
    }
}
hs.Sys.setFileTime <- function(wj, ...) Sys.setFileTime(normalizePath(wj), ...)
hs.wj.update <- function(wj, message = 1) {
    td <- difftime(Sys.time(), file.mtime(wj), units = "days")
    if (message) {
    }
}
hs.wjm.friendly <- function(wj, rep1 = "_") {
    wj <- hs.gsub(wj, "(?<=[,\\.]) ")
    wj <- hs.gsub(wj, " ", rep1)
    wj <- hs.gsub(wj, ":", "-")
    wj
}
yb.hs.gn <- function(gn, sep1 = "-", smart = 1) {
    if (smart) {
        if (!anyDuplicated(gn)) 
            return(gn)
    }
    a1 <- tapply(seq_along(gn), gn, . %=>% {
        hs.c(seq_along(x), x)
    }, simplify = 0)
    paste0(gn, sep1, names(sort(unlist(unname(a1)))))
}
hs.readLines4gz <- wk.readLines4gz <- function(wj, n = -1L, ...) {
    con <- gzfile(wj, "r", "UTF-8")
    hs.readLines(con, n = n, ...)
}
hs.writeLines <- function(text, con, closeCon = TRUE) {
    writeLines(text, con)
    if (closeCon && ("connection" %in% class(con))) 
        on.exit(close(con), add = TRUE)
}
hs.writeLines2gz <- wk.writeLines2gz <- function(text, wj) {
    con <- gzfile(wj, "w", "UTF-8")
    hs.writeLines(text, con)
}
hs.url_good <- function(url) {
    curl::curl_unescape(url)
}
.sys.perl.print <- function(e1, sep = "\t", a = 1) {
    c("perl", if (a) c(sprintf("-F\"\\%s\"", switch(sep, `\t` = "t", sep)), "-a"), "-lne", .q(e1))
}
.sys.skip_line <- function(n = 0) {
    if (n > 0) {
        .sys("| perl -e", .q(paste0("$i = ", formatSafe(ceiling(n)), ";\nwhile( $i) {\n <>;\n $i--;\n}\nwhile( <>) {\n  print;\n}")))
    }
    else ""
}
wk.readLines.using_perl <- function(wj, lineN, intern = 1, ...) {
    lineN.wj <- paste0(hs.wj.temp(), ".tsv.gz")
    on.exit(hs.wj.rm(lineN.wj))
    if (hs.len1(dim(lineN), 2)) {
        wk.dt.fwrite(lineN, lineN.wj, cn = 0)
    }
    else cat(lineN, sep = "\n", file = lineN.wj)
    .perl.e("%line_wanted = ()", paste("open( DATA, \"", if (hs.grepl(lineN.wj, "\\.gz$")) 
        "pigz -cd"
    else "cat", .sys.fpGood(lineN.wj), "|\")"), "$toEnd = 0", "while( <DATA>) { chomp; @v = split( \"\t\"); if( @v == 1) { $v[ 1] = $v[ 0]}; if( $v[ 1] < 0) { $toEnd = $v[ 0]}; for( $i = $v[ 0]; $i <= $v[ 1]; $i += 1) { $line_wanted{ $i} = 1}}", "close( DATA)", paste("open( DATA, \"", if (hs.grepl(wj, "\\.gz$")) 
        "pigz -cd"
    else "cat", .sys.fpGood(wj), "|\")"), "while( <DATA>) { if( exists( $line_wanted{ $.}) || ( $toEnd && ( $. >= $toEnd))) { print}}", "close( DATA)", intern = intern, ...)
}
.sys.uniq <- function() {
    "uniq | sort | uniq"
}
.unzip <- function(f1, l = 1, l.head = 9, d = dirname(f1), d.use.self.name = 1, t = 0) {
    if (t) 
        return(.wk("unzip -t", hs.fpGood(f1)))
    if (hs.grepl(f1, "(?i)\\.((tar(\\.(gz|xz|bz2))?)|tgz)$")) {
        tar.p <- local({
            a <- c("t", "x")
            b <- paste0("-", a, switch(hs.file.extension(f1), tgz = , gz = "z", xz = "J", bz2 = "j"), "vf")
            names(b) <- a
            b
        })
        if (l) {
            a <- .wk("tar", tar.p["t"], hs.fpGood(f1), "|", .sys.head(1000), intern = 1)
            if (length(a) > l.head) 
                a <- c(head(a, 4), "...", tail(a, 4))
            hs.catn(a)
        }
        else {
            extN <- hs.re(f1, "(?i)\\.((tar\\.(gz|xz|bz2))|tgz)$") %=>% hs.gre("\\.") %=>% unlist %=>% length
            wjj.out <- hs.wjm(f1, dn = d, ext = 0, extN = extN)
            if (!file.exists(wjj.out)) {
                wjj.tmp <- hs.wj.temp(tmpdir = dirname(f1))
                .wk("tar", "-k", tar.p["x"], hs.fpGood(f1), "-C", hs.fpGood(hs.wjj(wjj.tmp)))
                wjj <- hs.wjj.ls(wjj.tmp, wjj = 1, F = 0)
                if (wjj %=>% {
                  hs.len1(x) && (x == basename(f1))
                }) {
                  hs.wj.move(hs.fp(wjj.tmp, wjj), d)
                  unlink(wjj.tmp, 1)
                }
                else hs.wj.move(wjj.tmp, wjj.out)
            }
            return(wjj.out)
        }
    }
    if (hs.grepl(f1, "(?i)\\.tar\\.xz$")) {
        if (l) {
            .wk("xzcat", .sys.fpGood(f1), "| tar", "-tf -", "| head")
        }
        else {
            if (d.use.self.name) 
                d <- wk.wjj(hs.wjm(f1, dn = d, ext = 0, extN = 2))
            .wk("xzcat", .sys.fpGood(f1), "| tar", "-xf -", "-C", .sys.fpGood(d))
        }
        return(d)
    }
    if (hs.grepl(f1, "\\.zip$")) {
        bin <- "unzip"
        if (!length(d.use.self.name)) {
            if (0) {
                fc.0 <- .sys.do("unzip", "-l", .sys.fpGood(f1), intern = 1)
                fc <- {
                  i <- grep("^-", fc.0)
                  substring(fc.0[seq(i[1] + 1, i[2] - 1)], regexpr("Name", fc.0[2]))
                }
            }
            fc.0 <- .wk(bin, "-l", wk.wjlj(f1), "| gawk", .q("{ print $4}"), intern = 1)
            fc <- {
                i <- grep("^-", fc.0)
                tail(fc.0, -i) %rm% ""
            }
            d.use.self.name <- as.integer(substring(fc[1], nchar(fc[1])) != "/" || length(table(sapply(fc, ieiwk.dirname.1))) > 1)
        }
        if (l) {
            .wk("unzip", "-l", .sys.fpGood(f1), "|", .sys.head(l.head))
        }
        else {
            if (d.use.self.name) 
                d <- hs.wj(hs.wjm(f1, ext = 0, dn = d))
            if (file.exists(d)) {
                message("[target folder already exists]", f1)
                return(f1)
            }
            .wk(bin, "-d", .sys.fpGood(d), .sys.fpGood(f1))
        }
        return()
    }
    if (wk.re(f1, "\\.gz$", "detect")) {
        bin <- "pigz"
        if (l) {
            .wk("gzip", "-l", wk.wjlj(f1), "|", .sys.head(l.head))
        }
        else {
            .wk(bin, "-dc", wk.wjlj(f1), ">", wk.wjlj(hs.wjm(f1, ext = 0, dn = d)))
        }
        return(ieiwk.filename(f1))
    }
    if (hs.grepl(f1, "\\.7z$")) {
        bin <- "7z"
        if (l) {
            .wk(bin, "l", f1)
        }
        else {
            if (!length(d.use.self.name)) {
                browser("33.06.15.224151")
            }
            if (d.use.self.name) 
                d <- dirname(f1)
            .wk(bin, "x", "-aos -y", paste0("-o", hs.fpGood(d)), hs.fpGood(f1))
            return(hs.wjm(f1, ext = 0))
        }
    }
    if (wk.re(f1, "\\.rar$", "detect")) {
        bin <- "unrar"
        if (l) {
            .wk(bin, "v", f1, "|", .sys.head())
        }
        else {
            wjj2 <- hs.wjm(f1, ext = 0)
            .wk(bin, "x -u", .sys.fpGood(f1), .sys.fpGood(hs.wjj(wjj2)))
            return(f1)
        }
    }
}
wk.save_to_zip <- function(x1, wj2 = "a.xls", ow = 0, wjj2_temp = hs.wj.temp(tmpdir = if (dir.exists("/dev/shm")) "/dev/shm" else dirname(wj2)), hs1 = function(x1, wj2) wk.dt.fwrite(x1, wj2)) {
    wj_temp <- hs.wjm(wj2, dn = wjj2_temp)
    hs.update(wj_temp, hs.wjm(x, ext = 0), hs.grepl(x, "(?i)\\.zip$"))
    local({
        x <- wj_temp %=>% c(x, hs.wjm(x, dn = dirname(wj2), add = "zip"))
        if (ow) 
            hs.wj.rm(x)
        if (file.exists(x[1])) {
            stop("[39.03.15.044857|中间文件已存在]", .q(x[1]))
        }
        else if (file.exists(x[2])) 
            stop("[39_03_15_045856|文件已存在]", .q(x[2]))
    })
    on.exit(hs.wj.rm(dirname(wj_temp), 1), add = TRUE)
    hs1(x1, wj_temp)
    hs.with_wd(wj_temp, .wk("zip", hs.fpGood(wj2), .q(basename(wj_temp))))
    return(wj2)
    {
        con1 <- pipe(.sys("zip", .sys.fpGood(wj2), "-"), open = "w")
        sink(con1)
        wk.dt.fwrite(x1, "")
        sink()
        close(con1)
        if (!hs.grepl(wj2, "(?i)\\.zip$")) 
            wj2 %<=>% paste0(".zip")
        return(wj2)
    }
}
wk.unrar <- function(wj1, wjj2 = hs.wjm(wj1, ext = 0)) {
    hs.with_wd(wjj2, {
        .wk("unrar x -u -r", .sys.fpGood(wj1))
    })
    hs.wj.portable(wjj2)
}
wk.compressed_file_extension_pattern <- "(?i)\\.((tar\\.|[bt]?)gz|zip|rar|xz|bz2)$"
hs.wj.is_compressed <- function(wj1) length(wj1) && hs.grepl(wj1, wk.compressed_file_extension_pattern)
wk.untar <- function(wj1 = {
}, wj2 = {
}, ..., wj2.gz = !hs.wj.is_compressed(wj2), wjj2 = hs.wjm(wj1, ext = 0, extN = gz + 1), gz = hs.grepl(wj1, "(?i)\\.t?gz$"), list = 0, Av = list, O = !length(wj1), kn = 1, ow = 0, keep_old = 1, tar = switch(osType(), Linux = "tar", Darwin = "gtar")) {
    cmd1 <- .sys(tar, if (list) 
        "-t"
    else "-x", if (!ow) 
        "--skip-old-files", if (Av) 
        "-v", if (gz) 
        "-z", if (!(list || O)) 
        c("-C", hs.wjj(wjj2) %=>% .sys.fpGood), if (O || (length(wj2) && wj2.gz)) 
        "-O", "-f", .sys.fpGood(wj1), if (length(wj2)) 
        .q(wj2), ..., if (O) {
        if (hs.wj.is_compressed(wj2)) 
            "| gzip -cd"
    }
    else {
        if (length(wj2) && wj2.gz) 
            c("| gzip >", hs.wjm(wj2, dn = wjj2, add = "gz"))
    })
    if (O) {
        cmd1
    }
    else {
        .wk(cmd1)
        normalizePath(wjj2)
    }
}
wk.decompress <- function(wj1, wjj2 = {
}, ...) {
    lapply(wj1, function(wj1) {
        if (dir.exists(wj1)) {
            wj1 <- hs.wjj.ls(wj1, wk.compressed_file_extension_pattern, F = 1)
            wk.decompress(wj1, wjj2, ...)
        }
        else {
            if (!length(wjj2)) 
                wjj2 <- hs.sub(wj1, wk.compressed_file_extension_pattern)
            hs.with_wd(hs.wjj(wjj2), {
                if (hs.grepl(wj1, "(?i)\\.zip$")) {
                  unzip(wj1, setTimes = 1, overwrite = 0, ...)
                }
                else if (hs.grepl(wj1, "(?i)\\.(tar\\.gz|tgz)$")) {
                  if (0) {
                    hs1 <- utils::untar
                    hs1(gzfile(wj1, "rb"), extras = "--skip-old-files")
                  }
                  .wk("tar -xz --skip-old-files -f", .sys.fpGood(wj1))
                }
                else hs.browser("37_12_29_152514")
            })
        }
    })
}
.sys.cat.text <- local({
    ext.cat <- c(gz = "pigz -cd", bgz = "pbgzip -cd", xz = "xz -cd", zip = "unzip -p", rar = "unrar -p")
    function(wj, bash = 0, pipe = 0) {
        if (length(wj)) {
            wj.cmd <- sapply(wj, function(wj1) {
                ext <- hs.re(wj1, "[^\\.]+$")
                wj1 <- .sys.fpGood(wj1)
                if (ext %in% names(ext.cat)) {
                  a <- paste(ext.cat[ext], wj1)
                  if (length(wj) > 1) 
                    a <- paste("<(", a, ")")
                  a
                }
                else {
                  paste("cat", wj1)
                }
            })
            cmd1 <- if (length(wj.cmd) > 1) {
                paste(c("cat", wj.cmd), collapse = " ")
            }
            else wj.cmd
            if (bash) 
                cmd1 <- .sys("bash -c", .q(cmd1))
            if (pipe) 
                cmd1 <- .sys(cmd1, "|")
            cmd1
        }
    }
})
.usu <- "uniq | sort | uniq"
hs.isDir <- function(f, a = FALSE) {
    ifelse(a, length(f) == 1, TRUE) && dir.exists(f)
}
hs.isFile <- function(f, a = FALSE) {
    ifelse(a, length(f) == 1, TRUE) && file.exists(f) && (!dir.exists(f))
}
hs.wj.mtime <- function(f) file.info(f)[["mtime"]]
wk.file.newer <- function(f1, f2, tol = 1) {
    (.sys.mtime(f1) - .sys.mtime(f2)) >= tol
}
wk.read <- function(wj) {
    lapply(wj, function(wj1) {
        hs.case.over(paste0("\\.", c("rd", "xls(x)?", "(tsv|csv|txt)(\\.gz)?"), "$"), function(p1) hs.grepl(wj1, p1), list(hs.load, wk.xls.read, fread), no_match = readLines)(wj1)
    })
}
wk.rsync <- function(wj1, wjj2, bin = wj_rsync, type = if (hs.grepl(wj1, "/$")) "wjj" else wj, del = 0, dry = 0, z = 1, p = 1, ssh = wk.ssh, ssh.side = if (length(ssh)) 2 else 0, mode = "update", recursive = 1, cat.cmd = 0, L = 0, wait = 1, sync.type = "get", fs = "linux", checksum = 0, excluded = {
}, included = {
}, U = 0, home.only = 0, files_from = {
}, size_max = {
}, size_min = {
}, ext = {
}, bf_wjj0 = {
}, show_wjj = 0, additional = {
}) {
    if (home.only) 
        wj1 %<=>% {
            hs.wj.portable(x) %=>% hs.grep("^~", v = 1)
        }
    wj1.wjj.i <- which(dir.exists(wj1))
    if (length(wj1.wjj.i)) 
        switch(type, wj = {
            hs.browser("2023.08.06.223939", debug = 0)
            hs.sub(wj1[wj1.wjj.i], "/*")
        }, wjj = if (!hs.grepl(wj1[wj1.wjj.i], "/$")) wj1[wj1.wjj.i] %<=>% paste0("/"))
    if (!hs.grepl(wjj2, "/$")) 
        wjj2 <- paste0(wjj2, "/")
    node_configured <- local({
        a <- readLines(hs.home.expand("~/.ssh/config"))
        a <- hs.sepSeq(a, sep.p = "^(?i)host")
        sapply(a, . %=>% {
            x[1] %=>% hs.re("(?<=\\s)[^\\s]*")
        })
    })
    if (hs.len1(ssh) && is.character(ssh)) {
        if (ssh %not.in% node_configured) {
            ssh <- ieiwk.ssh.db(ssh)
        }
    }
    ssh.str <- paste(c(if (hs.len1(ssh)) ssh else c(ssh["usr"], "@", ssh["dz"]), ":"), collapse = "")
    if (0) 
        switch(as.character(ssh.side), `0` = {
            hs.wjj(wjj2)
            wjj2 <- .sys.fpGood(wjj2)
        }, `1` = {
            wj1 %<=>% sapply(.sys.fpGood)
            wj1 <- paste0(ssh.str, wj1)
            hs.wjj(wjj2)
            wjj2 <- .sys.fpGood(wjj2)
        }, `2` = {
            wjj2 %<=>% sapply(.sys.fpGood)
            wjj2_kept <- wjj2
            wjj2 <- paste0(ssh.str, wjj2)
            message(wjj2)
        })
    always <- "-rtDPvh --force --progress --inplace --no-implied-dirs"
    always.linux <- "-lgo"
    if (mode == "sync") 
        del <- 1
    if (0) {
        wj1 <- hs.gsub(wj1, "(\\()", "\\\\\\1")
        wj1 <- hs.gsub(wj1, "(\\))", "\\\\\\1")
        wj1 <- hs.gsub(wj1, "(;)", "\\\\\\1")
        wjj2 <- hs.gsub(wjj2, "( )", "\\\\\\1")
        wjj2 <- hs.gsub(wjj2, "(\\()", "\\\\\\1")
        wjj2 <- hs.gsub(wjj2, "(\\))", "\\\\\\1")
        wjj2 <- hs.gsub(wjj2, "(;)", "\\\\\\1")
    }
    if (length(wj1) > 1) {
        hs.browser("38.04.11.012803")
        wj.tmp <- tempfile()
        on.exit(hs.wj.rm(wj.tmp))
        cat(wj1, sep = "\n", file = wj.tmp)
        files_from <- paste0("--files-from=", wj.tmp)
        wj1 <- hs.dirname(wj1[1])
    }
    cmd <- .sys(bin, always, if (U) 
        "-u", if (p) 
        "-p", get(paste("always.", fs, sep = "")), if (checksum) 
        "-c", if (z) 
        c("-z", "--compress-level=6"), if (recursive) 
        "-r"
    else "-d", if (del) 
        "--delete", if (L) 
        "-L", if (length(included)) 
        paste0("--include=", .q(included)), if (length(excluded)) 
        paste0("--exclude=", .q(excluded)), if (length(ssh) > 1) 
        c("-e", .q(paste("ssh -p", " ", ssh["port"], sep = ""))), if (dry) 
        "-n", if (length(files_from)) 
        files_from, if (ssh.side == 2) 
        paste0("--rsync-path=", .q(paste0("mkdir -p ", .sys.fpGood(wjj2, home.expand = 0), "; rsync"))), if (length(size_max)) 
        paste0("--max-size=", size_max), if (length(size_min)) 
        paste0("--min-size=", size_min), bf_wjj0 %=>% {
        if (length(x)) {
            c("-b", paste0("--backup-dir=", hs.switch(ssh.side, 0, , 1, bf_wjj0 %=>% hs.fp(hs.uid.fp(base_nyr = 0)) %=>% normalizePath %=>% hs.sub("/*$", "/") %=>% .q, 2, .sys.fpGood(hs.fp(bf_wjj0, hs.uid.fp(base_nyr = 0)), home.expand = 0, qn = 1))))
        }
    }, additional, if (length(ext)) 
        paste("-f", c(.q("+ */"), tolower(ext) %=>% strsplit("") %=>% sapply(x, . %=>% {
            paste0("[", x, toupper(x), "]") %=>% {
                paste(c("+ *.", x), collapse = "")
            }
        }) %=>% .q, .q("- *"))), hs.switch(ssh.side, 0, , 2, .sys.fpGood(wj1), 1, paste0(ssh.str, .sys.fpGood(wj1, home.expand = 0, qn = switch(osType(), Darwin = 1, 1)))), hs.switch(ssh.side, 0, , 1, .sys.fpGood(hs.wjj(wjj2), home.expand = 0), 2, paste0(ssh.str, .sys.fpGood(wjj2, home.expand = 0, qn = 1))), if (!show_wjj) 
        "| grep -v", .q("/$"))
    if (0) {
        hs.browser("38.07.15.092108")
        message(cmd)
        .wk(cmd, "| less -Ni")
    }
    .wk(cmd, wait = wait)
    if (cat.cmd) 
        cat("cmd is", "\n", "\t", cmd, "\n")
}
wk.rsync.in <- function(wj1 = {
}, wjj2 = c("~/jz002", "/Volumes/bf4wk@mpb", "/media/wk/sda1-usb-WD_Elements_25A1", "/media/wk/sdb1-usb-WD_Elements_25A1", "/Volumes/in2") %=>% x[file.exists(x)] %=>% {
    if (length(x)) 
        x[1]
    else hs.home.expand("~/tmp")
}, ssh = wk.ssh, ssh.side = 1, type = "wj", ...) {
    if (!length(wj1)) {
        if (0) {
            wk.rsync("/", wjj2, ssh = ssh, ssh.side = ssh.side, additional = paste0("--files-from=:", .qq(paste0("<( gzip -cd ", wk.rsync.wj.wj, ")"))), ...)
            .wk("cat", "<( gzip -cd ", wk.rsync.wj.wj, ")")
            wk.rsync.wj.wj %=>% .head
            return()
        }
        wk.rsync.in(wk.rsync.wj.wj, wjj2, ssh = ssh)
        wj.wj <- hs.wjm(wk.rsync.wj.wj, dn = wjj2)
        on.exit(hs.wj.rm(wj.wj), add = TRUE)
        wj1 <- readLines(wj.wj)
    }
    if (0) {
        bn <- 1
        while (anyDuplicated(hs.basename(wj1, bn))) {
            bn <- bn + 1
        }
    }
    a <- if (length(wj1) > 1) {
        hs.hsgly("文件/名/公私.39_05_12_131245")(wj1)[["private"]]
    }
    else list(basename(wj1))
    sapply(seq_along(wj1), function(i1) {
        wj1 <- wj1[i1]
        wjj2 <- c(wjj2, a[[i1]]) %=>% head(-1) %=>% do.call(hs.fp, as.list(x))
        wk.rsync(wj1, wjj2, type = type, ssh = ssh, ssh.side = ssh.side, ...)
        hs.wjm(wj1, dn = wjj2)
    })
}
wk.rsync.in.same_place <- function(wj = {
}, ssh = wk.ssh, recursive = 1, U = 1, wj.wj = wk.rsync.wj.wj, home.only = 1, ...) {
    if (!length(wj)) {
        wk.rsync.in.same_place(wj.wj)
        wj <- readLines(wj.wj)
    }
    for (wj1 in wj) {
        wjj2 <- if (hs.grepl(wj1, "/$")) 
            wj1
        else hs.wjj(substr(wj1, 1, nchar(wj1) - nchar(basename(wj1))))
        wk.rsync.in(wj1, wjj2, ssh = ssh, recursive = recursive, U = U, home.only = home.only, type = ifelse(hs.grepl(wj1, "/$"), "wjj", "wj"), ...)
    }
}
wk.rsync.out <- function(wj1 = {
}, wjj2 = hs.home.expand("~/tmp"), ssh = {
}, ssh.side = 2, recursive = {
}, checksum = 0, U = 0, type = {
}, wj1.type = "auto", ...) {
    if (hs.any(!length(wjj2), !length(ssh))) {
        wj1_tmp <- .Last.value
        if ((!length(wj1)) && file.exists(wj1_tmp)) 
            wj1 <- wj1_tmp
        if (identical(type, "doc")) {
            if (hs.len1(wj1) && dir.exists(wj1)) {
                wj1 <- hs.wjj.ls(wj1, "\\.(pdf|png|xlsx)", R = 1)
                if (any(duplicated(basename(wj1)))) 
                  stop("【36.04.28.122010】有重复的文件名")
            }
            else hs.browser("36.04.17.071257")
        }
        cat(wj1, sep = "\n", file = paste("| gzip >", wk.rsync.wj.wj))
        return(wk.rsync.wj.wj)
    }
    sapply(wj1, function(wj1) {
        if (!file.exists(wj1)) 
            stop("36.04.08.134538 文件\"", wj1, "\"不存在")
        if (wj1.type %in% "auto") 
            wj1.type <- ifelse(hs.grepl(wj1, "/$"), "wjj", "wj")
        hs.with_wd(switch(wj1.type, wjj = wj1, wj = dirname(wj1)), {
            if (!length(recursive)) 
                recursive <- hs.isDir(wj1)
            wk.rsync(switch(wj1.type, wjj = "./", wj = basename(wj1)), wjj2, ssh = ssh, ssh.side = ssh.side, recursive = recursive, checksum = checksum, U = U, type = wj1.type, ...)
        })
    })
}
wk.rsync.out.same_place <- function(wj, ssh = wk.ssh, ssh.side = 2, U = 1, wj.wj = wk.rsync.wj.wj, home.only = 1, ...) {
    if (home.only) 
        wj %<=>% hs.wj.portable
    for (wj1 in wj) {
        wjj2 <- if (hs.grepl(wj1, "/$")) 
            wj1
        else hs.wjj(substr(wj1, 1, nchar(wj1) - nchar(basename(wj1))))
        if (!file.exists(wj1)) 
            stop(sprintf("文件（%s）不存在", wj1))
        wk.rsync(wj1, wjj2, ssh = ssh, ssh.side = ssh.side, recursive = dir.exists(wj1), U = U, home.only = home.only, ...)
    }
}
wk.rsync.wjj.local <- function(wjj1, wjj2, wj.log = .wk("date", "+%Y.%m.%d_%H.%M.%S", intern = 1) %=>% str_c("~/rsync.", x, ".log"), linux = 1, del = 1, dry = 1, method = "", wjj1.type = "wjj", u = 1, others = "") {
    switch(wjj1.type, wjj = {
        if (basename(wjj1) != basename(wjj2)) stop("basenames of wjj1 and wjj2 should be the same")
        wjj1 %<=>% wk.re("[/\\\\]*$", "replace", replacement = "/", all = 1)
    }, wj = {
        wjj1 %<=>% wk.re.replace("[/\\\\]*$", "")
    })
    .wk("rsync", "-rtvvh", "--force", "--inplace", "--progress", if (method == "chksum") 
        "-c", if (linux) 
        "-ogplD", if (del) 
        "--del", if (u) 
        "-u", if (dry) 
        "-n", wjj1 %=>% wk.re.replace("/*$", "/"), wjj2 %=>% wk.wjj, if (nchar(wj.log)) 
        c(">", wj.log %=>% path.expand %=>% .q), others)
    wj.log
}
wk.rsync.wjj.myWay <- function(wjj1, wjj2, wj.log = .wk("date", "+%Y.%m.%d_%H.%M.%S", intern = 1) %=>% str_c("~/rsync.", x, ".log"), linux = 1, del = 1, dry = 1, method = "", wjj1.type = "wjj", u = 1, others = "", way = "out") {
    wjj2.1 <- wk.wjj(wjj2, basename(wjj1))
    wjj.current <- wjj2.1 %=>% wk.wj("current")
    cmd1 <- .sys("rsync", "-rtuvh", "--force", "--inplace", "--progress", "--del", if (method == "chksum") 
        "-c", if (linux) 
        "-ogplD", if (dry) 
        "-n")
    if (.wk("find", wjj2.1 %=>% wk.re.replace("/*$", "/") %=>% path.expand %=>% .q, "| head", intern = 1) %=>% hs.len1) {
        .wk(cmd1, wjj1 %=>% .sys.fpGood %=>% wk.re.replace("/*$", "/"), wjj.current %=>% wk.wjj %=>% .sys.fpGood)
    }
    else {
        wjj.bf <- Sys.time() %=>% format("%Y.%m.%d.%H%M%S") %=>% wk.re.replace("\\d+", as.numeric(format(Sys.time(), "%Y")) - 1984, all = 0) %=>% wk.wjj(wjj2.1, x)
        cmd2 <- str_c("--backup --backup-dir=", wjj.bf %=>% .sys.fpGood)
        {
            wj.ref <- {
                a <- list.files(wjj2.1) %=>% (`%not%`(c("current", basename(wjj.bf))))
                a[file.info(wk.wj("..", a))[, "mtime"] %=>% order %=>% tail(1)] %=>% wk.wj(wjj2.1, x)
            }
            if (length(wj.ref)) {
                1
                {
                  setwd(wjj.current)
                  wj.new <- .wk("find", "-newer", wj.ref, intern = 1) %=>% {
                    wj.new <- x
                    wj.new %=>% wk.wj(wjj1, x) %=>% file.exists %=>% wj.new[!x]
                  }
                  if (length(wj.new)) {
                    wk.browser("34.04.19.220858")
                    for (wj1 in wj.new) {
                      .wk("cp", "-a", wj1 %=>% .sys.fpGood, wj1 %=>% wk.wj(wjj1, x) %=>% dirname %=>% wk.wjj %=>% .sys.fpGood)
                    }
                  }
                }
                {
                  setwd(wjj1)
                  wj.old <- .wk("find", "! -newer", wj.ref, "-print", intern = 1) %=>% {
                    a <- x
                    a %=>% wk.wj(wjj.current, x) %=>% file.exists %=>% {
                      a[!x]
                    }
                  }
                  wj.old %x>% {
                    if (length(x)) {
                    }
                  }
                }
            }
        }
        .wk(cmd1, cmd2, wjj1 %=>% .sys.fpGood %=>% wk.re.replace("/*$", "/"), wjj.current %=>% .sys.fpGood, ">>", wj.log %=>% .sys.fpGood)
        .wk(cmd1, cmd2, wjj.current %=>% .sys.fpGood %=>% wk.re.replace("/*$", "/"), wjj1 %=>% .sys.fpGood, ">>", wj.log %=>% .sys.fpGood)
        .wk("touch", wjj.bf %=>% .sys.fpGood)
        wj.log
    }
}
wk.rsync.sync2way <- function(wjj1, wjj2, type = "wjj", mode = "sync") {
    wk.rsync(wjj1, wjj2, type = type, del = 1)
}
wk.get_open <- local({
    wj_prev <- ""
    function() {
        wj1 <- .wk("xsel --clipboard --output", intern = 1)
        if (hs.len1(wj1)) {
            wk.rsync.in(wj1, "/dev/shm")
            wj_prev <<- hs.fp("/dev/shm", basename(wj1))
        }
        .wk(hs.home.expand("~/org/dm/sh/rj/o"), .sys.fpGood(wj_prev))
    }
})
ieiwk.sync.wjj2do <- function() {
    file.path("/cygdrive", "d", c("wk", "wk.os", "rjk"))
}
ieiwk.sync.wjj.mobile <- function(fx) {
    flag <- 1
    for (wjj1 in ieiwk.sync.wjj2do()) {
        wjj2 <- sub("\\/d\\/", "/m/", wjj1)
        if (flag) {
            .cat.time()
            flag <- 0
        }
        .cat("doing ", wjj1)
        switch(fx, come = ieiwk.sync.wjj(wjj2, wjj1), go = ieiwk.sync.wjj(wjj1, wjj2))
        .cat.time()
    }
}
wk.rsync.safe <- function(dz1, wjj2 = ".", co1 = 30, update = FALSE) {
    if (normalizePath(wjj2) != getwd()) {
        wjj.36_03_04_003344 <- getwd()
        on.exit(setwd(wjj.36_03_04_003344), add = TRUE)
        hs.wjj(wjj2, go = 1)
    }
    wj_obtained <- paste0(basename(dz1), ".obtained")
    if (file.exists(wj_obtained) && update && wj.update.reporter(basename(dz1), co1 = co1)) {
        hs.wj.rm(wj_obtained)
    }
    if (file.exists(wj_obtained)) {
    }
    else {
        if (hs.grepl(dz1, "^(ftp|https?):")) {
            dz1 <- hs.sub(dz1, "(ftp|https?)", "rsync")
        }
        msg <- 1
        msg <- .try(.wk("rsync -ahPv --no-motd", dz1, "."))
        if (!msg) {
            cat("", file = wj_obtained)
        }
    }
    normalizePath(wj_obtained)
}
.s <- function(..., max.print = 99999, width = 80, h = 100, w = 10000, file = hs.home.expand("~/tmp.r.sink.txt"), O = 0, append = TRUE, type = c("output", "message"), split = FALSE, print.result = 1) {
    op <- options(max.print = max.print, width = w)
    on.exit(options(max.print = h, width = width), add = TRUE)
    if (file != hs.home.expand("~/tmp.r.sink.txt")) 
        O <- 1
    if (O) 
        sink(file, append, type, split)
    r <- list(...)
    if (print.result) 
        invisible(sapply(r, print))
    if (O) 
        sink()
    invisible()
}
.s.no.blank.line <- function(...) {
    b <- capture.output(.s(...))
    b.1 <- grep("^(\\s+)*$", b)
    if (length(b.1)) 
        b <- b[-b.1]
    cat(b, sep = "\n")
}
.s1 <- .s.no.blank.line
.sh.no.blank.line <- function(...) {
    b <- capture.output(.sh(...))
    b.1 <- grep("^(\\s+)*$", b)
    if (length(b.1)) 
        b <- b[-b.1]
    cat(b, sep = "\n")
}
.sh1 <- .sh.no.blank.line
.s.h <- function(x, how.many = 3) {
    .s(.h(x, how.many))
}
.sh <- function(..., max.print = 99999, width = 80, h = 50, w = 120, file = NULL, append = FALSE, type = c("output", "message"), split = FALSE) {
    op <- options(max.print = max.print, width = w)
    on.exit(options(op), add = TRUE)
    if (is.null(file)) {
        sapply(list(...), print)
    }
    else {
        sink(file, append, type, split)
        sapply(list(...), print)
        sink()
    }
    invisible()
}
.sh.h <- function(x, how.many) {
    .sh(.h(x, how.many))
}
.cat.inside.function.31.10.20.111835 <- function(file.function = "", file.out = hs.home.expand("~/31.10.20.111907.txt"), append = TRUE) {
    a <- sys.calls()
    .cat(paste(a[[length(a) - 1]])[1], get(file.function, envir = .GlobalEnv), sep = "\t", file = file.out, append = append)
}
.c <- function(..., file = "", sep = " ", end = "\n", fill = FALSE, labels = NULL, append = FALSE, intern = FALSE) {
    if (intern) 
        return(capture.output(cat(..., file = "", sep = sep, fill = fill, labels = labels, append = append)))
    if (is.character(file)) 
        if (file == "") 
            file <- stdout()
        else if (substring(file, 1L, 1L) == "|") {
            file <- pipe(substring(file, 2L), "w")
            on.exit(close(file))
        }
        else {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file))
        }
    .Internal(cat(list(...), file, sep, fill, labels, append))
    .Internal(cat(list(end), file, sep, fill, labels, append))
}
.jc <- function(..., sep = "") str_c(c(...), collapse = sep)
.cat.time <- function(intern = FALSE, file = "", append = 1) hs.say("    **** ", paste(Sys.time()), " ****", intern = intern, file = file, append = append)
.cat4browser <- function(a) {
    .cat("****    browsing at ", a)
    a
}
.cat.exp <- defmacro(DOTS, expr = {
    invisible(sapply(list(...), function(x) {
        .cat(x, " =>")
        .cat(eval(parse(text = x)))
    }))
})
println <- catline <- .ss <- function(..., file = "", sep = "\n", fill = FALSE, labels = NULL, append = FALSE) {
    cat(..., file = file, sep = sep, fill = fill, labels = labels, append = append)
}
report <- function(x, file = "tmp") {
    sink(file)
    .s(x)
    sink()
}
ieiwk.s <- function(..., file = hs.home.expand("~/tmp.txt"), append = FALSE, sep = "\n") {
    sink(file, append)
    sapply(list(...), try)
    on.exit(sink())
}
ieiwk.s.list.in.row <- function(a, sep = "|") {
    if (is.null(names(a))) 
        names(a) <- seq.int(length(a))
    r <- c()
    ir <- sapply(names(a), function(x) {
        x <- a[x]
        a <- capture.output(.s(x))
        for (i in seq.int(max(nchar(a)))) {
            b <- substr(a, i, i)
            r <- rbind(r, ifelse(nchar(b), b, " "))
        }
        r <<- rbind(r, rep(sep, ncol(r)))
        max(nchar(a))
    })
    if (all(r[-sapply(seq_along(ir), function(i) {
        sum(ir[1:i]) + i
    }), ncol(r)] == " ")) 
        r[sapply(seq_along(ir), function(i) {
            sum(ir[1:i]) + i
        }), ncol(r)] <- paste(rep(" ", nchar(sep)), collapse = "")
    apply(r, 2, paste, collapse = "")
}
.str <- function(object, list.len = 3, ...) {
    str(object, list.len = list.len, ...)
}
ieiwk.cat.line <- function(v, sep = "\n", ...) {
    cat(v, "", sep = sep, ...)
}
ieiwk.cat.headline <- function(x, l = "_", r = "#") cat(sprintf("%s\\ %s /%s\n", rep(l, 9) %,% "", x, rep(r, 9) %,% ""))
hs.save <- ieiwk.save <- function(..., blm, file, wj, nt = 8, bin = wk.gz.good(), cl = 6, env1 = parent.frame()) {
    if (missing(blm)) 
        blm <- as.character(substitute(list(...)))[-1L]
    if (!{
        all(is.character(blm)) && all(substr(blm, 100, 100) == "")
    }) 
        stop("need a variable name")
    if (missing(wj)) 
        wj <- file
    hs.wj(wj)
    con1 <- pipe(.sys(switch(bin, pbgzip = .sys.pbgzip.in(n = nt), pigz = c("pigz -p", nt, "-c"), gzip = "gzip", browser("33.08.05.222656")), "2>&1 >", .sys.fpGood(wj)), "wb")
    on.exit(close(con1), add = TRUE)
    if (length(blm) == 1) {
        evalbq(do.call("saveRDS", list(object = .(as.symbol(blm)), file = .(con1))))
    }
    else save(list = blm, file = con1, envir = env1)
}
ieiwk.save.image <- function(file = c("image", readline("Input an ID: "), "RData") %,% ".", version = NULL, ascii = FALSE, compress = FALSE, safe = TRUE) {
    save.image(file, version, ascii, compress, safe)
}
hs.load.image <- function(f1 = ".RData", env = .GlobalEnv) {
    load(f1, envir = env)
}
ieiwk.where2seek4parallel <- function(f, chunk.size = 1e+07, n = {
}, skip = 0, skip.pattern) {
    con <- file(f)
    open(con)
    if (skip) {
        readLines(con, skip)
    }
    i <- seek(con)
    f.s <- .sys.stat.size(f)
    if (!is.null(n)) 
        chunk.size <- (f.s - i)/n
    while ({
        j <- seek(con)
    } < f.s) {
        seek(con, where = j + chunk.size)
        readLines(con, 1)
        i <- append(i, seek(con))
    }
    i[length(i)] <- f.s
    on.exit(close(con))
    cbind(head(i, -1), diff(i))
}
wk.fread.txtFromXls <- function(wj, header.ms1 = "^[^\t]\t", n2try = 100, ...) {
    n1 <- readLines(wj, n2try) %=>% hs.grep(header.ms1)
    wk.fread(wj, skip = n1 - 1, ...)
}
wk.gz.good <- local({
    bin <- {
    }
    function() {
        if (length(bin)) {
            return(bin)
        }
        wj.tmp <- tempfile()
        bin1 <- "gzip"
        for (bin1 in (bin %or% c("pbgzip", "pigz", "bgzip", "gzip"))) {
            unlink(wj.tmp)
            .try({
                con1 <- .sys(bin1, "-c", "2>/dev/null", ">", wj.tmp) %=>% pipe("w")
                cat(letters, sep = "\n", file = con1)
                close(con1)
            })
            if (hs.wj.exists(wj.tmp, size = 9)) {
                bin <<- bin1
                break
            }
        }
        unlink(wj.tmp)
        bin
    }
})
wk.fread.gz <- ieiwk.fread.gz <- function(wj.gz, bin = wk.gz.good(), N = {
}, n = N, ...) {
    wk.fread(cmd = .sys(switch(bin, pigz = .sys.pigz(), gzip = c("gzip -cd"), pbgzip = .sys.pbgzip.out()), wj.gz, if (length(n)) 
        c("|", .sys.head(n, pipe = 0))), ...)
}
ieiwk.fread.pipe <- function(input, sep = "auto", sep2 = "auto", nrows = -1L, header = "auto", na.strings = "NA", stringsAsFactors = FALSE, verbose = getOption("datatable.verbose"), autostart = 1L, skip = 0L, select = NULL, drop = NULL, colClasses = NULL, integer64 = getOption("datatable.integer64"), dec = if (sep != ".") "." else ",", col.names, check.names = FALSE, encoding = "unknown", strip.white = TRUE, showProgress = getOption("datatable.showProgress"), data.table = getOption("datatable.fread.datatable"), 
    chunk = 1e+09) {
    h1 <- list()
    i1 <- 1
    while (length(s1 <- readChar(input, chunk))) {
        .ss(i1)
        i1 <- i1 + 1
        s2 <- readLines(input, 1)
        s <- ifelse(length(s2), sprintf("%s%s", s1, s2), s1)
        h1[[length(h1) + 1]] <- ieiwk.fread(input = s, sep = sep, sep2 = sep2, nrows = nrows, header = header, na.strings = na.strings, stringsAsFactors = stringsAsFactors, verbose = verbose, autostart = autostart, skip = skip, select = select, drop = drop, colClasses = colClasses, integer64 = integer64, dec = dec, col.names = col.names, check.names = check.names, encoding = encoding, strip.white = strip.white, showProgress = showProgress, data.table = FALSE)
    }
    rbind %(% h1
}
wk.fwrite <- function(x, wj = "", append = FALSE, quote = "auto", sep = "\t", sep2 = c("", "|", ""), eol = if (.Platform$OS.type == "windows") "\r\n" else "\n", na = "", dec = ".", row.names = 0, col.names = !append, qmethod = c("double", "escape"), logicalAsInt = 0, dateTimeAs = c("ISO", "squash", "epoch", "write.csv"), buffMB = 8L, nThread = getDTthreads(), showProgress = TRUE, verbose = getOption("datatable.verbose"), compress = grepl("\\.gz$", wj)) {
    if (!is.list(x)) {
        x <- if (is.matrix(x)) {
            wk.dt.as(x, keep.rownames = as.logical(row.names))
        }
        else if (is.vector(x)) {
            list(x)
        }
        else {
            hs.browser("33.07.03.173719")
            stop("x不是列表、矩阵或向量")
        }
    }
    if (is.numeric(quote)) 
        quote <- as.logical(quote)
    fwrite(x = x, file = wj, append = as.logical(append), quote = quote, sep = sep, sep2 = sep2, eol = eol, na = na, dec = dec, row.names = FALSE, col.names = as.logical(col.names), qmethod = match.arg(qmethod), logicalAsInt = as.logical(logicalAsInt), dateTimeAs = match.arg(dateTimeAs), buffMB = buffMB, nThread = nThread, showProgress = showProgress, verbose = verbose)
    wj
}
wk.fwrite.gz <- function(x, wj, ...) {
    wk.fwrite(x, wj, ..., compress = 1)
}
ieiwk.write.tsv <- write.tsv <- function(x, file = "", append = 0, quote = 0, sep = "\t", eol = "\n", na = "NA", dec = ".", row.names = 0, col.names = 0, qmethod = c("escape", "double"), compress = 0) {
    {
        row.names <- as.logical(row.names)
        col.names <- as.logical(col.names)
        append <- as.logical(append)
        quote <- as.logical(quote)
    }
    if (row.names && col.names) {
        x <- cbind(rownames(x), x)
        row.names = FALSE
    }
    con <- if (compress) 
        pipe(sprintf("pigz -p8 -6 >%s %s.gz", ifelse(append, ">", ""), file))
    else file
    write.table(x, con, append, quote, sep, eol, na, dec, row.names, col.names, qmethod)
    ifelse(compress, hs.wjm(file, add = "gz"), file)
}
wk.writeLines <- function(text, file, append = file.exists(file), sep = "\n", quote = FALSE, ...) {
    wk.fwrite(as.list(text), wj = file, append = append, sep = sep, quote = quote, ...)
}
ieiwk.write.tsv.gz <- function(x, ...) {
    ieiwk.write.tsv(x, ..., compress = 1)
}
ieiwk.smart.sep <- function(f1, sep2try = c(","), quote = "\"", dec = ".", fill = TRUE, comment.char = "#", as.is = TRUE, skip = FALSE, ...) {
    {
        s1 <- "\t"
        if (grepl(s1, readLines(f1, skip + 1))) 
            return(s1)
    }
    a <- ieiwk.sapply(sep2try, function(s1) {
        read.csv(f1, header = FALSE, s1, quote, dec, fill, comment.char, as.is = as.is, nrows = 99, ...)
    })
    clg(sapply(names(a), function(s1) {
        a1 <- a[[s1]]
        flag <- 1
        ncol(a1) == 1
        if (nchar(a1[1, ncol(a1)]) == 0 & any(nchar(a1[-1, ncol(a1)]) != 0)) 
            flag <- 0
        flag
    }), 1)
}
wk.do.con.. <- function(con1, e1, ..., s1, env = parent.frame()) {
    e1 <- substitute(e1)
    repeat {
        assign(s1, {
        }, envir = env)
        assign(s1, wk.read.chunk(con1, ...), envir = env)
        if (!length(get(s1, envir = env))) 
            break
        eval(e1, env)
    }
}
ieiwk.rt <- function(f1) {
}
wk.read.csv <- ieiwk.read.table <- ieiwk.read.csv <- function(file, header = TRUE, sep = ",", sheet = 1, skip = 0, smart.sep = TRUE, quote = "\"", dec = ".", fill = TRUE, comment.char = "#", as.is = TRUE, row.names = {
}, ...) {
    if (local({
        bin <- hs.home.expand("~/wk/my/rj2/cygwin64/bin/file.exe")
        if (file.exists(bin)) {
            a <- .try(.sys.do(ifelse(osType() == "Windows", hs.home.expand("~/wk/my/rj2/cygwin64/bin/file.exe"), "file"), path.expand(file), intern = 1))
            re("[^:]+$", a) %=>% trim %=>% tolower %=>% grepl("microsoft excel", x)
        }
        else {
            tolower(ieiwk.filename(file)) %in% c("xlx", "xlxs")
        }
    })) {
        require(readxl)
        read_excel(file, sheet = sheet, col_names = header, skip = skip)
    }
    else {
        if (smart.sep) {
            sep <- ieiwk.smart.sep(file)
        }
        a <- read.table(file = file, header = FALSE, sep = sep, quote = quote, dec = dec, fill = fill, comment.char = comment.char, row.names = {
        }, as.is = as.is, skip = skip + as.integer(header), ...)
        if (header) 
            colnames(a) <- as.matrix(read.table(file = file, header = FALSE, sep = sep, quote = quote, dec = dec, fill = fill, comment.char = comment.char, row.names = {
            }, skip = skip, nrow = 1, colClasses = "character", ...))
        if (hs.len1(row.names) && is.numeric(row.names) && (row.names > 0)) {
            rownames(a) <- as.matrix(a[, row.names])
            a <- a[, -row.names, drop = FALSE]
        }
        a
    }
}
wk.readTableExtra <- function(wj, sep = "\t", header = 1) {
    a <- readLines(wj)
    i1 <- wk.re(a, sep, "locate", all = 1)
    b <- wk.re(a, act = "split", info = i1 %=>% lapply(wk.attr.hs, . %=>% head(x, length(i1[[1]]))))
    if (header) {
        b[-1] %=>% {
            rbind %(% x
        } %=>% data.table %=>% setnames(b[[1]])
    }
    else b %=>% {
        rbind %(% x
    } %=>% data.table
}
ieiwk.readxl <- function(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0) {
    require(readxl)
    read_excel(path = path, sheet = sheet, col_names = col_names, col_types = col_types, na = na, skip = skip)
}
ieiwk.xls.list.sheetNames <- function(wj1) {
    if (require(readxl)) {
        excel_sheets(path.expand(wj1))
    }
    else warning("install readxl first")
}
wk.xls.read <- ieiwk.xls.read <- function(wj1, sheet = 1, skip = 0, cn = 1, header = cn, as.dt = !rn, rn = 0, ...) {
    sn <- ieiwk.xls.list.sheetNames(wj1)
    hs.switch(sheet, 0, sn, "", {
        wk.xls.read(wj1, seq_along(sn), skip = skip, header = header, as.data.table = as.dt, ...)
    }, {
        i1 <- which(is.character(sheet))
        if (length(i1)) 
            sheet[i1] <- match(sheet[i1], sn)
        sheet %<=>% as.numeric
        r <- lapply(sheet, . %=>% {
            readxl::read_excel(wj1, x, skip = skip, col_names = as.logical(header), ...) %=>% {
                if (as.dt) {
                  wk.dt.as(x)
                }
                else {
                  if (rn) 
                    hs.df(x, rn = rn)
                  else x
                }
            }
        })
        if (hs.len1(sheet)) {
            r <- r[[1]]
        }
        else names(r) <- sn[sheet]
        r
    })
}
wk.xls.write <- function(bg, wj, sheetName = "Sheet1", cn = 1, rn = 0, append = 0, file = wj, widths = 8.43, ...) {
    wj <- path.expand(file)
    {
        hs.wj.rm(wj)
    }
    if (is.matrix(bg)) 
        bg <- wk.dt.as(bg, keep.rownames = "")
    openxlsx::write.xlsx(bg, hs.wj(file), rowNames = as.logical(rn), colNames = as.logical(cn), colWidths = widths)
    wj
}
wk.2xls <- local({
    hs.require("openxlsx")
    function(w1 = ".", ow = 0) {
        if (dir.exists(w1)) 
            w1 <- hs.wjj.ls(w1, "\\.(tsv|csv)(\\.gz)?$", R = 1)
        wk.mclapply(w1, function(w1) {
            w2 <- hs.sub(w1, "\\.(tsv|csv)(\\.gz)?$", ".xlsx")
            if ((!file.exists(w2)) || ow) 
                fread(w1) %=>% openxlsx::write.xlsx(w2)
            w2
        }) %=>% unlist
    }
})
read.tsv <- function(file, header = 1, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "", as.is = TRUE, ...) {
    read.csv(file, header, sep, quote, dec, fill, comment.char, as.is = as.is, ...)
}
ieiwk.line.sep.good <- function(f1) {
    f1 <- .sys.fpGood.file(f1)
    if (.file(f1) == "ASCII text, with CRLF line terminators") 
        .sys.do("sed -i", shQuote("s/\r//"), f1)
}
wk.r2sys <- function(lines = letters, cmd = "head", cmd.init = "cat", intern = 0) {
    wj1 <- hs.wj.temp()
    zz <- fifo(wj1, "w+")
    on.exit({
        close(zz)
        unlink(wj1)
    }, add = TRUE)
    writeLines(lines, zz)
    .wk(cmd.init, wj1, "|", cmd, "&", intern = intern)
}
wk.sys2sys <- function(cmd1, cmd2 = "head", cmd.init = "cat", intern = 0) {
    wj1 <- ieiwk.tempfile()
    zz <- fifo(wj1, "w+")
    on.exit({
        close(zz)
        unlink(wj1)
    }, add = TRUE)
    browser("33.07.02.154349")
    .wk(cmd1, ">", wj1, "&")
    .wk(cmd.init, wj1, "|", cmd2, intern = intern)
}
hs.bgz <- function(wj1, wj2 = paste0(wj1, ".bgz")) {
    .wk("zcat", wj1, "| pbgzip -c >", wj2)
    if (hs.grepl(wj1, "\\.bgz$")) {
        file.rename(wj2, wj1)
    }
    else hs.wj.rm(wj1)
    wj2
}
cn.hs.wj <- function(wj1, skip.comment = "auto") {
    size1 <- 1000
    repeat {
        line <- readLines(wj1, size1)
        if (substr(line[1], 1, 1) == "#") {
            i1 <- which(substr(line, 1, 1) == "#")
            i2 <- which(diff(i1) > 1)
            if (hs.len1(i2, 0)) 
                i2 <- tail(i1, 1)
            if (i2 == length(line)) {
                size1 <- size1 + 1000
                if (size1 > 10000) {
                  stop("37_03_09_125412")
                }
                next
            }
            if (is.character(skip.comment)) {
                switch(skip.comment, auto = {
                  a1 <- readLines(wj1, i2 + 1)
                  skip.comment <- length(strsplit(a1[i2], "\t")[[1]]) < length(strsplit(a1[i2 + 1], "\t")[[1]])
                })
            }
            else {
                if (is.numeric(skip.comment)) 
                  skip.comment <- as.logical(skip.comment)
            }
            if (skip.comment) 
                i2 <- i2 + 1
            line1 <- line[i2]
            break
        }
        else {
            line1 <- line[1]
            break
        }
    }
    strsplit(line1, "\t")[[1]]
}
text.cmd.text <- function(L, cmd1, reader = function(con1) fread(text = readLines(con1), header = FALSE)) {
    fifo1 <- hs.wj.temp()
    .wk("mkfifo", fifo1)
    in1 <- pipe(.sys("cat <", fifo1, "|", cmd1, "&"), "r")
    out1 <- fifo1
    hs.browser("37.03.05.184231")
    cat(L, sep = "\n", file = out1)
    close(out1)
    jg <- reader(in1)
    close(in1)
    hs.wj.rm(fifo1)
    jg
}
wk.wget2wj <- function(dz1, wj2, redo = 0, gzip = !hs.grepl(wj2, "(?i)\\.gz$"), extra = "") {
    if (redo) 
        hs.wj.rm(wj2)
    if (!file.exists(wj2)) {
        .wk("wget -O -", extra, .q(dz1), if (gzip) 
            "| gzip", ">", .sys.fpGood(hs.wj(wj2)))
    }
    wj2
}
hs.file_system_is_linkable <- function(wjj1) {
    hs.with_wd(wjj1, {
        wj1 <- "39_01_29_102630"
        wj2 <- paste0(wj1, ".lnk")
        hs.wj.rm(wj2)
        cat("", file = wj1)
        a <- file.symlink(wj1, wj2)
        hs.wj.rm(c(wj1, wj2))
        a
    })
}
.jl <- function(wj.org = hs.home.expand("~/org/文件历史.org")) {
    wj1 <- osType() %=>% hs.switch("Darwin", "pbpaste", "xsel --clipboard --output") %=>% .wk(intern = 1) %=>% if ((length(x) == 1) && file.exists(x)) 
        hs.wjm.home(x)
    if (length(wj1)) {
        old <- if (file.exists(wj.org)) 
            wk.fread(wj.org, colClasses = "character")
        else rep("", 2) %=>% t %=>% wk.dt.as(x)[-1]
        i <- old[, match(wj1, V1, nomatch = 0)]
        current <- if (i) {
            Sys.time() %=>% paste(old[i, V2], sep = "; ") %=>% list(wj1, x) %=>% wk.dt.as %=>% rbind(old[-i])
        }
        else {
            list(wj1, Sys.time() %=>% paste) %=>% wk.dt.as %=>% rbind(old)
        }
        wk.dt.fwrite(current, wj.org, cn = 0)
    }
    .less(wj.org)
    wj.org
}
wk.dt <- function(..., check.names = FALSE, key = NULL, stringsAsFactors = FALSE, sorted = TRUE, value.name = "value", na.rm = TRUE, as = 0, keep.rownames = "row_name") {
    if (...length()) {
        i <- match("x", ...names(), nomatch = 1)
        if (is.array(...elt(i)) && is.null(rownames(...elt(i)))) 
            keep.rownames <- FALSE
    }
    if (as) {
        data.table::as.data.table(..., keep.rownames = keep.rownames, key = key, sorted = sorted, value.name = value.name, na.rm = na.rm)
    }
    else data.table::data.table(..., keep.rownames = keep.rownames, check.names = check.names, key = key, stringsAsFactors = stringsAsFactors)
}
wk.dt.as <- data.table::as.data.table
wk.dt.uniq_n <- data.table::uniqueN
wk.dt.setattr <- function(x, name = {
}, value) {
    if (length(name)) 
        data.table::setattr(x, name, value)
    else attributes(x)
}
wk.dt.copy <- data.table::copy
wk.dt.absorb <- function(dt1, ..., env1 = parent.frame()) {
    for (dt2 in list(...)) {
        if ((if (is.list(dt2)) 
            dt2[[1]]
        else nrow(dt2)) %not.in% c(dt1[, .N], 1)) {
            stop("[36.02.04.153623]")
        }
        if (!is.list(dt2)) 
            dt2 <- data.table::as.data.table(dt2)
        dt1[, `:=`(names(dt2), dt2)]
    }
    dt1
}
wk.dt.is_clean <- function(dt, by = {
}, ...) {
    a <- if (length(by)) {
        dt[, lapply(.SD, hs.clean, ot = "l", ...), .SDcols = c(by)]
    }
    else dt[, lapply(.SD, hs.clean, ot = "l", ...)]
    hs.rowAlls(a)
}
wk.dt.absorb1.core <- local({
    hs.matcher <- function(c1, c2, ic) if (ic && (is.character(c1[1]) || is.character(c2[1]))) {
        hs.str_match(c1, c2, ic = TRUE, nomatch = 0L)
    }
    else match(c1, c2, nomatch = 0L)
    hs.eq <- function(c1, c2, ic) {
        if (ic && (is.character(c1[1]) || is.character(c2[1]))) {
            c1 <- tolower(c1)
            c2 <- tolower(c2)
        }
        c1 == c1
    }
    function(dt1, dt2, by = (names(dt1) %and% names(dt2))[1], by1 = if (length(dt1) == 1) 
        names(dt1)
    else by, by2 = by, what = names(dt2) %not% by2, as = what, ic = FALSE, ri2do, old = "replace", sep = ";") {
        if (!length(ri2do)) {
            ri2do <- which(wk.dt.is_clean(dt1, by = by1))
            if (length(ri2do) == dt1[, .N]) 
                ri2do <- {
                }
        }
        rj2do <- which(wk.dt.is_clean(dt2, by = by2))
        if (!length(rj2do)) 
            return(dt1)
        hs.switch(c(length(ri2do) > 0, length(rj2do) < dt2[, .N]), c(1, 1), {
            m1.38_09_24_233212 <- hs.matcher(dt1[[by1[1]]][ri2do], dt2[[by2[1]]][rj2do], ic = ic)
            i <- which(m1.38_09_24_233212 > 0)
            dt1.matched.ri <- ri2do[i]
            m1.38_09_24_233212 <- rj2do[m1.38_09_24_233212[i]]
        }, c(1, 0), {
            m1.38_09_24_233212 <- hs.matcher(dt1[[by1[1]]][ri2do], dt2[[by2[1]]], ic = ic)
            i <- which(m1.38_09_24_233212 > 0)
            if (length(i) < length(m1.38_09_24_233212)) {
                m1.38_09_24_233212 <- m1.38_09_24_233212[i]
                dt1.matched.ri <- ri2do[i]
            }
            else dt1.matched.ri <- ri2do
        }, c(0, 1), {
            m1.38_09_24_233212 <- hs.matcher(dt1[[by1[1]]], dt2[[by2[1]]][rj2do], ic = ic)
            dt1.matched.ri <- which(m1.38_09_24_233212 > 0)
            m1.38_09_24_233212 <- rj2do[m1.38_09_24_233212[dt1.matched.ri]]
        }, c(0, 0), {
            m1.38_09_24_233212 <- hs.matcher(dt1[[by1[1]]], dt2[[by2[1]]], ic = ic)
            dt1.matched.ri <- which(m1.38_09_24_233212 > 0)
            m1.38_09_24_233212 <- m1.38_09_24_233212[dt1.matched.ri]
        })
        if (!length(dt1.matched.ri)) 
            return(dt1)
        for (i in hs.seqForward(2, length(by1))) {
            m2 <- hs.eq(dt1[[by1[i]]][dt1.matched.ri], dt2[[by2[i]]][m1.38_09_24_233212], ic = ic)
            if (!all(m2)) {
                dt1.matched.ri <- dt1.matched.ri[m2]
                m1.38_09_24_233212 <- m1.38_09_24_233212[m2]
            }
            if (!length(dt1.matched.ri)) 
                return(dt1)
        }
        if (old == "merge") {
            for (i in seq_along(as)) local({
                as1 <- as[i]
                what1 <- what[i]
                a <- dt2[[what1]][m1.38_09_24_233212]
                b <- dt1[[as1]][dt1.matched.ri]
                bi <- hs.clean(b, ot = "i")
                if (length(bi)) {
                  if (!is.character(a)) 
                    a <- hs.str_as(a)
                  if (all(substring(b[bi], nchar(b[bi]) - nchar(a[bi]) + 1) == a[bi])) {
                    if (length(bi) < length(b)) {
                      a <- a[-bi]
                      dt1.matched.ri <- dt1.matched.ri[-bi]
                    }
                    else return()
                  }
                  else a[bi] <- paste(b[bi], a[bi], sep = sep)
                }
                if (is.character(dt2[[what1]]) && (!is.character(dt1[[as1]]))) 
                  wk.dt.set(dt1, , as1, hs.str_as(dt1[[as1]]))
                wk.dt.set(dt1, dt1.matched.ri, as1, a)
            })
            return(dt1)
        }
        else {
            wk.dt.set(dt1, dt1.matched.ri, as, dt2[, .SD[m1.38_09_24_233212], .SDcols = c(what)])
        }
    }
})
wk.dt.absorb1 <- function(sep = "auto", sep2 = if (sep %in% c("auto", "")) ";" else sep, sep_set = "[;\\|]", old = "merge", na2blank = TRUE, collapse = TRUE, hs.clean = .GlobalEnv[["hs.clean"]], hs.dirty = .GlobalEnv[["hs.dirty"]]) {
    force(sep2)
    if (!length(as)) 
        as <- what
    if (hs.clean(sep, ot = "n")) {
        if (sep == "auto") 
            sep <- ""
        for (by11 in by1) {
            sep <- hs.detect.sep(dt1[[by11]], sep_set = sep)
            if (nzchar(sep)) 
                break
        }
    }
    if (nzchar(sep)) {
        cn2do <- c(by1, if (old == "keep") as)
        dt1.by <- if (cn2do %all.in% names(dt1)) {
            dt1
        }
        else {
            dt1[, .SD, .SDcols = c(cn2do)]
        }
        dt1.by[, `:=`(i.38_09_11_140638, .I)]
    }
    else dt1.by <- dt1
    ri2do <- if (old == "keep") 
        local({
            as2do <- as %and% names(dt1.by)
            if (length(as2do)) {
                ri2do <- hs.dirty(dt1.by[[as2do[1]]], ot = "i")
                for (as1 in as2do[-1]) {
                  if (!length(ri2do)) 
                    break
                  ri2do <- ri2do[hs.dirty(dt1.by[[as1]][ri2do], ot = "l")]
                }
                ri2do
            }
        })
    if (hs.clean(sep, ot = "n")) 
        for (by11 in by1) dt1.by <- wk.dt.expand(dt1.by, by11, sep = sep, ri2do = ri2do)
    if (collapse) 
        dt2 <- wk.dt.collapse(dt2, by = by2, U = 1, sep = sep2)
    wk.dt.absorb1.core(dt1 = dt1.by, dt2 = dt2, by = by, by1 = by1, by2 = by2, what = what, as = as, ic = ic, ri2do = ri2do, old = old, sep = sep2)
    if ("i.38_09_11_140638" %in% names(dt1.by)) {
        if (dt1.by[, anyDuplicated(i.38_09_11_140638)]) {
            as_not_to_collapse <- local({
                as <- as %and% names(dt1)
                if (!length(as)) 
                  return()
                a <- dt1.by[, unique(.SD), by = i.38_09_11_140638, .SDcols = c(as)]
                as[sapply(as, function(as1) {
                  i <- is.na(a[[as1]])
                  if (!all(is.na(dt1[[as1]][i]))) 
                    return(FALSE)
                  i <- !i
                  all(a[[as]][i] == dt1[[as1]][i])
                })]
            })
            dt1.by <- wk.dt.collapse(dt1.by, by = unique(c("i.38_09_11_140638", as_not_to_collapse, names(dt1.by) %not% c(by1, as))), sep = sep2)
            if (length(as_not_to_collapse)) 
                as <- as %not% as_not_to_collapse
            if (length(as)) 
                wk.dt.set(dt1, , as, dt1.by[, .SD, .SDcols = c(as)])
        }
        dt1[, `:=`("i.38_09_11_140638", {
        })]
    }
    return(dt1)
    if (ic) {
        dt1.by <- dt1[, .SD, .SDcols = c(by1)]
        dt2.by <- dt2[, .SD, .SDcols = c(by2)]
        wk.dt.replace(dt1.by, . %=>% hs.str_as %=>% tolower)
        wk.dt.replace(dt2.by, . %=>% hs.str_as %=>% tolower)
    }
    {
        switch(old, keep = {
            for (as1 in as %and% names(dt1)) {
                i1 <- hs.clean(dt1[[as1]], ot = "l")
                i.38_09_23_103500[i1] <- -1
            }
        })
        ri2do <- which(i.38_09_23_103500 >= 0)
        for (by_i in seq_along(by1)) {
            m1 <- hs.str_match(dt1[[by1[by_i]]][ri2do], dt2[[by2[by_i]]], ic = ic)
            i1 <- !is.na(m1)
            i.38_09_23_103500[ri2do[i1]] <- m1[i1]
        }
    }
    ri2do <- which(i.38_09_23_103500 > 0)
    if (length(ri2do)) {
        i.38_09_23_103500 <- i.38_09_23_103500[ri2do]
        a <- dt2[, .SD[i.38_09_23_103500], .SDcols = -c(by2)]
        if (old == "merge") {
            hs.browser("38.09.23.154900")
            hs.for(as1, as %and% names(dt1), wk.dt.set(dt1, , as1, hs.str_as(dt1[[as1]])), by1i, seq_along(by1), by1[by1i])
        }
        wk.dt.set(dt1, ri2do, as, a)
    }
    return(dt1)
    if (sep == "auto") {
        sep <- if (length(by1) == 1) 
            local({
                i <- hs.grep(dt1[[by1]], sep_set)
                if (length(i)) 
                  hs.re(dt1[[by1]][i[1]], sep_set)
            })
        if (missing(sep2)) 
            sep2 <- sep
    }
    if (dt2[, anyDuplicated(.SD), .SDcols = c(by2)]) 
        dt2 <- wk.dt.collapse(dt2, by = by2, sep = if (length(sep2)) 
            sep2
        else ";")
    if (length(by2) && length(what)) {
        if (!length(as)) 
            as <- what
        if (length(as) != length(what)) 
            hs.browser("36.12.05.215327")
        dt1.by <- dt1[, .SD, .SDcols = c(c(by1, as) %and% names(dt1))]
        if (length(sep)) {
            dt1.by[, `:=`("i.38_09_11_140638", .I)]
            dt1.by <- wk.dt.expand(dt1.by, cn1 = by1, split = sep)
        }
        if (ic) {
            for (a in by1) dt1.by[, `:=`(c(a), tolower(get(a)))]
            dt2 <- wk.dt.copy(dt2)
            for (a in by2) dt2[, `:=`(c(a), tolower(get(a)))]
        }
        dt2 <- wk.dt.subset(dt2, dt1.by, by2, ot = "in")
        if (hs.is_empty(dt2)) 
            return(invisible(dt1))
        i.38_09_23_082919 <- wk.dt.subset.na(dt2, dt1, by2, ot = "i")
        if (all(is.na(i.38_09_23_082919))) 
            return(invisible(dt1))
        common <- wk.dt.subset(dt1.by, dt2.by, mult = "1")
        common <- wk.dt.is_clean(common)
        if (hs.is_empty(common)) 
            return(invisible(dt1))
        if (old %in% c("keep", "merge") && any(as %in% names(dt1))) {
            hs.browser("38.09.23.073514")
            lapply(which(as %in% names(dt1)), function(as1.i) {
                as <- as[as1.i]
                what <- what[as1.i]
                if (old == "keep") 
                  common %<>% (`%not%`(dt1.by[hs.clean(dt1.by[[as]], ot = "l"), get(by1)]))
                i1.38_09_12_065901 <- which(dt1.by[[by1]] %in% common)
                i2.38_09_11_075403 <- which(dt2.by %in% common)
                i <- match(dt1.by[[by1]][i1.38_09_12_065901], dt2.by[i2.38_09_11_075403])
                if (length(sep)) {
                  if (!is.character(dt1[[as]])) 
                    dt1[, `:=`(c(as), hs.str_as(as))]
                  wk.dt.set(dt1.by, i1.38_09_12_065901, as, dt2[[what]][i2.38_09_11_075403[i]])
                  a <- dt1.by[i.38_09_11_140638 %in% i.38_09_11_140638[i1.38_09_12_065901]]
                  a <- a[, lapply(.SD, function(x) {
                    if (na2blank) 
                      x <- ifelse(is.na(x), "", x)
                    if (all(x == "")) 
                      x <- ""
                    paste(x, collapse = sep2)
                  }), by = i.38_09_11_140638]
                  wk.dt.set(dt1, a[, i.38_09_11_140638], as, a[[as]])
                }
                else {
                  a <- dt2[[what]][i2.38_09_11_075403[i]]
                  if (old == "merge") 
                    a <- paste(dt1[i1.38_09_12_065901, get(as)], a, sep = sep2)
                  wk.dt.set(dt1, i1.38_09_12_065901, as, a)
                }
                {
                }
            })
            i <- as %not.in% names(dt1)
            as <- as[i]
            what <- what[i]
        }
        if (length(as)) {
            i1 <- wk.dt.subset(dt1.by, common, ot = "i")
            i2.38_09_11_205626 <- wk.dt.subset(dt2.by, common, ot = "i")
            i <- match(dt1.by[[by1]][i1], dt2.by[i2.38_09_11_205626])
            dt1.by
            if (length(sep)) {
                for (as1.i in seq_along(as)) wk.dt.set(dt1.by, i1, as[as1.i], dt2[[what[as1.i]]][i2.38_09_11_205626[i]])
                dt1.by <- dt1.by[, lapply(.SD, function(x) {
                  if (na2blank) 
                    x <- ifelse(is.na(x), "", x)
                  if (all(x == "")) 
                    x <- ""
                  paste(x, collapse = sep2)
                }), by = i.38_09_11_140638]
                dt1[, `:=`(c(as), dt1.by[, .SD, .SDcols = c(as)])]
            }
            else wk.dt.set(dt1, i1, as, dt2[, .SD[i2.38_09_11_205626[i]], .SDcols = c(what)])
        }
    }
    invisible(dt1)
}
formals(wk.dt.absorb1) <- hs.formals.merge(wk.dt.absorb1.core, wk.dt.absorb1)
wk.dt.absorb2 <- function(dt1, by1, ..., sep = "auto") {
    if (sep == "auto") 
        sep <- hs.cond(any(hs.grepl(dt1[[by1]], ";")), ";", any(hs.grepl(dt1[[by1]], ",")), ",", any(hs.grepl(dt1[[by1]], "|", fixed = TRUE)), "|")
    hs.browser("38.09.11.125104")
    if (length(sep)) {
        cn2do <- c(by1, {
            i <- match("what", ...names(), nomatch = 0L)
            if (i) ...elt(i)
        }, {
            i <- match("as", ...names(), nomatch = 0L)
            if (i) ...elt(i)
        })
        cn2do <- cn2do %and% names(dt1)
        a <- dt1[, .SD, .SDcols = c(cn2do)]
        a[, `:=`(i.38_09_11_112018, seq(.N))]
        a <- wk.dt.expand(a, by1, split = sep)
        wk.dt.absorb1(dt1 = a, by1 = by1, ...)
        a <- a[, lapply(.SD, paste, collapse = sep), by = i.38_09_11_112018]
        a[, `:=`(i.38_09_11_112018, {
        })]
        list(...)
    }
    else {
        wk.dt.absorb1(dt1 = dt1, by1 = by1, ...)
    }
}
wk.dt.list.merge <- function(L, cn1 = {
}) {
    if (length(L) == 1) 
        return(L[[1]])
    jg <- data.table::copy(L[[1]])
    for (i1 in seq_along(L)[-1]) {
        hs.say(i1)
        jg <- if (length(cn1)) {
            wk.dt.absorb(jg, wk.dt.subset.na(L[[i1]], jg[[cn1]], cn1)[, .SD, .SDcols = -cn1])
        }
        else wk.dt.absorb(jg, L[[i1]])
    }
    jg
}
wk.dt.list.combine <- function(sj, cn_common = lapply(sj, names) %=>% hs.list.common) {
    cn_common %<>% (`%and%`(lapply(sj, names) %=>% hs.list.common))
    jg <- lapply(sj, . %=>% x[, .SD, .SDcols = cn_common])
    jg <- rbindlist(jg) %=>% unique
    for (i1 in if (length(names(sj))) 
        names(sj) %=>% order
    else seq_along(sj)) {
        sj1 <- sj[[i1]]
        cn2do <- names(sj1) %not% cn_common
        jg[match(sj1[, Accession], Accession), `:=`(paste0(paste0("【", names(sj)[i1], "】"), cn2do), sj1[, .SD, .SDcols = c(cn2do)])]
    }
    if (0) {
        null <- lapply(names(sj), . %=>% {
            sj1 <- sj[[x]]
            cn_specific <- names(sj1) %=>% x[x %not.in% cn_common]
            setnames(sj1, cn_specific, paste0("【", x, "】", cn_specific))
        })
        jg <- sj[[1]]
        for (i1 in 2:length(sj)) {
            jg %<>% merge(sj[[i1]], all = 1)
            names(sj[[1]]) %not% names(sj[[i1]])
        }
        cn_common <- cn_common %and% names(jg)
    }
    if (0) {
        setcolorder(jg, c(cn_common, hs.grepV(names(jg), "# Unique Peptides")))
        setcolorder(jg, c(cn_common, hs.grepV(names(jg), "# PSMs")))
        Sds <- jg[, hs.rowSds(.SD), .SDcols = patterns("# PSMs")]
        not_NA <- jg[, hs.rowSums(!is.na(.SD)), .SDcols = patterns("# PSMs")]
        jg[, `:=`("not_NA", not_NA)]
        jg[, `:=`("Sds", Sds)]
        setorderv(jg, c("not_NA", "Sds"), c(-1, -1), na.last = TRUE)
        jg[, `:=`(c("not_NA", "Sds"), {
        })]
        {
            cn1 <- "# AAs"
            a1 <- lapply(jg[, .SD, .SDcols = patterns("】# PSMs$")], "/", jg[[cn1]])
            names(a1) %<>% paste0("/", cn1)
            jg[, `:=`(names(a1), a1)]
            wk.dt.col.reorder(jg, names(a1), after = tail(hs.grep(names(jg), "】# PSMs$"), 1))
        }
    }
    jg
}
wk.dt.ij <- wk.dt.subset <- function(dt, i, on = {
}, nomatch = 0, no_na = 1, inv = 0, ot = "t", exp_l = {
}, exp_j = {
}, env1 = parent.frame(), cum = 0, mult = "all", with_i = 0) {
    env2 <- new.env(parent = env1)
    env2$i1.38_08_30_224756 <- TRUE
    env2$with_i <- with_i
    if (!missing(i)) {
        if (!length(on)) {
            on <- wk.dt.index(dt, vectors = TRUE)
            if (length(on) == 1) {
                on <- unlist(on)
            }
            else {
                if (missing(i)) 
                  hs.browser("38.07.17.220949")
                on <- if (is.list(i)) {
                  if (names(i) %all.in% names(dt)) 
                    names(i)
                  else seq_along(i)
                }
                else {
                  dim(i)[2] %=>% {
                    if (length(x)) {
                      if (colnames(i) %all.in% names(dt)) 
                        names(i)
                      else seq.int(x)
                    }
                    else 1
                  }
                }
            }
        }
        if (length(on) && is.numeric(on)) 
            on <- colnames(dt)[on]
        if (ot == "on") 
            return(on)
        if (length(on)) {
            i1 <- local({
                if (!is.list(i)) 
                  i <- list(i)
                if (is.null(names(i))) 
                  names(i) <- on
                if (on %all.in% names(i)) {
                  hs.switch(mult, 1, dt[i, on = on, nomatch = nomatch, which = TRUE, allow.cartesian = TRUE, mult = "first"], "all", dt[i, on = on, nomatch = nomatch, which = TRUE, allow.cartesian = TRUE])
                }
                else {
                  if (!is(i, "data.table")) 
                    hs.browser("38.08.31.001908")
                  dt[i[, .SD, .SDcols = seq_along(on)] %=>% wk.dt.setnames(on), on = on, nomatch = nomatch, which = TRUE, allow.cartesian = TRUE]
                }
            })
            if (inv) 
                i1 <- dt[, .I] %rm% i1
        }
        else i1 <- TRUE
        env2$i <- i
        env2$on <- on
        env2$i1.38_08_30_224756 <- i1
    }
    env2$dt <- dt
    env2$nomatch <- nomatch
    env2$no_na <- no_na
    if (!missing(exp_l)) 
        env2$exp_l.37_12_30_093732 <- substitute(exp_l)
    env2$ot <- ot
    if (!missing(exp_j)) {
        env2$exp_j.37_12_31_092013 <- substitute(exp_j)
        env2$cum.38_08_07_224434 <- cum
    }
    eval(quote({
        if (exists("exp_l.37_12_30_093732", inherits = FALSE) && (length(exp_l.37_12_30_093732) > 1)) {
            acc.38_08_07_225803 <- if (exists("cum.38_08_07_224434", inherits = FALSE) && cum.38_08_07_224434) expandingList()
            for (exp1.38_01_05_234704 in as.list(exp_l.37_12_30_093732)[-1]) {
                if (!length(i1.38_08_30_224756)) break
                if (isTRUE(exp1.38_01_05_234704)) {
                  ...a <- TRUE
                } else {
                  ...a <- dt[i1.38_08_30_224756, eval(exp1.38_01_05_234704)]
                  if (is.logical(...a)) {
                    ...a <- which(...a)
                  } else if (is.null(...a)) next
                }
                i1.38_08_30_224756 <- if (isTRUE(i1.38_08_30_224756)) {
                  ...a
                } else i1.38_08_30_224756[...a]
                if (length(acc.38_08_07_225803)) acc.38_08_07_225803[["add"]](dt[i1.38_08_30_224756, eval(exp_j.37_12_31_092013)])
            }
            if (length(acc.38_08_07_225803)) return(acc.38_08_07_225803[["get"]]())
        }
        switch(ot, t = {
            jg <- if (exists("exp_j.37_12_31_092013", inherits = FALSE)) {
                dt[i1.38_08_30_224756, eval(exp_j.37_12_31_092013)]
            } else dt[i1.38_08_30_224756]
            if (is.na(nomatch) && no_na) {
                if (length(on) > 1) hs.browser("38.06.14.231859")
                if (is(jg, "data.table") && (on %in% names(jg))) {
                  i.38_06_14_235830 <- which(is.na(jg[[on]]))
                  if (length(i.38_06_14_235830)) data.table::set(jg, i.38_06_14_235830, on, (if (is(i, "list") || is(i, "data.frame")) i[[on]] else i)[i.38_06_14_235830])
                }
            }
            if (with_i) jg[, `:=`("i1.38_08_30_224756", i1.38_08_30_224756)]
            jg
        }, i = i1.38_08_30_224756, r = local({
            a.38_07_17_123841 <- {
            }
            dt[i1.38_08_30_224756, {
                a.38_07_17_123841 <<- eval(exp_j.37_12_31_092013)
                {
                }
            }]
            a.38_07_17_123841
        }), `in` = {
            i1.38_08_30_224756 <- unique(i1.38_08_30_224756)
            if (length(i1.38_08_30_224756) < dt[, .N]) {
                dt[sort(i1.38_08_30_224756)]
            } else dt
        })
    }), envir = env2)
}
wk.dt.subset.na <- function(..., no_na = 1) {
    wk.dt.subset(..., nomatch = NA, no_na = no_na)
}
wk.dt.index <- function(x, ...) {
    data.table::indices(x, ...)
}
wk.dt.setindexv <- function(x, cols = {
}, ...) {
    if (length(cols)) {
        if (is.numeric(cols)) 
            cols <- names(x)[cols]
        data.table::setindexv(x = x, cols = cols, ...)
    }
    else {
        wk.dt.setattr(x, "index", {
        })
    }
}
wk.dt.expand <- function(dt1, cn1 = names(dt1)[1], split = ";", sep = split, sep_set = "[;\\|]", l1 = {
}, ri2ignore = if (length(ri2do)) dt1[, .I] %not% ri2do, ri2do = {
}, ...) {
    if (!is.character(dt1[[cn1]])) 
        return(dt1)
    if (is.numeric(cn1)) 
        cn1 <- names(dt1)[cn1]
    if (length(l1) != dt1[, .N]) {
        if (length(l1)) 
            hs.browser("38.09.24.072841")
        if (length(sep)) {
            sep <- if (sep == "auto") {
                hs.detect.sep(dt1[[cn1]])
            }
            else hs.detect.sep(dt1[[cn1]], sep_set = sep)
            if (!nzchar(sep)) 
                return(dt1)
        }
        i <- hs.grepl(dt1[[cn1]], sep)
        i[ri2ignore] <- FALSE
        i[ri2do] <- TRUE
        if (!any(i)) 
            return(dt1)
        l1 <- as.list(dt1[[cn1]])
        l1[i] <- hs.str_split(dt1[[cn1]][i], sep = sep, ...)
        if (0) {
            l1 <- vector("list", dt1[, .N])
            i1 <- which((dt1[[cn1]] %in% "") | (dt1[[cn1]] %in% NULL))
            if (length(i1)) {
                l1[i1] <- as.list(dt1[[cn1]][i1])
                i1 <- seq_along(l1) %not% i1
            }
            else i1 <- seq_along(l1)
            l1[i1] <- strsplit(dt1[[cn1]][i1], sep)
        }
    }
    l1.len <- sapply(l1, length)
    lt1 <- l1.len > 1
    if (any(lt1)) {
        i_expanded <- dt1[, rep(.I, ifelse(lt1, l1.len, 1))]
        jg <- dt1[i_expanded, .SD]
        wk.dt.set(jg, which(i_expanded %in% which(lt1)), cn1, unlist(l1[lt1]))
    }
    else dt1
}
wk.dt.j <- wk.dt.get.j <- wk.dtSubsetCol <- function(dt, j1 = if (length(j2rm)) seq.int(length(dt)), s = {
}, hs = {
}, S, E, grep_or = {
}, grep_and = {
}, inv = FALSE, j2rm = {
}, sort = FALSE, unique = FALSE, ot = "ci") {
    if (length(grep_or)) {
        j1 %<=>% c(x, sapply(grep_or, . %=>% {
            hs.grep(names(dt), x) %=>% unlist %=>% unique
        }))
    }
    if (length(grep_and)) {
        j1 %<=>% c(x, grep_and %=>% {
            a1 <- lapply(x, . %=>% {
                hs.grep(names(dt), x)
            }) %=>% inverseList
            hs.nonempty(a1, length(x)) %=>% names %=>% as.integer
        })
    }
    if (length(j2rm)) 
        j1 %<=>% x[x %not.in% j2rm]
    if (length(s)) 
        j1 %<=>% c(x, match(s, colnames(dt)))
    if (inv) 
        j1 %<=>% {
            seq.int(length(dt)) %not% x
        }
    if (sort) 
        j1 <- sort(j1)
    return(switch(ot, cn = names(dt)[j1], ci = j1, dt = dt[, .SD, .SDcols = j1]))
    if (!(missing(S) & missing(E))) {
        if (missing(S)) 
            S <- 1
        if (missing(E)) 
            E <- ncol(dt)
        if (is.character(S)) 
            S %<>% match(colnames(dt))
        if (is.character(E)) 
            E %<>% match(colnames(dt))
        i %<>% c(S:E)
    }
    if (length(hs)) 
        i %<=>% c(x, colnames(dt) %=>% match(hs(x), x))
    if (length(j)) 
        i %<=>% x[x %not.in% j]
    if (length(i)) {
        if (sort) 
            i %<=>% sort
        if (unique) 
            i %<=>% unique
        dt[, .SD, .SDcols = colnames(dt)[i]]
    }
}
wk.dt.colConvert <- function(dt, cols, hs) {
    cols <- unique(cols)
    if (is.character(cols)) 
        cols <- which(names(dt) %in% cols)
    for (ci1 in cols) {
        v1.37_12_29_141028 <- hs(dt[[ci1]])
        dt[, `:=`(c(ci1), v1.37_12_29_141028)]
    }
    dt
}
wk.dt.col.reorder <- local({
    hs.in <- function(dt, i1, ci, type) {
        if (is.character(i1)) 
            i1 <- which(names(dt) == i1)
        if (length(i1) > 1) 
            i1 <- switch(type, before = i1[1], after = tail(i1, 1))
        n1 <- i1
        if (type == "before") 
            n1 <- n1 - 1
        if (n1) {
            c(seq.int(n1) %not% ci, ci)
        }
        else ci
    }
    function(dt, cn, i1 = {
    }, cn.pattern = {
    }, after = {
    }, before = {
    }, replace = TRUE) {
        if (length(cn.pattern)) {
            if (length(cn.pattern) > 1) {
                for (cn.pattern1 in cn.pattern) wk.dt.col.reorder(dt, i1 = i1, cn.pattern = cn.pattern1, after = after, before = before, replace = replace)
                return()
            }
            else cn <- hs.grep(names(dt), cn.pattern)
        }
        if (is.character(cn)) 
            cn <- match(cn, names(dt))
        i2 <- if (length(after)) {
            hs.in(dt, after[1], cn, "after")
        }
        else if (length(before)) {
            hs.in(dt, before[1], cn, "before")
        }
        else {
            if (length(i1) == 0) {
                hs.in(dt, min(cn) - 1, cn, "after")
            }
            else {
                if (length(i1) == length(cn)) {
                  if (is.character(i1)) 
                    i1 <- match(i1, names(dt))
                  a1 <- rep(0, length(dt))
                  a1[i1] <- cn
                  if (replace) 
                    a1[cn] <- i1
                  if (0 %in% a1) 
                    a1[a1 == 0] <- seq.int(length(dt)) %not% a1
                  a1
                }
                else stop("【36.02.14.202704】")
            }
        }
        setcolorder(dt, i2)
    }
})
wk.dt.fwrite <- function(dt, wj, i2rm = {
}, quote = 0, add = smart && file.exists(wj), cn = !add, rn = {
}, sep = "\t", smart = FALSE, ...) {
    hs.wj(wj)
    if (length(rn) && inherits(dt, "data.frame") && (!inherits(dt, "data.table"))) 
        dt <- wk.dt.as(dt, keep.rownames = rn)
    if (length(i2rm)) {
        dt[-i2rm, data.table::fwrite(.SD, wj, sep = sep, quote = as.logical(quote), col.names = as.logical(cn), append = as.logical(add), ...)]
    }
    else {
        if ((!is.list(dt)) && (!length(dim(dt)))) 
            dt <- list(dt)
        data.table::fwrite(dt, wj, sep = sep, quote = as.logical(quote), col.names = as.logical(cn), append = as.logical(add), ...)
    }
}
wk.setindex <- function(dt, cols = 1) {
    if (length(cols)) {
        if (is.numeric(cols)) 
            cols <- colnames(dt)[cols]
    }
    else stop("need indices")
    if (indices(dt, TRUE) %=>% sapply(identical, cols) %=>% any) {
        dt
    }
    else setindexv(dt, cols)
}
wk.setkey <- function(dt, cols = {
}, physical = TRUE) {
    if (!length(cols)) 
        cols <- colnames(dt)
    if (!is.character(cols)) 
        cols <- colnames(dt)[cols]
    if (haskey(dt) && (all(key(dt) == cols))) {
        dt
    }
    else setkeyv(dt, cols)
}
wk.fread <- function(..., cn = {
}, ot = ifelse(length(rn.cn), "df", "dt"), rn.cn = {
}, rn.cn.smart = 0, skip.comments = "#", tmpdir = hs.wj(dirname(hs.wj.temp()))) {
    A <- list(...)
    if ("skip" %not.in% names(A)) 
        A[["skip"]] <- 0
    fp1 <- hs.wj.temp()
    if ("cmd" %in% names(A)) {
        cmd1 <- A[["cmd"]]
        wj_tmp <- hs.wj.temp()
        on.exit(hs.wj.rm(wj_tmp), add = TRUE)
        cat(cmd1, file = wj_tmp)
        A[["cmd"]] <- .sys("bash", wj_tmp)
        (if (.Platform$OS.type == "unix") 
            system
        else shell)(paste("(", A[["cmd"]], ") >", fp1))
        A[["cmd"]] <- {
        }
        A[["file"]] <- fp1
        on.exit(unlink(fp1), add = TRUE)
    }
    else if ("text" %in% names(A)) {
        cat(A[["text"]], sep = "\n", file = fp1)
        A[["text"]] <- {
        }
        A[["file"]] <- fp1
    }
    fp1 <- A[[match("file", names(A), nomatch = 1)]]
    if (file.exists(fp1) && nzchar(skip.comments) && (A[["skip"]] == 0)) {
        v <- readLines(fp1, 1000)
        if (length(v)) {
            i <- which(substr(v, 1, 1) != skip.comments)[1] - 1
            if (i) 
                A[["skip"]] <- i - hs.grepl(v[i], "\t")
        }
    }
    A[["tmpdir"]] <- tmpdir
    if (length(cn)) 
        A[["header"]] <- as.logical(cn)
    jg <- do.call(data.table::fread, A)
    if (hs.all(!length(rn.cn), rn.cn.smart, length(jg) > 1, identical(unname(which(sapply(jg, is.character))), 1L))) 
        rn.cn <- 1
    switch(ot, dt = jg, m = wk.dt.2matrix(jg, rn.cn = rn.cn), df = wk.dt.2df(jg, rn.cn = rn.cn))
}
wk.fread0 <- function(...) wk.fread(..., cn = FALSE)
wk.fread1 <- function(...) wk.fread(..., cn = TRUE)
wk.fread.zip <- function(zip.wj, wj = {
}) {
    wk.fread(cmd = .sys("unzip -p", .sys.fpGood(zip.wj), .q(wj)))
}
wk.dt.setorderv <- function(..., na.last = TRUE) data.table::setorderv(..., na.last = na.last)
wk.dt.setnames <- function(dt, old = {
}, new = {
}, skip_absent = FALSE, fp = {
}, fp.sep = "\t") {
    A <- list(x = dt, skip_absent = skip_absent)
    if (hs.all(length(old), !length(new))) {
        new <- old
        old <- {
        }
    }
    if (hs.all(length(old) > 1, length(old) != length(new))) 
        hs.browser("38.05.27.235812")
    i <- which(is.na(new))
    if (length(i)) {
        if (length(old) == length(new)) 
            old <- old[-i]
        new <- new[-i]
    }
    if (hs.all(length(new) < ncol(dt), length(old) < length(new))) 
        old <- hs.switch(length(old), 0, seq_along(new), 1, seq(old, old + length(new) - 1))
    if (length(old)) 
        A[["old"]] <- old
    if (length(fp)) 
        new <- head(strsplit(readLines(fp, 1), fp.sep)[[1]], length(dt))
    if (length(new)) 
        A[["new"]] <- new
    do.call(data.table::setnames, A)
}
wk.dt.set <- function(x, i = NULL, j, value) {
    if (length(j)) {
        data.table::set(x = x, i = i, j = j, value = value)
    }
    else data.table::setDT(x)
}
wk.dt.setDT <- data.table::setDT
wk.dt.setDF <- data.table::setDF
wk.dt.fromList <- function(L, subset = if (length(passenger)) {
    seq_along(L) %not% {
        if (inherits(passenger, "character")) 
            which(names(L) %in% passenger)
        else passenger
    }
} else seq_along(L), passenger = {
}, ..., nm = c("group", if ((!length(dim(L[[1]]))) || ((length(dim(L[[1]])) == 2) && (ncol(L[[1]]) == 1))) "member"), by_row = {
}, list_name = 1) {
    if (inherits(subset, "character")) 
        subset <- match(subset, names(L))
    if (missing(passenger)) 
        passenger <- seq_along(L) %not% subset
    if (inherits(passenger, "character")) 
        passenger <- match(passenger, names(L))
    if (any(nm %in% names(L)[passenger])) 
        stop("2023_07_25_182415|列名重复：", nm %and% names(L)[passenger])
    by_row %<=>% if (length(x)) {
        as.logical(x)
    }
    else hs.any(length(dim(L[[subset[1]]])) == 2, is(L[[subset[1]]], "table"))
    lh <- sapply(subset, function(i) {
        x <- L[[i]]
        hs.switch(length(dim(x)), 2, nrow(x), if (by_row) 
            length(x) > 0
        else length(x))
    })
    i1 <- which(lh > 0)
    row_n <- sum(lh[i1])
    jg <- wk.dt(rep(hs.na(""), row_n))
    a <- L[[subset[i1[1]]]]
    lw <- hs.cond(is(a, "table"), 2, length(dim(a)) == 2, ncol(a), if (by_row) 
        length(a)
    else 1)
    ci_max <- max(lw) + 1
    a <- L[[subset[i1[which.max(lw)]]]]
    a <- hs.cond(inherits(a, "data.frame"), unname(lapply(a, hs.na)), is.atomic(a), local({
        b <- hs.i(L, subset[i1]) %=>% sapply(class)
        sapply(hs.i(L, subset[i1[match(unique(b), b)]]), hs.na)[1]
    }), inherits(a, "table"), list(hs.na(""), hs.na(1L)), inherits(a, "list"), hs.browser("38.07.09.214005"), as.list(rep(hs.na(a[1]), ci_max - 1)))
    jg[, `:=`(paste0("V", seq(2, ci_max)), a)]
    wk.dt.setnames(jg, nm)
    ri1 <- 1L
    for (i in i1) {
        a <- L[[subset[i]]]
        if (is.null(a)) 
            next
        if (inherits(a, "table")) 
            a <- wk.dt.as(a)
        if (length(dim(a)) == 2) {
            ri <- seq(ri1, ri1 + nrow(a) - 1)
            if (is(a, "data.frame")) {
                wk.dt.set(jg, ri, seq(2, ncol(a) + 1), a)
            }
            else for (j in 1:ncol(a)) wk.dt.set(jg, ri, j + 1L, a[, j])
        }
        else {
            if (by_row) {
                if (length(passenger)) 
                  hs.browser("2023.07.25.182749", debug = 0)
                ri <- ri1
                ci <- seq(2, 2 + length(a) - 1)
                for (i2 in seq_along(ci)) wk.dt.set(jg, ri, ci[i2], a[i2])
            }
            else {
                ri <- seq(ri1, ri1 + length(a) - 1)
                wk.dt.set(jg, ri, 2L, a)
                if (length(passenger)) {
                  wk.dt.set(jg, ri, names(L)[passenger], if (inherits(L, c("list"))) 
                    L[passenger]
                  else if (inherits(L, "data.table")) 
                    L[, .SD, .SDcols = passenger]
                  else if (inherits(L, "data.frame")) 
                    L[, passenger])
                }
            }
        }
        if (length(names(L))) 
            wk.dt.set(jg, ri, 1L, names(L)[subset[i]])
        ri1 <- ri[length(ri)] + 1L
    }
    L2 <- list(...)
    if (length(L2)) 
        hs.browser("38.05.20.150047")
    if (length(nm) == 1) {
        a <- L[[subset[i1[1]]]]
        nm2 <- if (is(a, "table")) {
            c("V1", "N")
        }
        else if (hs.len1(names(a), ncol(jg) - 1)) 
            names(a)
        if (length(nm2)) 
            wk.dt.setnames(jg, 2, nm2)
    }
    if (length(nm)) 
        wk.dt.setnames(jg, nm)
    if (!list_name) 
        wk.dt.set(jg, , 1L, {
        })
    if (!length(names(L))) 
        jg[, `:=`(1, {
        })]
    return(jg)
    a <- lapply(i1, function(i1) {
        hs.browser("38.05.20.141127")
        a <- do.call(wk.dt, c(if (by_row) c(list(names(L)[i1]), L[[i1]]) else list(names(L)[i1], L[[i1]]), lapply(L2, "[[", i1)))
        if (length(nm)) 
            wk.dt.setnames(a, nm)
        a
    })
    jg <- rbindlist(unname(a))
    if (length(nm)) 
        setnames(jg, seq_along(nm), nm)
    jg
}
wk.dt.fromMatrix <- function(m1, rn_name = "rn") {
    dt1 <- data.table::as.data.table(m1)
    if (length(rn_name)) {
        dt1[, `:=`(c(rn_name), rownames(m1))]
        wk.dt.col.reorder(dt1, rn_name, before = 1)
    }
    dt1
}
wk.dt.fromUTF16 <- function(wj1) {
    con1 <- file(wj1, encoding = "UTF-16")
    on.exit(close(con1), add = TRUE)
    fread(text = readLines(con1))
}
wk.dt.2matrix <- function(dt, cn = seq.int(ncol(dt)), cn.ms = {
}, rn.cn = if (inherits(dt[[1]], "character")) 1, cn.ri = {
}, cn2rm = {
}, df = 0) {
    hs1 <- if (df) 
        as.data.frame
    else as.matrix
    if (length(cn.ms)) 
        cn <- hs.grep(names(dt), cn.ms)
    if (length(cn2rm)) {
        if (is.character(cn2rm)) 
            cn2rm <- match(cn2rm, names(dt))
        cn <- cn %rm% cn2rm
    }
    if (length(rn.cn)) {
        if (is.character(rn.cn)) 
            rn.cn <- match(rn.cn, names(dt))
        cn <- cn %not% rn.cn
    }
    jg <- if (length(cn.ri)) 
        dt[-cn.ri, hs1(.SD), .SDcols = c(cn)]
    else dt[, hs1(.SD), .SDcols = c(cn)]
    if (length(cn.ri)) {
        colnames(jg) <- if (length(rn.cn)) 
            dt[cn.ri, as.character(.SD), .SDcols = -rn.cn]
        else dt[cn.ri, as.character(.SD)]
    }
    if (length(rn.cn)) {
        rownames(jg) <- if (length(cn.ri)) 
            dt[[rn.cn]][-cn.ri]
        else dt[[rn.cn]]
    }
    return(jg)
    if (length(rn.cn)) {
        if (is.numeric(rn.cn)) {
            cn <- cn[-rn.cn]
            rn.cn <- names(dt)[rn.cn]
        }
        else cn <- cn %rm% rn.cn
    }
    if (length(cn2rm)) {
        hs.browser("37.12.10.210528")
        if (is.numeric(cn2rm)) 
            cn2rm <- names(dt)[cn2rm]
        cn <- cn %rm% cn2rm
    }
    hs.browser("37.12.10.210531")
    X <- (if (df) 
        as.data.frame
    else as.matrix)(dt[, .SD, .SDcols = c(cn)])
    if (length(rn.cn)) 
        rownames(X) <- dt[[rn.cn]]
    X
}
wk.dt.2df <- function(dt, ...) {
    wk.dt.2matrix(dt, ..., df = 1)
}
wk.dt.widen <- function(dt, rn = names(dt)[1], cn = names(dt)[2], vn = names(dt)[3], vn.hs = if (is.numeric(dt[[vn]])) sum else identity, rn.sort = dn.sort, cn.sort = dn.sort, dn.sort = 0) {
    for (on1 in c("rn", "cn", "vn")) hs.update(on1, names(dt)[x], is.numeric)
    if (!inherits(dt, "data.table")) 
        dt <- wk.dt.as(dt)
    dt[, {
        dn <- list(unique(get(rn)) %=>% if (rn.sort) sort(x) else x, unique(get(cn)) %=>% if (rn.sort) sort(x) else x) %=>% lapply(paste)
        df <- lapply(dn[[2]], function(c1) {
            a <- dt[list(dn[[1]], c1), .SD, .SDcols = c(rn, vn), on = c(rn, cn)]
            a[, vn.hs(get(vn)), by = c(rn)][[2]]
        }) %=>% hs.df
        dimnames(df) <- dn
        df
    }]
}
wk.dt.tapply <- function(dt, element.cn = names(dt)[1], name.cn = names(dt)[2], hs = identity, ..., hs.element = identity, hs.name = identity, hs1 = {
}, hs2 = {
}) {
    a1 <- if (length(hs1)) 
        hs1(dt)
    else hs.element(dt[[element.cn]])
    a2 <- if (length(hs2)) 
        hs2(dt)
    else hs.name(dt[[name.cn]])
    tapply(a1, a2, hs, ...)
}
wk.fst.write <- function(sj, wj, ...) {
    write_fst(x = sj, path = wj, ..., compress = 100)
    wj
}
wk.fst.write.fromWj <- function(wj1, wj2 = if (file.exists(wj1)) hs.wjm(wj1, ext = "fst", extN = ifelse(hs.grepl(wj1, "\\.(tsv|csv|txt)\\.gz$"), 2, 1)), uniform_encoding = TRUE, ow = 0, smart = 1, hs1 = identity, ...) {
    if (!length(wj2)) 
        stop("37_02_05_150526")
    if (file.exists(wj2)) {
        if (smart && (!ow)) 
            ow <- file.mtime(wj2) < file.mtime(wj1)
        if (!ow) 
            return(wj2)
    }
    sj <- if (file.exists(wj1)) {
        wk.fread(wj1, ...)
    }
    else wk.fread(cmd = wj1, ...)
    wk.fst.write(hs1(sj), wj2, uniform_encoding = uniform_encoding)
}
ranges.hs.vector <- function(v1) {
    v1 <- unique(sort(v1))
    d1 <- diff(v1)
    i1 <- which(d1 > 1)
    t(matrix(c(v1[1 %or% (i1 + 1)], v1[i1 %or% length(v1)]), ncol = 2))
}
wk.fst.read <- function(wj, ri = NULL, cn = NULL, from = 1, to = NULL, as.data.table = TRUE, rbind = 1, dt = 1, ...) {
    ft <- fst(wj)
    if (length(cn) && is.numeric(cn)) {
        cn <- colnames(ft)[cn]
    }
    jg <- hs.switch(c(length(cn), length(ri)) > 0, rep(TRUE, 2), ft[ri, cn], c(TRUE, FALSE), ft[, cn], c(FALSE, TRUE), ft[ri, cn], rep(FALSE, 2), ft[TRUE])
    if (dt && is.data.frame(jg)) 
        setDT(jg)
    return(jg)
    if (identical(cn, "")) {
        return(wk.fst.read(wj, ri = 1))
    }
    if (length(ri)) {
        ri <- ranges.hs.vector(ri)
        jg <- apply(ri, 2, function(ri) {
            wk.fst.read(wj, cn = cn, from = ri[1], to = ri[2], as.data.table = as.data.table, ...)
        })
        if (rbind) {
            rbindlist(jg)
        }
        else jg
    }
    else {
        read_fst(path = wj, columns = cn, from = from, to = to, as.data.table = as.data.table)
    }
}
wk.dt.rbind <- function(..., use.names, U = FALSE, fill = "auto") {
    L <- list(...)
    nc <- sapply(L, . %=>% {
        a <- ncol(x)
        if (is.null(a)) 
            a <- length(x)
        a
    })
    ref_i <- which.max(nc)
    ref_cn <- wk.dt.copy(names(L[[ref_i]]))
    if (inherits(L[[ref_i]], "data.table")) {
        wk.dt.reset_name(L[[ref_i]])
        on.exit(wk.dt.setnames(L[[ref_i]], ref_cn), add = TRUE)
    }
    else {
        names(L[[ref_i]]) <- paste0("V", seq_along(L[[ref_i]]))
    }
    if (missing(use.names)) 
        use.names <- all(sapply(L[-ref_i], function(x) names(x) %all.in% ref_cn))
    if (missing(fill)) 
        fill <- TRUE
    if (wk.dt.uniq_n(nc) > 1 && !fill) 
        stop("2023_08_03_023343|不等宽")
    if (use.names) {
        for (li in seq_along(L)[-ref_i]) {
            a <- names(L[[li]])
            names(L[[li]]) <- paste0("V", match(a, ref_cn))
        }
    }
    else {
        for (li in seq_along(L)[-ref_i]) {
            names(L[[li]]) <- paste0("V", seq_along(L[[li]]))
        }
    }
    jg <- do.call(data.table::rbindlist, list(L, use.names = use.names, fill = fill))
    wk.dt.setnames(jg, ref_cn)
    if (U) 
        unique(jg)
    else jg
}
wk.dt.reshape <- function(dt1, row_name_name, column_name_name, value_function) {
    cn <- dt1[, get(column_name_name) %=>% hs.clean(U = 1)]
    rn <- dt1[, get(row_name_name) %=>% hs.clean(U = 1)]
    m1 <- matrix(NA, length(rn), length(cn), dimnames = list(rn, cn))
    for (i1 in dt1[, seq.int(.N)]) {
        if (0) {
            i1 <- 1426
            message(i1)
            if (i1 == 1426) 
                hs.browser("37.05.12.144909")
        }
        rn1 <- dt1[i1, get(row_name_name)]
        if (is.na(rn1)) 
            next
        cn1 <- dt1[i1, get(column_name_name)]
        m1[rn1, cn1] <- value_function(dt1[i1])
    }
    m1
}
wk.dt.clean <- function(dt1) {
    ci2rm <- sapply(dt1, wk.dt.uniq_n) %=>% seq_along(x)[x %in% 1]
    if (length(ci2rm)) 
        dt1[, `:=`(c(ci2rm), {
        })]
    dt1
}
wk.dt.replace <- function(dt, hs1, hs2 = function(...) TRUE, ci = {
}, ci_pattern = {
}) {
    if (!length(ci)) 
        ci <- seq_along(dt)
    if (is.character(ci)) 
        ci <- match(ci, names(dt))
    if (length(ci_pattern)) 
        ci <- ci %or% hs.grep(names(dt), ci_pattern)
    for (i1 in ci) {
        if (!hs2(dt[[i1]])) 
            next
        cn1 <- names(dt)[i1]
        cn2 <- paste0(cn1, ".38_03_16_210715")
        setnames(dt, i1, cn2)
        dt[, `:=`(c(i1), hs1(get(cn2)))]
        setnames(dt, i1, cn1)
    }
}
wk.dt.reset_name <- function(dt) {
    data.table::setnames(dt, paste0("V", seq_along(dt)))
}
wk.dt.1to1 <- function(dt, c1 = 1, c2 = 2, side = 1:2) {
    c1i <- if (is.character(c1)) 
        match(c1, names(dt))
    else c1
    c2i <- if (is.character(c2)) 
        match(c2, names(dt))
    else c2
    tag1 <- "_38_03_24_111356"
    c1 <- paste0(names(dt)[c1i], tag1)
    wk.dt.setnames(dt, c1i, c1)
    c2 <- paste0(names(dt)[c2i], tag1)
    wk.dt.setnames(dt, c2i, c2)
    dt0 <- dt
    for (i1 in side) {
        cn1 <- c(c1, c2)[i1]
        cn2 <- c(c1, c2)[-i1]
        dt <- evalbq(dt[, {
            if (wk.dt.uniq_n(.(as.symbol(cn2))) == 1) 
                .SD
        }, by = c(cn1)])
    }
    wk.dt.col.reorder(dt, c(c1, c2), c(c1i, c2i))
    wk.dt.setnames(dt0, c1, hs.sub(c1, tag1))
    wk.dt.setnames(dt0, c2, hs.sub(c2, tag1))
    wk.dt.setnames(dt, c1, hs.sub(c1, tag1))
    wk.dt.setnames(dt, c2, hs.sub(c2, tag1))
}
wk.dt.collapse <- function(dt, cn2do = {
}, by = {
}, sep = ";", keep_order = 1, U = 0, expand = 0) {
    if (!length(cn2do)) 
        cn2do <- names(dt) %not% by
    if (is.numeric(cn2do)) {
        cn2do.ci <- cn2do
        cn2do <- names(dt)[cn2do.ci]
    }
    else cn2do.ci <- match(cn2do, names(dt))
    if (!length(by)) 
        by <- (names(dt) %not% cn2do)[1]
    if (expand) 
        dt <- wk.dt.expand(dt, by, split = sep)
    sep.38_09_04_232534 <- sep
    if (dt[, wk.dt.uniq_n(.SD) == .N, .SDcols = c(by)]) 
        return(dt)
    jg <- if (U) {
        dt[, lapply(.SD, function(x) {
            paste(hs.str_as(unique(x)), collapse = sep.38_09_04_232534)
        }), by = c(by)]
    }
    else dt[, lapply(.SD, function(x) {
        paste(hs.str_as(x), collapse = sep.38_09_04_232534)
    }), by = c(by)]
    wk.dt.setnames(jg, length(by) + 1, cn2do)
    if (keep_order) 
        wk.dt.col.reorder(jg, names(dt) %and% names(jg))
    jg
}
wk.dt.db <- function(x, i = {
}, inv = FALSE) {
    if (!inherits(x, "data.table")) {
        if (inherits(x, "character") && (!length(dim(x)))) {
            x <- data.table::data.table(x[seq(1, length(x), by = 2)], x[seq(2, length(x), by = 2)])
        }
        else hs.browser("2023.07.29.120726", debug = 0)
    }
    if (inv) {
        data.table::setcolorder(x, rev(names(x)))
        wk.dt.reset_name(x)
    }
    cn1 <- names(x)[1]
    if (!identical(data.table::key(x), cn1)) 
        data.table::setkeyv(x, cn1)
    if (length(i)) {
        return(x[i, .SD[[2]]])
    }
    else return(x)
}
hs.hs.key2wj <- function(uid, rm = 0) {
    hs.name <- ls(pattern = sprintf("%s$", uid), envir = .GlobalEnv)
    if (length(hs.name)) {
        hs <- get(hs.name, envir = .GlobalEnv)
        if (rm) {
            db <- environment(hs)[["db"]]
            for (uid1 in db) unlink(.mfp(uid1))
            unlink(environment(hs)[["db.wj.save2"]])
            rm(list = hs.name, envir = .GlobalEnv)
        }
        else hs
    }
    else {
        hs.name <- sprintf("hs.key2wj.%s", uid)
        hs <- local({
            db <- c()
            db.wj.save2 <- .mfp(sprintf("%s.rd", uid))
            cat(sprintf("database file: %s\n", db.wj.save2))
            if (file.exists(db.wj.save2)) 
                db <- hs.load(db.wj.save2)
            save <- 0
            function(...) {
                kw <- as.character(c(...))
                if (!length(kw)) 
                  return(db)
                for (kw1 in kw) if (!(kw1 %in% names(db))) {
                  save <<- 1
                  db[kw1] <<- .muid4machine()
                }
                if (save) {
                  save(db, file = db.wj.save2)
                  save <<- 0
                }
                db[unlist(kw)]
            }
        })
        assign(hs.name, hs, envir = .GlobalEnv)
        get(hs.name, envir = .GlobalEnv)
    }
}
wk.sync.code.r.2server <- local({
    mt <- list()
    mt.wj <- .mfp("34.10.02.060845.rd", hs.home.expand("~/org/db"))
    if (file.exists(mt.wj)) 
        mt <- hs.load(mt.wj)
    function(where = wk.ssh, only = {
    }, rp = 0) {
        hs.init()
        wj <- c(hs.wj(wk.dm.wjj, c("init.r", "all.r.rd")), if (rp) hs.home.expand("~/rjk/ln-s/.Rprofile"))
        did <- 0
        if (length(only)) 
            return(switch(only, wj = wj))
        for (i1 in seq_along(wj)) {
            wj1 <- wj[i1]
            if (length(mt[[wj1]]) && (mt[[wj1]] >= file.mtime(wj1))) {
            }
            else {
                r1 <- wk.rsync.out.same_place(wj1, L = 1, ssh = where)
                mt[[wj1]] <<- Sys.time()
                did <- 1
            }
        }
        if (did) {
            save(mt, file = mt.wj)
            if (length(only)) 
                switch(only, wj = wj2sync)
        }
        else hs.cat("wk.sync.code.r.2server: no need to sync")
    }
})
hs.r.2server.4jdi <- function(wj1 = hs.home.expand("~/tmp/jdi.r")) {
    if (nchar(wj1) == 0) 
        wj1 <- readline("请输入文件路径：\n")
    wk.rsync.out.same_place(wj1)
}
hs.dm.2svr <- function(wjj = hs.home.expand("~/org/dm"), where = wk.ssh, only = {
}) {
    wjj_sub <- hs.wjj.ls(wjj) %=>% x[dir.exists(x)]
    if (length(wjj)) {
        if (length(only)) {
            switch(only, wj = wjj_sub)
        }
        else wk.rsync.out(wjj_sub, wjj, ssh = where)
    }
}
hs.jdi <- function(local = 1, up = parent.frame()) {
    wj <- hs.home.expand(paste0("~/tmp/jdi.", c("r", "rd", "r.xz")))
    wj <- wj[file.exists(wj)]
    if (length(wj) > 1) 
        wj <- wj[which.max(file.mtime(wj))]
    if (0) {
        a1 <- hs.load(wj)
        head(a1)
    }
    env1 <- if (local) 
        up
    else .GlobalEnv
    hs.source.wj(wj, env = env1)
}
hs.xm.do <- local({
    hs1 <- function(mb = {
    }, ff = {
    }) {
        if (length(mb)) {
            cat("The aim is ", mb, "\n", sep = "")
            if (length(ff)) {
            }
            else hs1(mb = mb, ff = readline("What is the method?\n"))
        }
        else hs1(mb = readline("What is the aim?\n"))
    }
    function(mb = {
    }, ff = {
    }) repeat {
        hs1()
    }
})
hs.xm.template <- function(uid) {
    wjj0 <- hs.wjj(.mfp(uid))
    hs.wj(wjj0, "main.r")
    "mb,ff,sj,dm,jg,gj,wd"
}
hs.source.jdi <- function(wj1 = hs.home.expand("~/tmp/jdi.r"), ...) {
    if (!file.exists(wj1)) {
        sep <- formals(hs.source.part)[["sep"]]
        cat(sep, sep = "\n", file = hs.wj(wj1))
    }
    hs.source.part(wj1, ...)
}
hs.jdi.source <- function(wj1, i1, i2, envUp = .GlobalEnv) {
    if (file.exists(wj1)) {
        nr <- readLines(wj1)
        if (missing(i2)) {
            i2 <- i1
            if (is.character(i2)) {
                mi <- which(nr == i1)
                if (!hs.len1(mi, 2)) 
                  stop("please check the logic @ 34.11.26.005157 @ ", hs.home.expand("~/org/dm/r/xm.r"))
                i1 <- mi[1]
                i2 <- mi[2]
            }
        }
        else {
            if (is.character(i1)) {
                i1 <- match(i1, nr)
            }
            if (is.character(i2)) {
                i2 <- match(i2, nr)
            }
        }
        if (i1 > i2) 
            stop("please check the logic @ 34.11.26.005442 @ ", hs.home.expand("~/org/dm/r/xm.r"))
        nr <- head(nr, i2)
        nr <- tail(nr, -i1 + 1)
        eval(parse(text = nr), envUp)
    }
}
hs.hsgly <- local({
    uid.pattern <- wk.muid.pattern
    uid_end.pattern <- hs.p0(uid.pattern, "$")
    env.lt <- "env.loadtime.33.04.27.080101"
    eval(bquote({
        if (!exists(.(env.lt))) 
            .(env.lt) <- new.env()
    }), .GlobalEnv)
    {
        r.wj.wjj.remote <- hs.home.expand("~/tmp2/dm")
        uid.wj.wj <- .mfp("36.03.04.104650.tsv.gz", hs.home.expand("~/tmp2")) %=>% hs.wj
        if (0) {
            if (!hs.machine.isLocal()) 
                uid.wj.wj <- hs.sub(uid.wj.wj, "~", r.wj.wjj.remote)
        }
        wj.wj <- {
        }
        wj.wj.scan_time <- {
        }
        uid.wj.wj.hs <- function(uid.wj = {
        }) {
            wj <- .wk("rg --no-line-number --no-filename -z", .q(paste0("(?i)", uid.pattern, "[^/]*\\.r(\\.[gx]z)?$")), wj.wj, intern = 1)
            wj.wj.scan_time <<- Sys.time()
            if (0) {
                wj <- hs.switch(Sys.info()["nodename"], "node9", "/dev/shm/wk", "~/1") %=>% normalizePath %=>% .wk("ag -g", .q(paste0("(?i)", uid.pattern, ".*\\.r(\\.[gx]z)?$")), .q(x), intern = 1) %=>% x[hs.grep(basename(x), paste0("(?i)", uid.pattern, ".*\\.r(\\.[gx]z)?$"))]
            }
            if (file.exists(r.wj.wjj.remote)) {
                wj <- c(wj, hs.wjj.ls(r.wj.wjj.remote, hs.p0(uid.pattern, "\\.r(\\.[gx]z)?$"), 1))
            }
            uid <- hs.re(basename(wj), uid.pattern)
            uid <- hs.gsub(uid, "[^0-9]", "_")
            if (length(uid) != length(wj)) 
                hs.browser("39.05.20.113718", debug = 0)
            tbl <- wk.dt(uid = uid, wj = wj)
            tbl <- tbl[, .(wj = if (.N > 1) 
                wj[which.max(file.mtime(wj))]
            else wj), by = uid]
            if (!identical(uid.wj, tbl)) {
                wk.dt.fwrite(tbl, uid.wj.wj)
                uid.wj <<- tbl
            }
        }
        uid.wj <- {
        }
        do.find.wj <- function(uid1) {
            uid1 <- hs.gsub(uid1, "[^0-9]", "_")
            jg <- uid.wj[hs.match(uid1, uid), hs.home.expand(wj)]
            if (length(jg) && (!file.exists(jg))) {
                a <- parse(uid.wj[hs.match("39_01_08_150201", uid), wj])
                eval(a[[1]])()
                uid.wj.wj.hs()
                jg <- uid.wj[hs.match(uid1, uid), wj]
            }
            jg
        }
    }
    function(wjm = {
    }, obj = {
    }, local = 0, ext = "R", wjj0 = wk.dm.wjj, env1 = .GlobalEnv, PO = 0, path.only = PO, blm.only = 0, load = 0, reload = 0, call = 0, argList = list(), rename = 0, bl = 1, parse.network = 0, less = 0, trim = 0, keep.mtime = 0, smart = 1, update.uid = !length(wjm)) {
        if (length(obj)) 
            return(uid.obj(wjm, obj))
        if (update.uid) {
            uid.wj.wj.hs()
            return()
        }
        uid <- hs.re(wjm, uid_end.pattern)
        wj <- do.find.wj(uid)
        if (!length(wj)) 
            stop(hs.p("####", wjm, "is not found. 34.07.04.195822 @ ", hs.home.expand("~/org/dm/r/xm.r"), " ####"))
        if (path.only) 
            return(wj)
        if (less) {
            .less(wj)
            return(wj)
        }
        blm <- hs.ls(env1, pattern = uid)
        if (length(blm)) {
            if (length(blm) > 1) {
                blm.lt_max.i1 <- which.max(sapply(blm, function(x1) env1[[env.lt]][[x1]]))
                rm(list = blm[-blm.lt_max.i1], envir = env1)
                blm <- blm[blm.lt_max.i1]
            }
            if (blm.only) 
                return(blm)
            blm.loadtime <- env1[[env.lt]][[blm]]
            if ((!length(blm.loadtime)) || (difftime(file.mtime(wj), blm.loadtime, units = "secs") > 1.5)) {
                message("Need to (re)build the function ", blm, " due to its lagged mtime.")
                blm <- {
                }
            }
        }
        if ((!length(blm)) || reload) {
            nr <- unlist(strsplit(hs.readLines.robust(wj), "[\n\r]"))
            blm <- {
                blm1 <- hs.sub(nr[1], "[# ]+")
                if (blm1 == "") 
                  blm1 <- hs.sub(basename(wj), paste0("[_\\.]?", wk.muid.pattern, ".*"))
                if (blm1 == "") 
                  blm1 <- basename(dirname(wj))
                paste0(blm1, ".", uid)
            }
            if (blm.only) 
                return(blm)
            src <- parse(text = nr)
            if (!identical(env1, .GlobalEnv)) 
                hs.browser("37.12.21.173759")
            eval(bquote({
                .(blm) <- wk.33.06.24.202656.v2.auto(.(src[[1]]))
                .(as.symbol(env.lt))[[.(blm)]] <- .(if (file.exists(wj)) 
                  file.mtime(wj)
                else Sys.time())
            }), envir = env1)
        }
        R <- env1[[blm]]
        if (0) {
            hs.env(R)[["env2.33.06.24.181217"]][["body.34.07.07.065614"]]
        }
        t1 <- file.mtime(wj)
        t2 <- -Inf
        uid1 <- unique(unlist(hs.gre(hs.readLines.robust(wj), uid.pattern)))
        if (length(uid1)) {
            t2.wj <- unlist(sapply(uid1, do.find.wj))
            if (length(t2.wj)) 
                t2.wj <- t2.wj[file.exists(t2.wj)]
            if (length(t2.wj)) 
                t2 <- max(file.mtime(t2.wj))
        }
        if (t2 > t1) {
            if (smart) {
                formals(R)[["..new"]] <- 1
                hs.Sys.setFileTime(wj, t2)
            }
            else if (!keep.mtime) 
                hs.Sys.setFileTime(wj, t2)
        }
        if (0) {
            hs.browser("38.04.20.222242")
            ls(hs.env(R))
            hs.env(R)[["env2.33.06.24.181217"]][["body.34.07.07.065614"]]
        }
        R
    }
})
hs.hsgly.bak <- function(wjm, bds, local = 0, ext = "R", wjj0 = ieiwk("wjj.dm"), env1 = .GlobalEnv, PO = 0, path.only = PO, blm.only = 0, load = 0, reload = 0, call = 0, argList = list(), rename = 0, bl = 1, parse.network = 0, less = 0, trim = 0, keep.mtime = 0, smart = 1) {
    uid <- hs.re(wjm, "\\d+\\.\\d{2}\\.\\d{2}\\.\\d{6}$")
    wj <- .mfp(wjm, wjj0, act = "find", uid.pos = "tail")
    wj2 <- hs.wjm(.mfp(wjm, wjj0, uid.pos = "tail"), add = ext)
    if (length(wj) > 1) 
        wk.browser("34.10.31.172013 @ ~/org/dm/r/xm.r")
    if (missing(bds)) {
        if (!length(wj)) 
            wj <- wj2
    }
    else {
        if (!hs0.seq.eql(wj, wj2)) {
            if (length(wj)) 
                file.rename(wj, wj2)
            wj <- wj2
        }
    }
    if (path.only) 
        return(wj)
    if (((!length(wj)) || (!file.exists(wj))) && missing(bds)) {
        wj3 <- .mfp(wjm, wk.dm.wjj, act = "find", uid.pos = "tail")
        wj3 <- hs.grep(wj3, "\\.r$", v = 1)
        if (length(wj3)) {
            if (length(wj3) > 1) {
                wk.browser("34.10.13.105040")
            }
            else source(wj3)
        }
        else {
            hs.browser("35.11.26.104320")
            stop(paste("####", wj, "is not found. 34.07.04.195822 @ ~/org/dm/r/function.common.R ####"))
        }
    }
    if (less) 
        return(.less(wj))
    cat2wj <- 0
    if (!missing(bds)) {
        bds.nr <- format(substitute(bds))
        if (trim) 
            bds.nr <- trim(bds.nr)
        cat2wj <- 1
        if (file.exists(wj)) {
            wj.nr <- readLines(wj, encoding = "UTF-8")
            if (hs0.seq.eql(wj.nr, bds.nr)) 
                cat2wj <- 0
        }
        if (cat2wj) {
            if (keep.mtime) 
                mt1 <- file.mtime(wj)
            cat(bds.nr, file = wj, sep = "\n")
            if (keep.mtime) {
                hs.say("hs.hsgly: keeping the previous mtime of", wj)
                Sys.setFileTime(wj, mt1)
            }
        }
    }
    wj.blm <- hs.wjm(wj, dn = 0, ext = 0)
    if (wj.blm == uid) {
        if (wjm != uid) {
            wj.blm <- wjm
        }
        else warning(paste("####", wjm, "is not meaningful, 34.07.04.195811 @", hs.home.expand("~/org/dm/r/xm.r"), " ####"))
    }
    blm <- ls(env1, pattern = uid, all.names = TRUE, sorted = FALSE)
    if (!length(blm)) 
        blm <- wj.blm
    if (blm.only) 
        return(blm)
    env.lt <- "env.loadtime.33.04.27.080101"
    if (!exists(env.lt, env1, inherits = FALSE)) 
        env1[[env.lt]] <- new.env(parent = env1)
    blm.loadtime <- env1[[env.lt]][[blm]]
    new <- 0
    if (missing(bds)) {
        t1 <- file.mtime(wj)
        t2 <- -Inf
        uid1 <- unique(unlist(hs.gre(readLines(wj), "\\b\\d+\\.\\d{2}\\.\\d{2}\\.\\d{6}\\b")))
        if (length(uid1)) {
            t2.wj <- sapply(uid1, hs.hsgly, path.only = 1)
            t2.wj <- t2.wj[file.exists(t2.wj)]
            if (length(t2.wj)) 
                t2 <- max(file.mtime(t2.wj))
        }
        if (t2 > t1) {
            if (smart) {
                new <- load <- 1
                Sys.setFileTime(wj, t2)
            }
            else if (!keep.mtime) 
                Sys.setFileTime(wj, t2)
        }
    }
    if (exists(blm, env1, inherits = FALSE)) {
        if ((!length(blm.loadtime)) || (difftime(file.mtime(wj), blm.loadtime, units = "mins") > 1)) 
            load <- 1
    }
    else {
        if (bl) {
            load <- 1
        }
        else wk.browser("34.08.12.223657")
    }
    if (load || reload) {
        if (!file.exists(wj)) 
            stop("no file found. 34.07.13.054404 @ ", hs.home.expand("~/org/dm/r/xm.r"))
        bds <- eval(parse(wj, encoding = "UTF-8", keep.source = FALSE))
    }
    if (0) {
        if (length(bds)) {
            bds.str <- format(bds)
            .less(wj)
            if (any(grepl("hs.dmgly(\"sj.34.01.09.135117\")()", bds.str, fix = TRUE))) {
                hs.browser("34.08.12.233818")
                sys.calls()
            }
        }
    }
    if (cat2wj || new || (!missing(bds))) {
        bds <- bquote(wk.33.06.24.202656.v2.auto(.(bds), new = .(new)))
        R <- eval(bds, env1)
        if (bl) {
            env1[[env.lt]][[blm]] <- Sys.time()
            env1[[blm]] <- R
        }
        R
    }
    else {
        if (missing(bds)) {
            R <- env1[[blm]]
            if (formals(R)[["..new"]]) 
                formals(R)[["..new"]] <- 0
            R
        }
    }
}
hs.dmgly <- function(uid = {
}, wj = {
}, env1 = parent.frame()) {
    if (length(uid)) {
        wj %<=>% (`%or%`(sapply(uid, function(uid1) {
            uid1 <- hs.re(basename(uid1), environment(hs.hsgly)[["uid.pattern"]])
            hs.hsgly(uid1, PO = 1)
        })))
    }
    for (wj1 in wj) source(wj1, local = env1)
}
hs.sjgly <- function(uid1, bf = {
}, ot = "wjj2") {
    hs.hsgly()
    uid1 <- hs.re(basename(uid1), wk.muid.pattern)
    wjj0 <- hs.hsgly(uid1, PO = 1) %=>% hs.dirname
    if (ot == "wjj0") 
        return(wjj0)
    wjj0 %<=>% normalizePath
    wjj1 <- wjj0
    repeat {
        wj1 <- hs.wjj.ls(wjj1, paste0("^根?文件夹\\.", wk.muid.pattern, "\\.r(\\.gz)?$"))
        if (length(wj1) == 1) 
            break
        if (length(wj1) > 1) 
            stop("[38_11_27_001548]")
        wjj1 <- dirname(wjj1)
        if (wjj1 %=>% (x == dirname(x))) 
            stop("[39_03_02_160542|没有上层文件夹]", wjj0)
    }
    wjj2 <- wjj0_jg <- hs.hsgly(hs.re(basename(wj1), wk.muid.pattern))()
    if (nchar(wjj0) > nchar(wjj1)) 
        wjj2 <- paste0(wjj0_jg, substring(wjj0, nchar(wjj1) + 1))
    wj_flag <- hs.fp(wjj2, paste0("代码号_", uid1))
    if (0) {
        wj_flag %=>% {
            if (file.exists(x) && (!file.is_symlink(x))) 
                hs.wj.rm(x)
        }
    }
    if (length(bf)) {
        if (inherits(bf, c("numeric", "logical"))) 
            bf <- hs.uid.fp()
        wjj_bf <- hs.fp(wjj2, "备份", bf)
        wj2bf <- hs.wjj.ls(wjj2, wjj = 1, wj = 1) %=>% x[-hs.grep(basename(x), "^(代码号_|备份)")]
        hs.wj.move(wj2bf, wjj_bf)
        return()
    }
    if ((!file.exists(wj_flag))) 
        local({
            a <- hs.wjj.ls(wjj0_jg, uid1, R = 1)
            hs.switch(length(a), 1, {
                wjj_old <- dirname(a)
                if (dir.exists(wjj2)) {
                  for (wj1 in hs.wjj.ls(wjj_old, A = 1, wjj = 1, wj = 1)) hs.wj.move(wj1, wjj2)
                  hs.wj.rm(wjj_old, 1)
                }
                else {
                  wjj_temp <- paste0(wjj_old, ".39_05_15_112003")
                  hs.wj.move(wjj_old, wjj_temp)
                  hs.wj.move(wjj_temp, hs.wj(wjj2))
                }
            }, 0, {
                a <- environment(hs.hsgly)[["uid.wj"]][hs.grep(uid, hs.gsub(uid1, "_", ".")), wj]
                {
                  if (0) {
                    file.symlink(a, wj_flag)
                  }
                  cat(normalizePath(a), file = hs.wj(wj_flag))
                }
            }, hs.browser("38.11.27.083724", debug = 0))
        })
    if (file.exists(wj_flag)) 
        local({
            fp_real <- environment(hs.hsgly)[["uid.wj"]][hs.grep(uid, hs.gsub(uid1, "_", ".")), wj %=>% normalizePath]
            fp_recorded <- readLines(wj_flag)
            if (fp_recorded != fp_real) 
                cat(fp_real, file = wj_flag)
        })
    wjj2
}
uid.obj <- function(uid1, on1, ...) {
    if (0) {
        uid1 <- "36_12_21_002840"
        on1 <- "ver.hs.org1"
    }
    obj1 <- hs.getter(hs.hsgly(uid1), on1)
    if (...length()) {
        obj1(...)
    }
    else obj1
}
uid.hs.str <- function(s1) {
    hs.re(s1, wk.muid.pattern)
}
hsm.hs.uid <- function(uid) {
    wj <- hs.hsgly(uid, PO = 1)
    if (hs.len1(wj) && file.exists(wj)) {
        l1 <- readLines(wj, 1)
        paste0(hs.sub(l1, "^#+ *"), ".", uid.hs.str(uid))
    }
}
hs.hsm.updator <- function() {
    if (any(hs.grepl(hs.getter(sjk.uid.hsm, "hsm"), "[\"']"))) 
        stop("36_11_11_230733 警告：函数名带引号")
    if (min(nchar(hs.sub(hs.getter(sjk.uid.hsm, "hsm"), wk.muid.pattern))) < 3) 
        stop("36_11_11_233445 函数名太短")
    wj.V <- .wk("ag -g \"\\.(r|org)$\"", hs.home.expand("~/org"), intern = 1)
    for (wj1 in wj.V) {
        changed <- 0
        nr <- readLines(wj1)
        if (!length(nr)) 
            next
        re_match <- gregexpr("(?<=hs\\.hsgly\\() *[\"'][^\"']+", nr, perl = TRUE)
        li <- which(sapply(re_match, function(.) hs.len1(.) && (. > 0)))
        for (li1 in rev(li)) {
            matched <- regmatches(nr[li1], re_match[[li1]])
            hsm <- hs.clean(sjk.uid.hsm(matched))
            if (!length(hsm)) 
                next
            if (hs.re(matched, "[^\"']+$", no_match = "") != hsm) {
                changed <- 1
                regmatches(nr[li1], re_match[[li1]]) <- hs.sub(matched, "[^\"']+$", hsm)
            }
        }
        if (changed) {
            hs.cat(wj1)
            cat(nr, sep = "\n", file = wj1)
        }
    }
}
hs.uid_hs <- function() {
    line.V <- .wk("ag -G \"(?i)\\.(r|org)$\" --nofilename \"hs\\.hsgly\\(\"", hs.home.expand("~/org"), intern = 1)
    str_raw <- hs.gre(hs.clean(line.V), "(?<=hs\\.hsgly\\() *[\"'][^\"']+") %=>% unlist %=>% unique
    unique(uid.hs.str(str_raw))
}
sjk.uid.hsm <- local({
    hsm <- c()
    hsm.hs <- function() {
        uid.V <- hs.uid_hs() %=>% sort
        hsm <- hs.lapply(uid.V, hsm.hs.uid)
        if (max(sapply(hsm, length)) > 1) 
            stop("36_11_11_224307")
        hsm <<- unlist(hsm)
    }
    function(uid) {
        if (!length(hsm)) 
            hsm.hs()
        hsm[uid.hs.str(uid)]
    }
})
wk.33.06.24.203348 <- function(env = parent.frame()) {
    evalq({
        env1.33.06.24.181214 <- environment()
        env2.33.06.24.181217 <- new.env()
    }, env)
}
ieiwk.make.arg.list <- function(...) {
    args <- list(...)
    names(args) <- paste(substitute(list(...)))[-1]
    args
}
hs.make.arg.wj <- function(..., link = "=", sep = "-", outer = sep, before = {
}, after = {
}) {
    z <- c(...)
    m <- paste(substitute(list(...)))[-1]
    str_c(c(before, if (length(z)) str_c(c(outer, str_c(str_c(m, z, sep = link), collapse = sep), outer), collapse = ""), after), collapse = ".")
}
hs.make.arg.wj.1 <- function(x, default, before = {
}, link = "=", sep = "-", outer = sep, after = {
}, up = parent.frame()) {
    if (any(c("symbol", "character") %in% typeof(x))) {
        M <- as.character(x)
        x <- get(M, envir = up)
    }
    else M <- as.character(substitute(x))
    if (missing(default) || (default != x)) {
        paste(c(before, sprintf("%s%s%s%s%s", outer, M, link, x, outer), after), collapse = ".")
    }
    else before
}
hs.mfp.arg <- function(uid, L, ext = "rd", up = parent.frame(), ...) {
    str_arg <- paste(unlist(sapply(seq_along(L), function(i1) {
        L1 <- L[i1]
        hs.make.arg.wj.1(names(L1), L1, up = up)
    })), collapse = ".")
    .mfp(paste(c(uid, if (nchar(str_arg)) str_arg, ext), collapse = "."), ...)
}
env_name.37_12_22_104219 <- ""
hs.localize <- function(hs1, e1 = parent.frame(), full = !identical(environment(hs1), .GlobalEnv), new = 1) {
    if (full) {
        ep <- if (new) 
            new.env(parent = e1)
        else e1
        on <- hs.ls(environment(hs1))
        for (on1 in on) ep[[on1]] <- environment(hs1)[[on1]]
        environment(hs1) <- ep
    }
    else environment(hs1) <- e1
    hs1
}
wk.33.06.24.202656.v2.auto <- function(hs1, envUp = parent.frame(), new = 0) {
    e1 <- substitute(hs1)
    env1 <- if (is.call(e1) && (e1[[1]] == as.symbol("local"))) {
        environment(hs1)
    }
    else new.env(parent = envUp)
    {
        i1 <- sapply(seq_along(body(hs1)), function(i1) {
            e1 <- body(hs1)[[i1]]
            if (hs.len1(e1) && ("name" %in% class(e1)) && (as.character(e1)[1] == "..meta.wj_out")) 
                i1
        }) %=>% hs.unlist
        if (length(i1) == 1) 
            body(hs1)[[i1]] <- body(..meta.wj_out)
    }
    eval(bquote({
        env2.33.06.24.181217 <- new.env()
        env2.33.06.24.181217[["body.34.07.07.065614"]] <- body(.(hs1))
    }), env1)
    a1 <- list(..mem = 0, ..mem.object_size.cutoff = 1e+05, ..only.wj_out = 0, ..only.wj2pass = 0, ..wj_old = {
    }, ..load = local({
        e1 <- as.character(tail(body(hs1), 1)[[1]])
        ifelse(hs.len1(e1) && (e1 == "wj_out"), 0, 1)
    }), ..load.hs = "smart", ..suppress.time = 0, ..new = new, ..finally = identity, ..debug = 0, ..ssh = {
    })
    a1 <- a1[names(a1) %rm% names(formals(hs1))]
    if (length(a1)) 
        formals(hs1) <- c(formals(hs1), a1)
    body(hs1) <- bquote({
        if (..mem) {
            ac.34.07.17.183129 <- "jg.34.07.06.060444"
            ac.34.07.17.183129 <- wk.digest.args(failed = "jg.34.07.06.060444", by = "size", fa = .(formalArgs(hs1)))
            if ((ac.34.07.17.183129 == "jg.34.07.06.060444") || (!hs.exists(ac.34.07.17.183129, env2.33.06.24.181217)) || ..new) {
                env2.33.06.24.181217[[ac.34.07.17.183129]] <- eval(env2.33.06.24.181217[["body.34.07.07.065614"]])
            }
            if (length(ac.34.07.17.183129) && ("jg.34.07.06.060444" != ac.34.07.17.183129)) 
                env2.33.06.24.181217[["jg.34.07.06.060444"]] <- env2.33.06.24.181217[[ac.34.07.17.183129]]
            env2.33.06.24.181217[["jg.34.07.06.060444"]]
        }
        else eval(env2.33.06.24.181217[["body.34.07.07.065614"]], NULL)
    })
    hs.localize(hs1, env1, new = 0)
}
..meta.wj_out <- function() {
    if (exists("..wj2pass", inherits = FALSEs)) {
        if (..only.wj2pass) 
            return(..wj2pass)
    }
    if (exists("wj_out", inherits = FALSE) && (length(wj_out))) {
        if (length(..ssh)) {
            ..only.wj_out <- 1
            if (all(file.exists(wj_out))) {
                for (wj1 in wj_out) wk.rsync.out.same_place(wj1, ssh = ..ssh)
            }
            else stop("结果文件不存在：", wj_out %=>% x[!file.exists(x)] %=>% paste(collapse = "\n"))
        }
        if (..only.wj_out) 
            return(wj_out)
        if (..load == "smart") 
            ..load <- hs.wj.exists(wj_out) && (file.size(wj_out) < 1e+06)
        if (..load && identical(..load.hs, "smart") && hs.len1(wj_out)) {
            ..load.hs <- if (any(hs.grepl(wj_out, "(?i)\\.(txt|tsv|csv)(\\.gz)?$"))) {
                fread
            }
            else hs.load
        }
        if (!exists("..wj_old", inherits = FALSE)) 
            ..wj_old <- {
            }
        ..wj_old <- hs.unlist(..wj_old)
        if (exists("wj_in", inherits = FALSE)) 
            ..wj_old <- ..wj_old %or% hs.unlist(wj_in)
        if (length(..wj_old) && ..suppress.time) {
            if (hs.wj.exists(wj_out)) 
                local({
                  t2 <- min(file.mtime(hs.unlist(wj_out)))
                  for (wj1 in ..wj_old) {
                    if (file.mtime(wj1) >= file.mtime(wj_out)) {
                      hs.Sys.setFileTime(wj1, t2)
                    }
                  }
                })
        }
        if ((!..new) && all(hs.wj.exists(wj_out)) && ifelse(length(..wj_old), max(file.mtime(..wj_old)) <= min(file.mtime(hs.unlist(wj_out))), TRUE)) {
            if (..load && hs.len1(wj_out)) {
                return(..finally(..load.hs(wj_out)))
                wj_out %=>% .less
            }
            else return(wj_out)
        }
    }
}
wk.33.06.24.202656.v2 <- function(env1 = parent.frame(), fa) {
    eval(bquote({
        ac.34.07.17.183129 <- "jg.34.07.06.060444"
        if (..mem) {
            ac.34.07.17.183129 <- wk.digest.args(failed = "jg.34.07.06.060444", by = "size", fa = .(fa), env1 = .(env1))
        }
        if ((!hs.exists(ac.34.07.17.183129, env2.33.06.24.181217)) || ..new) {
            env2.33.06.24.181217[[ac.34.07.17.183129]] <- eval(env2.33.06.24.181217[["body.34.07.07.065614"]])
        }
        if (length(ac.34.07.17.183129) && ("jg.34.07.06.060444" != ac.34.07.17.183129)) {
            env2.33.06.24.181217[["jg.34.07.06.060444"]] <- env2.33.06.24.181217[[ac.34.07.17.183129]]
        }
        env2.33.06.24.181217[["jg.34.07.06.060444"]]
    }), env1)
}
hs.try_with_time_limit <- function(expr, cpu = Inf, elapsed = Inf, failed = {
}) {
    y <- try({
        expr
    }, silent = TRUE)
    if (inherits(y, "try-error")) {
        message("time out 34.07.07.061813 @ ~/org/dm/r/xm.r")
        failed
    }
    else y
}
wk.digest.args <- function(env1 = parent.frame(), failed = {
}, by = "size", fa = formalArgs(sys.function(-1))) {
    PL <- mget(fa %rm% "...", env1, inherits = TRUE)
    if ("..." %in% fa) 
        PL <- append(PL, evalq(list(...), env1))
    an <- names(PL)
    if (length(an)) 
        PL <- PL[order(c(an, rep("", length(PL) - length(an))), sapply(PL, object.size))]
    switch(by, size = {
        if (object.size(PL) < 1e+06) {
            for (i1 in seq_along(PL)) if ("function" %in% class(PL[[i1]])) PL[i1] <- list(format(PL[[i1]]))
            digest::digest(PL)
        } else failed
    }, time = {
        hs.try_with_time_limit(digest::digest(PL), 3, failed = .(failed))
    })
}
.r1 <- function() hs.say("34.11.29.071505")
.ag.less <- function(cmd1, C = 9, l = 0, H = 1, exclude_fixed = {
}, exclude = {
}, zim = 0, less = 1, include = {
}) {
    a1 <- .wk(cmd1, "| ag --passthrough", .q(s1), intern = 1)
    if (length(exclude_fixed)) 
        a1 <- a1 %rm% exclude_fixed
    for (e1 in exclude) a1 <- hs.grep(a1, e1, v = 1, inv = 1)
    for (p1 in include) 1
    if (zim) {
        i1 <- hs.grep(a1, sprintf("^\033\\[1;32m%s", hs.home.expand("~")))
        wj <- a1[i1]
        wj <- hs.sub(wj, "^\033\\[1;32m")
        wj <- hs.sub(wj, "\033\\[0m\033\\[K$")
        wj <- hs.path2zim(wj)
        a1[i1] <- sprintf("\033[1;32m%s\033[0m\033[K", wj)
    }
    if (less) {
        wj4less <- hs.wj.temp()
        on.exit(hs.rm.wj(wj4less), add = TRUE)
        cat(a1, file = wj4less, sep = "\n")
        .wk("less -R -N", wj4less)
    }
    else return(a1)
    if (0) 
        if (length(a1) && (!hs.match("", a1))) {
            wj1 <- hs.sub(a1[1], "^\033\\[1;32m")
            wj1 <- hs.sub(wj1, "\033\\[0m\033\\[K")
            .wk("~/wk/rj/zim/zim.py", "~/xt", wj1, "&")
        }
    if (0) {
        wj <- .ag(sprintf("%s", s1), "~/xt", G = "\\.txt$", l = 1, m = 1, intern = 1)
        wj <- hs.wjm(wj, ext = 0)
        wj <- hs.sub(wj, path.expand("~/xt"))
        wj <- hs.gsub(wj, "/", ":")
        if (!length(wj)) {
            hs.say("nothing")
        }
        else if (hs.len1(wj)) {
            .wk("~/wk/rj/zim/zim.py", "~/xt", wj, "&")
        }
        else {
            if (l) {
                cat(wj, sep = "\n")
            }
            else .ag(sprintf("%s", s1), "~/xt", G = "\\.txt$", H = H)
        }
    }
}
.ag1 <- function(s1, wjj1, ext1 = "txt|org|r|el", C = 9, l = 0, H = 1, exclude_fixed = {
}, exclude = {
}, exclude_wj = {
}, zim = 0, less = 1, include = {
}, m1 = 0) {
    if (0) {
        s1 <- c("@前线", "@尽快")
        wjj1 <- "~/xt"
    }
    if (0) {
        cmd1 <- .sys("ag -l", .q(s1[1]), wjj1)
        if (length(s1) > 1) 
            for (i1 in 2:length(s1)) cmd1 <- .sys(cmd1, "| xargs ag -l", .q(s1[i1]))
        .wk(cmd1, "| xargs ag --color -H -C", .q(paste(paste0("(", s1, ")"), collapse = "|")))
    }
    p1 <- .q(paste(rep(paste0("(", paste(paste0("(", s1, ")"), collapse = "|"), ")"), length(s1)), collapse = ".*"))
    cmd1 <- .sys("ag --color -H -C", C, if (length(exclude_wj)) 
        paste("--ignore", exclude_wj), "-m", m1, sprintf("-G \"(?i)\\.(%s)$\"", ext1), p1, wjj1, "2>/dev/null", "| ag --passthrough", p1)
    a1 <- suppressWarnings(.wk(cmd1, intern = 1))
    if (length(exclude_fixed)) 
        a1 <- a1 %rm% exclude_fixed
    for (e1 in exclude) a1 <- hs.grep(a1, e1, v = 1, inv = 1)
    if (zim) {
        i1 <- hs.grep(a1, sprintf("^\033\\[1;32m%s", hs.home.expand("~")))
        wj <- a1[i1]
        wj <- hs.sub(wj, "^\033\\[1;32m")
        wj <- hs.sub(wj, "\033\\[0m\033\\[K$")
        wj <- hs.path2zim(wj)
        a1[i1] <- sprintf("\033[1;32m%s\033[0m\033[K", wj)
    }
    if (less) {
        wj4less <- hs.wj.temp()
        on.exit(hs.rm.wj(wj4less), add = TRUE)
        cat(a1, file = wj4less, sep = "\n")
        .wk("less -R -N", wj4less)
    }
    else return(a1)
    if (0) 
        if (length(a1) && (!hs.match("", a1))) {
            wj1 <- hs.sub(a1[1], "^\033\\[1;32m")
            wj1 <- hs.sub(wj1, "\033\\[0m\033\\[K")
            .wk("~/wk/rj/zim/zim.py", "~/xt", wj1, "&")
        }
    if (0) {
        wj <- .ag(sprintf("%s", s1), "~/xt", G = "\\.txt$", l = 1, m = 1, intern = 1)
        wj <- hs.wjm(wj, ext = 0)
        wj <- hs.sub(wj, path.expand("~/xt"))
        wj <- hs.gsub(wj, "/", ":")
        if (!length(wj)) {
            hs.say("nothing")
        }
        else if (hs.len1(wj)) {
            .wk("~/wk/rj/zim/zim.py", "~/xt", wj, "&")
        }
        else {
            if (l) {
                cat(wj, sep = "\n")
            }
            else .ag(sprintf("%s", s1), "~/xt", G = "\\.txt$", H = H)
        }
    }
}
.ag <- function(s1, wjj2do = hs.home.expand("~/1"), wj = {
}, C = 9, less = 1, l = 0, intern = 0, G = {
}, ig = {
}, m = 0, silent = 1, H = 1) {
    if (!length(wjj2do)) 
        wjj2do <- c("~/org", "~/xt", "~/rj/sh", "~/rjk/emacs/myLisp") %=>% hs.home.expand
    if (length(wj)) {
        wjj2do <- switch(wj, r = "~/org/dm/r", org = "~/org", zim = hs.home.expand("~/xt"), sh = hs.home.expand("~/rj/sh"), el = hs.home.expand("~/rjk/emacs/myLisp"))
        G <- switch(wj, r = "\\.[rR]$", org = "\\.org$", zim = "\\.txt$", el = "\\.el$")
        ig <- switch(wj, org = c("org/2do.org", "org/dm.r.org"))
    }
    .wk("ag", if (l) 
        "-l", if (C) 
        c("-C", C), if (less) 
        c("--pager", .q("less -R"))
    else "--nocolor", "--ignore", .q("*~"), "--ignore", .q("*#"), if (length(G)) 
        c("-G", .q(G)), if (length(ig)) 
        paste("--ignore", .q(ig)), if (m) 
        c("-m", m), if (silent) 
        "--silent", ifelse(H, "--heading", "--noheading"), "-S", .q(s1), wjj2do)
    a1 <- unlist(sapply(wjj2do, function(wjj1) {
        .wk("ag", if (l) 
            "-l", if (C) 
            c("-C", C), if (less) 
            "--color"
        else "--nocolor", "--ignore", .q("*~"), "--ignore", .q("*#"), if (length(G)) 
            c("-G", .q(G)), if (length(ig)) 
            paste("--ignore", .q(ig)), if (m) 
            c("-m", m), if (silent) 
            "--silent", ifelse(H, "--heading", "--noheading"), "-S", .q(s1), wjj1, intern = 1)
    }))
    unlist(sapply(wjj2do, function(wjj1) {
        .wk("ag", if (l) 
            "-l", if (C) 
            c("-C", C), if (less) 
            c("--pager", .q("less -R"))
        else "--nocolor", "--ignore", .q("*~"), "--ignore", .q("*#"), if (length(G)) 
            c("-G", .q(G)), if (length(ig)) 
            paste("--ignore", .q(ig)), if (m) 
            c("-m", m), if (silent) 
            "--silent", ifelse(H, "--heading", "--noheading"), "-S", .q(s1), wjj1, intern = intern)
    }))
}
.orz <- function(C = 2) {
    .wk("ag -az -C", C, "-G \"$4\" --pager \"less -NiR\" -S \"$1\" $3")
}
r.str <- local({
    hs1 <- function(v1, color = "number", width.equal = 1) {
        paste0({
        }, 1)
        color1 <- if (length(color)) 
            switch(color, number = "\033[1;32m", , search = "\033[30;43m", space1 = "\033[7;96m", space2 = "\033[7;93m")
        color1.end <- if (length(color)) 
            "\033[0m\033[K"
        sprintf(paste0(color1, "%", if (width.equal) 
            max(nchar(v1)), "s", color1.end), v1)
    }
    tab.space.hs <- function(w1) sprintf(paste0("%", w1, "s"), " ")
    tab.width.in <- 2
    tab.space <- tab.space.hs(tab.width.in)
    wj1 <- ""
    function(wj.r = readline("输入文件名："), tab.width = 2, ud = 0) {
        if ((!length(wj.r)) || (!file.exists(wj.r[1]))) 
            wj.r <- wj1
        if (!file.exists(wj.r)) 
            stop("【文件不存在】", wj.r)
        if (wj1 != wj.r) 
            wj1 <<- wj.r
        orz("^(#|.*{\\s*##)", wj2do = wj.r, ud = ud)
        return()
        if (tab.width.in != tab.width) {
            tab.width.in <<- tab.width
            tab.space <<- tab.space.hs(tab.width)
        }
        a1 <- .wk("ag -az", .q("^(##+|.*{ *##)"), wj.r, intern = 1)
        rn <- hs1(hs.re(a1, "\\d+"))
        lines <- hs.re(a1, "(?<=:).*")
        spaces_leading <- {
            a1 <- hs.re(lines, "^\\s*")
            hs.gsub(a1, "\t", tab.space)
        }
        cn <- {
            cn <- 1 + nchar(spaces_leading)
            hs1(ceiling(cn/2), color = {
            })
        }
        spaces_leading_colored <- sapply(spaces_leading, function(s1) {
            l1 <- nchar(s1)
            if (l1) {
                if (l1%%tab.width) {
                  hs1(s1, color = "search", width.equal = 0)
                }
                else {
                  n1 <- l1/tab.width
                  block.color <- head(rep(c("space1", "space2"), ceiling(n1/2)), n1)
                  paste(sapply(block.color, function(c1) hs1(tab.space, color = c1)), collapse = "")
                }
            }
            else s1
        })
        wj_temp <- hs.wj.temp()
        on.exit(hs.rm.wj(wj_temp), add = TRUE)
        cat(paste0(rn, ":", cn, ":", spaces_leading_colored, trim(lines)), file = wj_temp, sep = "\n")
        .wk("less -NiR", wj_temp)
    }
})
z.str <- local({
    wj.wjj <- hs.wjj(.mfp("36.02.16.141824"))
    function(node = .wk("xsel --clipboard --output", intern = 1), tab.width = 2) {
        wj <- paste0("~/xt/", hs.gsub(node[1], ":", "/"), ".txt")
        if (file.exists(wj)) {
            wj.wjj %=>% hs.wjj.clean
            wjj <- hs.wjm(wj, ext = 0)
            file.symlink(c(wj, if (file.exists(wjj)) wjj), wj.wjj)
        }
        if (length(hs.wjj.ls(wj.wjj))) {
            wjj0 <- getwd()
            on.exit(setwd(wjj0), add = TRUE)
            setwd(wj.wjj)
            hs.wjj.ls() %=>% x[!file.exists(x)] %=>% {
                if (length(x)) 
                  hs.rm.wj(x)
            }
            .wk("ag -f", "--pager", .q("less -NiR"), .q("^(=+ .* =+|\\s*\\* .*)$"), "--color-match", .q("1;34"))
        }
    }
})
.gz <- function(s1 = readline()) {
    .wk("ag", "--pager", .q("less -Rr"), "-f", "-G", .q("dw\\.(txt|org)$"), s1, "~/wk/热点")
}
.ot <- function(s1, C = 9, l = 0, H = 1) {
    .ag1(s1 = s1, wjj1 = "~/org", ext1 = "org", C = C, l = l, H = H, less = 1, exclude_wj = c("dm.r.org", "wl.r.org", "wl.org"))
}
.ag.org <- function(s1, C = 9, l = 0, H = 1) {
    .ag1(s1 = s1, wjj1 = "~/org", C = C, l = l, H = H, less = 1, exclude_wj = c("dm.r.org", "wl.r.org", "wl.org"))
}
zim <- hs.path2zim <- function(wj1 = readline(), open = 1) {
    a <- hs.re(wj1, "[^:]*")
    if (file.exists(a)) {
        wj1 <- a
    }
    else {
        ag_jg <- .wk("ag", .q(wj1), "~/xt", intern = 1)
        wj1 <- hs.re(ag_jg, "[^:]*")
    }
    wj1 <- hs.sub(normalizePath(wj1), normalizePath(sprintf("%s/xt", path.expand("~"))))
    wj1 <- hs.gsub(wj1, "/", ":")
    wj1 <- hs.wjm(wj1, ext = 0)
    if (open) {
        .wk("~/wk/rj/zim-desktop-wiki-develop/zim.py ~/xt", .q(wj1), "&")
    }
    else wj1
}
.zt <- function(s1, C = 9, l = 0, H = 1) {
    .ag1(s1 = s1, wjj1 = "~/xt", C = C, l = l, H = H, exclude_fixed = c("\033[1;33m1\033[0m\033[K-Content-Type: text/x-zim-wiki", "\033[1;33m2\033[0m\033[K-Wiki-Format: zim 0.4"), exclude = "-Creation-Date", zim = 1, less = 1)
}
.rt <- function(s1 = readline(), C = 9, l = 0, H = 1) {
    .ag1(s1 = s1, wjj1 = "~/org/dm/r", C = C, l = l, H = H, less = 1)
}
.ohd <- function(s1, ic = 1) {
    .wk("find", "-L ~/org", "-type f", "-name", .q("*.org"), "-print0", "|", "xargs", "-0", "gawk", if (ic) 
        "-v IGNORECASE=1", .q(sprintf("{ if( FNR > 1) { nextfile}} { if( /%s/) { print FILENAME\" : \"$0; nextfile}}", s1)))
    invisible()
}
.zhd <- function(s1, ic = 1) {
    .wk("find", "-L ~/xt", "-type f", "-name", .q("*.txt"), "-print0", "|", "xargs", "-0", "gawk", if (ic) 
        "-v IGNORECASE=1", .q(sprintf("\n{ if( ! length( $0)) { nbl++}}\n{ if( FNR > 6) { nbl = 0; nextfile}}\n{ if( nbl > 1) { nbl = 0; nextfile}}\n{ if( /%s/) { sub( \"%s\", \"\", FILENAME); sub( \"\\\\.txt$\", \":\", FILENAME); gsub( \"/\", \":\", FILENAME); print FILENAME\" : \"$0; nbl = 0; nextfile}}", s1, path.expand("~/xt"))))
    invisible()
}
.oh <- function(s1) {
    .ag1(sprintf("^\\*+ +.*%s.*", s1), "~/org", "org", exclude_wj = c("dm.r.org", "wl.r.org", "wl.org"))
}
.oh1 <- function(s1) {
    .ag1(sprintf("^\\* +.*%s.*", s1), "~/org", "org", exclude_wj = c("dm.r.org", "wl.r.org", "wl.org"), m1 = 1)
}
.oht <- function(h = {
}, t = {
}, C = 9) {
    .ag.ht(h = h, t = t, type = "org")
}
.zh <- function(s1) {
    if (0) {
        s1 <- "轴"
        .ag1(s1 = sprintf("^(\\t*\\* +.*%s)|(=+ +.*%s.* =+)", s1, s1), wjj1 = "~/xt", "txt")
    }
    .ag1(s1 = sprintf("^(\\t*\\* +.*%s)|(=+ +.*%s.* =+)", s1, s1), wjj1 = "~/xt", "txt", exclude_fixed = c("\033[1;33m1\033[0m\033[K-Content-Type: text/x-zim-wiki", "\033[1;33m2\033[0m\033[K-Wiki-Format: zim 0.4"), exclude = "-Creation-Date", zim = 1)
}
.zht <- function(h = {
}, t = {
}, C = 9) {
    .ag.ht(h = h, t = t, type = "zim")
}
.zh2 <- function(s1, C = 9) {
    s1 <- unlist(strsplit(s1, ",,+"))
    cmd1 <- .sys("ag", if (length(s1) > 1) 
        c("-l", "-0"), .q(sprintf("^=+ .*%s.* =+", s1[1])), "~/xt")
    for (p1 in s1[-1]) cmd1 <- .sys(cmd1, "| xargs -0", "ag", "-l -0", .q(sprintf("^=+ .*%s.* =+$", p1)))
    if (length(s1) > 1) 
        cmd1 <- .sys(cmd1, "| xargs -0 ag -H -C", C, .q(sprintf("^=+ .*(%s).* =+$", paste(s1, collapse = "|"))))
    .wk(cmd1)
}
.ag.ht <- function(h = {
}, t = {
}, C = 9, type = "zim") {
    cmd1 <- {
    }
    wjj1 <- sprintf("~/%s", switch(type, org = "org", zim = "xt"))
    if (length(h)) {
        h <- s1 <- unlist(strsplit(h, ",{2,}"))
        ph <- switch(type, zim = "^=+ .*%s.* =+", org = "^\\*+ .*%s")
        cmd1 <- .sys("ag", "-l -0 -S", .q(sprintf(ph, s1[1])), wjj1)
        for (p1 in s1[-1]) cmd1 <- .sys(cmd1, "| xargs -0", "ag", "-l -0", .q(sprintf(ph, p1)))
    }
    if (length(t)) {
        t <- s1 <- unlist(strsplit(t, ",{2,}"))
        cmd1 <- .sys(cmd1, if (length(cmd1)) 
            "| xargs -0", "ag", "-l -0 -S", .q(s1[1]), if (!length(cmd1)) 
            wjj1)
        for (p1 in s1[-1]) cmd1 <- .sys(cmd1, "| xargs -0", "ag", "-l -0", .q(p1))
    }
    if (length(cmd1)) {
        cmd1 <- strsplit(cmd1, "-0") %=>% unlist %=>% paste0(paste0(head(x, -1), collapse = "-0"), tail(x, 1))
        wj <- .wk(cmd1, intern = 1)
        if (length(wj)) {
            wj_tmp <- hs.wj.temp()
            on.exit(hs.rm.wj(wj_tmp), add = TRUE)
            cat(wj, file = wj_tmp, sep = "\n")
            cmd1 <- .sys("cat", wj_tmp, "| tr", .q("\n"), .q("\\0"), "| xargs -0 ag -C", C, "-H", .q(paste(paste0("(", c(sprintf(ph, h), t), ")"), collapse = "|")))
            .wk(cmd1)
        }
    }
    cmd1
}
.oln <- function(s1) {
    .ag(sprintf("\\[\\[.*%s.*\\]\\]", s1), "~/org")
}
.zln <- function(s1) {
    .wk("ag", .q(sprintf("\\[\\[[^\\[\\]]*%s[^\\[\\]]*\\]\\]", s1)), "~/xt")
}
.e <- function(wj1) .wk("source activate emacs;", "e", wj1)
.dm <- hs.org.uid2wj.openInEmacs <- function(uid1 = readline()) .e(hs.org.uid2wj(uid1))
.wj <- function(s1, wjj1 = "~/org") {
    wj <- .ag(s1, wjj1, C = 0, l = 1, intern = 1)
    if (!length(wj)) {
        hs.say("nothing")
    }
    else if (hs.len1(wj)) {
        .e(wj)
        return()
    }
    if (length(wj) > 9) {
        hs.browser("35.03.28.082317")
        hs.say("inspect wj:")
    }
    else cat(wj, sep = "\n")
}
wk.bf.wk.bypy.wjj2rm <- hs.wj(path.expand("~"), c("wk/rj/exe/Firefox_56/firefox56/cache2", "wk/rjk/ff/p4current/wk/cache2", "wk/rjk/tb/p/wk/cache2", "wk/rjk/ztr/p/wk/cache2", "wk/wk.1/rj/firefox/wk/cache2", "wk/wk.1/rjk/ln-s/.wiznote", c("wk/wk.1/rjk/ln-s/.cache/chromium", "wk/wk.1/rjk/ln-s/.cache/vivaldi", "wk/wk.1/rjk/ln-s/.config/chromium", "wk/wk.1/rjk/ln-s/.config/SogouPY", "wk/wk.1/rjk/ln-s/.config/vivaldi", "wk/rjk/有道云笔记", "wk/wk.1/rj/firefox/wk/cache2", "wk/wk.1/rjk/ztr/p/wk/cache2"), 
    c("wk/wk.1/rjk/ln-s/.wiznote"), "wk/wk.1/rjk/ln-s/.julia"))
wk.bf.bypy.syncup <- function(wjj1, wjj2 = {
}, wjj2rm = wk.bf.wk.bypy.wjj2rm) {
    wjj1 <- path.expand(wjj1)
    if (hs.home.expand("~") %=>% {
        substr(wjj1, 1, nchar(x)) != x
    }) 
        stop("35.04.08.054812")
    if (!length(wjj2)) 
        wjj2 <- hs.sub(wjj1, sprintf("%s/", hs.home.expand("~")))
    hs.say("doing", wjj1)
    .wk("bypy", "-v", "--include-regex", .q(sprintf("^((?!%s).)*$", paste(wjj2rm, collapse = "|"))), "syncup", .q(wjj1), .q(wjj2), "yes")
}
wk.bypy.upload.doc <- function(local.wjj, baidu.wjj) {
    wjj.36_03_04_003344 <- getwd()
    on.exit(setwd(wjj.36_03_04_003344), add = TRUE)
    setwd(local.wjj)
    baidu.wjj %<=>% file.path(basename(local.wjj))
    wjj2do <- c(".", hs.wjj.ls(local.wjj, wjj = 1, wj = 0, R = 1, F = 0))
    for (wjj1 in wjj2do) {
        wjj2 <- ifelse(wjj1 == ".", baidu.wjj, file.path(baidu.wjj, wjj1))
        .wk("bypy mkdir", wjj2)
        wj <- hs.wjj.ls(wjj1, "(?i)\\.(pdf|png|jpg|jpeg|xls|xlsx)$")
        for (wj1 in wj) {
            message(wj1)
            .wk("bypy upload", wj1, wjj2)
        }
    }
}
wk.bf.bypy.wk <- function() {
    if (0) {
        wjj <- .wk("find", "-L", hs.home.expand("~/wk"), "-type d", intern = 1)
        {
            wjj2rm <- {
            }
            wjj1 <- "/home/wk/wk/rj/exe/Firefox_56/firefox56/cache2"
            for (wjj1 in sprintf("%s/%s", path.expand("~"), wk.bf.wk.bypy.wjj2rm)) {
                p1 <- sprintf("^%s", wjj1)
                wjj2rm <- c(wjj2rm, hs.grep(wjj, p1, v = 1))
                wjj1.up <- wjj1
                while (wjj1.up != "/") {
                  wjj2 <- dirname(wjj1.up)
                  wjj2rm <- c(wjj2rm, wjj2)
                  wjj1.up <- wjj2
                }
            }
            wjj <- wjj %not% wjj2rm
        }
        {
            wjj <- sort(wjj)
            i1 <- 1
            while (i1 <= length(wjj)) {
                i2 <- hs.grep(wjj, sprintf("^%s", wjj[i1]))
                if (length(i2) > 1) {
                  wjj <- wjj[-i2[-1]]
                }
                i1 <- i1 + 1
            }
        }
        if (0) {
            wjj1 <- wjj[1]
            wjj1 <- hs.grep(wjj, " ", v = 1)
        }
        null <- sapply(wjj, function(wjj1) {
            hs.say("\ndoing", wjj1, "\n")
            wjj2 <- hs.sub(wjj1, sprintf("%s/", path.expand("~")))
            .wk("bypy", "-v", "syncup", .q(wjj1), .q(wjj2), "yes")
        })
    }
    wk.bf.bypy.syncup(hs.home.expand("~/wk"))
}
wk.bf.swxx.bypy <- function() {
    wjj1 <- hs.home.expand("~/e/swxx")
    if (!file.exists(wjj1)) 
        stop("35.04.08.055924")
    wk.bf.bypy.syncup(wjj1, "swxx", wjj2rm = hs.wj(path.expand(wjj1), "sj/xm"))
}
wk.bf.swxx.byRsync.2in2 <- function() {
    wjj1 <- hs.home.expand("~/e/swxx")
    wk.rsync(wjj1, hs.home.expand("~/in2/bf"), excluded = "sj/xm")
}
wk.bf.bypy.1h <- function() {
    repeat {
        for (wjj1 in hs.wj(hs.home.expand("~"), c("wk/wk.1/org", "wk/wk.1/xt", "wk/wk.1/rjk/ln-s"))) wk.bf.bypy.syncup(wjj1)
        d1 <- 60 * 60
        hs.say("sleep for", d1, "seconds")
        .sleep(d1)
    }
}
wj.zim.bin <- hs.home.expand("~/wk/rj/zim/zim.py")
wj.zim.notebook.xt <- paste0(hs.home.expand("~/.cache/zim/notebook-home_"), Sys.info()["user"], "_xt/index.db")
wk.zim.down <- function(p1, rz = 0) {
    if (0) {
        p1.txt <- paste0("~/xt", hs.gsub(l1, ":", "/"), ".txt")
        down <- readLines(p1.txt) %=>% hs.re("\\[\\[.+\\]\\]") %=>% unique %=>% {
            substr(x, 3, nchar(x) - 2) %not% p1
        }
    }
    {
        hs.require("RSQLite")
        con1 <- dbConnect(RSQLite::SQLite(), dbname = wj.zim.notebook.xt)
        on.exit(dbDisconnect(con1), add = TRUE)
        p1.id <- dbGetQuery(con1, paste0("select * from pages where name is ", "\"", substring(p1, 2), "\""))[, "id"]
        down <- paste0(":", dbGetQuery(con1, paste0("select * from links where source is ", p1.id))[, "names"])
    }
    if (0) {
        down <- paste0(":", .wk(wj.zim.bin, "--search", "~/xt", paste0("LinksFrom", p1), intern = 1)) %not% p1
    }
    if (!rz) 
        down %<=>% hs.grep(":\\d+:\\d{2,2}:\\d{2,2}:{0,1}$", inv = 1, v = 1)
    down
}
wk.zim.up <- function(p1, rz = 0, exclude = {
}) {
    hs.require("RSQLite")
    con1 <- dbConnect(RSQLite::SQLite(), dbname = wj.zim.notebook.xt)
    on.exit(dbDisconnect(con1), add = TRUE)
    p1.id <- dbGetQuery(con1, paste0("select * from pages where name is ", "\"", substring(p1, 2), "\""))[, "id"]
    up.id <- dbGetQuery(con1, paste0("select * from links where target is ", p1.id))[, "source"]
    up <- sapply(up.id, function(id1) dbGetQuery(con1, paste0("select * from pages where id is ", id1))[, "name"])
    if (!length(up)) 
        return()
    up <- paste0(":", up)
    if (!rz) {
        i1 <- hs.grep(up, ":\\d+:\\d{2,2}:\\d{2,2}:{0,1}$")
        if (length(i1)) 
            up <- up[-i1]
    }
    for (i1 in seq_along(exclude)) {
        ms1 <- exclude[i1]
        i2 <- hs.grep(up, ms1)
        if (length(i2)) 
            up <- up[-i2]
    }
    up
}
wk.zim.up.print <- function(p1, lvl = 1, seen = p1) {
    if (lvl == 1) {
        wjj0 <- hs.wjj("~/tmp")
        sep <- ",,,"
        wj_tmp <- hs.wj(wjj0, hs.gsub(p1, ":", sep)) %=>% hs.wjm(add = "org")
        p1 %<=>% paste0(":", x)
        sink(wj_tmp)
    }
    cat(rep("*", lvl), " ", p1, "\n", sep = "")
    up <- wk.zim.up(p1, rz = 0, ) %not% seen
    if (length(up)) {
        for (p1 in up) wk.zim.up.print(p1, lvl = lvl + 1, seen = c(seen, p1))
    }
    if (lvl == 1) {
        sink()
        if (file.exists(wj_tmp)) {
            .wk("e", wj_tmp)
        }
        wj_tmp
    }
}
hs.jl <- function(what = {
}) {
    wj_out <- c(午休 = "~/org/35/04/08/35.04.08.191442.org", 早睡 = "~/org/35/04/08/35.04.08.193007.org", 健身 = "~/org/35/04/08/35.04.08.193141.org")
    if (length(what)) {
        wj_out <- wj_out[what]
        hs.say(wj_out)
        jt <- paste0(as.integer(format(Sys.time(), "%Y")) - 1984, "/", format(Sys.time(), "%m/%d"))
        if (length(.wk("grep", "-m 1", .q(sprintf("^%s$", jt)), wj_out, intern = 1))) {
            hs.say("已经记录过了")
        }
        else cat(jt, file = wj_out, append = TRUE)
    }
    else hs.say(paste(names(wj_out), collapse = ", "))
}
wk.backup_file.clean <- function(wjj1) {
    if (0) {
        wjj1 <- "~/org"
        wjj1 <- "~/rjk/emacs"
    }
    wj <- .wk("ag", "-g", .q("~$"), wjj1, intern = 1)
    hs.rm.wj(wj)
}
hs.emacs.windows <- function() {
    wj1 <- hs.home.expand(c("~/.windows", "~/.emacs.d/eshell/lastdir"))
    lapply(wj1, function(wj1) {
        if (file.exists(wj1)) {
            nr1 <- readLines(wj1, encoding = "UTF8")
            nr1 <- hs.gsub(nr1, "/(home|Users)/[^/]+", path.expand("~"))
            writeLines(nr1, wj1)
        }
    })
}
wk.path_wine <- function(wj1 = readline()) {
    wj1 <- .wk("wine", .sys.fpGood(wj1), "2>&1", intern = 1)
    hs.re(wj1, "(?<=wine: Bad EXE format for ).*(?=\\.)") %=>% hs.gsub("\\\\", "\\\\\\\\") %=>% cat("\n", sep = "")
}
wk.make.home.ready <- function() {
    if (!hs.machine.isLocal()) 
        return()
    {
        wj1 <- hs.home.expand("~/.bash_history")
        if (!file.is_symlink(wj1)) {
            wj2 <- hs.home.expand("~/rjk/ln-s/.bash_history")
            cat(readLines(wj1) %or% readLines(wj2), sep = "\n", file = wj2)
            hs.rm.wj(wj1)
            file.symlink(wj2, wj1)
        }
    }
    {
        wj1 <- hs.wjj(hs.home.expand("~/.cache/mozilla/firefox/cache2"))
        wj2 <- hs.wjm(wj1, dn = hs.home.expand("~/wk/rjk/ff/p4current/wk"))
        if (!file.exists(wj2)) {
            hs.rm.wj(wj2)
            file.symlink(hs.home.expand("~/.cache/mozilla/firefox/cache2"), hs.home.expand("~/wk/rjk/ff/p4current/wk"))
        }
    }
    {
        wj1 <- hs.home.expand("~/Zotero")
        if (!file.exists(wj1)) {
            hs.rm.wj(wj1)
            file.symlink("~/wk/wk/wd/db/mf/ff/ztr", wj1)
        }
    }
}
hs.org.options.export <- function() c("#+options: ^:{} _:{} \\n:nil", "#+LANGUAGE: zh-CN")
hs.give.me <- function(n1) switch(n1, samtools = hs.hsgly("rj.find.35.08.20.070052")(n1), swxx = hs.hsgly("wjj.swxx.35.09.21.183350")())
wk.find.uid <- function(uid1, update = 0) {
    cat("【文件名】\n")
    wk.mlocate(pat = uid1, update = update)
    .wk("ag", "-t", uid1, "~/org")
    .wk("ag", "-t", uid1, "~/xt")
    .wk("ag", "-t", "-G", "\\.org$", uid1, "~")
}
wk.zip1 <- function(wjj1, wjj2 = dirname(wjj1), zip.name.pre = basename(wjj1), hs.wjj.ls.A = {
}) {
    if (!file.exists(wjj1)) 
        stop("[38_06_01_144332]", wjj1, "does not exists")
    if (!file.exists(wjj2)) 
        stop("[38_06_01_144337]", wjj2, "does not exists")
    hs.with_wd(wjj1, {
        wj1 <- tempfile()
        on.exit(hs.rm.wj(wj1), add = TRUE)
        do.call(hs.wjj.ls, c(list(wjj1, R = 1, F = 0), hs.wjj.ls.A)) %=>% cat(file = wj1, sep = "\n")
        wj2 <- paste0("../", zip.name.pre, ".zip")
        .wk("cat", wj1, "| zip -ru", "-@", wj2)
        normalizePath(wj2)
    })
}
wk.7z1 <- function(wjj1, wjj2 = dirname(wjj1), name.pre = basename(wjj1), ...) {
    if (!file.exists(wjj1)) 
        stop("[38_06_01_145352]", wjj1, "does not exists")
    hs.with_wd(wjj1, {
        wj2 <- hs.wj(wjj2, paste0(name.pre, ".7z"))
        .wk("7z u -r", hs.fpGood(wj2), "*")
        normalizePath(wj2)
    })
}
hs.36_02_03_174626 <- function(wj.wj, wjm = hs.wjm(wj.wj, dn = 0, ext = 0, extN = ifelse(hs.grepl(wj.wj, "\\.gz$"), 2, 1))) {
    wjj0 <- getwd()
    on.exit(setwd(wjj0), add = TRUE)
    wjj1 <- dirname(wj.wj)
    setwd(wjj1)
    wj_temp <- tempfile()
    on.exit(hs.rm.wj(wj_temp), add = TRUE)
    hs.browser("36.07.23.132608")
    wj1 <- readLines(wj.wj) %not% ""
    wj1 <- hs.sub(normalizePath(wj1), normalizePath(wjj1), fixed = TRUE) %=>% hs.sub("^/")
    cat(path.expand(wj1), sep = "\n", file = wj_temp)
    cat(basename(wj1), sep = "\n", file = wj_temp)
    cat(wj1, sep = "\n", file = wj_temp)
    .wk("cat", wj_temp, "| zip -@ -ruj", paste0(wjm, ".zip"))
    wj.wj %=>% .head
    .wk("unzip -l", paste0(wjm, ".zip"), "| less -Ni")
    paste0(wjm, ".zip") %=>% .ls
    paste0(wjm, ".zip") %=>% hs.wj.rm
    wj.wj %=>% hs.wjm(ext = "zip")
}
wk.bash <- function(wjj1 = ".") {
    if (!dir.exists(wjj1)) 
        wjj1 %<=>% dirname
    wjj0 <- getwd()
    on.exit(setwd(wjj0), add = TRUE)
    setwd(wjj1)
    .wk("bash")
}
note.update_time <- function(wj1, case_id = "", wj_info = {
}, ot = "message", co1 = 365/2) {
    if (!length(wj_info)) 
        wj_info <- wj1
    t1 <- difftime(Sys.time(), wj1 %=>% file.mtime %=>% min, units = "days") %=>% as.numeric
    switch(ot, message = message("\t#【", if (nzchar(case_id)) c(case_id, "|"), "提示】", wj_info, "更新于", t1, "天前. 更新频率: 每", co1, "天."), time = t1, `01` = t1 > co1)
}
hs.time.4filename <- function(t1 = Sys.time()) format(t1, "%Y-%m-%d-%H%M%S")
wj.updater <- function(wj1 = {
}, dz1 = {
}, wjj2 = if (length(wj1)) dirname(wj1) else ".", co1 = if (missing(wj1)) eval(formals(wj.update.reporter)[["co1"]]) else Inf, gz = !hs.grepl(dz1, hs.hsgly("不需要压缩的模式.2023_08_10_015813")())) {
    if (!length(dz1)) 
        stop("[37_12_19_203015]无意义！")
    if (!length(wj1)) {
        wj1 <- file.path(wjj2, basename(dz1))
        if (gz && (!hs.grepl(wj1, "(?i)\\.gz$"))) 
            wj1 <- paste0(wj1, ".gz")
    }
    if (file.exists(wj1)) {
        if (note.update_time(wj1, ot = "01", co1 = co1)) {
            wj1_bf <- paste0(wj1, ".bf_", hs.time.4filename())
            file.rename(wj1, wj1_bf)
        }
        else {
            note.update_time(normalizePath(wj1), co1 = co1)
        }
    }
    if (!file.exists(wj1)) {
        hs.msg(paste0("获取", dz1, "到", wj1))
        hs.browser("2023.06.29.214655", debug = 0)
        hs.hsgly("1.信息学/文件/获取/用aria2c.2023_06_26_120607")(dz = dz1, wjj2 = wjj2)
    }
    hs.wj.portable(wj1)
}
wj.update.reporter.co1 <- 30 * 3
wj.update.reporter <- function(wj2, wj1 = {
}, dz1 = {
}, co1 = wj.update.reporter.co1) {
    co1 <- wj.update.reporter.co1
    if (!all(file.exists(wj2))) 
        return(TRUE)
    if (length(dz1)) {
        if (!note.update_time(wj2, ot = "01", co1 = co1)) 
            return(FALSE)
        hs.msg(paste0("检查时间：", dz1))
        a1 <- .try(.wk("curl -I", dz1, intern = 1))
        if (is.character(a1) && any(hs.grepl(a1, "(?i)Last-Modified:"))) {
            time_pattern1 <- "(?i)Last-Modified:"
            t1 <- hs.grepV(a1, time_pattern1) %=>% hs.sub(time_pattern1) %=>% trim
            t1 <- as.POSIXlt(.wk("date", "-d", .q(t1), "-u", "+\"%F %T\"", intern = 1))
            a1 <- difftime(Sys.time(), t1, units = "days") >= co1
            if (a1) {
                .wk("touch", .sys.fpGood(wj2))
            }
            return(!a1)
        }
        else hs.browser(paste0("\t[38.03.23.190500]没能获取", dz1, "的修改时间"))
    }
    if (length(wj1)) {
        !all(file.exists(wj1)) || min(file.mtime(wj2)) < max(file.mtime(wj1))
    }
    else note.update_time(wj2, ot = "01", co1 = co1)
}
hs.wj.ext.sub <- function(wj1, ext) hs.wjm(wj1, ext = ext, extN = ifelse(hs.grepl(wj1, "\\.gz$"), 2, 1))
wk.2xlsx <- function(...) hs.hsgly("txt2xlsx.36.02.01.203801")(...)
wk.pack.wj <- function(wj) {
    a1 <- strsplit(wj, "/")
    n1 <- min(sapply(a1, length))
    b1 <- 1
    repeat {
        if (b1 > n1) 
            stop("【36.04.29.164929】不能得到唯一的文件名")
        a2 <- sapply(a1, . %=>% {
            tail(x, b1) %=>% paste(collapse = ".")
        })
        if (uniqueN(a2) < length(a2)) {
            b1 <- b1 + 1
            next
        }
        else break
    }
    wjj_temp <- file.path(hs.home.expand("~/tmp"), .muid4machine())
    link <- hs.wj(wjj_temp, a2)
    file.symlink(normalizePath(wj), link)
    wjj_temp
}
wk.pack <- function(wjj1 = {
}, wj.wj = {
}, name = paste(tail(unlist(strsplit(normalizePath(wjj1), "/")) %rm% "", up), collapse = "."), up = 1, pattern = "\\.(pdf|png|jpg|jpeg|xlsx)$", ext = "pdf,xlsx,xls,png,jpg,jpeg,tiff,doc,docx,ppt,pptx,html", wj = {
}, R = 1, to_xls = 1) {
    if (length(wj)) {
        wj %<=>% x[file.exists(x)]
        if (!length(wj)) 
            return()
        wjj1 <- wk.pack.wj(wj)
        name <- hs.wjm(wj[1], ext = 0, dn = 0)
    }
    if (!length(wjj1)) 
        wjj1 <- hs.home.expand("~/tmp") %=>% hs.wjj
    wjj.36_03_04_003344 <- getwd()
    on.exit(setwd(wjj.36_03_04_003344), add = TRUE)
    setwd(wjj1)
    if (to_xls && hs.grepl(ext, "\\bxlsx?\\b")) 
        .try(wk.2xls(if (length(wj)) 
            wj
        else "."))
    {
        if (0) {
            if (!length(wj.wj)) {
                wj.wj <- hs.wj(wjj1, paste0(name, ".txt"))
                if (!length(wj)) 
                  wj <- hs.wjj.ls(wjj1, pattern, R = R)
                cat(wj, sep = "\n", file = wj.wj)
            }
            hs.36_02_03_174626(wj.wj)
        }
        if (!hs.grepl(name, "(?i)\\.zip$")) 
            name %<=>% paste0(".zip")
        .wk("zip -ru", .q(name), ". -i", .q(paste0("*.", unlist(strsplit(ext, ",")))))
    }
    normalizePath(name)
}
.l <- wk.less <- function(s1, start = {
}, max.print = 10000, wj2 = {
}) {
    if (!length(wj2)) {
        wj2 <- hs.wj.temp()
        on.exit(hs.rm.wj(wj2), add = TRUE)
    }
    op <- options(max.print = max.print)
    on.exit(options(op), add = TRUE)
    if (!is.character(s1)) 
        s1 <- capture.output(s1)
    cat(s1, sep = "\n", file = wj2)
    .wk("less -Ni", if (length(start)) 
        paste0("+", start), wj2)
    wj2
}
get.function_definition <- function(f1, wj2 = hs.home.expand("~/tmp/a.r")) {
    h1 <- head(capture.output(args(f1)), -1)
    b1 <- capture.output(body(f1))
    fn1 <- as.character(substitute(f1))
    if (length(fn1) > 1) 
        fn1 <- tail(fn1, 1)
    cat(paste(fn1, "<- "), h1, b1, sep = "\n", file = wj2)
    wj2
}
hs.wj.open.in_emacs <- local({
    bin <- switch(Sys.info()["sysname"], Darwin = "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient", Linux = "emacsclient", hs.browser("36.02.29.213731"))
    function(wj) {
        .wk(bin, "-n -nw", .sys.fpGood(wj))
    }
})
hs.do.xm <- function() {
    wjj1 <- hs.home.expand("~/org/dm/r")
    hs.wjj.ls(wjj1, "(?i)\\.r$") %=>% sapply(hs.wj.open.in_emacs)
}
hs.org.jt <- function() {
    .muid4machine()
}
hs.2hotspot <- function(wj, up = 0) {
    wjj <- hs.wjj("~/org/经常访问的")
    name <- if (up) {
        paste(tail(strsplit(wj, "/")[[1]], up + 1), collapse = ".")
    }
    else basename(wj)
    hs.wj.symlink(wj, wjj, name = name)
}
rw.wjj0.hs <- function(wj, uid1 = {
}) {
    if (length(uid1)) 
        wj <- environment(hs.hsgly)[["uid.wj"]][uid == uid1][["wj"]]
    con1 <- .sys.cat.skip(wj, pipe = "r")
    .sys.cat.skip(wj)
    on.exit(close(con1), add = TRUE)
    line1 <- readLines(con1, 1)
    eval(parse(text = line1)[[1]][[3]])
}
hs.head <- function(X, N = 6, P = {
}, ...) {
    if (length(P)) 
        N <- floor(length(X) * P)
    head(X, N, ...)
}
hs.tail <- function(X, N = 6, P = {
}, ...) {
    if (length(P)) 
        N <- ceiling(length(X) * P)
    tail(X, N, ...)
}
wk.wait <- function(N = 9) {
    n1 <- sample(1:N, 1, prob = rev(seq(N))/N)
    hs.say("\twait for", n1, "seconds.")
    .sleep(n1)
}
hs.wjj.str_4parameters <- function(...) {
    jg <- expandingSequence(named = 1)
    wjj2 <- file.path(...)
    repeat {
        a1 <- list(wjj = hs.wjj.ls(wjj2, wjj = 1, F = 0), wj = hs.wjj.ls(wjj2, F = 0))
        jg[["add"]](wjj2, a1)
        if (length(a1[["wjj"]])) {
            wjj2 %<=>% file.path(a1[["wjj"]][1])
        }
        else break
    }
    jg[["get"]]()
}
hs.wjj.str_4parameters.parameters.L <- function(wjj1) {
    wjj_str1 <- hs.wjj.str_4parameters(wjj1)
    names(wjj_str1) %=>% x[seq(1, length(), 2)] %=>% lapply(x, . %=>% list(basename(x), wjj_str1[[x]][["wjj"]]))
}
hs.wjj.str_4parameters.parameters_str <- function(parameters.L, type1 = "wjm", sep1 = switch(type1, wjm = ",", wj = "/"), sep2 = switch(type1, wjm = "__", wj = "/")) {
    do.call(expand.grid, lapply(parameters.L, . %=>% do.call(paste, c(x, sep = sep1)))) %=>% apply(1, paste, collapse = sep2)
}
hs.wjj.str_4parameters.parameter_set <- function(wjj.V, p1 = wjj.V[1] %=>% hs.wjj.ls(wjj = 1, F = 0) %=>% x[1]) {
    parameters.L <- hs.wjj.str_4parameters.parameters.L(file.path(wjj.V[1], p1))
    sj.wj.part_right <- hs.wjj.str_4parameters.parameters_str(parameters.L, "wj")
    jg.wj.part <- hs.wjj.str_4parameters.parameters_str(parameters.L, "wjm")
    hs.c(jg.wj.part, seq_along(sj.wj.part_right) %=>% lapply(x, . %=>% file.path(wjj.V, sj.wj.part_right[x])))
}
pv.hs.str <- function(x1) hs.grepV(x1, "^(?i)p[-_\\.]?v(al)?")
hs.xm <- function(wd = hs.home.expand("~/org")) {
    if (dir.exists(wd)) {
        cfg_wj <- hs.fp(wd, ".windows")
    }
    else cfg_wj <- wd
    if (!file.exists(cfg_wj)) 
        cat("", file = cfg_wj)
    wj2 <- hs.home.expand("~/.windows")
    hs.wj.rm(wj2)
    hs.wj.symlink(wj1 = cfg_wj, wjj1 = dirname(wj2))
}
"30.10.31.202238" <- NULL
.update <- function(all = rp, where = wk.ssh, U = TRUE, rp = FALSE) {
    hs.source.robust(hs.home.expand("~/.Rprofile"))
    {
        .Last()
        .First()
    }
    if (all) {
        .Rsync(rp = rp, where = where, U = U)
    }
    else {
        hs.hsgly(update.uid = TRUE)
    }
    return()
    wj1 <- hs.home.expand("~/org/dm/r/function.common.R")
    wj2 <- sub("[^\\.]+$", "rd", wj1)
    hs.ready()
    if (file.exists(wj1) && do.call("difftime", as.list(file.info(c(wj2, wj1))[, "mtime"])) < 0) {
        a <- readLines(wj1, encoding = "UTF-8")
        save(a, file = wj2)
        if (0) 
            .try({
                wk.rsync("~/.Rprofile", "~", ssh = ieiwk.ssh.db(where), ssh.side = 2, dry = 0, L = 1)
                hs.wjm("function.common", add = c("R", "rd")) %=>% file.path("~/wk/my/wd/dm/R", x) %=>% sapply(wk.rsync.out, "~/wk/my/wd/dm/R")
            })
    }
    `33.06.02.124853` <- load(wj2)
    eval(parse(text = get(`33.06.02.124853`), encoding = "UTF-8"), envir = .GlobalEnv)
}
.Rsync <- function(where = wk.ssh, wjj2 = hs.home.expand("~/wk/"), rp = FALSE, U = TRUE) {
    hs.hsgly(update.uid = TRUE)
    if (0) {
        wj <- "~/org/dm"
        local({
            wj <- tempfile()
            on.exit(hs.wj.rm(wj), add = TRUE)
            environment(hs.hsgly) %=>% {
                c(x[["uid.wj.wj"]], x[["uid.wj"]][, wj %=>% hs.grepV("~/org/dm/", inv = 1)]) %=>% hs.sub("~/") %=>% cat(sep = "\n", file = wj)
            }
            ssh <- ieiwk.ssh.db(where)
            bin <- switch(osType(), Linux = "rsync", Darwin = "/usr/local/bin/rsync")
            .wk(bin, "-rtDPvhlpgou", "--progress", "-e", .q(paste0("ssh -p ", ssh["port"])), paste0("--files-from=", wj), "~", paste0(ssh["user"], "@", ssh["dz"], ":", environment(hs.hsgly)[["r.wj.wjj.remote"]]))
        })
    }
    if (length(where)) {
        wjj_sub <- "1"
        cmd1 <- .sys(wj_rsync, "-ahPzv --delete", "-c", "--ignore-errors", "-e ssh", "-f", .q("- .DS_Store"), "-f", .q("+ **.[rR]"), "-f", .q("+ **.[rR].[gGxX][zZ]"), "-f", .q("+ **.py"), "-f", .q("+ **.py.[gGxX][zZ]"), "-f", .q("+ **.py.[zZ][iI][pP]"), "-f", .q("+ **.json"), "-f", .q("+ */"), "-f", .q("- **"), paste0("--rsync-path=", .q(paste0("mkdir -p ", .sys.fpGood(wjj2, home.expand = 0), "; rsync"))), paste0(hs.home.expand("~/wk/"), wjj_sub, "/"), paste0(ieiwk.ssh.db(where)["user"], "@", where, 
            ":", paste0(wjj2, wjj_sub, "/")), "| grep -v", .q("^cannot delete non-empty directory: "), "| grep -v", .q("/$"))
        .wk(cmd1)
    }
    if (rp) 
        wk.rsync.out.same_place(hs.home.expand("~/.Rprofile"), ssh = where, L = 1, U = 1)
}
.r820 <- function(...) {
    .Rsync("r820", ...)
}
hostname <- Sys.getenv("HOSTNAME")
r.home <- Sys.getenv("R_HOME")
wk.path <- ieiwk.path <- local({
    a <- osType()
    list(lib = {
        if (a == "Windows") {
            file.path(dirname(hs.home.expand("~")), "wk.os/rjk/R")
        } else if (length(grep("^CYGWIN_NT", a))) {
            hs.wjj("/cygdrive/c/rjk/R")
        } else hs.home.expand(c("~/rj/lib/R", "~/rj2/lib/R"))
    })
})
hs.libPaths <- wk.libPaths <- function(...) {
    .libPaths(unique(c(..., .libPaths())))
}
wk.install_github <- function(..., lib = wk.path[["lib"]][1]) {
    hs.require("devtools", 1)
    install_github(..., lib = lib)
}
ieiwk.workflowInstall <- function(package, lib = ieiwk.path[["lib"]][1], update.Bioconductor = 0) {
    f1 <- ieiwk.file.path("~/my/wd/dm/R/workflows.R")
    if (update.Bioconductor) {
        unlink(f1)
    }
    {
        if (file.exists(f1)) {
            a <- readLines(f1)
        }
        else {
            f2 <- "http://bioconductor.org/workflows.R"
            a <- readLines(f2)
            writeLines(a, con = f1)
        }
    }
    {
        i <- grep("[^'\"]install.packages", a)
        a[i] <- sub("install.packages", "ieiwk.install.packages", a[i])
    }
    if (0) {
        options(repos = c(CRAN = "http://cran.rstudio.com"))
        options(BioC_mirror = "http://bioconductor.org")
    }
    {
        i <- grep("(?i)https", a)
        for (i1 in i) {
            a[i1] <- gsub("https", "http", a[i1])
        }
    }
    eval(parse(text = a))
    workflowInstall(package, lib = lib)
}
hs.library <- ieiwk.library <- function(package) {
    library(package, lib.loc = ieiwk.path[["lib"]])
}
pkgNSisMissing <- function() {
    pkgNS <- NULL
    for (i in seq_along(.libPaths())) {
        libLoc <- .libPaths()[i]
        pkgs <- installed.packages(lib.loc = libLoc)[, 1]
        pkgNS <- c(pkgNS, sapply(unname(pkgs), packageHasNamespace, package.lib = libLoc))
    }
    pkgNS[!pkgNS]
}
.def.1 <- function(p, to = "", env = parent.frame()) {
    eval(body(.t(p, 1, env = env)), env = env)
}
.def.4 <- function(p, v, env = parent.frame()) {
    .t(p = p, actCode = 4, v = v, env = env)
}
.def.3 <- function(p = ".", child = FALSE, level = Inf, env = parent.frame(), format = "vector", sep = formals(.t)$sep, print = TRUE) {
    if (child) 
        return(names(attributes(.t(p, actCode = 1, env = env, self = FALSE))))
    a <- .t(p, actCode = 3, env = env)
    b <- sort(unlist(a))
    if (print) 
        switch(format, vector = .ss(sapply(strsplit(b, sep), function(x) sprintf(".t( c( %s))", paste(shQuote(x), collapse = ", ")))), string = .ss(sprintf(".t( %s)", b)))
    invisible(b)
}
.def.expr <- function(...) {
    list(...)
}
.def.expr.show <- function(progn = NULL, pool = .t(c(".Expr", "all expressions"), env = def.30.04.26.152622)) {
    if (is.null(progn)) 
        progn <- names(pool)
    sapply(progn, function(b) {
        src <- capture.output(attr(pool[[b]], "srcref"))
        .ss(sprintf("\"%s\" = function() {", b))
        .ss(src[-c(1, length(src))])
        .ss("}")
    })
    invisible()
}
.def.expr.eval <- function(progn = NULL, pool = .t(c(".Expr", "all expressions"), env = def.30.04.26.152622), env) {
    if (length(progn)) {
        sapply(progn, function(b) {
            eval(body(pool[[b]]), env = env)
        })
    }
    invisible()
}
.def.save <- function(env = ef.30.04.26.152622, fp = .ma("30.04.26.152817.RData", "~/data/wengkai/handy")) {
    save(env, file = fp)
}
ieiwk <- local({
    ieiwk.dir <- hs.home.expand("~/calvin")
    meta.dir <- hs.home.expand("~/calvin/meta")
    meta.dir.data <- hs.home.expand("~/calvin/data")
    meta.dir.code <- hs.home.expand("~/calvin/code")
    wjj.dm <- switch(system("hostname", intern = TRUE), ThinkPad = , `AFAAW-704251807` = , `wk-VirtualBox` = , `wk-ThinkPad-X220-Tablet` = hs.home.expand("~/wk/my/wd/dm/R"), hs.home.expand("~/wk/my/wd/dm/R"))
    wjj.sj <- hs.wjj(hs.home.expand("~/wk/my/wd"), "sj")
    function(x) {
        switch(x, dir = file.path(hs.home.expand("~"), "calvin"), meta.dir = file.path(ieiwk("dir"), "meta"), meta.dir.data = file.path(ieiwk("dir"), "data"), meta.dir.code = file.path(ieiwk("dir"), "code"), wjj.dm = wjj.dm, wjj.sj = wjj.sj, host.with.internet = local({
            n1 <- Sys.info()["nodename"]
            switch(n1, n1)
        }))
    }
})
.muid.file.path.2 <- function(.muid, dataDir = ieiwk("meta.dir.data"), split = "\\.") {
    a <- strsplit(.muid, split)[[1]]
    p <- ieiwk.file.path(dataDir, a[1], a[2], a[3])
    r <- ieiwk.list.files(p, pattern = head(a, 6) %,% ".")
    if (length(r)) 
        gsub(sprintf("^%s", path.expand(p)), p, r)
    else ieiwk.file.path(p, .muid)
}
ieiwk.move.dirContents <- function(wjj1, wjj2) {
    wjj0 <- getwd()
    setwd(wjj1)
    for (wj1 in list.files(recursive = 1)) {
        .ss(wj1)
        wj2 <- ieiwk.file.path(wjj2, wj1)
        file.rename(wj1, wj2)
    }
    setwd(wjj0)
}
.mfp.ws <- function(.muid, dataDir = ieiwk("meta.dir.data"), split = "\\.") {
    a <- strsplit(.muid, split)[[1]]
    lvl <- 3
    p <- do.call("ieiwk.file.path", as.list(c(dataDir, head(a, lvl))))
    fp <- ieiwk.file.path(p, .muid)
    dir.create(fp)
    fp
}
.mfp.data <- function(.muid, split = "\\.") {
    .muid.file.path(.muid, ieiwk("meta.dir.data"), split)
}
.mfp.code <- function(.muid, split = "\\.") {
    .muid.file.path(.muid, ieiwk("meta.dir.code"), split)
}
.mfp2load <- function(.muid, split = "\\.") {
    hs.load(.muid.file.path(.muid))
}
.mfi <- function(.muid, type = "data") {
    switch(type, data = file.info(.mfp.data(.muid)), code = file.info(.mfp.code(.muid)))
}
.mfr <- function(.muid, type = "data") {
    file.remove(switch(type, data = .mfp.data(.muid), code = .mfp.code(.muid)))
}
ieiwk.muid.ln <- function(.muid, fp.mesa, ..., env = .GlobalEnv, compress = 1) {
    fp.meta <- .muid.file.path(.muid)
    try(if (file.info(fp.mesa)[["isdir"]]) 
        fp.mesa <- ieiwk.file.path(fp.mesa, .muid), sil = 1)
    if (...length()) {
        if (file.exists(fp.meta)) 
            stop(".muid already used")
        save(..., file = fp.mesa, envir = env, compress = compress, compression_level = 1)
        file.symlink(fp.mesa, fp.meta)
    }
}
.mln <- ieiwk.muid.ln
ieiwk.muid.rm <- function(.muid) {
    fp.meta <- .muid.file.path(.muid)
    fp.mesa <- Sys.readlink(fp.meta)
    if (!is.na(fp.mesa)) 
        file.remove(fp.mesa)
    file.remove(fp.meta)
}
.mrm <- ieiwk.muid.rm
.ma.code <- function(.muid) {
    exp <- sprintf("{\nexpression({\n\n.ma( '%s', xxx)\n})\n.ma( '%s')\n}", .muid, .muid)
    .ss(exp)
}
.ma.code.pdf <- function(.muid) {
    src <- sprintf("{\npdf( file = .mfp( '%s.pdf')) # [[%s.pdf][ ]]\nxxx\ndev.off()\n}", .muid, .mfp(.muid))
    .ss(src)
}
ieiwk.muid.access <- ieiwk.muid.save <- function(.muid, root = ieiwk("meta.dir.data"), obj, act, r = 0, d = 0, rc = 0, fp = "", v.only = 0, ow = 0, env = parent.frame(), compress = TRUE, verbose = TRUE) {
    if (fp == "") {
        fp <- .mfp(.muid, root, v.only = 1)
        if (verbose) 
            .ss(sprintf(".ma( \"%s\", \"%s\")", .muid, root))
    }
    if (v.only) 
        return(fp)
    if (d) {
        .sys.do("rm -R", fp)
        return(fp)
    }
    obj.nm <- as.character(substitute(obj))
    if (r) {
        if ({
            !file.exists(fp)
        } & {
            obj.nm != ""
        }) {
            save(list = obj.nm, file = fp, envir = env, compress = compress, compression_level = 1)
        }
        else return(hs.load(fp))
    }
    if (obj.nm != "") {
        if (file.exists(fp) & !ow) {
            .ss(fp)
            stop("file already exists")
        }
        dir.create(dirname(fp), recursive = TRUE)
        save(list = obj.nm, file = fp, envir = env, compress = compress, compression_level = 1)
    }
    else {
        hs.load(fp)
    }
}
.ms <- ieiwk.muid.save
.ma <- ieiwk.muid.access
hs.listAsVector <- function(x, hs, name = 0, flat = FALSE) {
    sapply(x, hs) %=>% unlist(use.names = name, recursive = flat)
}
ieiwk.reorder <- function(x, i, j) {
    if (length(x) == 1) 
        x <- seq(x)
    x1 <- x[-i]
    if (j == 1) {
        c(i, x1)
    }
    else if (j == length(x)) {
        c(x1, j)
    }
    else {
        c(head(x1, j - 1), i, tail(x1, -j + 1))
    }
}
clg <- ieiwk.clg <- function(cl, kc = 1, c = NULL, sep = ".", invert = 0) {
    b <- length(dim(cl))
    if (b == 0) 
        b <- 1
    sep_1 <- glob2rx(sep)
    sep_1 <- substr(sep_1, 2, nchar(sep_1) - 1)
    switch(paste(b), `1` = {
        c1 <- cl %in% kc
        if (invert) c1 <- !c1
        i1 <- which(c1)
        if (length(names(cl))) {
            names(cl)[i1]
        } else i1
    }, `2` = {
        browser("32.12.01.191540")
        if (is.null(rownames(cl))) rownames(cl) <- seq.int(nrow(cl))
        if (is.null(c)) {
            a <- strsplit(as.character(kc), sep_1)
        } else {
            c <- rep(c, len = length(kc))
            a <- lapply(1:length(kc), function(i) {
                list(kc[i], c[i])
            })
        }
        r <- lapply(a, function(a.1) {
            rownames(cl)[cl[, a.1[[1]]] %in% {
                cl[, a.1[[1]]] %keep% a.1[[2]]
            }]
        })
        names(r) <- sapply(a, function(a.1) {
            paste(unlist(a.1), collapse = sep)
        })
        if (length(r) == 1) unlist(r) else r
    })
}
ieiwk.clg2 <- function(cl, kc, c = NULL, sep = ".") {
    b <- length(dim(cl))
    if (b == 0) 
        b <- 1
    sep_1 <- glob2rx(sep)
    sep_1 <- substr(sep_1, 2, nchar(sep_1) - 1)
    if (is.list(cl)) 
        cl <- as.matrix(cl)
    switch(as.character(b), `1` = {
        r <- ifelse(kc %in% cl, 1, NA)
        i.cl <- {
            a <- which(cl %in% kc)
            names(a) <- cl %keep% kc
            a
        }
        r[!is.na(r)] <- names(cl[i.cl[kc %keep% cl]])
        r
    }, `2` = {
        if (is.null(c)) {
            a <- strsplit(as.character(kc), sep_1)
        } else {
            c <- rep(c, len = length(kc))
            a <- lapply(1:length(kc), function(i) {
                list(kc[i], c[i])
            })
        }
        r <- lapply(a, function(a.1) {
            names(cl[, a.1[[1]]] %keep% a.1[[2]])
        })
        names(r) <- sapply(a, function(a.1) {
            paste(unlist(a.1), collapse = sep)
        })
        if (length(r) == 1) unlist(r) else r
    })
}
clg2 <- ieiwk.clg2
ieiwk.in.range <- function(pool, range, inclusive = "both") {
    switch(inclusive, both = {
        fn1 <- fn2 <- "<="
    }, right = {
        fn1 <- "<"
        fn2 <- "<="
    }, left = {
        fn1 <- "<="
        fn2 <- "<"
    }, none = {
        fn1 <- fn2 <- "<"
    })
    if (!is.matrix(range)) 
        range <- t(range)
    apply(range, 1, function(range) which(do.call(fn1, list(range[1], pool)) & do.call(fn2, list(pool, range[2]))))
}
ieiwk.merge.range <- function(rng) {
    rng <- t(apply(rng, 1, sort))
    rng <- rng[order(rng[, 1], rng[, 2]), , drop = FALSE]
    row2do <- nrow(rng)
    while (row2do != 1) {
        a <- rng[row2do - 1, ]
        b <- rng[row2do, ]
        if (b[1] - a[2] < 2) {
            rng <- rng[-row2do, , drop = FALSE]
            rng[row2do - 1, ] <- c(a[1], b[2])
        }
        row2do <- row2do - 1
    }
    rng
}
ieiwk.range.change <- function(r, size) {
    c(min(r) - size, max(r) + size)
}
ieiwk.eql <- function(a, b, tolerance = .Machine$double.eps^0.5) {
    isTRUE(all.equal(a, b, tolerance = tolerance))
}
ieiwk.range.centerRadius <- function(radius, center = 0) {
    radius * c(-1, 1) + center
}
ieiwk.i <- function(x, i, is.list = 0) {
    if (is.list) 
        x[[i]]
    else x[i]
    tfs(sprintf(ifelse(is.list, "x[[ %s]]", "x[ %s]"), i))
}
.i <- ieiwk.i
ieiwk.replace <- function(x, i, y) {
    append(x[-i], y, i[1] - 1)
}
hs.sepSeq <- ieiwk.sepSeq <- function(Lines, sep = "", sep.p, sep.i = {
}, sep.i.pattern, sep.i.1is1 = 1, stepSize = {
}, iv = {
}, jv = {
}, k) {
    if (missing(k)) {
        if (is.null(sep)) {
            if (is.null(jv)) {
                jv <- c(iv[-1] - 1, length(Lines))
            }
        }
        else {
            if (is.null(sep.i)) {
                sep.i <- {
                  if (missing(sep.p)) 
                    which(Lines == sep)
                  else hs.grep(Lines, sep.p)
                }
            }
            if (sep.i.1is1 & sep.i[1] != 1) 
                sep.i <- c(1, sep.i)
            if (length(Lines) >= tail(sep.i, 1)) 
                sep.i <- append(sep.i, length(Lines) + 1)
            block.size <- diff(sep.i)
            sep.i.i <- which(block.size > 0)
            iv <- sep.i[sep.i.i]
            jv <- sep.i[sep.i.i + 1] - 1
        }
    }
    else {
        a <- sort(cbind(seq_along(Lines), seq(k))[, 2])
        iv <- match(unique(a), a)
        jv <- c(iv[-1] - 1, length(a))
    }
    lapply(seq_along(iv), function(i) {
        Lines[iv[i]:jv[i]]
    })
}
ieiwk.i.block <- function(dat, i.sep, iv) {
    L1 <- ieiwk.sepSeq(dat, i.sep)
    lapply(iv, function(i) {
        i.sep.i <- tail(which(i.sep < i), 1)
        L1[[i.sep.i]]
    })
}
hs.connection <- ieiwk.make.connection <- function(con, skip.line.n = 0, skip.char.n = 0, skip.order = c("line", "char"), skip.char.lineSep.count = FALSE) {
    if (!"connection" %in% class(con)) {
        if (is.character(con)) {
            con <- {
                if (file.exists(con)) {
                  file(con)
                }
                else {
                  pipe(con)
                }
            }
        }
        else {
            stop("what is the connection? 34.10.05.070316 @ ~/org/dm/r/function.common.R")
        }
    }
    if (!isOpen(con)) {
        open(con)
    }
    for (o1 in skip.order) {
        switch(o1, line = {
            step_size.max <- 10000
            repeat {
                step_size <- min(skip.line.n, step_size.max)
                a <- readLines(con, step_size)
                if (!length(a)) break
                skip.line.n <- skip.line.n - step_size
            }
        }, char = {
            nchars.remain <- skip.char.n
            while (skip.char.n) {
                a <- readChar(con, skip.char.n)
                if (!length(a)) break
                if (!skip.char.lineSep.count) skip.char.n <- skip.char.n - nchar(gsub("\n", "", a))
            }
        })
    }
    con
}
ieiwk.readChar <- function(con, nchars = 10, useBytes = FALSE, keep.line.sep = FALSE, count.line.sep = FALSE, skip.char.n = 0) {
    con <- ieiwk.make.connection(con)
    r <- list()
    stepSize.max <- 1e+09
    while (nchars) {
        stepSize <- min(stepSize.max, nchars)
        a <- readChar(con, stepSize)
        if (!length(a)) 
            break
        nchars <- nchars - nchar(a)
        if (!count.line.sep) 
            nchars <- nchars + str_count(a, "\n")
        r[length(r) + 1] <- a
    }
    r <- do.call(paste0, r)
    if (!keep.line.sep) 
        gsub("\n", "", r)
    else r
}
ieiwk.readChar.all <- function(con, stepSize = 1e+09, useBytes = FALSE, skip.char.n = 0) {
    con <- ieiwk.make.connection(con)
    r <- list()
    if (skip.char.n) {
        readChar(con, skip.char.n)
    }
    while (length(a <- readChar(con, stepSize))) {
        r[[length(r) + 1]] <- a
    }
    do.call(paste0, r)
}
wk.read.chunk <- local({
    db1 <- list()
    db2 <- list()
    function(wj, chunkSize = 1e+08, redo = 0, reset = 0) {
        if (is.character(wj)) {
            con_id <- wj
            if (reset) {
                if (length(db1[[wj]])) {
                  .try(close(db1[[wj]]))
                }
                db1[[wj]] <<- {
                }
                db2[[con_id]] <<- {
                }
                return()
            }
            if (redo) {
                if (length(db1[[wj]])) {
                  .try(close(db1[[wj]]))
                  db1[[wj]] <<- {
                  }
                }
                db2[[con_id]] <<- {
                }
            }
            if (length(db1[[wj]]) == 0) {
                db1[[wj]] <<- {
                  if (file.exists(wj)) {
                    file(wj)
                  }
                  else {
                    pipe(wj)
                  }
                }
            }
            con1 <- db1[[wj]]
        }
        else if ("connection" %in% class(wj)) {
            con1 <- wj
            con_id <- capture.output(attr(wj, "conn_id"))
        }
        else {
            wk.browser("34.10.05.162756 @ ~/org/dm/r/function.common.R")
        }
        if (!isOpen(con1)) 
            open(con1)
        reader1 <- {
            if (length(db2[[con_id]]) == 0) 
                db2[con_id] <<- list(chunk.reader(con1))
            db2[[con_id]]
        }
        chunk1 <- read.chunk(reader1, max.size = chunkSize)
        if (length(chunk1)) {
            rawToChar(chunk1)
        }
        else {
            db2[[con_id]] <<- {
            }
            close(con1)
            db1[[wj]] <<- {
            }
        }
    }
})
wk.fread.chunk.1 <- ieiwk.fread.chunk <- function(con, stepSize = 1e+08, useBytes = FALSE, skip.char.n = 0, gz = 0, ...) {
    chunk <- wk.read.chunk(con)
    if (length(chunk)) 
        return(wk.fread(chunk, ...))
    if (skip.char.n) {
        readChar(con, skip.char.n)
    }
    a <- readChar(con, stepSize, useBytes = TRUE)
    a1 <- readLines(con, 1)
    if (1) {
        return(ieiwk.fread(sprintf("%s%s\n", a, a1), ...))
    }
    if (0) {
        tt <- tempfile(tmpdir = "/dev/shm")
        on.exit(unlink(tt), add = TRUE)
        cat(a, a1, "\n", sep = "", file = tt)
        if (file.info(tt)[["size"]] < 2) 
            return()
        ieiwk.fread(tt, ...)
    }
}
wk.fread.chunk.2 <- function(con, stepSize = 1e+09, complete.line = 1, useBytes = FALSE, skip.char.n = 0, ...) {
    con <- hs.make.connection(con)
    if (skip.char.n) 
        readChar(con, skip.char.n)
    a <- readChar(con, stepSize)
    a1 <- readLines(con, 1)
    wk.fread(paste0(a, a1, "\n"), ...)
}
wk.fread.chunk.3 <- function(con, stepSize = 1e+08, useBytes = FALSE, skip.char.n = 0, ...) {
    con <- hs.make.connection(con)
    if (skip.char.n) 
        readChar(con, skip.char.n)
    a <- readChar(con, stepSize)
    a1 <- readLines(con, 1)
    a.len <- str_length(a)
    substr(a, a.len + 1, a.len + 2 + str_length(a1)) <- sprintf("%s\n", a1)
    wk.fread(a, ...)
}
wk.fread.chunk.4 <- function(con, nrows = 1e+05, ...) {
    con <- hs.make.connection(con)
    wj_tmp <- hs.wj.temp()
    on.exit(hs.rm.wj(wj_tmp), add = TRUE)
    a1 <- readLines(con, nrows)
    writeLines(a1, wj_tmp)
    wk.fread(wj_tmp, ...)
}
wk.fread.chunk.5 <- local({
    wj_tmp <- {
    }
    remaining <- ""
    function(con, stepSize = 1e+08, pre = {
    }, ...) {
        con <- hs.make.connection(con)
        if (length(wj_tmp) == 0) 
            wj_tmp <<- hs.wj.temp()
        if (length(pre)) {
            sink(wj_tmp, append = TRUE)
            writeChar(pre, stdout(), eos = {
            })
            sink()
        }
        a1 <- readChar(con, stepSize)
        if (length(a1)) {
            sink(wj_tmp, append = TRUE)
            writeChar(a1, stdout(), eos = {
            })
            sink()
        }
        else close(con)
        repeat {
            a2 <- readChar(con, 1000)
            if (length(a2)) {
                if (hs.grepl(a2, "\n$")) {
                  sink(wj_tmp, append = TRUE)
                  writeChar(a2, stdout())
                  sink()
                  break
                }
                else {
                  v1 <- strsplit(a2, "[\r\n]")[[1]]
                  if (length(v1) > 1) {
                    sink(wj_tmp, append = TRUE)
                    writeLines(head(v1, -1))
                    sink()
                    remaining <<- tail(v1, 1)
                    break
                  }
                  else {
                    sink(wj_tmp, append = TRUE)
                    writeChar(a2, stdout())
                    sink()
                  }
                }
            }
            else break
        }
        if (file.exists(wj_tmp) && file.size(wj_tmp)) {
            on.exit({
                sink(wj_tmp, append = FALSE)
                writeChar(remaining, stdout(), eos = {
                })
                sink()
                remaining <<- ""
            }, add = TRUE)
            wk.fread(wj_tmp, ...)
        }
        else {
            hs.rm.wj(wj_tmp)
            remaining <<- ""
            {
            }
        }
    }
})
wk.fread.chunk.6 <- local({
    remaining <- ""
    function(con, stepSize = 1e+08, ...) {
        con <- hs.make.connection(con)
        a1 <- readChar(con, stepSize)
        if (length(a1)) {
            if (nchar(remaining)) 
                a1 <- sprintf("%s%s", remaining, a1)
        }
        else {
            close(con)
            if (nchar(remaining)) {
                remaining.nc <- nchar(remaining)
                a1 <- if (substr(remaining, remaining.nc, remaining.nc) == "\n") {
                  remaining
                }
                else sprintf("%s\n", remaining)
            }
            else return()
        }
        a1.nc <- nchar(a1)
        if (substr(a1, a1.nc, a1.nc) == "\n") {
            remaining <<- ""
            suppressWarnings(wk.fread(a1, ...))
        }
        else {
            lei <- gregexpr("\n", a1, useBytes = TRUE)[[1]]
            remaining <<- substr(a1, tail(lei, 1) + 1, a1.nc)
            suppressWarnings(wk.fread(a1, nrow = length(lei), ...))
        }
    }
})
wk.fread.chunk.7 <- function(con, nrows = 1e+06, dt = 1, ...) {
    con <- hs.make.connection(con)
    a1 <- readLines(con, nrows)
    if (dt && length(a1)) 
        a1 <- fread(text = a1, header = FALSE)
    a1
}
wk.fread.chunk.8 <- local({
    left <- {
    }
    column_name <- {
    }
    function(in1, nbits = 1e+06, sep_n = charToRaw("\n"), ot = "dt", long_line = 0, cn = ot == "dt", ...) {
        in1 <- hs.make.connection(in1, "rb")
        if ("binary" %not.in% summary(in1)[["text"]]) 
            stop("37_03_05_201133")
        a1 <- readBin(in1, "raw", nbits)
        if (is.null(left) && cn) {
            ni <- match(sep_n, if (long_line) 
                a1
            else head(a1, 10000))
            column_name <<- strsplit(readBin(head(a1, ni - 1), "character"), "\t")[[1]]
            a1 <- tail(a1, -ni)
        }
        if (length(left) > 0) {
            if (length(a1) > 0) {
                ni <- hs.match.from_tail(sep_n, a1)
                if (ni < length(a1)) {
                  left2 <- tail(a1, length(a1) - ni)
                  a1 <- c(left, head(a1, ni))
                  left <<- left2
                }
                else {
                  a1 <- c(left, a1)
                  left <<- raw()
                }
            }
            else {
                a1 <- if (tail(left, 1) == sep_n) 
                  left
                else c(left, sep_n)
                left <<- NULL
            }
        }
        else {
            if (length(a1) > 0) {
                ni <- hs.match.from_tail(sep_n, a1)
                if (ni < length(a1)) {
                  left <<- tail(a1, -ni)
                  a1 <- head(a1, ni)
                }
            }
            else {
                left <<- NULL
                column_name <<- NULL
                close(in1)
                return()
            }
        }
        switch(ot, dt = {
            jg <- fread(text = readBin(a1, "character"), header = FALSE)
            if (cn) setnames(jg, column_name)
            jg
        }, row = fread(readBin(a1, "character"), sep = "\n", header = FALSE), raw = a1)
    }
})
wk.fread.chunk <- wk.fread.chunk.8
wk.dt.inList <- function() {
    methods <- list()
    dtList <- list()
    check <- 0
    methods$set <- function(dtl) {
        dtList <<- lapply(dtl, . %=>% {
            attr(x, "rows") <- x[, seq.int(.N)]
            x
        })
        check <<- 1
    }
    methods$add <- function(dt) {
        if (dt[, .N]) {
            attr(dt, "rows") <- dt[, seq.int(.N)]
            dtList[[length(dtList) + 1]] <<- dt
            check <<- 1
        }
    }
    methods$clean <- function() {
        if (check == 0) 
            return()
        l1 <- which(sapply(dtList, . %=>% attr("rows") %=>% length) == 0)
        if (length(l1)) 
            dtList[i1] <<- {
            }
        check <<- 0
    }
    methods$nrow <- function() sapply(dtList, . %=>% attr("rows") %=>% length) %=>% sum
    methods$ncol <- function() {
        methods$clean()
        length(dtList[[1]])
    }
    methods$getRows <- function(i, rm = 0) {
        if (length(i) == 0) 
            return()
        methods$clean()
        i1 <- sapply(dtList, . %=>% attr("rows") %=>% length) %=>% cumsum
        g1 <- cut(i, c(0, i1), labels = seq_along(i1))
        i2 <- tapply(i, g1, c) %=>% hs.nonempty
        for (g2 in names(i2) %not% "1") i2[[g2]] %<=>% {
            x - i1[as.integer(g2) - 1]
        }
        if (rm) {
            check <<- 1
            for (g2 in names(i2)) attr(dtList[[as.integer(g2)]], "rows") <<- attr(dtList[[as.integer(g2)]], "rows")[-sort(unique(i2[[g2]]))]
        }
        a <- names(i2) %=>% lapply(. %=>% dtList[[as.integer(x)]][i2[[x]]]) %=>% rbindlist
        if (isSorted(i)) {
            a
        }
        else a[seq.int(.N) %=>% match(order(g1))]
    }
    methods$get <- function(i = seq_along(dtList), rbind = 1) {
        methods$clean()
        R <- lapply(dtList[i], function(x) {
            i <- attr(x, "rows")
            if (length(i) == x[, .N]) {
                x
            }
            else x[i]
        })
        if (rbind) {
            rbindlist(R)
        }
        else R
    }
    methods$do <- function(hs, ex, sep = 0) {
        methods$clean()
        if (sep) {
            if (missing(hs)) {
                lapply(seq_along(dtList), function(i1) {
                  dtList[[i1]] %=>% {
                    l1 <- attr(x, "rows")
                    if (.[, .N == length(l1)]) {
                      x[, eval(ex)]
                    }
                    else x[l1, eval(ex)]
                  }
                })
            }
            else lapply(seq_along(dtList), function(i1) {
                dtList[[i1]] %=>% {
                  l1 <- attr(x, "rows")
                  if (x[, .N == length(l1)]) {
                    hs(x)
                  }
                  else hs(x[l1])
                }
            })
        }
        else if (missing(hs)) {
            methods$get()[, eval(ex)]
        }
        else hs(methods$get())
    }
    methods
}
wk.fread.chunk.do.line.1 <- local({
    cn1 <- {
    }
    function(io, hs = identity, hs.cn = function(chunk) if (sep != "\n") 
        names(fread(chunk, nrow = 0, header = TRUE, sep = sep)), all = 0, simplify = all, header = sep != "\n", sep = "\t", smart = 1, ...) {
        if (smart) 
            io <- hs.make.connection(io)
        if (header && (length(cn1) == 0)) {
            chunk1 <- readChar(io, 1e+05)
            i1 <- regexpr("\n", chunk1)
            if (length(i1)) {
                cn1 <<- hs.cn(chunk1)
                on.exit(cn1 <<- {
                }, add = TRUE)
                environment(wk.fread.chunk.6)[["remaining"]] <- substr(chunk1, i1 + 1, nchar(chunk1))
            }
            else stop("incomplete line read @34.12.06.085808")
        }
        R <- expandingList()
        Args <- c(list(io), sep = sep, header = FALSE, ...)
        if (all) 
            Args["stepSize"] <- list(rep(1e+09, 1000))
        repeat {
            L <- .try(do.call(wk.fread.chunk.6, Args))
            if ((length(L) == 0) || (class(L) == "try-error")) 
                break
            if (length(cn1)) 
                setnames(L, cn1)
            t1 <- system.time(r1 <- hs(L), gcFirst = FALSE)
            if (t1["elapsed"] < 3) 
                warning("too short running time @34.12.06.094358")
            R$add(r1)
        }
        R <- R$get()
        if (simplify) {
            R[[1]]
        }
        else R
    }
})
wk.fread.chunk.do.line.2 <- local({
    cn1 <- {
    }
    function(io = {
    }, hs = identity, hs.cn = function(io) readLines(io, 1) %=>% strsplit(sep) %=>% unlist, nrows = 1e+05, all = 0, simplify = all, header = sep != "\n", sep = "\t", smart = 1, clear.cn = length(io), co1 = Inf, co.hs = length, co.acc = 0, dt = sep != "\n", reader = wk.fread.chunk.7, ...) {
        if (clear.cn) {
            cn1 <<- {
            }
        }
        if (!length(io)) 
            return()
        if (smart) 
            io <- hs.make.connection(io)
        if (header && (length(cn1) == 0)) 
            cn1 <<- hs.cn(io)
        on.exit(cn1 <<- {
        }, add = TRUE)
        R <- expandingList()
        Args <- c(list(io), nrows = nrows, sep = sep, dt = dt, ...)
        if (all) 
            Args["stepSize"] <- list(-1L)
        repeat {
            if (co.acc >= co1) 
                break
            L <- do.call(reader, Args)
            if (length(L) == 0) {
                break
            }
            if (length(cn1)) 
                setnames(L, cn1)
            t1 <- system.time(r1 <- hs(L), gcFirst = FALSE)
            co.acc %<=>% {
                x + co.hs(r1)
            }
            if (t1["elapsed"] < 3) 
                warning("too short running time @34.12.06.094358@")
            R$add(r1)
        }
        R <- R$get()
        if (simplify) {
            R[[1]]
        }
        else R
    }
})
wk.fread.chunk.do.line.3 <- local({
    cn1 <- {
    }
    function(io = {
    }, hs = identity, hs.cn = function(io) readLines(io, 1) %=>% strsplit(sep) %=>% unlist, nrows = 1e+05, all = 0, simplify = all, header = sep != "\n", sep = "\t", smart = 1, clear.cn = length(io), co1 = Inf, co.hs = length, co.acc = 0, dt = sep != "\n", reader = wk.fread.chunk.7, ...) {
        if (clear.cn) {
            cn1 <<- {
            }
        }
        if (!length(io)) 
            return()
        if (smart) 
            io <- hs.make.connection(io)
        if (header && (length(cn1) == 0)) 
            cn1 <<- hs.cn(io)
        on.exit(cn1 <<- {
        }, add = TRUE)
        R <- expandingList()
        Args <- c(list(io), nrows = nrows, sep = sep, dt = dt, ...)
        if (all) 
            Args["stepSize"] <- list(-1L)
        repeat {
            if (co.acc >= co1) 
                break
            L <- do.call(reader, Args)
            if (length(L) == 0) {
                break
            }
            if (length(cn1)) 
                setnames(L, cn1)
            t1 <- system.time(r1 <- hs(L), gcFirst = FALSE)
            co.acc %<=>% {
                x + co.hs(r1)
            }
            if (t1["elapsed"] < 3) 
                warning("too short running time @34.12.06.094358@")
            R$add(r1)
        }
        R <- R$get()
        if (simplify) {
            R[[1]]
        }
        else R
    }
})
wk.fread.chunk.do.line <- wk.fread.chunk.do.line.2
wk.fread.chunk.do.lines <- local({
    L <- {
    }
    function(io, hs, hs.sep.i, hs.sep = hs.sepLines, hs.sep.arg = {
    }, hs.stop = {
    }, ...) {
        L <<- {
        }
        if ("connection" %not.in% class(io)) {
            if (file.exists(io)) {
                io %<=>% gzfile("r")
            }
            else io %<=>% pipe("r")
        }
        on.exit(close(io), add = TRUE)
        R <- expandingList()
        L <- c()
        miss <- 0
        repeat {
            if (miss > 3) 
                hs.browser("Too many times reading without doing anything. 34.06.29.173224")
            L1 <- wk.fread.chunk(io, dt = 0, ...)
            if (length(L1)) 
                if (length(L)) {
                  L %<=>% c(L1)
                }
                else L <- L1
            if (length(L) == 0) 
                break
            parts <- do.call(hs.sep, append(list(L, hs.i = hs.sep.i, at.last = !length(L1)), hs.sep.arg))
            if (length(parts[[1]])) {
                R$add(do.call(hs, parts[-2]))
                miss <- 0
            }
            if (length(hs.stop) && hs.stop(L)) 
                break
            if (identical(parts[[2]], L) && length(L1)) 
                miss <- miss + 1
            L <- parts[[2]]
        }
        R$get()
    }
})
hs.sepLines <- function(., hs.i, bs = 1, keep.i = bs > 1, at.last = 0) {
    parts <- vector("list", 2 + as.logical(keep.i))
    i1 <- hs.i(.)
    bn <- (length(i1) - ifelse(at.last, 0, 1))%/%bs
    if (bn) {
        i1.end <- bn * bs
        i2 <- ifelse(at.last, length(.), i1[i1.end + 1] - 1)
        parts[[1]] <- .[seq(i1[1], i2)]
        parts[[2]] <- tail(., length(.) - i2)
        if (keep.i) {
            parts[[3]] <- head(i1, i1.end) %=>% {
                if (x[1] > 1) {
                  x <- x - (x[1] - 1)
                }
                x
            }
        }
    }
    else {
        parts[[2]] <- .
    }
    parts
}
wk.fread.chunk.do <- function(io, hs, hs.sep, ...) {
    if ("connection" %not.in% class(io)) 
        io %<=>% pipe("r")
    on.exit(close(io), add = TRUE)
    R <- expandingList()
    tmp <- expandingList()
    L <- wk.dt.inList()
    it <- 0
    repeat {
        L1 <- ieiwk.fread.chunk(io, ...)
        if (missing(hs.sep)) {
            if (length(L1) == 0) 
                break
        }
        else {
            if (length(L1)) 
                L$add(L1)
            if (L$nrow() == 0) 
                break
            i1 <- L$do(hs.sep)
            if (length(i1)) 
                R$add(hs(L$getRows(i1, rm = 1)))
        }
        it %<=>% {
            x + 1
        }
        .ss(it)
        if (0) 
            if (it == 9) 
                hs.browser("34.06.28.225616")
    }
    R$get()
}
ieiwk.readChar.chunk <- function(con, stepSize = 1e+09, complete.line = 1, useBytes = FALSE, skip.char.n = 0) {
    con <- ieiwk.make.connection(con)
    if (skip.char.n) {
        readChar(con, skip.char.n)
    }
    a <- readChar(con, stepSize)
    browser("33.04.21.122438")
    tt <- tempfile(tmpdir = "/dev/shm")
    if (length(a)) {
        a.len <- str_length(a)
        if (grepl("\n$", substring(a, a.len))) {
            a1 <- paste0(readLines(con, 1), "\n")
            system.time(substr(a, a.len + 1, a.len + 1 + str_length(a1)) <- a1)
            system.time(a <- paste0(a, readLines(con, 1), "\n"))
            system.time(a <- sprintf("%s%s\n", a, readLines(con, 1)))
            system.time(cat(a, file = tt))
            cat(a1, file = tt, append = 1)
            readChar()
            .less(tt)
            unlink(tt)
        }
    }
    a
}
ieiwk.readChar.chunk.all <- function(con, stepSize = 1e+09, complete.line = 1, useBytes = FALSE, skip.char.n = FALSE) {
    con <- ieiwk.make.connection(con)
    r <- list()
    if (skip.char.n) {
        readChar(con, skip.char.n)
    }
    while (length(a <- ieiwk.readChar.chunk(con, stepSize = stepSize))) {
        r[[length(r) + 1]] <- a
    }
    r
}
ieiwk.nrc <- function(n, L) {
    a <- n%%L
    b <- n/L
    if (a) {
        c(ceiling(b), a)
    }
    else {
        c(b, L)
    }
}
ieiwk.nrc.seq <- function(i, j = NULL, S) {
    if (is.null(j)) {
        if (length(i) == 1) {
            j <- i
        }
        else {
            j <- i[2]
            i <- i[1]
        }
    }
    if (i > j) 
        stop("小的在左")
    L <- nchar(S[1])
    i.xy <- ieiwk.nrc(min(i, j), L)
    j.xy <- ieiwk.nrc(max(i, j), L)
    switch(as.character(diff(c(i.xy[1], j.xy[1]))), `0` = substr(S[i.xy[1]], i.xy[2], j.xy[2]), `1` = {
        paste(c(substr(S[i.xy[1]], i.xy[2], L), substr(S[j.xy[1]], 1, j.xy[2])), collapse = "")
    }, paste(c(substr(S[i.xy[1]], i.xy[2], L), S[seq.int(i.xy[1] + 1, j.xy[1] - 1)], substr(S[j.xy[1]], 1, j.xy[2])), collapse = ""))
}
hs.pick <- hs.select <- function(x, hs, v = 0, inv = 0) {
    if (missing(hs)) 
        return(x)
    i1 <- {
        if ("function" %not.in% class(hs)) {
            hs
        }
        else if (v) {
            hs(x)
        }
        else sapply(x, hs)
    }
    if (inv) 
        i1 <- !i1
    x[i1]
}
"%no.less.than%" <- function(x, y) as.numeric(x) >= as.numeric(y)
"%larger.than%" <- function(x, y) as.numeric(x) > as.numeric(y)
"%smaller.than%" <- function(x, y) as.numeric(x) < as.numeric(y)
"%no.larger.than%" <- function(x, y) as.numeric(x) <= as.numeric(y)
ieiwk.tvvt <- function(tv) {
    a <- names(tv)
    names(a) <- tv
    a
}
inverseList <- function(l) {
    rId <- unlist(l, use.names = FALSE)
    lId <- rep(names(l), sapply(l, length))
    return(split(lId, rId))
}
ieiwk.intersect <- function(...) {
    a <- list(...)
    repeat {
        if (length(a) < 2) 
            break
        a[[2]] <- intersect(a[[1]], a[[2]])
        a <- a[-1]
    }
    a[[1]]
}
tt <- function(x) table(table(x))
.lu <- function(x) length(unique(x))
.luu <- function(x) length(unique(unlist(x)))
nm <- names
"%|%" <- function(x, y) {
    .sys(x, "|", y)
}
"%(%" <- function(x, y, env1 = parent.frame()) {
    if (!is.function(x)) {
        x <- paste(c(x), collapse = "")
    }
    ye <- substitute(y)
    if (("call" %in% class(ye)) && (length(ye) > 1)) {
        ye <- as.call(c(list, as.list(ye)[-1]))
    }
    y <- eval(ye, env1)
    do.call(x, as.list(y))
}
"%(c%" <- function(x, y, env1 = parent.frame()) {
    if (!is.function(x)) {
        x <- paste(c(x), collapse = "")
    }
    do.call(x, as.list(y))
}
"%(unname%" <- function(x, y) {
    do.call(x, unname(as.list(y)))
}
"%)%" <- function(y, x) {
    if (!is.list(y)) 
        y <- as.list(y)
    do.call(x, y)
}
`%xor%` <- function(x, y) setdiff(union(x, y), intersect(x, y))
"%overlap%" <- function(r1, r2) {
    !{
        r1[2] < r2[1] | r1[1] > r2[2]
    }
}
"%within%" <- function(x, r) {
    r <- sort(r)
    x >= r[1] & x <= r[2]
}
"%!in%" <- function(x, y) {
    !(x %in% y)
}
wk.which <- function(l, inv = 0, na = 1) {
    if (inv) {
        if (na) {
            i <- which(l)
            if (length(i)) {
                seq_along(l)[-i]
            }
            else seq_along(l)
        }
        else which(!l)
    }
    else {
        if (na) {
            wk.which(!l, inv = 1, na = 1)
        }
        else {
            which(l)
        }
    }
}
ieiwk.within.core <- function(x, range, left = ">=", right = "<=", output = "position") {
    range <- sort(range)
    i <- do.call(left, list(x, range[1])) & do.call(right, list(x, range[2]))
    switch(output, position = i, content = x[i])
}
hs.within <- wk.within <- ieiwk.within <- function(x, ranges, left = ">=", right = "<=", output = "position") {
    if (!is.list(ranges)) {
        if (length(dim(ranges))) {
            ranges2 <- lapply(seq(nrow(ranges)), function(i) ranges[i, ])
            if (length(rownames(ranges))) 
                names(ranges2) <- rownames(ranges)
            ranges <- ranges2
        }
        else ranges <- list(ranges)
    }
    lapply(ranges, function(range) {
        ieiwk.within.core(x, range)
    }) %=>% as.data.table
}
ieiwk.sample <- function(pool, sizes) {
    r <- c()
    while (length(sizes)) {
        i <- sample(seq.int(length(pool)), sizes[1])
        r[[length(r) + 1]] <- pool[i]
        pool <- pool[-i]
        sizes <- tail(sizes, -1)
    }
    r
}
ieiwk.combn <- function(x, y, sep = "|") {
    as.vector(sapply(x, function(x) {
        sprintf("%s%s%s", x, sep, y)
    }))
}
wk.combn <- function(x, m = 2, sep, diag = 1) {
    if (!diag) 
        return(combn(x, m) %=>% t %=>% data.table)
    g <- seq_along(x) %=>% list %=>% rep(m) %=>% (`%)%`(expand.grid))
    for (ci in hs.seqForward(1, m - 1)) {
        ri <- which(g[, ci] <= g[, ci + 1])
        g <- g[ri, ]
    }
    hs.browser("2023.08.04.213453", debug = 0)
    R <- g %=>% data.table %=>% setkey %=>% {
        x[, lapply(.SD, . %>% x[.])]
    } %=>% setnames(x, paste("V", seq.int(length(x)), sep = ""))
    if (!missing(sep)) 
        R %<=>% ieiwk.sprintf(sep)
    R
}
wk.sapply <- ieiwk.sapply <- function(x, hs, ..., mc = 1, nc = ifelse(mc, hs.NC(), 1), nc.smart = 1, finally = {
}) {
    if (!length(x)) 
        return()
    if (nc == "max") 
        nc <- length(x)
    Names <- {
        if (length(names(x))) {
            names(x)
        }
        else if ("character" %in% class(x)) {
            x
        }
        else if (is.numeric(x)) {
            as.character(x)
        }
    }
    hs <- match.fun(hs)
    R <- {
        if (identical(hs, match.fun("switch"))) {
            hs2 <- substitute(list(...))
            if (nc.smart && (capture.output(hs2) %=>% wk.re("^\\s*wk\\.browser\\(", "detect") %=>% any)) 
                nc <- 1
            c1 <- hs2 %=>% as.list %=>% {
                x[[1]] <- as.symbol("switch")
                x
            }
            if (0) {
                wk.sapply(names(x), . %>% {
                  a <- .
                  hs2[[.]] %>% {
                    if (typeof(.) %in% c("language")) 
                      .
                    else match.fun(.)
                  } %>% {
                    eval(bquote({
                      hs <- .(.)
                      hs(x[[a]])
                    }))
                  }
                }, mc = mc, nc = nc)
            }
            hs.browser("2023.08.04.213640", debug = 0)
            wk.sapply(names(x), . %=>% {
                a <- x
                c1 %=>% append(a, 1) %=>% as.call %=>% eval %=>% {
                  if (length(x)) 
                    {
                      if (typeof(x) %in% c("language")) 
                        x
                      else match.fun(x)
                    } %>% {
                      .(x[[a]])
                    }
                }
            }, mc = mc, nc = nc)
        }
        else {
            if (nc.smart && (ifelse("fseq" %in% class(hs), functions, identity)(hs) %>% capture.output %>% wk.re("^\\s*(wk\\.browser\\(|<<-)", "detect", do.group = 0) %>% any)) {
                nc <- 1
            }
            a <- wk.mclapply(x, hs, ..., mc.cores = nc)
            if (length(Names)) 
                names(a) <- Names
            a
        }
    }
    if (!missing(finally)) 
        eval(finally)
    R
}
hs.lapply <- wk.lapply <- function(x1, FUN, ..., simplify = FALSE, smart = TRUE, mz = {
}, x1.sep = ",") {
    if ((length(x1) == 1) && is.character(x1)) 
        x1 <- strsplit(x1, ",")[[1]]
    mz <- if (smart) {
        if (length(names(x1))) {
            names(x1)
        }
        else if (is(x1, "character")) {
            x1
        }
        else if (is.numeric(x1)) {
            as.character(x1)
        }
    }
    if (is.symbol(substitute(FUN))) 
        FUN <- paste(substitute(FUN))
    jg <- sapply(x1, FUN, ..., simplify = simplify)
    if (length(mz)) 
        names(jg) <- mz
    jg
}
wk.apply <- function(x, m, hs, ..., mc = 1, nc = ifelse(mc, 8, 1)) {
    hs <- match.fun(hs)
    hs.switch(m, 1, wk.sapply(seq(nrow(x)), function(.) hs(x[., ], ...), mc = mc, nc = nc) %=>% {
        if (!length(names(x))) 
            names(x) <- rownames(x)
        x
    }, 2, wk.sapply(seq(ncol(x)), function(.) hs(x[, ., with = FALSE][[1]], ...), mc = mc, nc = nc) %=>% {
        names(x) %<=>% {
            if (length(x)) 
                x
            else colnames(x)
        }
        x
    })
}
wk.tapply <- function(x, i, hs, ..., mc = 1, nc = ifelse(mc, hs.NC(), 1), chk.i = 1) {
    hs <- match.fun(hs)
    if (is.factor(i) && chk.i) {
        if (levels(i) %=>% {
            (length(x) > length(i)) || length(x %not% i)
        }) 
            i %<=>% as.character
    }
    j <- tapply(seq_along(x), as.vector(i), c)
    if (identical(hs, match.fun("switch"))) {
        hs2 <- substitute(list(...))
        if (capture.output(hs2) %=>% wk.re("^[^#]*\\bbrowser\\(", "detect") %=>% any) 
            nc <- 1
        c1 <- hs2 %=>% as.list %=>% {
            x[[1]] <- as.symbol("switch")
            x
        }
        . <- "gene"
        wk.sapply(names(j), . %>% {
            a <- .
            c1 %>% append(a, 1) %>% as.call %>% eval %>% {
                if (typeof(.) %in% c("language")) 
                  .
                else match.fun(.)
            } %>% {
                .(x[j[[a]]])
            }
        }, mc = mc, nc = nc)
    }
    else {
        if (capture.output(hs) %>% wk.re("^[^#]*\\bbrowser\\(", "detect") %>% any) 
            nc <- 1
        wk.sapply(j, . %>% {
            hs(x[.], ...)
        }, mc = mc, nc = nc)
    }
}
ieiwk.rapply.get <- function(l, i) {
    r <- list()
    if (missing(i)) {
        rapply(l, function(x) {
            r <<- append(r, list(x))
        })
    }
    else {
        it <- 1
        rapply(l, function(x) {
            if (it == i) 
                r <<- append(r, list(x))
            it <<- it + 1
        })
    }
    r
}
wk.rapply.flat <- local({
    acc <- {
    }
    hs1 <- function(x, node = {
    }, sep = ":") {
        if ("list" %in% class(x)) {
            sapply(names(x), . %>% {
                hs1(x[[.]], c(node, .))
            })
        }
        else {
            acc[["add"]](node %>% paste(collapse = sep), x)
            acc %>% names
        }
    }
    function(x, node = {
    }, sep = ":") {
        acc <<- namedExpandingList()
        hs1(x = x, node = node, sep = sep)
        acc[["get"]]()
    }
})
ieiwk.replace.stepwise <- function() local({
    i.considered <- {
    }
    function(d1, i1, c1, reset = 0) {
        if (reset) 
            i.considered <<- {
            }
        i1 <- i1 %not% i.considered
        if (length(i1)) {
            d1[i1] <- c1
            i.considered <<- unique(c(i.considered, i1))
        }
        d1
    }
})
hs.once <- wk.once <- function(x, inv = 0, out = "x") {
    i <- duplicated(x)
    i <- x %in% x[i]
    if (!inv) 
        i <- !i
    switch(out, i = i, x = x[i])
}
hs.once_simple <- function(x) {
    i <- duplicated(x)
    x %not% x[i]
}
hs.twice <- wk.twice <- function(x, out = "x") {
    R <- hs.once(x[duplicated(x)])
    if (out == "i") 
        R <- x %in% R
    R
}
wk.make.hash_table <- function() {
    env1 <- new.env()
    evalq(h1 <- new.env(), env1)
    evalq(V <- expandingVector(), env1)
    evalq(hasher <- function(v1) {
        if (is.character(v1) && (length(v1) == 1)) {
            v1
        }
        else rlang::hash(v1)
    }, env1)
    evalq(set <- function(v1, k1) {
        v1 <- hasher(v1)
        V[["add"]](v1)
        h1[[v1]] <- k1
    }, env1)
    evalq(has <- function(v1) {
        v1 <- hasher(v1)
        exists(v1, envir = h1, inherits = FALSE)
    }, env1)
    evalq("+=" <- function(v1, k1, n1 = 0) {
        v1 <- hasher(v1)
        n0 <- if (has(v1)) {
            h1[[v1]]
        }
        else {
            V[["add"]](v1)
            n1
        }
        h1[[v1]] <- k1 + n0
    }, env1)
    evalq(get <- function(v1) {
        if (missing(v1)) {
            v1 <- V[["get"]]() %and% ls(envir = h1, all.names = TRUE, sorted = FALSE)
            mget(v1, h1)
        }
        else {
            v1 <- hasher(v1)
            h1[[v1]]
        }
    }, env1)
    evalq(mget <- function(v1) {
        if (missing(v1)) 
            v1 <- V[["get"]]() %and% ls(envir = h1, all.names = TRUE, sorted = FALSE)
        mget(as.character(v1), h1)
    }, env1)
    evalq(pop <- del <- function(v1) {
        v1 <- hasher(v1)
        if (has(v1)) {
            k1 <- h1[[v1]]
            rm(list = v1, envir = h1)
            k1
        }
    }, env1)
    env1
}
wk.strCutL <- function(x, L) {
    M <- cumsum(L) %=>% {
        cbind(c(1, head(x, -1) + 1), x)
    }
    substr(x, M[, 1], M[, 2])
}
wk.strCutSL <- function(x, S, L) {
    substr(x, S, S + L - 1)
}
ieiwk.string.col <- function(str, col = 1, sep = "\t") {
    if (!length(col)) 
        return()
    col.0 <- col
    col <- sort(unique(col))
    acc <- matrix(nrow = length(str), ncol = length(col))
    colnames(acc) <- col
    if (max(col) < 3) {
        for (c1 in as.character(col)) {
            acc[, c1] <- wk.re(str, switch(c1, `1` = paste0("[^", sep, "]+"), `2` = paste0("(?<=", sep, ")[^", sep, "]+")))
        }
    }
    else {
        a <- wk.re(str, "[^:]+", which = col, all = 1)
        a <- {
            if (length(col) > 1) 
                t(sapply(a, c))
            else as.matrix(unlist(a))
        }
        for (i in seq_along(col)) acc[, paste0(col[i])] <- a[, i]
        if (0) {
            sep.pos <- str_locate_all(str, sep)
            start <- matrix(nrow = length(str), ncol = length(col))
            end <- matrix(nrow = length(str), ncol = length(col))
            for (i in seq_along(sep.pos)) {
                start[i, ] <- sep.pos[[i]][col - 1, 2] + 1
                end[i, ] <- sep.pos[[i]][col, 1] - 1
            }
            for (i in seq_along(col)) acc[, paste0(col[i])] <- substr(str, start[, i], end[, i])
        }
    }
    acc[, paste0(col.0), drop = 0]
}
wk.str_split_fixed <- ieiwk.str_split_fixed <- function(string, pattern = "\t", n = str_count(string[1], pattern) + 1) data.table(str_split_fixed(string, pattern, n))
ieiwk.str_subset <- function(s, p, inv = 1) {
    i <- str_detect(s, p)
    if (inv) 
        s[!i]
    else s[i]
}
hs.trim <- trim <- function(x, trim = "\\s", l = TRUE, r = TRUE, m = FALSE) {
    if (l) 
        x <- sub(sprintf("^%s+", trim), "", x)
    if (r) 
        x <- sub(sprintf("%s+$", trim), "", x)
    if (m) 
        x <- gsub(sprintf("(?<=[^%s])%s+(?=[^%s])", trim, trim, trim), "", x, perl = TRUE)
    x
}
ieiwk.capitalize.1st.letter <- function(s) {
    sapply(s, function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
    })
}
ieiwk.strsplit.pick <- ieiwk.substr.i <- function(str, i, sep = "\\.") {
    strsplit(str, sep)[[1]][i]
}
"%,%" <- function(x, y) paste(x, collapse = y)
"%.%" <- function(x, y) sprintf("%s%s", x, y)
revStr <- function(x, sep = "") {
    sapply(strsplit(as.character(x), sep), function(a) {
        paste(rev(a), collapse = sep)
    })
}
revStr.int <- function(x) intToUtf8(rev(utf8ToInt(x)))
ieiwk.dna.nulceotide.complementary <- function(x) switch(x, A = "T", T = "A", C = "G", G = "C")
ieiwk.dna.complementaryRev.viaInt <- local({
    a <- c()
    a[utf8ToInt("ATCG")] <- utf8ToInt("TAGC")
    a[utf8ToInt("atcg")] <- utf8ToInt("tagc")
    a[utf8ToInt("N")] <- utf8ToInt("N")
    function(x) {
        b <- a[utf8ToInt(x)]
        i.na <- which(is.na(b))
        rev(seq_along(b))
        intToUtf8(rev(a[utf8ToInt(x)]))
    }
})
ieiwk.str.insert <- function(string, pos, insert) {
    paste(read.fwf(textConnection(string), c(pos, nchar(string)), as.is = TRUE), collapse = insert)
}
ieiwk.substr <- function(x, starts = 1, lengths = 1, stops = NULL) {
    if (length(lengths)) {
        stops <- starts + lengths - 1
    }
    else {
        if (!length(stops)) {
            return(substring(x, starts))
        }
    }
    substr(x, starts, stops)
}
wk.re <- function(text, pattern, act = "extract", replacement = "", which = {
}, do.group = if (missing(pattern)) {
    0
} else {
    grepl("[^\\\\]\\([^\\?]", pattern, perl = TRUE)
}, sub.len = 30, ic = if (missing(pattern)) 1 else {
    !grepl("[A-Z]", pattern, perl = TRUE)
}, ignore.case = ic, perl = 1, fixed = 0, whole = 0, useBytes = 0, invert = 0, all = 0, info, shift.start = 0, shift.length = 0) {
    invert <- as.logical(invert)
    if (missing(info)) {
        if (whole) {
            if (str_sub(pattern, 1, 1) != "^") 
                pattern <- .j("^", pattern)
            if (str_sub(pattern, -1) != "$") 
                pattern <- .j(pattern, "$")
        }
        if (act %in% c("detect", "which", "subset")) {
            hs1 <- regexpr
            i1 <- seq_along(text)
            i2 <- seq_along(pattern)
            i <- cbind(i1, i2)
            r <- rep(TRUE, nrow(i))
            for (i.p in unique(i[, 2])) {
                i1.1 <- which(i[, 2] %in% i.p)
                m <- hs1(pattern = pattern[i.p], text = text[i[i1.1, 1]], ignore.case = as.logical(ignore.case), perl = as.logical(perl), fixed = as.logical(fixed), useBytes = as.logical(useBytes))
                r[i1.1] <- m %in% -1
            }
            if (!invert) 
                r <- !r
            return(switch(act, detect = r, which = which(r), subset = text[r]))
        }
        if (act == "count") 
            all <- 1
        hs1 <- ifelse(all, gregexpr, regexpr)
        m <- hs1(pattern = pattern, text = text, ignore.case = as.logical(ignore.case), perl = as.logical(perl), fixed = fixed, useBytes = useBytes)
        if (0) {
            regexpr(".*腾讯.*", "腾讯123", perl = TRUE, ignore.case = TRUE)
            grepl("[A-Z]", pattern, perl = FALSE)
            grepl("[A-Z]", ".*腾讯A.*", perl = TRUE)
            grepl("[A-Z]", "aBc")
            hs1(pattern = pattern, text = text, perl = TRUE)
            hs1(pattern = pattern, text = text, perl = TRUE, ignore.case = as.logical(ignore.case))
            hs1(pattern = pattern, text = text, ignore.case = as.logical(ignore.case), perl = as.logical(perl), fixed = fixed, useBytes = useBytes)
        }
    }
    else m <- info
    switch(act, extract = {
        if (do.group) {
            if (all) {
                m1 <- {
                  if (length(which)) {
                    lapply(m, function(x) {
                      m1 <- attr(x, "capture.start")[, which, drop = 0]
                      attr(m1, "match.length") <- attr(x, "capture.length")[, which, drop = 0]
                      m1
                    })
                  } else {
                    lapply(m, function(x) {
                      m1 <- attr(x, "capture.start")
                      attr(m1, "match.length") <- attr(x, "capture.length")
                      m1
                    })
                  }
                }
            } else {
                if (length(which)) {
                  m1 <- attr(m, "capture.start")[, which, drop = 0]
                  attr(m1, "match.length") <- attr(m, "capture.length")[, which, drop = 0]
                } else {
                  if (0) m1 <- lapply(seq_along(m), function(i) {
                    m1 <- attr(m, "capture.start")[i, ]
                    attr(m1, "match.length") <- attr(m, "capture.length")[i, ]
                    m1
                  })
                  return(lapply(seq.int(ncol(attr(m, "capture.start"))), function(i) {
                    m1 <- attr(m, "capture.start")[, i]
                    attr(m1, "match.length") <- attr(m, "capture.length")[, i]
                    regmatches(text, m1, invert)
                  }))
                }
            }
            if (0) {
                r <- vector("character", length(m))
                r[m %not.in% -1] <- regmatches(text, m, invert)
                r
            }
            regmatches(text, m1, invert)
        } else {
            if (all && length(which)) m <- lapply(m, function(x) {
                m1 <- x[which]
                attr(m1, "match.length") <- attr(x, "match.length")[which]
                m1
            })
            r <- vector("character", length(m))
            r[m %not.in% -1] <- regmatches(text, m, invert)
            r
        }
    }, count = {
        sapply(m, . %=>% sum(x > 0))
    }, length = {
        if (all) {
            sapply(m, attr, "match.length")
        } else attr(m, "match.length")
    }, end = {
        if (all) {
            sapply(m, function(x) {
                as.vector(x + attr(x, "match.length") - 1)
            })
        } else as.vector(m + attr(m, "match.length") - 1)
    }, replace = {
        regmatches(text, m, invert) <- replacement
        text
    }, locate = m, sub = {
        str_sub(text, m, m + sub.len - 1)
    }, split = {
        regmatches(text, m, invert = 1)
    })
}
wk.re.replace <- function(text, pattern, replacement = "", all = 1, ...) {
    wk.re(text, pattern, act = "replace", replacement = replacement, all = all, ...)
}
wk.re.split <- function(text, pattern = "\t", all = 1, ...) {
    wk.re(text, pattern, act = "split", all = all, ...)
}
wk.re.ss <- function(text, pattern = "\t", ...) {
    wk.re(text, pattern, act = "subset", ...)
}
wk.striLocSub <- function(text, pattern = "\t", which = "self", ic = TRUE, last = 0, fixed = 0) {
    args <- list(str = text, mode = ifelse(last, "last", "first"), case_insensitive = ic)
    args[[ifelse(fixed, "fixed", "regex")]] <- pattern
    i <- do.call("stri_locate", args)
    switch(which, self = {
        j <- i[, "start"]
        k <- i[, "end"]
    }, before = {
        j <- 1
        k <- i[, "start"] - 1
    }, after = {
        j <- i[, "end"] + 1
        k <- -1L
    })
    stri_sub(text, j, k)
}
wk.re.ms <- function(n = 1, sep = "\t") {
    p1 <- p2 <- str_c("[^", sep, "]+")
    if (n > 1) 
        p2 <- str_c("(", p1, sep, ")")
    str_c(c(p2, if (n > 1) c("{", n - 1, "}", p1)), collapse = "")
}
wk.gre <- function(pattern, text, ignore.case = TRUE, perl = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
    wk.re(pattern = pattern, text = text, ignore.case = as.logical(ignore.case), perl = as.logical(perl), fixed = fixed, useBytes = useBytes, all = TRUE)
}
ieiwk.grep <- function(patterns, x, ignore.case = TRUE, extended = TRUE, perl = FALSE, value = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
    sapply(patterns, function(pattern) {
        grep(pattern, x, ignore.case, perl, value, fixed, useBytes, invert)
    })
}
ieiwk.grep.all <- function(patterns, x, ignore.case = FALSE, extended = TRUE, perl = FALSE, value = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
    i <- seq_along(x)
    args <- ieiwk.make.arg.list(perl, fixed, useBytes, invert)
    args["value"] <- FALSE
    if (!fixed) 
        args["ignore.case"] <- ignore.case
    for (p1 in patterns) {
        if (!length(i)) 
            break
        args[["pattern"]] <- p1
        args[["x"]] <- x[i]
        i <- i[do.call("grep", args)]
    }
    if (value) 
        x[i]
    else i
}
ieiwk.grep.any <- function(patterns, x, ignore.case = TRUE, extended = TRUE, perl = FALSE, value = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
    i <- rep(FALSE, length(x))
    j <- seq_along(x)
    args <- ieiwk.make.arg.list(perl, fixed, useBytes, invert)
    args["value"] <- FALSE
    if (!fixed) 
        args["ignore.case"] <- ignore.case
    p1 <- patterns[1]
    for (p1 in patterns) {
        args[["pattern"]] <- p1
        args[["x"]] <- x[j]
        k <- j[do.call("grep", args)]
        i[k] <- TRUE
        j <- j %not% k
    }
    i <- which(i)
    if (value) 
        x[i]
    else i
}
ieiwk.match <- function(x, y, all = 0, ...) {
    if (is.list(y)) {
        g <- rep(seq_along(y), sapply(y, length))
        y2 <- unlist(y)
    }
    else {
        g <- seq_along(y)
        y2 <- y
    }
    if (all) {
        lapply(x, function(x1) {
            g[which(y2 == x1)]
        })
    }
    else g[match(x, y2, ...)]
}
.conda <- function(act = "list") {
    .wk("conda", act)
}
wk.sprintf <- ieiwk.sprintf <- function(matrix, sep = "\t") {
    if (0) {
        matrix %<=>% as.data.table
    }
    hs.update(matrix, wk.dt.as, !inherits(x, "data.table"))
    matrix[, do.call(str_c, c(unname(.SD), sep = sep))]
}
ieiwk.as.date <- function(x, sep = c("/", "-", ".")) {
    f1 <- 1
    x <- trim(x)
    for (s1 in sep) {
        if (grepl(s1, x, fixed = 1)) {
            f1 <- 0
            break
        }
    }
    if (f1) {
        paste(apply(matrix(c(1, 4, 5, 6, 7, 8), 2), 2, function(i) {
            substr(x, i[1], i[2])
        }), collapse = "-")
    }
    else x
}
ieiwk.thisYear <- function(n = 0) {
    a <- format(Sys.time(), "%Y")
    if (n) 
        as.integer(a)
    else a
}
ieiwk.time.add <- function(t1 = 0, week = 0, day = 0, hour = 0, minute = 0, second = 0) {
    t1 + (((week * 7 + day) * 24 + hour) * 60 + minute) * 60 + second
}
.sys.mtime <- function(f) {
    strptime(paste(strsplit(.sys.do("ls -l -G -n --time-style=full-iso", .sys.fpGood(f), intern = 1), " ")[[1]][c(5, 6)], collapse = " "), "%Y-%m-%d %H:%M:%S")
}
data.check <- local({
    helper <- function(d1, d2) {
        r <- list()
        i1 <- which(sapply(d1, is.na))
        if (length(i1)) 
            r[["na"]] <- i1
        i1 <- which(sapply(d2, function(x) {
            is.na(x) || class(x) == "try-error"
        })) %not% r[["na"]]
        if (length(i1)) 
            r[["illegal"]] <- data.frame(row = i1, value = d1[i1])
        if (length(r)) 
            r
        else "nothing wrong"
    }
    function(d1, type = "age") {
        switch(type, age = helper(d1, as.numeric(d1)), date = helper(d1, lapply(d1, function(x) .try(as.Date(x)))), string = helper(d1, d1))
    }
})
wk.table.margin <- function(x, n1 = "Total") {
    r <- cbind(rbind(x, colSums(x)), c(rowSums(x), sum(x)))
    colnames(r)[ncol(r)] <- n1
    rownames(r)[nrow(r)] <- n1
    r
}
ieiwk.select.matrix <- function(m1, ic, ir, f1, rt = "v", inv = 0, sm) {
    q1 <- {
        g1 <- as.list(f1)
        if (missing(sm)) {
            cbind(seq_along(ic), seq_along(g1))
        }
        else {
            if (length(dim(sm))) {
                sm
            }
            else {
                matrix(sm, byrow = 1, ncol = 2)
            }
        }
    }
    i0 <- i <- seq.int(nrow(m1))
    for (i1 in seq.int(nrow(q1))) {
        if (!length(i)) 
            break
        ic1 <- ic[q1[i1, 1]]
        if (identical(g1[[q1[i1, 2]]], "-")) {
            if (is.character(ic1)) 
                ic1 <- match(ic1, colnames(m1))
            m1 <- m1[, -ic1, drop = 0]
        }
        else {
            x <- m1[i, ic1]
            i <- i[which({
                f2 <- g1[[q1[i1, 2]]]
                if (is.character(f2)) {
                  eval(parse(text = str_c("x", f2, sep = " ")))
                }
                else f2(x)
            })]
        }
    }
    if (inv) 
        i <- i0[-i]
    switch(rt, i = i, v = m1[i, , drop = 0])
}
wk.seq.nrow <- function(m) {
    seq.int(nrow(m))
}
wk.seq.ncol <- function(m) {
    seq.int(ncol(m))
}
ieiwk.as.matirx <- function(x, f1 = identity) {
    x <- as.matrix(x)
    r <- f1(x)
    dim(r) <- dim(x)
    r
}
ieiwk.t <- function(M) {
    sapply(if (is.null(rownames(M))) 
        seq(nrow(M))
    else rownames(M), function(c1) {
        sapply(if (is.null(colnames(M))) 
            seq(ncol(M))
        else colnames(M), function(c2) {
            M[c1, c2]
        })
    })
}
ieiwk.l2m <- function(l) {
    matrix(unlist(l), nrow = length(l[[1]]))
}
ieiwk.data.frame <- function(..., row.names = NULL, check.rows = FALSE, check.names = TRUE, stringsAsFactors = FALSE) {
    data.frame(..., row.names = row.names, check.rows = check.rows, check.names = check.names, stringsAsFactors = stringsAsFactors)
}
"%[%" <- function(...) {
    a <- as.list(substitute(...[]))
    a[[3]][[2]] <- a[[2]]
    eval(as(c(as.list(a[[3]]), drop = FALSE), "call"))
}
ieiwk.mpstat <- function(where = "", interval = 1, count = 1) {
    mpstat <- {
        a <- .sys.do(if (!missing(where)) 
            c("ssh", where), "mpstat -P ALL", interval, count, intern = 1)
        tail(a, -tail(which(a == ""), 1))
    }
    mpstat.tbl <- {
        a <- t(sapply(mpstat, function(x) {
            strsplit(x, "\\s+")[[1]]
        }))[, -1]
        rownames(a) <- c()
        b <- a[-1, -1]
        rownames(b) <- a[-1, 1]
        colnames(b) <- a[1, -1]
        b
    }
    mpstat.tbl
}
wk.xcs <- floor(parallel::detectCores()/2)
wk.mclapply <- ieiwk.mclapply <- local({
    require(parallel)
    mc.cores.max <- detectCores() - 1
    function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, mc.cleanup = TRUE, mc.cores = {
    }, mc.cores.factor = 0.9, mc.cores.win = 1, required.packages = {
    }) {
        if (!length(X)) 
            return()
        if (!length(mc.cores)) 
            mc.cores <- wk.xcs
        if (is.na(mc.cores.max)) 
            mc.cores.max <- 4
        if (osType() == "Windows") 
            mc.cores <- mc.cores.win
        mc.cores <- min(mc.cores, mc.cores.max, length(X), na.rm = TRUE)
        windows <- Sys.info()["sysname"] == "Windows"
        a <- {
            if (windows) {
                cl <- makeCluster(mc.cores)
                f1 <- function(x, fun, required.packages = {
                }, ...) {
                  if (length(required.packages)) {
                    for (p1 in required.packages) {
                      eval(call("require", p1))
                    }
                  }
                  fun(x, ...)
                }
                a <- try(parLapply(cl, X, f1, FUN, required.packages = required.packages, ...), silent = TRUE)
                stopCluster(cl)
                a
            }
            else {
                a <- mclapply(X, FUN, ..., mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores, mc.cleanup = mc.cleanup)
            }
        }
        if (is.null(names(X))) {
            if ((class(X) %in% c("character", "numeric", "integer")) && (!anyDuplicated(X))) 
                names(a) <- X
        }
        else {
            names(a) <- names(X)
        }
        a
    }
})
ieiwk.pvec <- function(v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE, mc.cleanup = TRUE) {
    mc.cores <- Ncpu()[1]
    if (mc.cores > length(v)) 
        mc.cores <- length(v)
    parallel::pvec(v, FUN, ..., mc.set.seed = mc.set.seed, mc.silent = mc.silent, mc.cores = mc.cores, mc.cleanup = mc.cleanup)
}
hs.methods <- function(obj) .sh(methods(class = class(obj)))
ieiwk.last.value <- function() .Last.value
ieiwk.get.singleElement <- function(o1, i = 1) {
    f1 <- if (class(o1) %in% "list") 
        "[["
    else "["
    do.call(f1, list(o1, i))
}
wk.attr.hs <- function(o, hs = head) {
    r <- hs(o)
    mz <- names(attributes(o))
    for (mz1 in mz) {
        attr(r, mz1) <- {
            if (length(attr(o, mz1)) == length(o)) {
                hs(attr(o, mz1))
            }
            else attr(o, mz1)
        }
    }
    r
}
.last.cmd <- function(n = 1, file = hs.home.expand("~/.Rhistory")) {
    .sys.do("tail -n", n, file, intern = 1)
    a <- capture.output(history())
    a <- savehistory(con)
}
ieiwk.clean.obj <- function(x) {
    lj <- c("na.action")
    for (a in lj) {
        attr(x, a) <- {
        }
    }
    x
}
os <- function(..., env = parent.frame(), units = "auto") {
    args <- list(...)
    if (!length(args)) 
        args <- sapply(ls(env, all.names = TRUE), as.symbol)
    a <- lapply(args, function(o) {
        eval(bquote({
            object.size(.(as.symbol(o)))
        }), env)
    })
    size <- sum %(% a
    class(size) <- class(a[[1]])
    format(size, units = units)
}
ll <- function(pos = 1, env = parent.frame()) {
    if (is.null(env)) 
        env <- as.environment(pos)
    a <- sapply(ls(env, all.names = TRUE), function(o) eval(call("object.size", as.symbol(o)), env = env))
    sapply(names(a)[order(a)], function(x) {
        cat(x, "\t")
        print(eval(call("object.size", as.symbol(x)), env = env), units = "auto")
    })
    a <- sum(a)
    class(a) <- "object_size"
    print(a, units = "auto")
}
rm.all <- function() {
    rm(list = ls(pos = 1), pos = 1)
}
rm.data <- function(but = "") {
    sapply(ls(pos = 1) %not% but, function(o) {
        eval(substitute(if (class(get(o)) != "function") rm(o), list(o = o)), env = .GlobalEnv)
    })
    cat(c())
}
rm.var <- function(but = c("@", "handy", "Somel2008Plos1", "Khaitovich2005Science", "Blekhman2009GenomeResearch", "Blekhman2009GenomeResearch_Khaitovich2005Science", ls(pat = "^\\d{4}\\.\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{2}[(\\.\\w)|(_\\d+\\.\\d+)]", env = parent.frame()))) {
    browser()
    sapply(ls(envir = env) %not% but, function(o) {
        eval(substitute(if (class(get(o)) != "function") rm(o), list(o = o)), env = env)
    })
    cat(c())
}
rm.var.4def <- function(but = c("."), env = parent.frame()) {
    sapply(ls(envir = env), function(o) {
        if (!eval(call("typeof", as.symbol(o)), env) %in% c("closure")) 
            eval(call("rm", as.symbol(o)), env)
    })
}
rm.function <- function(but = "") {
    sapply(ls(pos = 1) %not% but, function(o) {
        eval(substitute(if (class(get(o)) == "function") rm(o), list(o = o)), env = .GlobalEnv)
    })
    cat(c())
}
ls.var <- function() {
    obj.v <- ls(pos = 1)
    obj.nonFn <- {
        a <- sapply(obj.v, function(o) {
            eval(substitute(class(o), list(o = as.symbol(o))), envir = .GlobalEnv)
        })
        names(a[a != "function"])
    }
    obj.nonFn
}
listElementCounts <- function(l, recursive = 1) if (isList(l)) sum(sapply(l, listElementCounts, recursive)) else 1
listPrintLinesCounts <- function(l) {
    sum(rapply(l, function(x) {
        a <- dim(x)
        if (is.null(a)) {
            1
        } else {
            if (length(a) == 2) {
                nrow(x)
            }
        }
    }))
}
.at <- function(o, path = NULL, fn = NULL, as = "", ..., leaf = 0, v = !leaf, r = 0) {
    if (leaf) 
        return(ieiwk.attr(o, path, "identity", leaf = 0))
    if (is.null(fn)) 
        fn <- switch(as.character(length(path)), `0` = "names", ".str")
    if (is.numeric(path)) 
        path[1] <- names(attributes(o))[path[1]]
    cmd <- ifelse(length(path), sprintf("attr( o, %s)", shQuote(path[1])), "attributes( o)")
    cmd <- paste(cmd, sprintf("$%s", path[-1]), sep = "")
    cmd <- sprintf("do.call( \"%s\", list( %s quote( %s), ...))", fn[1], as, cmd)
    R <- eval(parse(text = cmd))
    for (fn.1 in fn[-1]) {
        R <- do.call(fn.1, list(R))
    }
    if (r) 
        return(o)
    if (v) {
        if (tail(fn, 1) != ".str") 
            return(R)
    }
}
.t <- function(p, actCode = 1, v = NULL, v.fun.env, to = "", sub = 1, env, self = 1, sep = "////") {
    if (missing(env)) 
        env <- parent.frame()
    p <- unlist(sapply(p, function(p) if (grepl(sep, p)) 
        strsplit(p, sep)[[1]]
    else p))
    fa <- a1 <- as.symbol(p[1])
    if (self) 
        p <- c(p, ".self.30.04.27")
    switch(as.character(actCode), `1` = {
        fa <- parse(text = paste(c(rep("attr(", length(p) - 1), p[1], sprintf(",\"%s\")", p[-1])), collapse = ""))[[1]]
        o <- eval(fa, env = env)
        switch(to, eval = eval(body(o), env = env), show = {
            src <- attr(o, "srcref")
            src <- capture.output(src)
            .ss("{")
            .ss(head(src[-1], -1))
            .ss("}")
        }, {
            if (!sub) {
                for (a in attributes(o)) {
                  attr(o, a) <- NULL
                }
            }
            o
        })
    }, `4` = {
        for (p1 in tail(p, -1)) {
            if (eval(call("is.null", fa), env = env)) eval(call("<-", fa, list()), env = env)
            fa <- call("attr", fa, p1)
        }
        eval(call("<-", fa, substitute(quote(v), list(v = v))), env = env)
        invisible()
    }, `3` = {
        p <- p[!p %in% ".self.30.04.27"]
        r <- NULL
        for (p1 in tail(p, -1)) {
            fa <- call("attr", fa, p1)
        }
        leaf <- eval(call("names", call("attributes", fa)), env) %not% ".self.30.04.27"
        if (length(leaf)) {
            r <- lapply(leaf, function(leaf) eval(call(".t", c(p, leaf), 3, env = env), env))
        }
        r <- r[!sapply(r, is.null)]
        if (eval(call("is.null", call("attr", fa, ".self.30.04.27")), env)) {
            as.list(r)
        } else {
            a <- list(p, r)
            as.list(unlist(rapply(a, paste, collapse = sep, "character", how = "list")))
        }
    }, `2` = {
    })
}
.tl <- function(p, actCode = 1, v = NULL, v.fun.env, to = "", sub = 1, env, self = TRUE, sep = "////", cat.p = 1, tree = list(), tree.form = 1) {
    if (missing(env)) 
        env <- parent.frame()
    p <- unlist(strsplit(p, sep))
    if (self) 
        p <- c(p, ".self.30.04.27")
    switch(paste(actCode), `1` = {
        fa <- ieiwk.access.list(p)
        o <- eval(fa, env = env)
        switch(to, eval = eval(body(o), env = env), show = {
            src <- attr(o, "srcref")
            src <- capture.output(src)
            .ss("{")
            .ss(head(src[-1], -1))
            .ss("}")
        }, {
            o
        })
    }, `4` = {
        fa <- ieiwk.access.list(p, "element")
        eval(bquote({
            .(fa) <- list()
            .(fa) <- .(v)
        }), env)
        fa <- ieiwk.access.list(head(p, -1))
        eval(bquote({
            if (length(.(fa)) == 0) .(fa) <- {
            }
        }), env)
        {
        }
        invisible()
    }, `3` = {
        p <- p[!p %in% ".self.30.04.27"]
        fa <- ieiwk.access.list(p)
        leaf <- eval(call("names", fa), env)
        r <- sort(unlist(c(if (".self.30.04.27" %in% leaf) paste(p, collapse = sep), sapply(leaf %not% ".self.30.04.27", function(leaf) eval(call(".tl", c(p, leaf), 3, cat.p = FALSE, env = env), env))), use.names = FALSE))
        if (cat.p) .ss(r)
        invisible(r)
    }, `2` = {
        switch(to, rm = {
            browser("31.04.24.101902")
            fa <- ieiwk.access.list(p)
        })
    })
}
.te <- function(p, actCode = 1, v = NULL, v.fun.env, to = "", sub = 1, env, self = TRUE, sep = "////", cat.p = FALSE, tree = list(), tree.form = 1) {
    if (missing(env)) 
        env <- parent.frame()
    p <- unlist(strsplit(p, sep))
    if (self) 
        p <- c(p, ".self.30.04.27")
    switch(paste(actCode), `1` = {
        fa <- ieiwk.access.list(p)
        o <- eval(fa, env = env)
        switch(to, eval = eval(body(o), env = env), show = {
            src <- attr(o, "srcref")
            src <- capture.output(src)
            .ss("{")
            .ss(head(src[-1], -1))
            .ss("}")
        }, {
            o
        })
    }, `4` = {
        fa <- ieiwk.access.list(p)
        eval(call("<-", fa, list()), env = env)
        eval(call("<-", fa, substitute(quote(v), list(v = v))), env = env)
        invisible()
    }, `3` = {
        p <- p[!p %in% ".self.30.04.27"]
        fa <- ieiwk.access.list(p)
        leaf <- eval(call("names", fa), env)
        r <- sort(unlist(c(if (".self.30.04.27" %in% leaf) paste(p, collapse = sep), sapply(leaf %not% ".self.30.04.27", function(leaf) eval(call(".tl", c(p, leaf), 3, cat.p = FALSE, env = env), env))), use.names = FALSE))
        if (cat.p) .ss(r)
        invisible(r)
    }, `2` = {
    })
}
lb <- function(e1 = "", n = 0, env = new.env(parent = parent.frame())) {
    while (n && {
        length(e1) > 1
    }) {
        eval(e1[[2]], env = env)
        e1[[2]] <- {
        }
        n <- n - 1
    }
    eval(bquote({
        browser()
        .(e1)
    }), env)
}
lb.env <- local({
    fn.30.04.22.143406 <- function(where) {
        env.root <- .GlobalEnv
        .env <- try(get(where, env.root), silent = 1)
        if (identical(class(.env), "try-error")) 
            assign(.env, new.env(), env = env.root)
        eval(quote(browser()), env = .env)
    }
    function(what = "lb", where = "lb.env.env", ...) {
        switch(what, lb = fn.30.04.22.143406(where), keep = {
            o = list(...)
            names(o) <- match.call(expand.dots = 0)$...
            sapply(names(o), function(x) assign(x, o[[x]], env = .GlobalEnv))
        }, ls = ls(environment(lb.env)[["aEnv"]]))
    }
})
lb.env.update <- function(act = 1) {
    switch(act, assign("aEnv.30.04.21", environment(lb.env)[["aEnv"]], env = .GlobalEnv), assign("aEnv", aEnv.30.04.21, env = environment(lb.env)))
}
bin.Rscript <- "/public/software/R-3.0.2/bin/Rscript"
.sys.rs <- function(..., bin = bin.Rscript, myFun = 0, trim = 0, mc.cores = 1, wait = 0, qsub = 0, jn = "testing", qsub.o.d = "$HOME/test/qsub", nodes = "1", ncpu = 1, dz = {
}, chk = 0, report = 1) {
    code <- {
        a <- ieiwk.mp.code2str.r(..., trim = trim)
        a <- c(sprintf("mc.cores <- %s", mc.cores), a)
        if (myFun) {
            b <- ieiwk.mp.code2str.r({
                source("~/calvin/code/R/function.common.R")
            }, trim = trim)
            a <- c(b, a, ".r()", "cat( Sys.time(), \"\n\", sep = \"\")")
        }
        shQuote(a)
    }
    code <- gsub("\\\\", "\\\\\\\\\\\\\\\\", code)
    if (qsub) 
        .sys.qsub(bin, "-e " %.% code, qsub.o.d = qsub.o.d, jn = jn, dz = dz, nodes = nodes, ncpu = ncpu, chk = chk, report = report)
    else {
        .sys.do(bin, "-e " %.% code, wait = wait, report = report)
    }
}
.sys.qsub.rsStdin <- function(..., bin = bin.Rscript, myFun = 1, trim = 0, jn = "testing", qsub.o.d = "$HOME/test/qsub", nodes = "1", ncpu = 1, dz = {
}, chk = 0, interactive = 0, report = 1) {
    code <- {
        a <- ieiwk.mp.code2str.r(..., trim = trim)
        if (myFun) {
            b <- ieiwk.mp.code2str.r(quote({
                source("~/calvin/code/R/function.common.R")
                ieiwk.cat.line(sprintf("\n========= Start at %s", Sys.time()))
            }), trim = trim)
            a <- c(b, a)
        }
        c(a, "cat( sprintf( \"========= end at %s\n\", Sys.time()))", ".r()")
    }
    cmd.all <- c(sprintf("%s - <<RscriptEOF", bin), code, "RscriptEOF") %,% "\n"
    cmd.all <- gsub("\\\\", "\\\\\\\\\\\\\\\\", cmd.all)
    .sys.qsub(cmd.all, qsub.o.d = qsub.o.d, jn = jn, dz = dz, nodes = nodes, ncpu = ncpu, chk = chk, interactive = interactive, report = report)
}
ieiwk.rsStdin <- function(..., bin = "/home/local/bin/R-3.0.2/bin/Rscript", myFun = 1, trim = 0, report = 1) {
    code <- {
        a <- ieiwk.mp.code2str.r(..., trim = trim)
        if (myFun) {
            b <- ieiwk.mp.code2str.r(quote({
                source("~/calvin/code/R/function.common.R")
            }), trim = trim)
            a <- c(b, a)
        }
        c(a, "cat( sprintf( \"end at %s\n\", Sys.time()))", ifelse(report, ".r()", {
        }))
    }
    cmd.all <- c(sprintf("%s - <<RscriptEOF", bin), code, "RscriptEOF") %,% "\n"
    ieiwk.cat.line(cmd.all)
    ieiwk.cat.line(sprintf("\nStart at %s", Sys.time()))
    .sys.do(cmd.all)
}
.sys.qsub.i <- function(nd = 1, ncpu = 1) {
    .sys.do(sprintf("qsub -I -l nodes=%s:ppn=%s -l walltime=%s:00:00", nd, ncpu, prod(24, 365)))
}
.sys.qsub.sleep <- function(nd = 1, ncpu = 1, howLong = prod(60, 60)) {
    .sys.qsub("sleep", howLong, nodes = sprintf("node%s", nd), ncpu = ncpu)
}
.sys.qsub <- function(..., jn = "testing", qsub.o.d = "$HOME/test/qsub", nodes = "1", ncpu = 1, dz = {
}, chk = 0, interactive = 0, report = 1) {
    cmd <- .sys(..., intern = 1)
    cmd.all <- c(sprintf("qsub %s- <<EOF", ifelse(interactive, "-I ", "")), sprintf("#PBS -N %s", jn), sprintf("#PBS -l nodes=%s:ppn=%s", nodes, ncpu), "#PBS -l walltime=1000:00:00", "#PBS -j oe", sprintf("#PBS -o %s", qsub.o.d), sprintf("#PBS -e %s", qsub.o.d), cmd, "EOF") %,% "\n"
    ieiwk.cat.line(cmd.all)
    if (chk) 
        return()
    a <- .sys.do(cmd.all, dz = dz, intern = 1)
    re("\\d+", a)
}
.sys.qstat <- function(dz = {
}) {
    .sys.do("qstat -n -1 -M -R", dz = dz)
}
.sys.qsub.chk <- function(job.id, d = "~/test/qsub", n = 20, dz = "sg") {
    f.size.limit <- 1024
    f.p <- grep(job.id, .sys.ls(d, dz = dz), v = 1)
    f.size <- .sys.stat.size(f.p, dz = dz)
    ieiwk.cat.line(sprintf("file size: %s", f.size))
    .sys.do(sprintf("head -%s", n), f.p, dz = dz, intern = 1)
}
.sys.qsub.chk.r <- function(job.id, dz = "sg") {
    .sys.do("myperl -w ~/bin/report.qsub.jobs.done.pl", job.id, dz = dz, wait = 0)
}
.rls <- function(f = "~/calvin/code/2010/07/23/2010.07.23.23.08.30.Fri.R") source(f)
jdi <- function(which = "hh", env = parent.frame()) {
    wj <- switch(which, nx3200 = "/home/share/nx3200/windows/wengkai/calvin/code/R/justDoIt.R", local = "F:/data.daily/code/justDoIt.R", local.cygwin = "", hh = "/gpfsDATA/home/calvin/wk/my/wd/dm/R/justDoIt.R")
    eval(call("source", wj, local = TRUE), env)
    {
    }
}
.sys.exit <- function() {
    .sys.do("~/apps/scripts/exit.sh")
}
ieiwk.favor.node <- "fat01"
ieiwk.wait.file <- function(f, m, ..., ti = 5, tl = 15) {
    repeat {
        if (missing(m)) {
            t1 <- max(file.info(f)[["mtime"]])
            if (any(is.na(t1))) {
                td <- tl - 1
            }
            else {
                t2 <- Sys.time()
                td <- as.integer(difftime(t2, t1, units = "secs"))
            }
            if (td > tl) 
                break
        }
        else {
            if (length(.sys.do("grep", "-e", shQuote(m), ..., .sys.fpGood(f), intern = 1))) 
                break
        }
        .sys.do("sleep", ti)
    }
}
ieiwk.wait.thread <- function(thread.str, until = c("quit", "cpu0"), co.pcpu = 5, node = Sys.info()["nodename"], wait = 7) {
    u1 <- match.arg(until)
    e1 <- switch(u1, cpu0 = {
        expression({
            pid <- re("\\d+", wdjc.search(thread.str, node, jg = "ps"))
            !jc.is.sleeping(pid)
        })
    }, quit = {
        expression({
            if (wdjc.search(thread.str, node)) {
                .sleep(wait)
                1
            } else 0
        })
    })
    while (eval(e1)) {
    }
}
cluster.all <- function() .sys.do("bhosts", "|gawk", shQuote("$4>3 { print $1}"), "| tail -n +2", intern = 1)
.sys.killall <- function(c1 = .hostname(), dd = "gzfy") {
    if (any(c1 %in% cluster.all())) {
        c1 <- c1 %and% cluster.all()
    }
    else {
        c1 <- switch(c1, here = .sys.do("hostname", intern = 1), all = .sys.do("bhosts", "|gawk", shQuote("$4>3 { print $1}"), "| tail -n +2", intern = 1))
    }
    switch(dd, gzfy = {
        u <- "calvin"
        lapply(c1, function(c1) {
            .sys.do("ssh", c1, "killall -u", u)
        })
    })
}
wdjc <- function(c1 = cluster.all(), o = paste(c("pid", "cmd"), collapse = ",")) {
    a <- ieiwk.mclapply(c1, function(c1.1) {
        u <- Sys.info()["user"]
        hn <- Sys.info()["nodename"]
        a <- trim(.sys.do(if (c1.1 != .hostname()) 
            c("ssh", c1.1, "\""), "ps", "-U", u, "-u", u, "-o", o, if (c1.1 != .hostname()) 
            "\"", intern = 1)[-1])
    }, mc.cores = length(c1))
    names(a) <- c1
    a
}
jc.is.sleeping <- function(pid, co = 5, time.interval = 3) {
    as.numeric(.sys.do("pidstat", "-p", pid, "-u", time.interval, 1, "| tail -n +4", "| gawk", .q("{ print( $6)}"), intern = 1)[1]) < co
}
wdjc.search <- function(p1, nodes = "log01", times = 1, wait = 3, fix = 1, jg = c("logical", "length", "ps")) {
    ps <- wdjc(nodes)
    p1.ps <- sapply(ps, function(ps1) {
        ieiwk.grep.all(p1, ps1, fix = fix, value = 1)
    })
    jg <- match.arg(jg)
    if (jg == "ps") 
        p1.ps
    else {
        p1.ps.length <- sapply(p1.ps, length)
        switch(jg, logical = any(p1.ps.length), length = p1.ps.length)
    }
}
disk.is.busy <- function(c1 = Sys.info()["nodename"], times = 4) {
    this <- Sys.info()["nodename"]
    wk.sapply(c1, function(c1.1) {
        n1 <- times
        any(sapply(seq(times), function(i) {
            a <- table(ieiwk.eval(quote(ps.state()), n1 = c1.1, email = 0))
            n1 <<- n1 - 1
            if (n1) .sleep(2)
            "D" %in% names(a)
        }))
    })
}
ps.state <- function() {
    a <- .sys("ps", "-eo pid,ppid", "|gawk", shQuote("BEGIN{OFS=\"\\t\"}{ print( $1,$2)}")) %=>% pipe %=>% read.table(header = TRUE)
    .sys.do("ps", "-p", paste(a[a[, "PPID"] != 2, "PID"], collapse = ","), "-o state", intern = 1)
}
disk.wait.not.busy <- function(c1 = "fat01", wait = 7) {
    while (disk.is.busy(c1)[[1]]) .sleep(wait)
    1
}
mem.is.enough <- function(c1, co = 50) {
    sapply(c1, function(c1) {
        m1 <- cluster.mem(c1)
        if (sum(m1[c("free", "cached")]) > co) 
            1
        else 0
    })
}
cluster.mem <- function(c1 = {
}) {
    m1 <- .wk(if (length(c1)) 
        c("ssh", c1), "cat /proc/meminfo", intern = 1)
    hs.re(m1, "\\d+")
    strsplit(m1, ":") %=>% {
        hs.c(sapply(x, . %=>% hs.sub(x[1], "^(?i)mem")), sapply(x, . %=>% hs.re(x[2], "\\d+") %=>% as.numeric)/(1024^2))
    }
}
give.me.a.node <- function(co.mem = 50, ps.pattern, ps.pattern.max = 1, wait = 3, wait.first = 0, verbose = 1, nodes2exclude = "", target.wj) {
    if (!missing(target.wj)) 
        if (all(file.exists(target.wj))) 
            return()
    if (wait.first) 
        .sleep(wait)
    repeat {
        cluster.all.mem <- {
            nodes2do <- setdiff(cluster.all(), nodes2exclude)
            a <- t(sapply(ieiwk.mclapply(nodes2do, cluster.mem), c))
            rownames(a) <- nodes2do
            a[rev(order(apply(a[, c("free", "cached")], 1, sum))), ]
        }
        houxuan <- clg(apply(cluster.all.mem[, c("free", "cached")], 1, sum) > co.mem, 1)
        houxuan <- setdiff(houxuan, nodes2exclude)
        for (h1 in houxuan) {
            if (unlist(disk.is.busy(h1))) {
                if (verbose) 
                  cat(paste("Disk is busy for", h1), sep = "\n")
                next
            }
            if (!missing(ps.pattern) && wdjc.search(ps.pattern, h1, jg = "length") >= ps.pattern.max) 
                next
            return(h1)
        }
        if (wait > 0) {
            .sleep(wait)
        }
        else break
    }
}
node.wait.available <- function(c1 = Sys.info()[["nodename"]], wait = sample(seq(3, 100), 1), mem.co = switch(c1, fat01 = 50, 10)) {
    repeat {
        if (disk.wait.not.busy(c1, wait) && mem.is.enough(c1, mem.co)) {
            return(1)
        }
    }
}
ieiwk.vt <- function(vcf.wj) {
}
.gemini <- function(cmd = c("query", "load", "db_info"), db, q, load, v, p, t = "VEP", cores = 4, skip.gene.tables = 1, header = 1) {
    switch(match.arg(cmd), query = .sys.do("gemini", "query", "-q", .q(q), if (header) "--header", db), load = .sys.do("gemini", "load", "-v", v, "-p", p, "-t", t, "--cores", cores, if (skip.gene.tables) "--skip-gene-tables", db), db_info = .sys.do("gemini", cmd, db))
}
.samtools <- function(f.bam, cmd = "view", col = 0, less = 1, pipe = 0) {
    .sys("samtools", cmd, f.bam, "| gawk", .q(sprintf("{ print %s}", paste(sprintf("$%s", col), collapse = ","))), if (less && !pipe) 
        "| less -n", pipe = pipe, chk = 0)
}
.blat <- function(base = "~/prj/pub/genomes/hg19.fa", qry, f.out = hs.wjm(qry, ext = "blatOut"), stepSize = 5, repMatch = 2253, minScore = 0, minIdentity = 0, out = "psl", c1 = ieiwk.favor.node, bg = 0) {
    env1 <- ieiwk.env.here()
    o <- sapply(c("stepSize", "repMatch", "minScore", "minIdentity", "out"), function(o1) {
        sprintf("-%s=%s", o1, get(o1, env = env1))
    })
    ieiwk.eval(bquote({
        .sys.do("blat", .(base), .(qry), .(f.out), .(o))
    }), c1, bg = bg)
    f.out
}
.bwa <- function(qry, idxbase = "~/prj/pub/genomes/hg19", k = 48, nt = 10, d.out = dirname(qry[1]), f.out = ieiwk.filename(qry[1], dn = 0, ext = "bam"), c1 = ieiwk.favor.node) {
    f.out <- ieiwk.file.path(d.out, f.out)
    ieiwk.eval(bquote({
        .sys.do("samtools view -b", "<(", "bwa mem", "-k", .(k), "-t", nt, .(idxbase), .(qry), ")", "-o", .(f.out))
    }), c1)
    f.out
}
.bwa.index <- function(fa, p = {
}, a = c("bwtsw", "is"), n1 = ieiwk.favor.node, fa.fifo.name = {
}) {
    ieiwk.eval(bquote({
        fa <- .(fa)
        a <- .(match.arg(a))
        p <- .(p)
        if (length(p)) 
            p <- ieiwk.file.path(p)
        if (length(fa) == 1) {
            if (!length(p)) 
                p <- ieiwk.file.path(ieiwk.filename(fa))
            .sys.do("bwa index", "-a", a, "-p", p, fa)
        }
        else if (length(fa) > 1) {
            fa.fifo.name <- .(fa.fifo.name)
            if (!length(fa.fifo.name)) 
                fa.fifo.name <- ieiwk.filename(p, add = "fa")
            if (file.exists(fa.fifo.name)) 
                file.remove(fa.fifo.name)
            .sys.do("mkfifo", .sys.fpGood(fa.fifo.name))
            .sys.do("cat", fa, ">", fa.fifo.name, "&")
            .sys.do("bwa index", "-a", a, "-p", p, fa.fifo.name, "&")
            browser("32.07.21.143544")
            .sleep(11)
            jc.ms <- c("bwa", "index", p)
            ieiwk.wait.thread(jc.ms, until = "cpu0")
            wdjc.search(c("bwa", "index", p), ieiwk.favor.node, jg = "ps")
            .sys.do("cat", fa, ">", fa.fifo.name)
            ieiwk.wait.thread(jc.ms, until = "quit")
        }
    }), n1)
}
.sys.bam.chk <- function(fp, nl = 10, dz = {
}) {
    bin <- "~/apps/samtools/samtools view"
    if (length(dz)) {
        .sys.ssh(bin, fp, "| head", sprintf("-%s", nl))
    }
    else {
        .sys.do(bin, fp, "| head", sprintf("-%s", nl))
    }
}
ieiwk.index.bam <- function(f.bam, f.picard = "~/rj/picard-tools-2.1.0/picard.jar") {
    .sys.do("java -jar", f.picard, "BuildBamIndex", "I=", f.bam, log = ieiwk.filename(f.bam, ext = "bai", add = "log"))
}
ieiwk.index.fa <- function(f.fa, f.picard = "~/rj/picard-tools-2.1.0/picard.jar") {
    f.out <- ieiwk.filename(f.fa, ext = "dict")
    .sys.do("java -jar", f.picard, "CreateSequenceDictionary", "R=", f.fa, "O=", f.out, log = ieiwk.filename(f.out, add = "log"))
}
bi.qse <- function(qs) {
    as.numeric(charToRaw(qs)) - 33
}
bin.samtools <- "~/apps/samtools/samtools"
bin.bwa <- "~/apps/bwa-0.7.5a/bwa"
bin.java <- "/public/software/jdk1.7.0_45/bin/java"
bin.MarkDuplicates <- "~/apps/picard-tools-1.102/MarkDuplicates.jar"
bin.intersectBed <- "/public/software/bedtools/bin/intersectBed"
bin.ValidateSamFile <- "~/apps/picard-tools-1.102/ValidateSamFile.jar"
fn.x.left.30.03.26.162558 <- function(v, m, tolerance, plain.width.min, plain.fluc.overall, adjacent) {
    i <- seq(m)
    j <- which(diff((diff(v[i]) + tolerance) >= 0) == 1)
    if (length(j)) {
        x1 <- i[tail(j, 1) + 2]
        if (abs(v[x1 + plain.width.min] - v[x1]) < plain.fluc.overall) {
            x1 <- x1 + plain.width.min
        }
        else {
            j2 <- seq(max(1, x1 - adjacent), x1 - 1)
            m2 <- j2[which.max(v[j2])]
            if (v[m2] - v[m2 - 1] > tolerance) 
                x1 <- fn.x.left.30.03.26.162558(v, m2, tolerance, plain.width.min, plain.fluc.overall, adjacent)
        }
    }
    else x1 <- 1
    x1
}
fn.x.right.30.03.26.162646 <- function(v, m, tolerance, plain.width.min, plain.fluc.overall, adjacent) {
    i <- seq(m, length(v))
    j <- which(diff((diff(v[i]) - tolerance) <= 0) == -1)
    if (length(j)) {
        x2 <- i[j[1] + 1]
        if (abs(v[x2] - v[x2 - plain.width.min]) < plain.fluc.overall) {
            x2 - plain.width.min
        }
        else {
            j2 <- seq(x2 + 1, min(length(v), x2 + adjacent))
            m2 <- j2[which.max(v[j2])]
            if (v[m2] - v[m2 + 1] > tolerance) 
                x2 <- fn.x.right.30.03.26.162646(v, m2, tolerance, plain.width.min, plain.fluc.overall, adjacent)
        }
    }
    else x2 <- length(v)
    x2
}
ieiwk.peak2valley <- function(v, to = min(v), adjacent = 15, x1x2 = NULL) {
    tolerance <- (function(v, extra = 4) {
        fluc <- diff(v)
        i <- sapply(seq(1, length(fluc) - extra), function(i) {
            i.v <- seq(i, i + extra)
            if (length(unique(sign(fluc[i.v]))) > 1) 
                i.v
        })
        r <- abs(fluc[unique(unlist(i))])
        quantile(r, 0.95)
    })(v)
    plain.width.min <- 50
    plain.fluc.overall <- tolerance * 10
    m <- which.max(v)
    if (m < 3) 
        x1 <- 1
    else {
        x1 <- fn.x.left.30.03.26.162558(v, m, tolerance, plain.width.min, plain.fluc.overall, adjacent)
    }
    x2 <- fn.x.right.30.03.26.162646(v, m, tolerance, plain.width.min, plain.fluc.overall, adjacent)
    v[x1:x2] <- to
    v
}
fastq_EndType <- function(f1) {
    has3OrMoreReads <- {
        n <- 3
        nl <- n * 4
        as.integer(.sys.do("head -n", formatSafe(nl), f1, "| wc -l", intern = 1)) >= nl
    }
    r1 <- strsplit(readLines(f1, 1), "\\s+")[[1]]
    r2 <- strsplit(readLines(f1, 5)[5], "\\s+")[[1]]
    if ({
        r1[[1]] == r2[[1]]
    } && {
        substr(r1[[2]], 1, 2) == "1:"
    } && {
        substr(r2[[2]], 1, 2) == "2:"
    }) 
        return("interleavedPairedEnd")
    rLast <- strsplit(.sys.do("tail -n 4", f1, intern = 1)[1], "\\s+")[[1]]
    if (length(grep("^2\\:", rLast[[2]])) && has3OrMoreReads) 
        return("catPairedEnd")
}
fastq_read_HowMany <- function(f1, want = c("read", "line")) {
    switch(fastq_EndType(f1), catPairedEnd = {
        r1 <- .sys.do("bioawk", "-c fastx", shQuote("{print $1}"), f1, "| head -n 1", intern = 1)
        r <- as.integer(.sys.do("bioawk -c fastx", .q(paste(c("BEGIN{ RN =0 } { if( $1==", shQuote(r1, "cmd"), ")", "RN = RN + 1} { if( RN == 2) { print NR; exit}}"), collapse = " ")), f1, intern = 1)) - 1
        switch(match.arg(want), read = r, line = r * 8)
    }, -1)
}
ieiwk.make.hs <- function(f1, f2.base = "hs.RData", save2file = 0, load = 1, load2env = parent.frame()) {
    source(f1, local = TRUE)
    obj <- ls()
    fl <- obj[sapply(obj, function(x) is.function(get(x)))]
    if (save2file) {
        save(list = fl, file = file.path(dirname(f1), f2.base))
    }
    if (load) {
        ieiwk.sapply(fl, function(f1) {
            get(f1) %=>% assign(f1, x, load2env)
        })
    }
    invisible()
}
ieiwk.make.hs.dir <- function(d1, f2.base = "hs.RData", unique = 1) {
}
.si <- function(file = ".RData", version = NULL, ascii = FALSE, compress = !ascii, compression_level = TRUE, safe = TRUE) {
    if (!is.character(file) || file == "") 
        stop("'file' must be non-empty string")
    opts <- getOption("save.image.defaults")
    if (is.null(opts)) 
        opts <- getOption("save.defaults")
    if (missing(safe) && !is.null(opts$safe)) 
        safe <- opts$safe
    if (missing(ascii) && !is.null(opts$ascii)) 
        ascii <- opts$ascii
    if (missing(compress) && !is.null(opts$compress)) 
        compress <- opts$compress
    if (missing(version)) 
        version <- opts$version
    if (safe) {
        outfile <- paste(file, "Tmp", sep = "")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste(file, "Tmp", i, sep = "")
        }
    }
    else outfile <- file
    on.exit(file.remove(outfile))
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = outfile, version = version, ascii = ascii, compress = compress, compression_level = compression_level, envir = .GlobalEnv, precheck = FALSE)
    if (safe) 
        if (!file.rename(outfile, file)) {
            on.exit()
            stop("image could not be renamed and is left in ", outfile)
        }
    on.exit()
}
ieiwk.password_random.generator <- function(n = 32, set = 1) {
    candidates <- list(letters = list(lower = letters, upper = toupper(letters)), numbers = seq(0, 9), others = list(normal = c("_", ".", "-"), wild = c("~", "`", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "+", "=", "\\", "|", "[", "]", "{", "}", "'", "\"", ":", ";", ",", "<", ">")))
    set <- switch(paste(set), `1` = unlist(candidates), `2` = unlist(c(candidates[["letters"]], candidates[["numbers"]], candidates[["others"]][["normal"]])), `3` = unlist(c(candidates[["letters"]], candidates[["numbers"]])))
    sample(set, n) %,% ""
}
ieiwk.fp.par <- function(..., sep = ";") {
    a <- list(...)
    for (v in names(a)) {
        assign(v, a[[v]], envir = parent.frame())
    }
    paste(sprintf("%s=%s", names(a), a), collapse = sep)
}
random.password.generator <- function(n, set = 1) {
    switch(paste(set), `1` = unlist(c(letters, toupper(letters), seq(0, 9), strsplit("~!@#$%^&*()_-+-=<>[]{}|<>,./?;\\", ""))))
    paste(sample(unlist(c(letters, toupper(letters), seq(0, 9), strsplit("~!@#$%^&*()_-+-=<>[]{}|<>,./?;\\", ""))), n), collapse = "")
}
not <- get("!")
wk.zim.find <- function(s1, where = "name", nb = "系统") {
    if (where == "name") 
        s1 <- str_c("*", s1, "*")
    s1 <- if (nchar(where)) 
        str_c(where, ":", s1)
    .wk("zim", "--search", nb, .q(s1), intern = 1) %=>% {
        if (length(x)) 
            str_c(":", x) %=>% .ss
    }
}
ieiwk.zim.31.12.27.100406 <- function(what = tp, to = "ss", ss.exact = FALSE, ss.field = c("name", "tag", "LinksTo")) {
    ss.field <- match.arg(ss.field)
    switch(to, ss = {
        .sys.do("zim --search", "/home/wk/wk/my/wd/db/zim/zb", sprintf("%s:%s", ss.field, sprintf(ifelse(ss.exact, "%s", "*%s*"), what)), intern = 1)
    }, switch(what, tp = {
        t1 <- Sys.time()
        sprintf(":%s:%s", as.integer(format(t1, "%Y")) - 1984, format(t1, "%m:%d:"))
    }))
}
wk.mlocate <- function(update = 0, pat = readline(), ignore.case = 1, regex = 1, db.wjj = hs.wjj(hs.home.expand("~/.db"), "mlocate"), wjj2do = c(hs.home.expand("~/wk")) %=>% x[dir.exists(x)], intern = 0) {
    db.wj <- wk.sapply(wjj2do, function(wjj1) {
        hs.wj(db.wjj, ifelse(wjj1 == "~", "home", basename(wjj1)))
    })
    1
    {
        if (0) 
            for (wjj1 in names(db.wj)) {
                t1 <- file.mtime(db.wj[[wjj1]])
                if (is.na(t1) || (difftime(Sys.time(), t1, units = "mins") > 100)) {
                  update <- 1
                  break
                }
            }
        if (update) {
            for (wjj1 in names(db.wj)) {
                hs.msg(paste("[locate更新]", wjj1))
                .wk("updatedb -l 0 -o", .sys.fpGood(db.wj[[wjj1]]), "-U", .sys.fpGood(wjj1))
            }
            hs.msg("完成", 0)
            return()
        }
    }
    1
    {
        r1 <- .wk("locate", "-b", "-e", if (ignore.case) 
            "-i", if (regex) 
            "--regex", sapply(db.wj, function(x) c("-d", x)), .q(pat), "2>/dev/null | sort | uniq", intern = 1)
        n1 <- 20
        cat(head(r1, n1), sep = "\n")
        if (length(r1) > n1) {
            hs.browser("34.11.10.225103")
        }
    }
}
.find <- function(ext) {
    if (!missing(ext)) 
        sprintf("*%s", ext)
}
wk.head <- function(x, n = 10) {
    a <- readLines(x, n)
    cat(a, sep = "\n")
}
ieiwk.deComment.codeFile.R <- function(wj1) {
    wj1.nr <- parse(wj1)
    a <- unlist(lapply(wj1.nr, function(x) {
        capture.output(bquote(.(x)))
    }))
    .ss(a, file = wj1)
}
ieiwk.deComment.codeFile.awk <- function(wj1) {
    nr <- readLines(wj1)
    p1 <- " *#ieiwk.*"
    lines2do <- which(grepl(p1, nr))
    if (length(lines2do)) {
        nr[lines2do] <- sub(p1, "", nr[lines2do])
    }
    .ss(nr, file = wj1)
}
ieiwk.deComment.codeFiles <- function(wjj1 = "/gpfsDATA/home/calvin/wk/my/wd/dm/R") {
    {
        wj <- ieiwk.list.files(wjj1, "(?i).*\\.r$", recursive = 1)
        sapply(wj, ieiwk.deComment.codeFile.R)
    }
    {
        wj <- ieiwk.list.files(wjj1, "(?i).*\\.(awk|pl|py)$", recursive = 1)
        sapply(wj, ieiwk.deComment.codeFile.awk)
    }
}
ieiwk.append <- function(x, values, after = length(x), replace = 0, use.what = ifelse(length(names(x)), "names", "identity")) {
    y <- use.what %(% list(x)
    if (replace) {
        i1 <- na.omit(match(values, y))
        if (length(i1)) {
            y <- y[-i1]
            x <- x[-i1]
        }
    }
    if (is.character(after)) 
        after <- match(after, y)
    r <- append(x, values, after)
    r
}
ieiwk.IRanges <- function(start = NULL, end = NULL, width = NULL, names = NULL) {
    if (!require(IRanges)) 
        stop("install IRanges first")
    IRanges(start = start, end = end, width = width, names = names)
}
ieiwk.GRanges <- function(seqnames = Rle(), ranges = NULL, strand = NULL, ..., seqlengths = NULL, seqinfo = NULL, start = NULL, end = NULL, width = NULL) {
    if (!require(GenomicRanges)) 
        stop("install GenomicRanges first")
    if (is.null(ranges)) 
        ranges <- ieiwk.IRanges(start, end, width)
    GRanges(seqnames = seqnames, ranges = ranges, strand = strand, ..., seqlengths = seqlengths, seqinfo = seqinfo)
}
wk.browser <- function(text = "", condition = {
}, expr = TRUE, skipCalls = 0L, t1, env = parent.frame()) {
    eval(bquote({
        text <- .(text)
        .r(str_interp("browser at ${ text} at ${ date()} on ${ .hostname()}"), title.postfix = "")
        browser(text = .(text), condition = .(condition), expr = .(expr), skipCalls = .(skipCalls))
    }), envir = env)
}
ieiwk.pubmed <- function(kw = {
}, high.if = 1, year.start = 2006, year.end = 3000) {
    browser("33.03.03.171028")
    r <- paste(c(if (length(kw)) paste("(", paste(sprintf("\"%s\"[Title/Abstract]", kw), collapse = "OR"), ")", sep = ""), if (high.if) "(\"0007-9235\"[Journal] OR \"0028-4793\"[Journal] OR \"0732-0582\"[Journal] OR \"0034-6861\"[Journal] OR \"0009-2665\"[Journal] OR \"1471-0072\"[Journal] OR \"0140-6736\"[Journal] OR \"1471-0056\"[Journal] OR \"1474-175X\"[Journal] OR \"0001-8732\"[Journal] OR \"0028-0836\"[Journal] OR \"1061-4036\"[Journal] OR \"0066-4154\"[Journal] OR \"1474-1733\"[Journal] OR \"1476-1122\"[Journal] OR \"0092-8674\"[Journal] OR \"1301-8361\"[Journal] OR \"0036-8075\"[Journal] OR \"1471-003X\"[Journal] OR \"0098-7484\"[Journal] OR \"1749-4885\"[Journal] OR \"1474-1776\"[Journal] OR \"0306-0012\"[Journal] OR \"1748-3387\"[Journal] OR \"0031-9333\"[Journal] OR \"1535-6108\"[Journal] OR \"0066-4146\"[Journal] OR \"1529-2908\"[Journal] OR \"1543-5008\"[Journal] OR \"0147-006X\"[Journal] OR \"1934-5909\"[Journal] OR \"0079-6700\"[Journal] OR \"1474-4422\"[Journal] OR \"1087-0156\"[Journal] OR \"1470-2045\"[Journal] OR \"1078-8956\"[Journal] OR \"0066-4197\"[Journal] OR \"0001-4842\"[Journal] OR \"0362-1642\"[Journal] OR \"1074-7613\"[Journal] OR \"1740-1526\"[Journal] OR \"0066-4278\"[Journal] OR \"1755-4330\"[Journal] OR \"0370-1573\"[Journal] OR \"0031-6997\"[Journal] OR \"1553-4006\"[Journal] OR \"0027-8424\"[Journal] OR \"0006-4971\"[Journal] OR \"1063-5157\"[Journal] OR \"0021-9525\"[Journal] OR \"0002-9297\"[Journal] OR \"1544-9173\"[Journal] OR \"0890-9369\"[Journal] OR \"0962-8924\"[Journal] OR \"0955-0674\"[Journal] OR \"1088-9051\"[Journal] OR \"1534-5807\"[Journal] OR \"1097-2765\"[Journal] OR \"0896-6273\"[Journal] OR \"0009-7322\"[Journal] OR \"1097-6256\"[Journal] OR \"1549-1277\"[Journal] OR \"1465-7392\"[Journal] OR \"0168-9525\"[Journal] OR \"0960-9822\"[Journal] OR \"0261-4189\"[Journal] OR \"1474-7596\"[Journal] OR \"1553-7390\"[Journal] OR \"1744-4292\"[Journal] OR \"0028-3878\"[Journal] OR \"1001-0602\"[Journal] OR \"0305-1048\"[Journal])", 
        sprintf("(\"%s/01/01\"[Date - Publication] : \"%s\"[Date - Publication])", year.start, year.end)), collapse = " AND ")
    .ss(r)
}
.wk.baloosearch <- function(q1, d = {
}) {
    .wk("baloosearch", if (length(d)) 
        c("-d", d), q1, "| uniq", "|", .sys.less())
}
.wk.open <- function(wj1, bin = "nautilus") {
    .wk(bin, wk.wjlj(wj1), "&")
}
.wk.open.pdf <- function(wj1, bin = "PDFXEdit") {
    if (bin == "PDFXEdit") {
        wj1 <- path.expand(wj1)
        if (!wk.re(wj1, "^/", "detect")) 
            wj1 <- wk.wj(getwd(), wj1)
        wj1 <- str_c("Z:", wj1)
        wj1 <- wk.re(wj1, "/", "replace", replacement = "\\", all = 1)
    }
    bin <- switch(bin, PDFXEdit = "~/rj/exe/PDFXEdit6_Portable/PDFXEdit.exe", FoxitReader = "/home/wk/rj/foxitreader/FoxitReader.sh")
    browser("33.06.16.181001")
    .sys("wine", bin, wj1)
}
.wk.wjj.db <- function(uid1, open = 1) {
    wjj1 <- path.expand(wk.wjj(.mfp(uid1, "~/wk/my/wd/db")))
    if (open) 
        .wk.open(wjj1)
    else .ss(path.expand(wk.wjj(.mfp(uid1, "~/wk/my/wd/db"))))
}
wk.is.a.local.machine <- function(hn1 = Sys.info()["nodename"]) {
    hn1 %in% c("wk-Veriton-D630")
}
wk.remote.setup <- function() {
    wj.jg.33.06.19.154855 <- "~/R.communication.txt"
    port <- 6011L
    op <- options(timeout = 99999)
    on.exit(options(op), add = TRUE)
    repeat {
        con1 <- socketConnection(port = port, blocking = TRUE, open = "rb", server = TRUE)
        r <- .try(eval(parse(con1, encoding = "UTF-8")))
        close(con1)
        .cat.time()
        if (0) {
            .wk("sleep .5")
            con1 <- socketConnection("log01", port = port)
            writeLines(capture.output(r), con1)
            close(con1)
        }
        .ss(head(capture.output(r), 99), file = wj.jg.33.06.19.154855, append = FALSE)
    }
}
wk.remote <- function(e1) {
    e1 <- substitute(e1)
    wj.jg.33.06.19.154855 <- "~/R.communication.txt"
    wj.comm <- ieiwk.tempfile()
    on.exit(unlink(wj.comm), add = TRUE)
    .ss(head(trim(capture.output(bquote({
        con1 <- socketConnection("fat01", port = .(port))
        writeLines(capture.output(quote({
            .(e1)
        })), con1)
        close(con1)
    }))), -1)[-1], file = wj.comm)
    wk.rsync(wj.comm, wj.comm, ssh = wk.ssh, ssh.side = 2)
    .wk("ssh", "-p", "7050", "calvin@113.108.182.54", "R --no-save --slave -e", .q(.q(.sys("source(", .q(wj.comm), ")"))), wait = 1)
    wk.rsync(wj.jg.33.06.19.154855, dirname(wj.jg.33.06.19.154855), ssh = wk.ssh, ssh.side = 1)
    .ss(readLines("~/R.communication.txt"))
}
wk.filter <- function(x, f) {
    x[wk.sapply(x, f) %=>% unlist]
}
wk.axel <- function(url1, wjj2 = ".", shm = file.exists("/dev/shm") && (!hs.grepl(url1, "(?i)\\.([xg]z|zip|tgz|bgz|rar)$"))) {
    if (0) {
        wj1 <- "/home/wk/swxx/sj/xm/hy/gtf2f1/sh/rna-seq/ENCFF401ZXD.fastq.gz"
        t1 <- file.mtime(wj1)
        repeat {
            .wk("sleep 30")
            t2 <- file.mtime(wj1)
            if (difftime(t2, t1, units = "secs") > 15) 
                0
        }
    }
    wj2 <- hs.wjm(url1, dn = wjj2, add = if (shm) 
        "gz") %=>% normalizePath
    wj1.st <- paste0(wj2, ".st")
    if (file.exists(wj2) && (!file.exists(wj1.st))) 
        return(wj2)
    wjj2_tmp <- if (shm) {
        hs.wjj(hs.wj.temp())
    }
    else wjj2
    if (0) 
        .wk("df", "/dev/shm", intern = 1)[2] %=>% strsplit("\\s+") %=>% unlist %=>% x[4] %=>% as.integer
    wj2_tmp <- hs.with_wd(wjj2_tmp, {
        wj1 <- basename(url1)
        cmd1 <- .sys("axel -ca -T 2", .q(url1))
        .wk(cmd1)
        N <- 0
        while (file.exists(wj1.st) && (N < 1000)) {
            message("retrying...")
            .wk(cmd1)
            N <- N + 1
        }
        normalizePath(wj1)
    })
    if (shm) {
        .wk("cat", wj2_tmp, "|", wk.gz.good(), ">", .sys.fpGood(wj2))
        on.exit(hs.wj.rm(wj2_tmp), add = TRUE)
    }
    else wj2 <- wj2_tmp
    wj2
}
wk.wget <- function(url1, wjj2 = ".", wjm = basename(url1), timestamping = 0, touch = 0, T = 10, log = 0, gz = !hs.grepl(wjm, "(?i)\\.(t?gz|zip|xz|7z|rar|feather)$")) {
    hs.with_wd(hs.wjj(wjj2), {
        if (0) {
            wjjTemp <- tempfile("wget.", tmpdir = ".") %=>% hs.wjj
            on.exit(hs.rm.wj(wjjTemp, 1), add = TRUE)
            wjLog <- tempfile("log", tmpdir = wjjTemp)
        }
        if (log) {
            wjLog <- tempfile("log_wget.", tmpdir = ".")
        }
        if (0) 
            .wk("wget", "-c", "--random-wait", hs.url_good(url1), "-o", wjLog)
        if (gz) {
            if (!hs.grepl(wjm, "\\.gz$")) {
                wjm %<=>% paste0(".gz")
                if (file.exists(wjm)) 
                  return(normalizePath(wjm))
            }
            wjm <- c("- | gzip >", .q(wjm))
        }
        else {
            if (file.exists(wjm)) 
                return(hs.wj.portable(wjm))
        }
        cmd1 <- .sys(hs.home.expand("~/rj/miniconda3/bin/wget") %=>% {
            if (file.exists(x)) 
                x
            else "wget"
        }, if (!gz) 
            "-c", if (timestamping && (!gz)) 
            "--timestamping", "-T", T, "-t 9999", if (log) 
            c("-o", wjLog), .q(url1), if (length(wjm)) 
            c("-O", wjm))
        .wk(cmd1)
        if (touch) 
            .wk("touch", .q(wjm))
        if (log) {
            info <- tail(readLines(wjLog) %not% "", 1)
            return(hs.wj(wjj2, hs.sub.capture(info, "File ‘(.+)’ already there")))
        }
        return(hs.wj.portable(wjm))
        if (0) {
            Log <- wjLog %=>% readLines
            if (hs.grepl(tail(tail(Log) %not% "", 1), "saved")) {
                wjGot <- normalizePath(hs.listWj(wjjTemp, recursive = FALSE)) %not% normalizePath(wjLog)
                if (length(wjGot)) {
                  file.rename(wjGot, wjj2)
                }
            }
            else {
                hs.browser("35.08.24.182324")
            }
        }
    })
}
wk.curl <- function(dz1, wjj2) {
    hs.with_wd(hs.wjj(wjj2), {
        .wk("curl", "-C -", "-R", "-J", "-O", dz1)
    })
}
R_session.location.hs <- function() {
    a1 <- .wk("ifconfig", intern = 1)
    a1 <- hs.sepSeq(a1, sep.i = which(switch(Sys.info()["sysname"], Linux = {
        a1 == ""
    }, Darwin = {
        hs.grepl(a1, "^[^\\t]")
    })))
    a1 %<=>% {
        lapply(x, "%not%", "") %=>% hs.nonempty
    }
    ip <- unlist(sapply(a1, . %=>% hs.re("\\d+\\.\\d+\\.\\d+\\.\\d+")))
    if ("192.168.10.237" %in% ip) {
        "fwq"
    }
    else if ("172.16.28.105" %in% ip) {
        "hp1"
    }
    else if (length(hs.grep(ip, "^192\\.115\\."))) {
        "a409"
    }
    else if ("10.80.0.29" %in% ip) {
        "cpu14"
    }
    else if ("192.168.0.108" %in% ip) {
        "r820"
    }
    else if ("192.168.0.100" %in% ip) {
        ""
    }
    else ""
}
R_session.location <- R_session.location.hs()
wk_node_remote <- c(R_session.location.hs())
ieiwk.ssh.db <- local({
    if (0) {
        dz.db <- c(sg = "172.18.202.8", hp = "172.18.202.7", g1 = "gate1.picb.ac.cn", g2 = "gate2.picb.ac.cn", hh = "113.108.182.54", gsp = "172.21.109.131")
        p.db <- c(sg = 53641, hp = 53641, g1 = 22, gsp = 22, g2 = 22, hh = 7050)
        u.db <- c(sg = "wengkai", hp = "wengkai", g1 = "ieiwk", g2 = "ieiwk", hh = "calvin", gsp = "wk")
        h.db <- c(hh = "/gpfsDATA/home")
    }
    {
        if (!file.exists(hs.home.expand("~/.ssh/config"))) 
            return()
        ssh.config <- readLines(hs.home.expand("~/.ssh/config"))
        ssh.config <- hs.sepSeq(ssh.config, sep.p = "^Host\\s")
        names(ssh.config) <- sapply(ssh.config, function(x1) {
            hs.re(x1[1], "(?<=\\s)[^\\s]+")
        })
        ssh.config["apt"]
        ssh.ip <- function(ssh1) {
            hs.sub(hs.grepV(ssh.config[[ssh1]], "^\\s*HostName\\s*"), "^\\s*HostName\\s*")
        }
    }
    db1 <- list(jz = c("calvin", "192.115.110.254"), cpu14 = c("calvin", "cpu14"), a409 = c("wk", switch(R_session.location, jz = , cpu14 = , cpu13 = , a409 = "192.115.103.196", "172.16.3.62")), ucdom = c("ucdom", "s2.natfrp.org", "48875"), seq1 = c("wk", "172.46.114.134"), r820 = c("wk", "r820"), hp1 = c("wk", "hp1"), apt = c("root", "apt"), n9 = c("kweng", "n9"), n8 = c("kweng", "n8"), hh = c("calvin", "113.108.182.54", "7050"))
    function(dz, p = 22, u = "") {
        if (dz %in% names(db1)) {
            db1 <- db1[[dz]]
            u <- db1[1]
            dz <- db1[2]
            p <- db1[3]
            if (is.na(p)) 
                p <- "22"
        }
        a <- c(dz, p, u)
        names(a) <- c("dz", "port", "user")
        a
    }
})
wk.match_partial <- local({
    hs.require("stringdist")
    hs1 <- function(s1, dot2_ = 1) {
        s1 <- tolower(s1)
        if (dot2_) 
            s1 %<=>% hs.gsub("_ \\.", "_")
        s1
    }
    function(set1, set2, dot2_ = 1) {
        hs2 <- stringdist::stringsim
        set1_new <- hs1(set1, dot2_ = dot2_)
        set2_new <- hs1(set2, dot2_ = dot2_)
        nm <- set1_new %not% set2_new
        jg <- lapply(nm, function(nm1) {
            D <- hs2(nm1, set2_new, weight = c(0.9, 0.9, 1, 1))
            i1 <- which.max(D)
            set2_new1 <- set2_new[i1]
            data.table(similarity = D[i1], set1_new = nm1, set2_new = set2_new1, set1 = set1[match(nm1, set1_new)], set2 = set2[match(set2_new1, set2_new)])
        })
        jg %<=>% rbindlist
        setorderv(jg, "similarity", -1)
    }
})
wk.view <- function(text1, pattern1, C = 3) {
    cat(text1, sep = "\n", file = paste("| ag -C", C, .q(pattern1)))
}
wk.color <- c(blue = "navy", red = "firebrick1")
hs.snv_is <- function(ref, alt) {
    (tolower(ref) %in% c("a", "c", "g", "t")) & (tolower(alt) %in% c("a", "c", "g", "t"))
}
v.pct.hs <- function(v1, order = {
}, nl = 0, P = 1) {
    a1 <- hs.table(v1)
    a2 <- round(a1/sum(a1) * 100, 2)
    a2 <- paste0(names(a1), if (nl) 
        "\n"
    else " ", "(n=", a1, if (P) 
        paste0("; ", a2[names(a1)], "%")
    else "", ")")
    names(a2) <- names(a1)
    r1 <- factor(a2[v1], a2)
    if (length(order)) 
        r1 <- reorder(r1, X = order)
    r1
}
fq_wj.hs.wjj <- function(wjj1, R = 0, str2exclude = "unpaired", str2include = {
}, fq.pattern = "(?i)\\.f(ast)?q(\\.gz)?$", ...) {
    jg <- hs.wjj.ls(wjj1, fq.pattern, R = R, ...)
    for (str1 in str2exclude) {
        i1 <- hs.grep(basename(jg), paste0("(?i)", str1))
        if (length(i1)) 
            jg <- jg[-i1]
    }
    for (str1 in str2include) {
        i1 <- hs.grep(basename(jg), paste0("(?i)", str1))
        jg <- jg[i1]
        if (!length(jg)) 
            break
    }
    jg
}
.sys.speedseq <- function(what = "wjj.speedseq") {
    f1 <- sys.function()
    switch(what, wjj.speedseq = "~/swxx/rj/speedseq", wjj.anno = file.path(f1("wjj.speedseq"), "annotations"), f.speedseq = , wj.speedseq = file.path(f1("wjj.speedseq"), "bin/speedseq"), wj.sambamba = file.path(f1("wjj.speedseq"), "bin/sambamba"), f.ref = , wj.ref = "~/swxx/sj/jyz/human/1kg/37/human_g1k_v37.fasta", `f.ceph18.b37.include.2014-01-15.bed` = , `wj.ceph18.b37.include.2014-01-15.bed` = file.path(f1("wjj.anno"), "ceph18.b37.include.2014-01-15.bed"), `ceph18.b37.lumpy.exclude.2014-01-15.bed` = file.path(f1("wjj.anno"), 
        "ceph18.b37.lumpy.exclude.2014-01-15.bed"))
}
.sys.annovar <- function(what = "", wjj0 = "~/swxx/rj/annovar") {
    file.path(wjj0, what)
}
gene.id_symbol.hs.str <- function(str1) {
    strsplit(str1, "[,; ]+") %=>% sapply(hs.hsgly("更新/gene.id_symbol.updater.36_09_12_235937"))
}
gene_id_name.37_04_29_145914 <- function(v1) {
    hs.grepV(v1, "(?i)gene[_\\.]?id")
}
log_fold_change_name.37_04_29_150019 <- function(v1) {
    hs.grepV(v1, "(?i)l(og[0-9]*)?[_\\.]?f(old)?[_\\.]?c(hange)?")
}
qvalue_name.37_04_29_150328 <- function(v1) {
    hs.grepV(v1, "(?i)fdr|q[_\\.]?v(al(ue)?)?")
}
pvalue_name.37_04_29_150328 <- function(v1) {
    hs.grepV(v1, "(?i)p[_\\.]?v(al(ue)?)?")
}
hs.pfm2pwm <- function(pfm1, bg = {
}, pseudocounts = 0.8) {
    if (!length(bg)) 
        bg <- hs.c(c("A", "C", "G", "T"), rep(0.25, 4))
    prior <- bg/sum(bg)
    post <- (pfm1 + bg * pseudocounts)/(colSums(pfm1) + sum(bg) * pseudocounts)
    log2(post/prior)
}
wk.pfm2pwm.for_TFBSTools <- function(pfm1, bg = {
}, ...) {
    if (!length(bg)) 
        bg <- hs.c(c("A", "C", "G", "T"), rep(0.25, 4))
    pwm1 <- hs.pfm2pwm(pfm1, bg = {
    })
    PWMatrix(profileMatrix = pwm1, bg = bg, ...)
}
.sys.wjj.fa_wj <- function(wjj1, cat = ot != "jg", ot = "cmd") {
    cmd1 <- .sys.find(.sys.fpGood(wjj1), ".*\\.fn?a(\\.b?gz)?$", iname = c("*unlocalized*", "*rna*"), iname.inv = 1)
    jg <- .wk(cmd1, intern = 1)
    if ((length(jg) > 1) && any(hs.grepl(jg, "/unplaced\\."))) {
        cmd1 <- .sys(cmd1, "! -iname unplaced.*")
    }
    if (cat) 
        cmd1 <- .sys(cmd1, "| xargs -I {} ~/org/dm/sh/rj/cat_wk {}")
    switch(ot, cmd = cmd1, jg = .wk(cmd1))
}
seq_org.hs.fa <- function(fa.wj, org, fa.wjj = {
}, seq.org.wj = if (length(fa.wjj)) hs.fp(fa.wjj, "seq.org.tsv") else hs.wjm(fa.wj, ext = "seq.org.tsv")) {
    if (!file.exists(seq.org.wj)) {
        if (missing(org)) 
            stop("37_03_05_113031")
        cmd1 <- if (length(fa.wjj)) {
            .sys(.sys.wjj.fa_wj(fa.wjj), "| perl -lane", .q("if( /^>/) { print}"))
        }
        else {
            .sys(.sys.cat.text(fa.wj), "| perl -lane", .q("if( /^>/) { print}"))
        }
        a1 <- .wk(cmd1, intern = 1)
        for (a2 in hs.re(a1, "(?<=^>)[^\\s]*")) hs.cat(a2, org, sep = "\t", file = seq.org.wj)
    }
    seq.org.wj
}
blastdb.wk.fa <- function(wjj1, org) {
    makeblastdb <- "~/swxx/rj/ncbi-blast-2.11.0+-x64-linux/ncbi-blast-2.11.0+/bin/makeblastdb"
    wjj.36_03_04_003344 <- getwd()
    on.exit(setwd(wjj.36_03_04_003344), add = TRUE)
    setwd(wjj1)
    title1 <- basename(normalizePath("."))
    seq.org.wj <- "seq.org.tsv"
    if (!file.exists(seq.org.wj)) {
        if (missing(org)) 
            stop("37_03_05_124954")
        seq_org.hs.fa(fa.wjj = ".", org = org, seq.org.wj = seq.org.wj)
    }
    if (!all(file.exists(paste0("blastdb.", c("ndb", "nhr", "nin", "nog", "nos", "not", "nsq", "ntf", "nto"))))) {
        .wk(.sys.wjj.fa_wj("."), "|", makeblastdb, "-title", title1, "-out", "blastdb", "-parse_seqids -blastdb_version 5", "-taxid_map", seq.org.wj, "-dbtype", "nucl")
    }
    hs.fp(normalizePath("."), "blastdb")
}
wk.blast <- function(fa, db, wj2 = {
}, seqid = {
}, blastn = "~/swxx/rj/ncbi-blast-2.11.0+-x64-linux/ncbi-blast-2.11.0+/bin/blastn", blastdb_aliastool = hs.fp(dirname(blastn), "blastdb_aliastool"), global_best = 1, append = hs.len1(wj2) && file.exists(wj2), nc = Ncpu()[1]) {
    if (length(seqid)) {
        hs.browser("37.03.09.184634")
        .wk(blastdb_aliastool, "-seqid_file_in")
    }
    arg <- .sys(if (global_best) 
        c("-max_hsps", global_best, "-num_alignments", global_best), "-db", .sys.fpGood(db), if (nc > 0) 
        c("-num_threads", nc), if (length(wj2) == 1) 
        c(if (hs.grepl(wj2, "(?i)\\.gz$")) "| gzip", if (append) ">>" else ">", .sys.fpGood(wj2)))
    if (hs.len1(fa) && file.exists(fa)) {
        .wk(blastn, "-query", fa, arg)
    }
    else cat(fa, sep = wk.n, file = .sys("|", blastn, arg))
    if (length(wj2)) 
        wj2
}
geneset.heatmap.37_04_29_151302 <- function(geneset1, exp1, geneset1_name, gene_name = {
}, de1 = if (hs.len1(de1_wj)) wk.fread(de1_wj), de1_wj = {
}, q.co1 = 0.05, lfc.co1 = log2(fc.co1), fc.co1 = 1.2, q.cn = qvalue_name.37_04_29_150328(names(de1)), lfc.cn = log_fold_change_name.37_04_29_150019(names(de1)), fc.cn = "fc", gene_id_cn = gene_id_name.37_04_29_145914(names(de1)), sample_info1, control1 = {
}, subject = sample_info1[[1]] %not% control1, org1, condition1 = "", to_z = 1, sample_info_for_z_i = 1, legend.name = if (to_z) "Z" else "Exp", wj2 = hs.wj(wjj2, hs.pastec0(geneset1_name, if (nzchar(condition1)) c(".", condition1), ".heatmap.pdf")), wjj2 = ".") {
    if ((!length(gene_name)) && length(de1)) 
        gene_name <- local({
            i1 <- (de1[[q.cn]] < q.co1) & (abs(de1[[lfc.cn]]) > lfc.co1)
            if (any(i1)) {
                star_n <- rep("", nrow(de1))
                i2 <- de1[[q.cn]] < 0.05
                if (any(i2)) 
                  star_n[i2] <- "*"
                i2 <- de1[[q.cn]] < 0.1
                if (any(i2)) 
                  star_n[i2] <- "**"
                de1[[gene_id_cn]][i1] %=>% hs.c(x, paste0(x, " (", star_n[i1], ")"))
            }
        })
    geneset1 <- geneset1 %and% rownames(exp1)
    if (length(geneset1) < 2) 
        stop("[37_04_28_222754|基因太少了]")
    exp1 <- exp1[geneset1, , drop = 0]
    sample_to_use <- rownames(sample_info1)[sample_info1[[1]] %in% (control1 %or% subject)]
    exp1 <- exp1[, colnames(exp1) %in% sample_to_use, drop = 0]
    sample_info1 <- sample_info1[sample_to_use, , drop = 0]
    if (to_z) {
        sample_to_use_for_z <- if (missing(control1)) {
            colnames(exp1)
        }
        else rownames(sample_info1)[sample_info1[[sample_info_for_z_i]] %in% control1]
        mean_for_z <- hs.rowMeans(exp1[, sample_to_use_for_z, drop = 0])
        sd_for_z <- hs.rowSds(exp1[, sample_to_use_for_z, drop = 0])
        m1 <- sapply(seq.int(nrow(exp1)), function(row_i) {
            (exp1[row_i, ] - mean_for_z[row_i])/sd_for_z[row_i]
        }) %=>% t
        i1 <- !hs.clean(m1, ot = "l")
        if (any(i1)) 
            m1[i1] <- NA
        rownames(m1) <- rownames(exp1)
    }
    else m1 <- exp1
    m1 <- apply(m1, 2, hs.deInf)
    gene_name %<>% {
        .[names(.) %in% rownames(m1)]
    }
    if (length(gene_name)) {
        i1 <- match(names(gene_name), rownames(m1))
        rownames(m1)[i1] <- gene_name
    }
    m1 <- m1[hs.rowMeans(is.na(m1)) < 0.5, , drop = 0]
    hs.hsgly("ComplexHeatmap/36.08.12.233720")(m1, legend.name = "Z", wj = wj2, column_split = sample_info1)
}
wk.color <- c(blue = "navy", red = "firebrick1", green = "darkolivegreen2", gray = "darkgray")
wk.plot.step <- function(x, y, ...) {
    plot(x, y, type = "n", ...)
    for (i in seq.int(length(x) - 1)) {
        lines(x[c(i, i + 1)], rep(y[i], 2))
        lines(rep(x[i + 1], 2), y[c(i, i + 1)])
    }
}
ieiwk.heatmap <- function(x, Rowv = NA, Colv = NA, distfun = dist, hclustfun = hclust, reorderfun = function(d, w) reorder(d, w), add.expr, symm = FALSE, revC = identical(Colv, "Rowv"), scale = c("none", "row", "column"), na.rm = TRUE, margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE, verbose = getOption("verbose"), revR = 1, di = dim(x), nr = di[1L], nc = di[2L], 
    ...) {
    if (revR) 
        x <- x[rev(seq(nrow(x))), , drop = FALSE]
    heatmap(x, Rowv, Colv, distfun, hclustfun, reorderfun, add.expr, symm, revC, match.arg(scale), na.rm, margins, ColSideColors, RowSideColors, cexRow, cexCol, labRow, labCol, main, xlab, ylab, keep.dendro, verbose, ...)
}
ieiwk.heatmap.2 <- function(m1, col = colorpanel(75, "green", "gray", "red"), key = TRUE, symkey = FALSE, density.info = "none", trace = "none", cexRow = 0.5, margins = c(5, 5)) {
    require("gplots")
    heatmap.2(m1, col = col, key = as.logical(key), symkey = as.logical(symkey), density.info = density.info, trace = trace, cexRow = cexRow, margins = margins)
}
ieiwk.density <- function(x, from = min(x), to = max(x), bw = "sj", ...) {
    density(x = x, from = from, to = to, bw = bw, ...)
}
Mycol2rgb <- function(x, alpha = 1) {
    x <- col2rgb(x)
    a1 <- as.list(x/255)
    if (length(alpha)) 
        a1[[length(a1) + 1]] <- alpha
    do.call(rgb, a1)
}
ieiwk.col2rgb <- Mycol2rgb
hs.fig_wj_wjj <- function() {
    c(if (dir.exists("/dev/shm")) hs.fp("/dev/shm", Sys.info()["user"]), "~/swxx", "~/tmp") %=>% x[dir.exists(hs.home.expand(x))][1]
}
.pdf <- function(exp1, wj = "tmp", width = height + 5, height = 15 * rh, rh = 1, wjj2 = hs.fig_wj_wjj(), family = "mono", local = 1, env1 = parent.frame(), wjm = "tmp", append = 0, el = {
}, verbose = 1) {
    if (missing(wj)) 
        wj <- hs.wj(wjj2, wj)
    if (!hs.grepl(wj, "(?i)\\.pdf$")) 
        wj %<=>% paste0(".pdf")
    if (0) 
        if (length(el)) {
            wj_temp <- paste0(sapply(el, function(i1) hs.wj.temp()), ".pdf")
            on.exit(hs.wj.rm(wj_temp), add = TRUE)
            for (i1 in seq_along(el)) {
                wj2 <- wj_temp[i1]
                .pdf(el[[i1]], wj = wj2, env1 = env1, width = width[hs.recycle(i1, length(el))], height = height[hs.recycle(i1, length(el))], verbose = 0)
            }
            wjj.36_03_04_003344 <- getwd()
            on.exit(setwd(wjj.36_03_04_003344), add = TRUE)
            setwd(dirname(wj_temp[1]))
            .wk(hs.home.expand("~/swxx/rj/pdftk/pdftk"), .q(basename(wj_temp)), "cat", "output", .sys.fpGood(wj))
            if (verbose) 
                message(wj)
            return(wj)
        }
    wj_tmp <- if (append && file.exists(wj)) 
        hs.wj.temp(ext = "pdf")
    else wj
    if (inherits(exp1, c("grob", "ggplot")) || (inherits(exp1, "list") && inherits(exp1[[1]], c("grob", "ggplot")))) {
        if (inherits(exp1, "ggplot")) 
            exp1 %<=>% +ggplot2::theme(text = ggplot2::element_text(family = family))
        ggplot2::ggsave(wj, exp1, width = width, height = height, units = "cm", limitsize = FALSE)
        return(wj)
    }
    pdf(hs.wj(wj_tmp), height = height/2.54, width = width/2.54, family = family, fonts = family)
    on.exit(dev.off(), add = TRUE)
    {
        par.37_02_23_180042 <- par(no.readonly = TRUE)
        on.exit(par(par.37_02_23_180042), add = TRUE)
    }
    eval(substitute(exp1), if (local) 
        new.env(parent = env1)
    else env1)
    if (wj_tmp != wj) {
        hs.browser("38.03.10.093340")
        wj2_tmp <- hs.wj.temp(ext = "pdf")
        on.exit(hs.wj.rm(wj_tmp), add = TRUE)
        hs.hsgly("pdf/合并多个文件.36_11_09_121255")(c(wj, wj_tmp), wj2_tmp)
        hs.wj.move(wj2_tmp, wj)
    }
    if (0) {
        env2 <- if (local) 
            new.env(parent = env1)
        else env1
        sapply(list(...), eval, env2)
    }
    if (verbose) {
        1
        {
            message(normalizePath(wj) %=>% hs.wjm.home)
        }
    }
    wj
}
.png <- function(exp1, wj = "tmp", width = height + 5, height = 10 * rh, wjj2 = hs.fig_wj_wjj(), units = "cm", rw = 1, rh = 1, pointsize = 12, res = 100, env1 = parent.frame(), family = "mono") {
    on.exit(dev.off(), add = TRUE)
    if (!hs.grepl(wj, "(?i)\\.png$")) 
        wj %<>% paste0(".png")
    if ((!hs.grepl(wj, "/")) && dir.exists(wjj2)) 
        wj <- hs.fp(wjj2, wj)
    here <- new.env(parent = env1)
    png(hs.wj(wj), width, height, units = units, pointsize = pointsize, res = res, family = family)
    eval(substitute(exp1), here)
    wj <- normalizePath(wj)
    message(hs.wjm.home(wj))
    wj
}
.figs <- function(..., type = list(.png, .pdf), nc = Inf, env1 = parent.frame()) {
    fc <- as.list(substitute(list(...)))
    i <- match("wj", names(fc), nomatch = 2 + 1)
    wj <- eval(fc[[i]], envir = env1)
    if (hs.grepl(wj, "(?i)\\.(pdf|png)$")) 
        fc[[i]] <- hs.sub(wj, "(?i)\\.[^\\.]+$")
    wk.mclapply(type, . %=>% {
        fc[[1]] <- x
        fc[["env1"]] <- env1
        eval(as.call(fc), envir = env1)
    }, mc.cores = nc)
}
.fig <- function(..., env1 = parent.frame()) {
    fc <- as.list(substitute(list(...)))
    i <- match("wj", names(fc), nomatch = 2 + 1)
    wj <- eval(fc[[i]], envir = env1)
    if (hs.grepl(wj, "(?i)\\.(pdf|png)$")) {
        hs1 <- tolower(hs.re(wj, "(?i)\\.[^\\.]+$"))
        fc[[i]] <- hs.sub(wj, "(?i)\\.[^\\.]+$")
    }
    else hs1 <- ".pdf"
    fc[[1]] <- hs.find_function(hs1)
    fc[["env1"]] <- env1
    eval(as.call(fc), envir = env1)
}
.tiff <- function(exp1, wj = "tmp", width = 480, height = 480, wjj2 = hs.fig_wj_wjj(), pointsize = 12, env1 = parent.frame(), ...) {
    on.exit(dev.off(), add = TRUE)
    if (!hs.grepl(wj, "(?i)\\.tiff$")) 
        wj %<>% hs.wjm(add = "tiff")
    if ((!hs.grepl(wj, "/")) && dir.exists(wjj2)) 
        wj <- hs.fp(wjj2, wj)
    here <- new.env(parent = env1)
    tiff(wj, width, height, pointsize = pointsize, ...)
    eval(substitute(exp1), here)
    wj
}
.jpeg <- function(..., wj = hs.home.expand("~/tmp.jpeg"), wjj2 = hs.fig_wj_wjj(), width = 480, height = 480, pointsize = 12) {
    on.exit(dev.off(), add = TRUE)
    if (tolower(hs.re(wj, "[^\\.]+$")) != "jpeg") 
        wj %<>% hs.wjm(add = "jpeg")
    jpeg(wj, width, height, pointsize = pointsize)
    sapply(list(...), function(src) {
        eval(src, env = .GlobalEnv)
    })
}
ieiwk.plot.blank <- function(main) {
    op <- par(xaxt = "n", yaxt = "n")
    plot(1:3, type = "n", xlab = "", ylab = "", main = main)
    par(op)
}
ieiwk.plot.text <- function(main = "", t1, width = 79, cex = 0.7, family = "mono", wrap = 0) {
    ieiwk.plot.blank(strwrap(main, 31))
    if (wrap) 
        t1 <- strwrap(t1, width)
    mtext(paste(t1, collapse = "\n"), line = -1, at = 1, adj = 0, padj = 1, family = family, cex = cex)
}
mergePDF <- function(files, file, gsversion = NULL, in.file = NULL) {
    if (is.null(gsversion)) {
        gsversion <- names(which(Sys.which(c("gs", "gswin32c", "gswin64c")) != ""))
        if (length(gsversion) == 0) 
            stop("Please install Ghostscript and ensure it is in your PATH")
        if (length(gsversion) > 1) 
            stop("More than one Ghostscript executable was found:", paste(gsversion, collapse = " "), ". Please specify which version should be used with the gsversion argument")
    }
    .sys.do(gsversion, "-dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite", sprintf("-sOutputFile=%s", .sys.fpGood(file)), files)
    invisible(file)
}
wk.venn.diagram.2 <- local({
    hs.require("VennDiagram")
    function(x1, u1, col1 = c("cornflowerblue", "darkorchid1"), wjm = hs.gsub(paste0(main, ".", paste(names(x1), collapse = "_vs_")), " ", "_"), wjj = hs.home.expand("~/tmp"), wj.type = "pdf", side.length = 10, main = paste(names(x1), collapse = " vs "), col = "black", ...) {
        x1 <- lapply(x1, . %=>% (`%and%`(u1)))
        t1 <- fisher.test.ele(u1, x1[[1]], x1[[2]])
        sub <- paste0("Odds ratio: ", round(t1$est, 2), " (P ", t1$p.v %=>% {
            if (x < 2.2e-16) 
                "< 2.2e-16"
            else paste("=", format(t1$p.v, sci = TRUE))
        }, ")")
        inv <- sapply(x1, length) %>% {
            .[1] < .[2]
        }
        cat.pos <- 20 %=>% {
            ifelse(!inv, ., 180 - .)
        } %=>% (x * c(-1, 1))
        wj <- hs.wj(wjj, paste0(wjm, ".", wj.type))
        .pdf({
            grid.draw(venn.diagram(x = x1, filename = {
            }, height = side.length, width = side.length, units = "cm", lwd = 1, fill = col1, col = col, alpha = 0.66, label.col = "black", cex = 1, fontfamily = "serif", fontface = "bold", margin = 0.03, cat.col = col1, cat.cex = 1, cat.fontfamily = "serif", cat.fontface = "bold", cat.dist = c(0.05, 0.05), cat.just = list(c(NA, NA), c(0.3, 0)) %=>% {
                if (inv) 
                  rev(x)
                else x
            }, cat.pos = cat.pos, ext.length = 0.9, ext.pos = 180, main = main, sub = sub, sub.cex = 0.8, inverted = FALSE, ...))
        }, wj = wj)
        wj
    }
})
wk.heatmap.0orMore <- function(m1, wj_out = "heatmap.pdf", name = "Exp", color.breaks = c("white", "black"), log = 0, row_names_side = "left", column_names_side = "top", ...) {
    hs.require("ComplexHeatmap")
    m1.range <- range(as.vector(m1))
    if (log) 
        m1 <- log(m1 + 1, log)
    colors <- circlize::colorRamp2(c(0, max(as.vector(m1))), color.breaks)
    gray1 <- do.call(rgb, c(as.list(c(132, 133, 135)/255), 0.33))
    pdf.width <- 0.5 * ncol(m1)
    pdf.height <- 0.5 * nrow(m1)
    pdf(wj_out, width = pdf.width, height = pdf.height)
    on.exit(dev.off(), add = TRUE)
    ht <- Heatmap(m1, name = name, row_names_side = row_names_side, column_names_side = column_names_side, cluster_rows = FALSE, cluster_columns = FALSE, col = colors, border = ieiwk.col2rgb("black", 1), rect_gp = gpar(col = gray1, lwd = 1), cluster_column_slices = FALSE, column_title = {
    }, row_title = {
    }, column_gap = unit(0, "mm"), row_gap = unit(0, "mm"), width = grid::unit(0.5 * ncol(m1), "cm"), height = grid::unit(0.5 * nrow(m1), "cm"), cell_fun = function(j, i, x, y, width, height, fill) {
    }, show_heatmap_legend = !log)
    if (log) {
        lgd_spc <- Legend(title = name, col = colors, at = c(0, max(as.vector(m1))), labels = c("0", m1.range[2]))
        draw(ht, annotation_legend_list = list(lgd_spc))
    }
    else draw(ht)
}
text.width.hs <- function(t1, min1 = Inf, cm = 1, scale = 1, ...) {
    if (0) {
        wj <- hs.wj.temp()
        on.exit(hs.wj.rm(wj), add = TRUE)
        pdf(wj)
        on.exit(dev.off(), add = TRUE)
    }
    if (length(list(...))) {
        op <- par(...)
        on.exit(par(op), add = TRUE)
    }
    jg <- t1 %=>% strwidth("inches") %=>% max %=>% min(min1)
    if (cm) 
        jg <- jg * 2.54
    jg * scale
}
wk.gseaplot2 <- function(x, geneSetID, title = "", color = "green", base_size = 11, rel_heights = c(1.5, 0.5, 1), subplots = 1:3, pvalue_table = FALSE, ES_geom = "line", gene2label = {
}) {
    ES_geom <- match.arg(ES_geom, c("line", "dot"))
    geneList <- position <- NULL
    if (length(geneSetID) == 1) {
        gsdata <- enrichplot:::gsInfo(x, geneSetID)
    }
    else {
        gsdata <- do.call(rbind, lapply(geneSetID, enrichplot:::gsInfo, object = x))
    }
    p <- ggplot(gsdata, aes_(x = ~x)) + xlab(NULL) + theme_classic(base_size)
    if (length(gene2label)) {
        xi <- match(gene2label, names(attr(x, "geneList")))
        p <- p + geom_linerange(aes_(ymin = 0, ymax = ~runningScore), color = "black", data = gsdata[xi, ])
    }
    p <- p + theme(panel.grid.major = element_line(colour = "grey92"), panel.grid.minor = element_line(colour = "grey92"), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) + scale_x_continuous(expand = c(0, 0))
    if (ES_geom == "line") {
        es_layer <- geom_line(aes_(y = ~runningScore, color = ~Description), size = 1)
    }
    else {
        es_layer <- geom_point(aes_(y = ~runningScore, color = ~Description), size = 1, data = subset(gsdata, position == 1))
    }
    p.res <- p + es_layer + theme(legend.position = c(0.8, 0.8), legend.title = element_blank(), legend.background = element_rect(fill = "transparent"))
    p.res <- p.res + ylab("Running Enrichment Score") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), plot.margin = margin(t = 0.2, r = 0.2, b = 0, l = 0.2, unit = "cm"), axis.title.y = element_text(size = 8))
    i <- 0
    for (term in unique(gsdata$Description)) {
        idx <- which(gsdata$ymin != 0 & gsdata$Description == term)
        gsdata[idx, "ymin"] <- i
        gsdata[idx, "ymax"] <- i + 1
        i <- i + 1
    }
    p2 <- ggplot(gsdata, aes_(x = ~x)) + geom_linerange(aes_(ymin = ~ymin, ymax = ~ymax, color = ~Description)) + xlab(NULL) + ylab(NULL) + theme_classic(base_size) + theme(legend.position = "none", plot.margin = margin(t = -0.1, b = 0, unit = "cm"), axis.ticks = element_blank(), axis.text = element_blank(), axis.line.x = element_blank()) + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
    if (length(geneSetID) == 1) {
        v <- seq(1, sum(gsdata$position), length.out = 9)
        inv <- findInterval(rev(cumsum(gsdata$position)), v)
        if (0) {
            v <- seq(1, 100, length.out = 9)
            inv <- c(rep(100, 100), rep(0, length(gsdata$position) - 100))
            inv <- inv + 1
            xmin <- which(!duplicated(inv))
            xmax <- xmin + as.numeric(table(inv)[unique(inv)])
        }
        if (min(inv) == 0) 
            inv <- inv + 1
        col = c(rev(RColorBrewer::brewer.pal(5, "Blues")), brewer.pal(5, "Reds"))
        ymin <- min(p2$data$ymin)
        yy <- max(p2$data$ymax - p2$data$ymin) * 0.3
        xmin <- which(!duplicated(inv))
        xmax <- xmin + as.numeric(table(inv)[as.character(unique(inv))])
        d <- data.frame(ymin = ymin, ymax = yy, xmin = xmin, xmax = xmax, col = col[unique(inv)])
        p2 <- p2 + geom_rect(aes_(xmin = ~xmin, xmax = ~xmax, ymin = ~ymin, ymax = ~ymax, fill = ~I(col)), data = d, alpha = 0.9, inherit.aes = FALSE)
    }
    df2 <- p$data
    df2$y <- p$data$geneList[df2$x]
    p.pos <- p + geom_segment(data = df2, aes_(x = ~x, xend = ~x, y = ~y, yend = 0), color = "grey")
    p.pos <- p.pos + ylab(quote("Ranked Log"[2] * "FoldChange")) + xlab("Rank in Ordered Dataset") + theme(plot.margin = margin(t = -0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"), axis.title.y = element_text(size = 8))
    if (!is.null(title) && !is.na(title) && title != "") 
        p.res <- p.res + ggtitle(title)
    if (length(color) == length(geneSetID)) {
        p.res <- p.res + scale_color_manual(values = color)
        if (length(color) == 1) {
            p.res <- p.res + theme(legend.position = "none")
            p2 <- p2 + scale_color_manual(values = "black")
        }
        else {
            p2 <- p2 + scale_color_manual(values = color)
        }
    }
    if (pvalue_table) {
        pd <- x[geneSetID, c("Description", "pvalue", "p.adjust")]
        pd <- pd[order(pd[, 1], decreasing = FALSE), ]
        rownames(pd) <- pd$Description
        pd <- pd[, -1]
        pd <- round(pd, 4)
        tp <- tableGrob2(pd, p.res)
        p.res <- p.res + theme(legend.position = "none") + annotation_custom(tp, xmin = quantile(p.res$data$x, 0.5), xmax = quantile(p.res$data$x, 0.95), ymin = quantile(p.res$data$runningScore, 0.75), ymax = quantile(p.res$data$runningScore, 0.9))
    }
    plotlist <- list(p.res, p2, p.pos)[subplots]
    n <- length(plotlist)
    plotlist[[n]] <- plotlist[[n]] + theme(axis.line.x = element_line(), axis.ticks.x = element_line(), axis.text.x = element_text())
    if (length(subplots) == 1) 
        return(plotlist[[1]] + theme(plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, unit = "cm")))
    if (length(rel_heights) > length(subplots)) 
        rel_heights <- rel_heights[subplots]
    cowplot::plot_grid(plotlist = plotlist, ncol = 1, align = "v", rel_heights = rel_heights)
}
wk.pheatmap <- local({
    hs.require("pheatmap")
    hs.callback <- function(hc, mat) {
        sv <- svd(t(mat))$v[, 1]
        dend <- reorder(as.dendrogram(hc), wts = sv)
        as.hclust(dend)
    }
    function(M, wj, color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), breaks.boundary = max(abs(M), na.rm = 1), breaks.N = 100, breaks = seq(-breaks.boundary, breaks.boundary, length.out = breaks.N), cluster = 1, cluster_rows = cluster, cluster_cols = cluster, column.anno = NA, row.anno = NA, anno.color = NA, info = FALSE, info.color = "green", fontsize_row = if (show_rownames) 
        fontsize
    else NA, fontsize_col = fontsize, fontsize = 5, show_rownames = nrow(M) < 100, show_colnames = 1, width = NA, height = if (show_rownames) 
        NA
    else 5, ...) {
        na.i1 <- which(is.na(M))
        if (length(na.i1)) {
            M[na.i1] <- 0
        }
        if (0) {
            method1 <- "spearman"
            method1 <- "pearson"
            row_hclust <- if (cluster_rows) 
                hclust(M %=>% t %=>% cor(method = method1) %=>% {
                  1 - x
                } %=>% as.dist)
            else FALSE
            row_hclust <- 1
            row_hclust <- hclust(dist(M))
        }
        row_hclust <- cluster_rows
        column_hclust <- if (cluster_cols) 
            hclust(M %=>% cor(method = method1) %=>% {
                1 - x
            } %=>% as.dist)
        else FALSE
        if (length(na.i1)) 
            M[na.i1] <- NA
        pheatmap(M, filename = wj, cellwidth = fontsize_col, cellheight = fontsize_row, fontsize = fontsize, color = colorRampPalette(c("navy", "white", "firebrick3"))(51), breaks = breaks, na_col = "gray", border_color = NA, cluster_rows = row_hclust, cluster_cols = column_hclust, clustering_callback = hs.callback, annotation_col = column.anno, annotation_row = row.anno, annotation_colors = anno.color, display_numbers = info, number_color = info.color, show_rownames = as.logical(show_rownames), 
            show_colnames = as.logical(show_colnames), width = width, height = height, ...)
        wj
    }
})
wk.ggsave <- function(fig_list, wj2 = hs.home.expand("~/tmp.pdf"), units = "cm", family = "GB1", env1 = base::parent.frame(), env2 = if (local) base::new.env(parent = env1) else env1, local = 1, redo = 1, ...) {
    if (redo) 
        hs.wj.rm(wj2)
    if (!file.exists(wj2)) {
        if (is.language(fig_list)) 
            fig_list <- base::eval(fig_list, env2)
        if ("list" %not.in% class(fig_list)) {
            fig_list <- list(fig_list)
        }
        args <- list(..., family = family)
        height <- if ("height" %in% names(args)) 
            args[["height"]]
        else NA
        width <- if ("width" %in% names(args)) 
            args[["width"]]
        else NA
        hs.wj(wj2)
        hs.switch(length(fig_list), 1, {
            ggplot2::ggsave(wj2, fig_list[[1]], units = units, family = family, ...)
        }, 0, hs.browser("38.02.16.140027"), {
            wjj_temp <- hs.wjj(hs.wj.temp())
            on.exit(hs.wj.rm(wjj_temp, 1), add = TRUE)
            wj_temp <- sapply(seq_along(fig_list), function(i1) {
                wj2 <- hs.wj(wjj_temp, paste0(i1, ".pdf"))
                args[["width"]] <- width[hs.recycle(i1, length(width))]
                args[["height"]] <- height[hs.recycle(i1, length(height))]
                args <- c(wj2, fig_list[i1], args, units = units)
                do.call(ggplot2::ggsave, args)
                wj2
            })
            hs.hsgly("pdf/合并多个文件.36_11_09_121255")(wj_temp, wj2)
        })
    }
    wj2
}
hs.lab_log <- function(vn = "Exp", logN = 2) {
    bquote(~Log[.(logN)] * .(vn))
}
bg_white.hs <- function(go1) {
    go1 + ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))
}
vhline00.hs <- function(go1) {
    go1 + ggplot2::geom_hline(yintercept = 0, color = "grey", linetype = "dashed", lwd = 1) + ggplot2::geom_vline(xintercept = 0, color = "grey", linetype = "dashed", lwd = 1)
}
hline_qv.hs <- function(go) {
    go + ggplot2::geom_hline(yintercept = -log10(0.05), color = "grey", linetype = "dashed", lwd = 1)
}
annotate_text <- function(label, x, y, facets = "", hjust = 0, vjust = 0, color = "black", alpha = NA, family = thm$text$family, size = thm$text$size, fontface = 1, lineheight = 1, box_just = ifelse(c(x, y) < 0.5, 0, 1), margin = unit(size/2, "pt"), thm = ggplot2::theme_get()) {
    x <- scales::squish_infinite(x)
    y <- scales::squish_infinite(y)
    data <- if (is.null(facets)) 
        data.frame(x = NA)
    else data.frame(x = NA, facets)
    tg <- grid::textGrob(label, x = 0, y = 0, hjust = hjust, vjust = vjust, gp = grid::gpar(col = alpha(color, alpha), fontsize = size, fontfamily = family, fontface = fontface, lineheight = lineheight))
    ts <- grid::unit.c(grid::grobWidth(tg), grid::grobHeight(tg))
    vp <- grid::viewport(x = x, y = y, width = ts[1], height = ts[2], just = box_just)
    tg <- grid::editGrob(tg, x = ts[1] * hjust, y = ts[2] * vjust, vp = vp)
    inner <- grid::grobTree(tg, vp = grid::viewport(width = unit(1, "npc") - margin * 2, height = unit(1, "npc") - margin * 2))
    layer(data = NULL, stat = StatIdentity, position = PositionIdentity, geom = GeomCustomAnn, inherit.aes = TRUE, params = list(grob = grid::grobTree(inner), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf))
}
hs_plot.track.add_text <- local({
    x_unit <- {
    }
    y_unit <- {
    }
    th <- {
    }
    function(text, X, y_range, cex1 = 1, th.f = 1.3, tw.f = 1.1, only = {
    }, ...) {
        x_unit <<- par("usr")[1:2] %=>% diff %=>% {
            x/par("pin")[1]
        }
        y_unit <<- par("usr")[3:4] %=>% diff %=>% {
            x/par("pin")[2]
        }
        th <<- strheight("a", "inches", cex1) * th.f * y_unit
        y1 <- y_range[1] + th/2 * th.f
        Y <- rep(0, length(X))
        occupied <- list()
        for (i1 in seq_along(text)) {
            t1 <- text[i1]
            x1 <- X[i1]
            t1w <- strwidth(t1, "inches", cex1) * tw.f * x_unit
            o1 <- cbind(x1 %=>% {
                c(x, x + t1w)
            }, y1 + th/2 * c(-1, 1))
            i2 <- sapply(occupied, function(o2) {
                is_overlapping.hs.range1(o1[, 1], o2[, 1])
            })
            if (length(i2) && any(i2)) {
                occupied_x <- occupied[i2]
                y_allowed <- sapply(occupied_x, . %=>% x[, 2]) %=>% t %=>% reduced.hs.range_1d %=>% dif.hs.range_1d(c(y1 - th/2 * th.f, y_range[2]), x)
                o1[, 2] <- y_allowed[which(apply(y_allowed, 1, diff) >= th)[1], 1] %=>% c(x, x + th)
            }
            Y[i1] <- mean(o1[, 2])
            occupied[length(occupied) + 1] <- list(o1)
        }
        if (length(only)) 
            return(switch(only, xy = list(X, Y)))
        text(X, Y, text, pos = 4, offset = 0, cex = cex1, ...)
    }
})
hs.with_par <- function(l1, exp1) {
    par.37_11_16_121131 <- do.call(par, l1)
    on.exit(par(par.37_11_16_121131))
    exp1
}
hs.grid.add_text <- function(text = "A", wh_ratio = 1) {
    grid.text(text, 0.05, unit(1, "npc") - unit(1, "lines"), just = 1, gp = gpar(fontsize = 20, fontface = "bold"))
    if (0) {
        hs.with_par(list(mar = c(0, 0, 0, 0), xaxt = "n", yaxt = "n", bty = "n"), {
            pushViewport(viewport(x = 0.01, y = 1, width = 0.05, height = 0.05 * wh_ratio, just = c("left", "top")))
            par(fig = gridFIG())
            plot(c(0, 1), c(0, 1), type = "n")
            text(0.5, 0.5, "A", cex = 2)
            popViewport()
        })
    }
}
hs.grid.print <- function(fig1, ..., pop = 1) {
    vp1 <- viewport(...)
    pushViewport(vp1)
    print(fig1, newpage = FALSE)
    if (pop) 
        popViewport()
}
hs.rowCvs <- function(x) {
    Mean <- hs.rowMeans(x)
    Sd <- hs.rowSds(x)
    Sd/Mean
}
hs.colCvs <- function(x) {
    Mean <- hs.colMeans(x)
    Sd <- hs.colSds(x)
    Sd/Mean
}
hs.num_is_equal <- function(n1, n2 = 0, co1 = 0.01) {
    n1 <- as.numeric(n1)
    n2 <- as.numeric(n2)
    abs(n1 - n2) < co1
}
hs.se <- function(x1) {
    BiocGenerics::sd(x1)/(base::length(x1)^0.5)
}
hs.sd_median <- function(v1) {
    (base::sum((v1 - stats::median(v1))^2)/(base::length(v1) - 1))^0.5
}
hs.mad <- function(v1, side = 1) {
    median1 <- stats::median(v1)
    v2 <- if (side) 
        v1[v1 >= median1]
    else v1[v1 < median1]
    stats::mad(v2, median1)
}
geometric_mean.hs <- function(v1) {
    exp(mean(log(v1)))
}
hs.prcomp <- function(..., scale = 1) {
    stats::prcomp(..., scale = as.logical(scale))
}
z_score.hs.v <- function(v1, log = 0, na.rm = TRUE, log_min = 1) {
    if (log) {
        v1 <- log(v1 + log_min, log)
    }
    a1 <- v1 - mean(v1, na.rm = na.rm)
    a1/sd(a1, na.rm = na.rm)
}
z_score.hs.m <- function(m1, side = 1) {
    if (side != 1) 
        hs.browser("38.05.04.234329")
    t((t(m1) - rep(hs.rowMeans(m1), each = ncol(m1)))/rep(hs.rowSds(m1), each = ncol(m1)))
}
hs.log_mean <- function(v1, base = {
}, base2 = base) {
    hs.exp <- if (length(base)) {
        function(x1) base^x1
    }
    else function(x1) expm1(x1) + 1
    hs.log <- if (length(base2)) {
        switch(paste(base2), `2` = base::log2, `10` = base::log10, function(x1) base::log(x1, base))
    }
    else base::log
    hs.log(base::mean(hs.exp(v1)))
}
hs.lfc <- function(v1, v2, base = 2, pseudocount = 1, loged = TRUE, base0 = {
}) {
    if (loged) {
        hs.log_mean(v1, base0, base) - hs.log_mean(v2, base0, base)
    }
    else {
        m1 <- base::mean(v1) + pseudocount
        m2 <- base::mean(v2) + pseudocount
        hs.log <- switch(paste(base), `2` = base::log2, `10` = base::log10, function(x1) base::log(x1, base))
        hs.log(m1) - hs.log(m2)
    }
}
fisher.test.ele <- function(u, s1, s2, workspace = 2e+05, hybrid = FALSE, control = list(), or = 1, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, simulate.p.value = FALSE, B = 2000, m.only = 0, verbose = TRUE, no0 = FALSE, u.n, s1.n, s2.n, m = {
}) {
    if (missing(u.n)) 
        u.n <- deparse(substitute(u), width.cutoff = 500L)
    if (missing(s1.n)) 
        s1.n <- deparse(substitute(s1), width.cutoff = 500L)
    if (missing(s2.n)) 
        s2.n <- deparse(substitute(s2), width.cutoff = 500L)
    if (length(m)) {
        if (hs.len1(dim(m), 2)) {
            if (!hs.len1(m, 4)) 
                stop("36_10_09_190618")
        }
        else m <- matrix(head(m, 4), nrow = 2)
    }
    else {
        s1 <- intersect(u, s1)
        s2 <- intersect(u, s2)
        m <- matrix(c(sum(s1 %in% s2), length(u %and% s1 %not% s2), length(u %and% s2 %not% s1), length(u %not% (s1 %or% s2))), 2)
        rownames(m) <- c(s2.n %.% "+", s2.n %.% "-")
        colnames(m) <- c(s1.n %.% "+", s1.n %.% "-")
    }
    if (no0) 
        m[m == 0] <- 1
    if (m.only) 
        return(m)
    a <- fisher.test(m, workspace = workspace, hybrid = hybrid, control = control, or = or, alternative = alternative, conf.int = conf.int, conf.level = conf.level, simulate.p.value = simulate.p.value, B = B)
    m <- cbind(m, rowSums(m))
    m <- rbind(m, colSums(m))
    if (verbose) {
        prt <- capture.output(.s(m))
        substr(prt[substr(prt, 1, 1) == " "], 1, 1) <- "."
        prt[length(prt)] <- paste(prt[length(prt)], " (", u.n, ")", sep = "")
        cat(prt, sep = "\n")
    }
    b <- list(as.vector(round(a$est, 5)), a$p.v)
    names(b) <- c("odds ratio", "p-value")
    if (verbose) 
        cat(ieiwk.s.list.in.row(b), sep = "\n")
    a
}
hs.cos_sim <- function(x, y) {
    if (length(dim(x)) == 2) {
        if ((ncol(x) != 2) && (nrow(x) == 2)) 
            x <- t(x)
        if (ncol(x) != 2) 
            hs.browser("38.08.09.224140")
        y <- x[, 2]
        x <- x[, 1]
    }
    (x %*% y)/(sum(x^2) * sum(y^2))^0.5
}
aov.pv <- function(a) {
    b <- head(summary(a)[[1]][["Pr(>F)"]], -1)
    names(b) <- gsub(" ", "", head(rownames(summary(a)[[1]]), -1))
    b
}
ieiwk.normalize.quantiles <- function(x, copy = 1, co.os = 100) {
    if (!is.matrix(x)) {
        x <- as.matrix(x)
        copy <- 0
    }
    if (as.numeric(sub("\\w+$", "", os("x", units = "Mb"))) > co.os) 
        copy <- 0
    a <- preprocessCore::normalize.quantiles(x, as.logical(copy))
    dimnames(a) <- dimnames(x)
    a
}
hs.p.adjust <- function(p, method = "fdr", n = length(p)) {
    stats::p.adjust(p, method = method, n = n)
}
reduced.hs.range_1d <- function(R) {
    if (is.vector(R)) 
        R <- t(R)
    ro <- order(R[, 1])
    wanted <- rep(TRUE, nrow(R))
    if (nrow(R) > 1) {
        for (i1 in 2:nrow(R)) {
            r1 <- R[ro[i1], ]
            i2 <- max(which(head(wanted, i1 - 1)))
            r2 <- R[ro[i2], ]
            if (r1[1] <= r2[2]) {
                if (r1[2] > r2[2]) 
                  R[ro[i2], 2] <- r1[2]
                wanted[i1] <- FALSE
            }
        }
    }
    R[ro[wanted > 0], , drop = 0]
}
is_overlapping.hs.range1_1d <- local({
    hs1 <- function(r1, r2) r2[1] <= r1[2]
    function(r1, r2) {
        if (r1[1] <= r2[1]) 
            hs1(r1, r2)
        else hs1(r2, r1)
    }
})
is_overlapping.hs.range1 <- function(r1, r2) {
    if (is.vector(r1)) 
        r1 <- matrix(r1)
    if (is.vector(r2)) 
        r2 <- matrix(r2)
    if (ncol(r1) != ncol(r2)) 
        stop("37_01_28_205438")
    for (i1 in 1:ncol(r1)) if (!is_overlapping.hs.range1_1d(r1[, i1], r2[, i1])) 
        return(FALSE)
    TRUE
}
dif.hs.range1_1d <- function(r1, r2) {
    if (length(r1)) {
        if (is_overlapping.hs.range1(as.vector(r1), as.vector(r2))) {
            rbind(if (r1[1] < r2[1]) 
                c(r1[1], r2[1]), if (r1[2] > r2[2]) 
                c(r2[2], r1[2]))
        }
        else r1
    }
}
dif.hs.range_1d <- function(r1, r2) {
    if (length(r1)) {
        if (is.vector(r1)) 
            r1 <- t(r1)
        if (length(r2)) {
            r2 <- reduced.hs.range_1d(r2)
            r2 <- r2[order(r2[, 1]), , drop = 0]
            a1 <- hs.apply(r1, 1, function(r1) {
                r2 <- r2[hs.filter(r2[, 1], function(x1) x1 >= r1[1], r2[, 2], function(x1) x1 <= r1[2]), , drop = 0]
                t(matrix(c(if (r1[1] < r2[1]) c(r1[1], r2[1]), if (nrow(r2) > 1) t(r2)[seq.int(2, length(r2) - 1)], if (r1[2] > r2[length(r2)]) c(r2[length(r2)], r1[2])), 2))
            })
            do.call("rbind", a1)
        }
        else r1
    }
}
cor.wt <- function(d, w = rep(1, nrow(d))/nrow(d), rank = 0, nt = 1) {
    w <- w/sum(w)
    if (rank) {
        d <- matrix(rank(d), nrow(d))
    }
    M <- apply(d, 2, function(d1) {
        sum(d1 * w)
    })
    N <- ncol(d)
    jg <- matrix(1, N, N)
    if (nt > 1) {
        tri <- vector("double", N * (N - 1)/2)
        combination <- combn(N, 2)
        tri <- seq.int(ncol(combination)) %=>% wk.mclapply(x, . %=>% {
            i1 <- combination[1, x]
            i2 <- combination[2, x]
            m1 <- M[i1]
            m2 <- M[i2]
            (sum(d[, i1] * d[, i2] * w) - m1 * m2)/sqrt((sum(d[, i1]^2 * w) - m1^2) * (sum(d[, i2]^2 * w) - m2^2))
        }, mc.cores = nt) %=>% unlist
        hs.browser("37.10.11.061326")
        lower
    }
    else {
        for (i1 in seq.int(N - 1)) {
            m1 <- M[i1]
            for (i2 in seq.int(i1 + 1, N)) {
                m2 <- M[i2]
                c1 <- (sum(d[, i1] * d[, i2] * w) - m1 * m2)/sqrt((sum(d[, i1]^2 * w) - m1^2) * (sum(d[, i2]^2 * w) - m2^2))
                jg[i1, i2] <- jg[i2, i1] <- c1
            }
        }
    }
    rownames(jg) <- colnames(jg) <- colnames(d)
    jg
}
wk.least_common_multiple <- function(...) {
    n1 <- unique(c(...))
    jg <- n1[1]
    if (length(n1) > 1) {
        for (i1 in 2:length(n1)) {
            jg <- pracma::Lcm(jg, n1[i1])
        }
    }
    jg
}
wk.integerRange.4cut <- function(v1, d1 = {
}, sep = "-", sep2 = ",") {
    v1 <- unique(sort(v1))
    if (length(v1) < 2) 
        return(v1)
    if (!length(d1)) 
        d1 <- diff(v1)
    tbl <- list(dif = hs.uniq(d1), first = hs.uniq(d1, "first"), last = hs.uniq(d1, "last")) %=>% wk.dt.as
    tbl <- tbl[dif == 1]
    if (tbl[, .N] == 0) 
        return(paste(v1, collapse = ","))
    tbl[, `:=`("last", last + 1)]
    c(tbl[1, first %=>% {
        if (x > 1) v1[seq.int(x - 1)]
    }], sapply(seq.int(tbl[, .N]), . %=>% {
        a1 <- tbl[x, v1[c(first, last)] %=>% unique %=>% paste(collapse = "-")]
        if (x < tbl[, .N]) {
            i1 <- tbl[x, last + 1]
            i2 <- tbl[x + 1, first - 1]
            if (i1 <= i2) a1 <- c(a1, paste(unique(v1[c(i1, i2)]), collapse = ","))
        }
        a1
    }) %=>% unlist) %=>% paste(collapse = ",")
}
formatSafe <- function(x, scientific = FALSE, trim = TRUE, ...) {
    format(x, scientific = scientific, trim = trim, ...)
}
wk.bigz <- function(n1) {
    if (inherits(n1, "character")) {
        return(gmp::as.bigz(n1))
    }
    if (is.infinite(n1)) 
        hs.browser("39.03.03.095309", debug = 0)
    if (n1 >= 100L) {
        n1 <- ceiling(log(n1, 10))
    }
    gmp::as.bigz(10)^n1
}
hs.median_i <- function(x) which.min(abs(x - median(x)))

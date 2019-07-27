# 实验目的

掌握R语言中Hexbin图的含义及绘制

# 实验原理

在本实验中，散点图的使用频率很高，它既可以描绘单变量也可以描绘多变量，但是我们需要注意的是：当数据量很大时，我们可能难以从散点图中发现数据的结构。比如说下图，是模拟随机生成10000组数的散点图，从中观察大量散点聚集区的数据结构就比较困难。这就是一个大数据问题。大数据问题一般需要不同的可视化方式。

# 算法源码
```
> rnorm
function (n, mean = 0, sd = 1) 
.Call(C_rnorm, n, mean, sd)

> lm
function (formula, data, subset, weights, na.action, method = "qr", 
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
    contrasts = NULL, offset, ...) 
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame") 
        return(mf)
    else if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) 
        stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y)) 
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                length(offset), NROW(y)), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
            3) else numeric(), residuals = y, fitted.values = 0 * 
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
            0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
            ...)
    }
    class(z) <- c(if (is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
    if (!qr) 
        z$qr <- NULL
    z
}

> graphics::plot
function (x, y, ...) 
UseMethod("plot")

> points
function (x, ...) 
UseMethod("points")

> hexbinplot
function (x, data, ...) 
UseMethod("hexbinplot")
```

# 实验步骤

点击屏幕上的图标rstudio，输入账户密码（均为guest),打开实验环境

模拟生成10000组数的散点图

```
> x<-rnorm(10000,mean=0,sd=100)
> y<-x+rnorm(10000,mean=0,sd=100)
> lr<-lm(y~x)
> plot(x,y)
> points(x, lr$coefficients[1] + lr$coefficients[2] * x,type = "l", col = 1)
```

![](/images/1-2-9-1_20171107075032.032.jpeg)

尽管我们可以运用颜色和透明度来解决这个问题，但是运用Hexbin图会更好。Hexbin图结合了散点图和直方图的特点。和散点图类似，Hexbin图也是在x，y坐标轴画图，第三个维度是运用阴影来描述数据的集中度。

下图就是用Hexbin图展示相同的数据，可以看出数据更加密集地聚集在聚集处的中心，大致沿着回归线分布。

```
> library(hexbin)
> hexbinplot(y ~ x,type=c("g", "r"))
```

![](/images/1-2-9-2_20171107075118.018.jpeg)

如下面的例子使用随机生成的大量数据绘制hexbin图：

```
> library(hexbin)
> mixdata <-
+     data.frame(x = c(rnorm(5000),rnorm(5000,4,1.5)),
+                y = c(rnorm(5000),rnorm(5000,2,3)),
+                a = gl(2, 5000))
> hexbinplot(y ~ x, mixdata, aspect = 1,
+            trans = sqrt, inv = function(x) x^2)
```

![](/images/1-2-9-3_20171107075209.009.jpeg)

```
> hexbinplot(y ~ x | a, mixdata)
```

![](/images/1-2-9-4_20171107075307.007.jpeg)
同样hexbinplot函数还可以绘制不同类型的图形：

```
> hexbinplot(y ~ x | a, mixdata, style = "lattice",
+            xbnds = "data", ybnds = "data")
```

![](/images/1-2-9-5_20171107075527.027.jpeg)
另外两个例子：

```
> hexbinplot(y ~ x | a, mixdata, style = "nested.centroids")
> hexbinplot(y ~ x | a, mixdata, style = "nested.centroids",
+            border = FALSE, type = c("g", "smooth"))
```

![](/images/1-2-9-6_20171107075631.031.jpeg)
![](/images/1-2-9-7_20171107075744.044.jpeg)
## Logistic regression using glm applied to s2w, s2u data
## created on 2019/MM/DD
## modified on 2020/02/26, 29, 03/04, 06, 09
## by Kow Kuroda

# Parameters ----

debugged <- F
#run.conversion <- F # likely to be offensive
validate.values <- T
get.landscape <- F
save.data <- F
save.plot <- F
use.complex <- F
use.s2u <- F
use.filtered <- T
loose.filter <- F
A.as.1 <- F
Y.as.UNA <- T
#
try.new <- F
#
clust.method <- "X-means"
n.clust <- 4
#
sans.fn <- "Lucida Sans Unicode"

# Settings -----

options(digits = 5, scipen = 100)
set.seed(12345)
par.old <- par
par(mar = c(5,6,5,6), family = eval(sans.fn), cex = .9, xpd = T)

# packages and parameters ----

require(readxl)
require(ggplot2)
require(clusternor)
#require(glm) # no need to import
require(MASS) # provides kde2d(..)
# graphics
require(RColorBrewer)
n.cols <- 11
my.cols <- rev(brewer.pal(n.cols, "RdYlBu"))

## directories ----
#rootdir <- "~/Dropbox/ARDJ"
#maindir <- paste(rootdir, "results", sep = "/")
#workdir <- paste(maindir, "ARDJ-responses", sep = "/")
#setwd(workdir)
#list.files()

## source selection ----
#if (use.s2u) {
#   d.type <- "s2u"
#   d.file <- "response-base-s2u.xlsx"
#   if (use.filtered) {
#      if (loose.filter) { d.sheet <- "s2u.sd.filtered2"
#      } else { d.sheet <- "s2u.sd.filtered1" }
#   } else { d.sheet <- "s2u.unfiltered" }
#} else {
#   d.type <- "s2w"
#   d.file <- "response-base-s2w.xlsx"
#   if (use.filtered) { d.sheet <- "s2w.r1.filtered"
#   } else { d.sheet <- "s2w.r1" }
#}
#d.file
#d.sheet
# 
## list available files
#candidate.files <- list.files()
#if (verbose) print(candidate.files)
#if (!d.file %in% candidate.files) {
#   print(sprintf("not found %s: stopped", d.file))
#   break
#} else {
#   print(sprintf("read %s", d.file))
#}

## load data ----
#s2x.resp.raw <- read_excel(d.file, sheet = d.sheet)
#s2x.resp <- data.frame(subset(s2x.resp.raw, S.ID > 0),
#                       row.names = 1) # remove extra rows
#str(s2x.resp)
#if (debugged) View(s2x.resp)

#write.csv(s2x.resp, "data-s2w-sd.filtered1.csv", row.names = F)

## aliasing ----
#d.resp <- s2x.resp
#if (debugged) View(d.resp)

data.fn <- "table-s2w-sd-filtered1.csv"
d.resp <- read.csv(data.fn, header = T)
View(d.resp)

# Filtering ----
range.names <- c("r01", "r12", "r23", "r3x", "r02", "r13", "r2x")
proper.range.names <- range.names[1:4]

if (validate.values) {
   for (name in range.names) {
      print(sprintf("validating: %s", name))
      d.resp[[name]] <- as.numeric(d.resp[[name]])
   }
   str(d.resp)
}
if (debugged) View(d.resp)

## Define predictors and predicted ----
predictors <- d.resp[range.names] # selects relevant variables only
str(predictors)
# checking validity
rowSums(predictors[proper.range.names])
#
range.pat <- "^r[0-3][1-3x]$"
cond.pat <- "^A[0-9]r?[0-9]?$"
#united.pat <- sprintf("%s|%s", range.pat, cond.pat)
pred.pat <- sprintf("%s", cond.pat)
predicted <- d.resp[grepl(pred.pat, colnames(d.resp))]
str(predicted)

### clustering of predictors ----

n.clust <- 5
clust.method <- "FuzzyCMeans"
if (clust.method == "X-means") {
   predictors.clustering <- Xmeans(as.matrix(predictors), kmax = n.clust)
} else {
   clust.method <- "FuzzyCMeans"
   predictors.clustering <- FuzzyCMeans(as.matrix(predictors), centers = n.clust)
}
str(predictors.clustering)

#cluster.id <- data.frame(
##cbind.data.frame(rownames(resp), cluster.id = resp.clustering$cluster),
## the above fails
##cbind.data.frame(rownames(resp), cluster.id = resp.clustering$cluster),
## or alternatively
#cbind.data.frame(cluster.id = predictors.clustering$cluster, row.names = 1)
clustered.predictors <- cbind(cluster = predictors.clustering$cluster,
                              predictors)
str(clustered.predictors)

k.value <- length(unique(clustered.predictors$cluster))
k.value

# 
#cluster.id <- clustered.predictors$cluster
#cluster.id <- clustered.predictors[[cluster]] # loses row.names
cluster.id <- clustered.predictors[ ,which("cluster" == colnames(clustered.predictors)), drop = F]

# Define conditions ----

conditions <- Filter(function(x) grepl("A[0-9]+r?[0-9]?$", x),
                  colnames(d.resp))
print(conditions)

## Run regression for each condition ----

# define formulae ----

form.simple <- as.formula(prediction ~ r01 + r12 + r23 + r3x)
form.complex <- as.formula(prediction ~ r01 + r12 + r23 + r3x + r02 + r13 + r2x)

# formula selection ----
if (use.complex) {
   form.to.use <- form.complex
} else { form.to.use <- form.simple }
print(sprintf("selected formula: %s", deparse(form.to.use)))

# run a batch of glm fittings ----
for (condition in conditions) {
   print(sprintf("Checking %s (A=1: %s; Y=0: %s)",
                 condition, A.as.1, Y.as.UNA))
   target <- predicted[[condition]] # dealing with matrix
   if (debugged) print(target)
   if (A.as.1) { # makes no significance results
      prediction <- ifelse(target == "A",1,0)
   } else { # UNA.as.0
      if (Y.as.UNA) { # produce significant results
         prediction <- ifelse(target == "UNA",0,
                              ifelse(target == "Y",0,1))
      } else { # produce significant results
         prediction <- ifelse(target == "UNA",0,1)
      }
   }
   if (debugged) print(prediction)
   #glm.fit <- glm(form.to.use, family = binomial)
   glm.fit <- glm(form.to.use, family = binomial, data = predictors)
   print(summary(glm.fit))
   if (save.data) {
      # set flags
      if (use.filtered) { filter.flag <- "filtered"
      } else { filter.flag <- "unfiltered" }
      if (A.as.1) { A.flag <- "A=1"
      } else { A.flag <- "UNA=0" }
      if (Y.as.UNA) { Y.flag <- "Y=0"
      } else { Y.flag <- "Y=1" }
      #require(broom)
      #output.fn <- sprintf("out-glm-%s-%s-%s-%s.txt",
      #                     d.type, filter.flag, A.flag, Y.flag)
      #write.table(glance(summary(glm.fit)), output.fn)
      require(prettyR)
      #write.table(delim.table(summary(glm.fit)), output.fn)
      #dput(summary(glm.fit), output.fn)
      require(apaTables)
      output.fn <- sprintf("out-glm-%s-%s-%s-%s.rtf",
                           d.type, filter.flag, A.flag, Y.flag)
      apa.reg.table(data = summary(glm.fit), filename = output.fn)
      #apa.reg.table(summary(glm.fit), variables = NULL, number = "1",
      #               title = " title ",
      #               filename = output.fn, note = NULL,
      #               landscape = FALSE, save = TRUE, type = "wide")
   }
}

## Predictions by model ----
print("Generate predicted values")
colnames(predicted)

# set target ----
c.names <- c("A4r1", "A6", "A6r1")
c.name <- c.names[3]
if (A.as.1) { 
   prediction <- ifelse(predicted[[c.name]] == "A",1,0)
} else {
   if (Y.as.UNA) {
      prediction <- ifelse(predicted[[c.name]] == "UNA",0,
                     ifelse(predicted[[c.name]] == "Y",0,1))
   } else {
      prediction <- ifelse(predicted[[c.name]] == "UNA",0,1)
   }
}
table(prediction)

# generate model ----
specfic.glm.fit <- glm(form.simple, data = predictors, family = binomial,
                   control = glm.control(maxit = 100))
specfic.glm.fit

# generate fitted values ----

if (debugged) names(specfic.glm.fit)
fitted.values <- specfic.glm.fit$fitted.values # modified plurality for compatibility
if (debugged) str(fitted.values)

# simple plot
if (debugged) {
   main.text <- sprintf("Plot of fitted values (sorted) [Y.as.UNA = %s]",
                        Y.as.UNA)
   plot(sort(fitted.values), main = main.text)
}

# clustered version of fitted.value ----

#fitted.values.x <- cbind.data.frame(fitted.value, cluster.id)
fitted.values.x <- cbind(fitted.values,
                                cluster.id, stringAsFactors = T)
str(fitted.values.x)
if (debugged) View(fitted.values.x)

# plot with list
#x <- reorder(ext.fitted.value, sort(ext.fitted.value$fitted.value))
#x <- ext.fitted.value[sort(ext.fitted.value$fitted.value), ] # not work
reordered.values <- fitted.values.x[order(fitted.values.x$fitted.values), ]
str(reordered.values)
main.text = sprintf("Plot of fitted values (sorted) [Y.as.UNA: %s, data type: %s; filtered: %s]",
                    Y.as.UNA, d.type, use.filtered)
plot(reordered.values$fitted.value, main = main.text)
#y <- reorder(ext.fitted.value$cluster.id, x)
#str(ext.fitted.value$cluster.id)
#levels(fitted.values.x$cluster.id)
cluster.list <- sort(unique(fitted.values.x$cluster))
print(cluster.list)

# plot
main.text <- sprintf("Plot of predicted values (generated by %s) with %s clusters
                     by %s [Y.as.UNA: %s, data type: %s, filtered: %s]",
                     c.name, k.value, clust.method, Y.as.UNA, d.type, use.filtered)
par(mfrow = c(1,1), cex = 0.8)
plot(reordered.values$fitted.value,
     main = main.text,
     ylab = "fitted value", xlab = "sentence (sorted by fitted value)", 
     col = reordered.values$cluster)
legend("topright", inset = c(-0.15,-0), pch = "o",
       title = "cluster",
       legend = cluster.list, col = cluster.list)
#text(reordered.fitted.value$fitted.value,
#     label = rownames(reordered.fitted.value), cex = 0.5, pos = 1)

## plot PCA ----

predictors.pca <- prcomp(predictors)
plot(predictors.pca$x, col = predictors.clustering$cluster,
     main = sprintf("PCA of responses (scaled) to sentences with %s clusters
                    by %s [Y.as.UNA: %s, data type: %s, filtered: %s]",
                    k.value, clust.method, Y.as.UNA, d.type, use.filtered))
text(predictors.pca$x, col = "grey", labels = rownames(predictors.pca$x),
     cex = 0.55, pos = 1)
legend("topright", inset = c(-0.15,0), title = "cluster",
       legend = cluster.list, pch = "o", col = cluster.list)
# add contours
contour.d = kde2d(predictors.pca$x[ ,1], predictors.pca$x[ ,2], n = 20)
contour(contour.d, drawlabels = T, add = T,
        nlevels = n.cols, col = my.cols)

## plot landscape ----
if (get.landscape) {
   persp(contour.d, theta = -45, phi = 45,
      xlab = "PC1", ylab = "PC2", zlab = "density")
}


## outputs ----

if (save.data) {
   out.file <- "s2w.filtered.logistic.fitted.values.tsv"
   write.table(fitted.values, out.file, quote = F, sep = "\t")
}


## end of script


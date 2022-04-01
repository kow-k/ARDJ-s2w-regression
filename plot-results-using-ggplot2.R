## plot glm fitting results using ggplot2

require(ggplot2)

debugged <- T

## only effective with fitted.values generated through
## run-logistic-regression.R

# plot with data.frame
values.df <- as.data.frame(fitted.values)
if (debugged) View(values.df)

## plot of full data ----
# takes long
sid <- reorder(rownames(values.df), fitted.values)
sid
ggplot(values.df,
       aes(x = fitted.values, y = sid)
       #aes(y = fitted.values, x = sid)
) + geom_point()

## plot of full data: alternative ----
# takes long
ggplot(values.df,
       #aes(y = fitted.values, x = sid)) +
       aes(x = fitted.values, y = sid)) +
   geom_point() +
   theme(axis.text.x = element_text(angle = 60, hjust = 1),
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_line(colour = "grey60",
                                           linetype = "dashed")
   )

# plot of samples
if (debugged) View(values.df)

## sampling by skipping ----

sample.rowns <- rownames(values.df)[rep(c(F,F,T),100)] # picks up every 3rd row
sample.rowns
## I had much difficulty in the following task
## because df[list, ] loses rownames of df, which is totally stupid.
#sample.values.df <- values.df[sample.rowns, ] # loses rownames! stupid!!
#sample.values.df <- values.df[rowns(values.df) %in% sample.rowns, ])
#sample.values.df <- subset(values.df, rownames(values.df) == sample.rowns) # too few
sample.values.df <- subset(values.df,
                          rownames(values.df) %in% sample.rowns)
class(sample.values.df)
nrow(sample.values.df)

# unsorted
plot(sample.values.df$fitted.value, main = "Unsorted")

#sorted.sample.values.df <- sample.values.df[order(sample.values.df$fitted.value), ]
#plot(sorted.sample.values.df$fitted.value)
#View(sorted.sample.values.df)
#class(sorted.sample.values.df)

## drop = F is crucial !
## but ineffective with ggplot
df1 <- sample.values.df[order(sample.values.df$fitted.values), ,
                       drop = F]
rownames(df1)

# sorted
plot(df1$fitted.values, main = "Sorted")
#
unflip.axes <- F
ggplot(df1,
       #aes(x = fitted.value, y = rownames(df) # doesn't work!
       # crucially the following:
       if (unflip.axes) {
         aes(y = fitted.values, x = reorder(rownames(df1),
                                           fitted.values))
       } else { # to flip axes
         aes(x = fitted.values, y = reorder(rownames(df1),
                                          fitted.values))
      }) + geom_point()

# random sampling ----
sample.rowns <- sample(rownames(values.df), 100)
sample.rowns
#sample.values.df <- values.df[sample.rid, ] # loses rownames
sample.values.df2 <- subset(values.df, rownames(values.df) %in% sample.rowns)
class(sample.values.df2)
if (debugged) View(sample.values.df2)
#
#sid <- order(rownames(sample.values.df2), fitted.values) # likely to cause an error
sid <- order(rownames(sample.values.df2), sample.values.df2$fitted.values)
sid
#
ggplot(sample.values.df2,
       #aes(x = fitted.value, y = rownames(df) # doesn't work!
       # crucially the following:
       if (unflip.axes) {
          aes(y = reorder(rownames(sample.values.df2), fitted.values),
              x = fitted.values)
       } else {
          aes(x = fitted.values,
              y = reorder(rownames(sample.values.df2), fitted.values))
       }) + geom_point()

## Real fitting to new data
#
if (try.new) {
   pred <- predict(fit, type = "response", newdata = X.data)
   plot(sort(t(pred)))
   colnames(pred) <- c("value")
   pred$sid <- row.names(pred)
   
   # plot
   pred.sorted <- sort(pred)
   dev.off()
   plot(pred.sorted)
   identify(pred.sorted)
   text(pred.sorted, labels = names(pred.sorted), cex = 0.7)
   
   ## plot using ggplot2
   require(ggplot2)
   pred$sid <- row.names(pred)
   
   # plot
   ggplot(pred, aes(x = value, y = reorder(sid, value))) + geom_point()
   
   # alternative
   ggplot(pred, aes(y = value, x = reorder(sid, value))) +
      geom_point() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour = "grey60", linetype = "dashed"))
   
   # plot of sample
   pred.sampled <- subset(pred, rep(c(F,F,T))) # picks up every 3rd row
   ggplot(pred.sampled, aes(x = value, y = reorder(sid, value))) + geom_point()
}

### end of script

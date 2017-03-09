# R Script to create volcano plot
# This is useable with any data frame
library("DESeq2") # if using DESeq2
library("RColorBrewer")
library("ggplot2")


# To use with DESeq2, do the following:
# #
# log-normalize, or use vsd
# # DESeq2Obj_transformed <- rlog(DESeq2Obj, blind=FALSE)
# then convert from object to matrix using assay
# # matrix_exampleDataFrame <- assay(DESeq2Obj_transformed)
# finally create data frame from matrix
# # exampleDataFrame <- as.data.frame(matrix_exampleDataFrame)
# NOTE that you will likely next have to do sorting, like pval < 0.05
# #


# Start, assumes you have a data frame of
# expression,pvalue,log2FoldChange,log10(padj),other,other,...
nm <- rownames(exampleDataFrame)
exampleDataFrame[,"names"] <- nm


# To create a factor for the sorted pvalues
# add column of 0 1 for TrtCtrl pvalue
# then add nested if to find pval < 0.05 in both data sets
exampleDataFrame$pTrtCtrl <- ifelse(df_res_b31_TrtCtrl$padj < 0.05, 1, 0)
exampleDataFrame$pRegnTrt <- ifelse(df_res_b31_Regrn_Trt$padj < 0.05, 1, 0)
# also can filter for both conditions pvalue
exampleDataFrame$pBoth <- ifelse(exampleDataFrame$pTrtCtrl == 1, ifelse(exampleDataFrame$pRegnTrt == 1, 1, 0), 0)
# optional sorting step
exampleDataFrame_sorted <- exampleDataFrame[order(exampleDataFrame$pBoth, decreasing = TRUE),]
# This step all in one:
# Either use a string as a factor--the first line and second--or use a numeric factor and label in ggplot2--use only the first line
exampleDataFrame$colorflags <- ifelse(exampleDataFrame$padj < 0.05, ifelse(exampleDataFrame$log2FoldChange > 1, 1, ifelse(exampleDataFrame$log2FoldChange < -1, -1, 0)),0)
exampleDataFrame$cflags <- ifelse(exampleDataFrame$colorflags == -1, "down", ifelse(exampleDataFrame$colorflags == 0, "notsignificant", ifelse(exampleDataFrame$colorflags == 1, "up", 0)))

# create Volcano Plot
p <- ggplot(exampleDataFrame, aes(x=log2FoldChange, y=-log10(padj), colour=cflags)) + geom_point(shape=21)
p <- p + geom_vline(xintercept=-1) + geom_vline(xintercept = 1) + geom_hline(yintercept = 1.3) + scale_x_continuous(limits=c(-26,26)) + xlab("Log2 Fold Change") + ylab("-log10 p-value") + geom_hline(yintercept = 1.3) + scale_x_continuous(limits=c(-26,26)) + xlab("Log2 Fold Change") + ylab("-log10 p-value")
p <- p + scale_color_manual(values=c(notsignificant="black", up="red", down="blue"))


# Create PCA Plot
# note that you need 2 groups or the plot will be odd or meaningless
# # such as treated counts and control counts
ggplot(dataFrame, aes(PC1, PC2, color=xVarColumn, shape=yVarColumn)) + geom_point(size=3) + xlab(paste0("PC1: ",percentVar[1],"% variance")) + ylab(paste0("PC2: ", percentVar[2],"% variance"))


# Create Heatmap
treatmentFactor <- ifelse(exampleDataFrame$Condition == "Treatment1", 1, ifelse(t$Condition == "Treatment2", 2, ifelse(t$Condition == "Control", 3, ifelse(t$Condition == "Treatment3", 4, -1))))
exampleDataFrame$CFactor <- treatmentFactor
# create a color palette that ggplot sources, note there are two sets of values
# # so you will need to provide a second set of values when the plot is created
colors <- colorRampPalette(c("lightblue", "green", "red"))(3)
p <- ggplot(exampleDataFrame, aes(x=Condition, y=Gene, fill=Count))
# a final color plot will be used for the colors in the heatmap
p + geom_tile() + scale_x_discrete() + scale_fill_gradientn(colors=c("blue", "darkred"))



# view plot in RStudio
p

# view plot in R
p + scale_color_manual(values=c(notsignificant="black", up="red", down="blue"))
# or
print(p)
# save to png, NOT using RStudio, or RStudio with Linux/Windows with RStudio
ggsave("exampleDataFramePlot.png", width=32, height=32, units="cm")
# save to png using RStudio with MacOSX
# MacOSX: View/Zoom, then take screenshot with Cmd-Ctrl-Shift-4, or Grab

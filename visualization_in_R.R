# IMPORTANT: This assumes you have 2 groups, 3 samples
# and the same Gene list G for all groups and samples, for example:
# Gene Sample Group Expression
# Gene1      1     A  0.5137255
#  Gene1      2     A  0.8946849
#  Gene1      3     A  0.1910234
#  Gene2      1     A  0.6132183
#  Gene2      2     A  0.8249944
#  Gene2      3     A  0.3590501
#  Geen3      1     A  0.5804939
#  Gene3      2     A  0.1091862
#  Gene3      3     A  3.4992056
# Gene1      1     B  2.3453736
# Gene1      2     B  1.8809525
# Geen1      3     B  0.5960169
# Gene2      1     B  1.1647344
# Geen2      2     B  3.3823561
# Gene2      3     B  1.3459597
# Gene3      1     B  2.9756238
# Geen3      2     B  2.7968380
# Gene3      3     B  3.9710568

# replace "tdata" with the name of your dataframe
# replace the Group name and Sample Name with the
# respective names from the the data frame columns

working_df <- tdata

# separate into groups
df_GroupA <- working_df[working_df$Group == "A", ] # <--- replace group name here
df_GroupB <- working_df[working_df$Group == "B", ] # <--- and here
df_GroupA_sample1 <- df_GroupA[df_GroupA$Sample == 1, ] # <--- Replace Sample name here
df_GroupA_sample2 <- df_GroupA[df_GroupA$Sample == 2, ] # <--- And here
df_GroupA_sample3 <- df_GroupA[df_GroupA$Sample == 3, ] # <--- And here

df_GroupB_sample1 <- df_GroupB[df_GroupB$Sample == 1, ] # <--- And here
df_GroupB_sample2 <- df_GroupB[df_GroupB$Sample == 2, ] # <--- And here
df_GroupB_sample3 <- df_GroupB[df_GroupB$Sample == 3, ] # <--- And here

# IMPORTANT: verify here that the gene order is the same!
head(df_GroupA_sample1)
tail(df_GroupA_sample1)
head(df_GroupB_sample2)
tail(df_GroupB_sample2)
head(df_GroupB_sample3)
tail(df_GroupB_sample3)

# order in columns is sample 1,2,3
df_GroupA_samples <- as.data.frame(df_GroupA_sample1[ ,c("Gene", "Expression")])
df_GroupA_samples$Exprn2 <- df_GroupA_sample2[ ,c("Expression")]
df_GroupA_samples$Exprn3 <- df_GroupA_sample3[ ,c("Expression")]
colnames(df_GroupA_samples) <- c("GeneName", "Exprn1", "Exprn2", "Exprn3")
nm <- as.character(df_GroupA_samples[,1])
df_GroupA_sampleVals <- as.data.frame(df_GroupA_samples[2:4], row.names = nm)

df_GroupA_sampleVals$meanVals <- apply(df_GroupA_sampleVals, 1, function(x) {mean(x, na.rm = TRUE) })
log2vals <- log2(df_GroupA_sampleVals$meanVals)
df_GroupA_sampleVals$log2Vals <- log2vals

# order in columns is sample 1,2,3
df_GroupB_samples <- as.data.frame(df_GroupB_sample1[ ,c("Gene", "Expression")])
df_GroupB_samples$Exprn2 <- df_GroupB_sample2[ ,c("Expression")]
df_GroupB_samples$Exprn3 <- df_GroupB_sample3[ ,c("Expression")]
colnames(df_GroupB_samples) <- c("GeneName", "Exprn1", "Exprn2", "Exprn3")
nm <- as.character(df_GroupB_samples[,1])
df_GroupB_sampleVals <- as.data.frame(df_GroupB_samples[2:4], row.names = nm)

df_GroupB_sampleVals$meanVals <- apply(df_GroupB_sampleVals, 1, function(x) {mean(x, na.rm = TRUE) })
log2vals <- log2(df_GroupB_sampleVals$meanVals)
df_GroupB_sampleVals$log2Vals <- log2vals

log2vals_sample1 <- df_GroupA_sampleVals$log2Vals
ggplot_df <- as.data.frame(log2vals_sample1)
ggplot_df$groupBlog2vals <- df_GroupB_sampleVals$log2Vals
colnames(ggplot_df) <- c("log2SampleA", "log2SampleB")
rownames(ggplot_df) <- nm

# parsed values are log2 values of the genes in the data frame ggplot_df
# log2SampleA log2SampleB
# gene1_log2_sampleA  gene1_log2_sampleB
# gene2_log2_sampleA  gene2_log2_sampleB
# gene3_log2_sampleA  gene3_log2_sampleB
# gene4_log2_sampleA  gene4_log2_sampleB

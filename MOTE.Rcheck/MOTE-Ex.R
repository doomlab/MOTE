pkgname <- "MOTE"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "MOTE-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('MOTE')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("apa")
### * apa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: apa
### Title: Format numbers for APA-style reporting
### Aliases: apa

### ** Examples

apa(0.54674, decimals = 3, leading = TRUE)   # "0.547"
apa(c(0.2, 1.2345, -0.04), decimals = 2)     # "0.20" "1.23" "-0.04"
apa(matrix(c(0.12, -0.9, 2.3, 10.5), 2), decimals = 1, leading = FALSE)
# returns a character matrix with ".1", "-.9", "2.3", "10.5"



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("apa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_dep_t_avg")
### * d_dep_t_avg

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_dep_t_avg
### Title: Cohen's d for Paired t Using the Average SD Denominator
### Aliases: d_dep_t_avg d.dep.t.avg
### Keywords: cohen's d d_average dependent effect measures paired-sample
###   repeated size t-test

### ** Examples

# The following example is derived from the "dept_data" dataset included
# in the MOTE package.

# Suppose seven people completed a measure of belief in the supernatural
# before and after watching a sci-fi movie.
# Higher scores indicate stronger belief.

    t.test(dept_data$before, dept_data$after, paired = TRUE)

# You can type in the numbers directly, or refer to the
# dataset, as shown below.

    d_dep_t_avg(m1 = 5.57, m2 = 4.43, sd1 = 1.99,
                sd2 = 2.88, n = 7, a = .05)

    d_dep_t_avg(5.57, 4.43, 1.99, 2.88, 7, .05)

    d_dep_t_avg(mean(dept_data$before), mean(dept_data$after),
                sd(dept_data$before), sd(dept_data$after),
                length(dept_data$before), .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_dep_t_avg", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_dep_t_diff")
### * d_dep_t_diff

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_dep_t_diff
### Title: Cohen's d for Paired t Using the SD of Difference Scores
### Aliases: d_dep_t_diff d.dep.t.diff
### Keywords: cohen's d dependent effect measures repeated size t-test

### ** Examples

# Example derived from the "dept_data" dataset included in MOTE

# Suppose seven people completed a measure of belief in the supernatural
# before and after watching a sci-fi movie.
# Higher scores indicate stronger belief.

t.test(dept_data$before, dept_data$after, paired = TRUE)

# Direct entry of summary statistics:
d_dep_t_diff(mdiff = 1.14, sddiff = 2.12, n = 7, a = .05)

# Equivalent shorthand:
d_dep_t_diff(1.14, 2.12, 7, .05)

# Using raw data from the dataset:
d_dep_t_diff(mdiff = mean(dept_data$before - dept_data$after),
             sddiff = sd(dept_data$before - dept_data$after),
             n = length(dept_data$before),
             a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_dep_t_diff", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_dep_t_diff_t")
### * d_dep_t_diff_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_dep_t_diff_t
### Title: Cohen's d from t for Paired Samples Using the SD of Difference
###   Scores
### Aliases: d_dep_t_diff_t
### Keywords: dependent effect measures paired repeated sample size t-test

### ** Examples

# Example derived from the "dept_data" dataset included in MOTE

# Suppose seven people completed a measure before and after an intervention.
# Higher scores indicate stronger endorsement.

    scifi <- t.test(dept_data$before, dept_data$after, paired = TRUE)

# The t-test value was 1.43. You can type in the numbers directly,
# or refer to the dataset, as shown below.

    d_dep_t_diff_t(t_value = 1.43, n = 7, a = .05)

    d_dep_t_diff_t(t_value = scifi$statistic,
        n = length(dept_data$before), a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_dep_t_diff_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_dep_t_rm")
### * d_dep_t_rm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_dep_t_rm
### Title: Cohen's d for Paired t Controlling for Correlation (Repeated
###   Measures)
### Aliases: d_dep_t_rm
### Keywords: cohen's correlation d dependent effect measures paired-sample
###   repeated size t-test

### ** Examples

# Example derived from the "dept_data" dataset included in MOTE

    t.test(dept_data$before, dept_data$after, paired = TRUE)

    scifi_cor <- cor(dept_data$before, dept_data$after, method = "pearson",
                     use = "pairwise.complete.obs")

# Direct entry of summary statistics, or refer to the dataset as shown below.

    d_dep_t_rm(m1 = 5.57, m2 = 4.43, sd1 = 1.99,
               sd2 = 2.88, r = .68, n = 7, a = .05)

    d_dep_t_rm(5.57, 4.43, 1.99, 2.88, .68, 7, .05)

    d_dep_t_rm(mean(dept_data$before), mean(dept_data$after),
               sd(dept_data$before), sd(dept_data$after),
               scifi_cor, length(dept_data$before), .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_dep_t_rm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_effect")
### * d_effect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_effect
### Title: General interface for Cohen's d
### Aliases: d_effect

### ** Examples

# Paired/dependent t-test using average SD denominator
# These arguments will route d() to d_dep_t_avg()
d_effect(
  m1 = 5.57, m2 = 4.43,
  sd1 = 1.99, sd2 = 2.88,
  n = 7, a = .05,
  design = "dep_t_avg"
)

# You can also call the helper directly
d_dep_t_avg(
  m1 = 5.57, m2 = 4.43,
  sd1 = 1.99, sd2 = 2.88,
  n = 7, a = .05
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_effect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_ind_t")
### * d_ind_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_ind_t
### Title: Cohen's d for Independent Samples Using the Pooled SD
### Aliases: d_ind_t
### Keywords: between-subjects deviation effect independent pooled size
###   standard t-test

### ** Examples

# The following example is derived from the "indt_data" dataset
# included in MOTE.

# A forensic psychologist examined whether being hypnotized during recall
# affects how well a witness remembers facts about an event.

t.test(correctq ~ group, data = indt_data)

# Direct entry of summary statistics:
d_ind_t(m1 = 17.75, m2 = 23, sd1 = 3.30,
        sd2 = 2.16, n1 = 4, n2 = 4, a = .05)

# Equivalent shorthand:
d_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

# Using raw data from the dataset:
d_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
        mean(indt_data$correctq[indt_data$group == 2]),
        sd(indt_data$correctq[indt_data$group == 1]),
        sd(indt_data$correctq[indt_data$group == 2]),
        length(indt_data$correctq[indt_data$group == 1]),
        length(indt_data$correctq[indt_data$group == 2]),
        .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_ind_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_ind_t_t")
### * d_ind_t_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_ind_t_t
### Title: Cohen's d from t for Independent Samples (Pooled SD)
### Aliases: d_ind_t_t d.ind.t.t
### Keywords: deviation effect independent pooled size standard t-test

### ** Examples

# The following example is derived from the "indt_data" dataset in MOTE.
    hyp <- t.test(correctq ~ group, data = indt_data)

# Direct entry of the t-statistic and sample sizes:
    d_ind_t_t(t = -2.6599, n1 = 4, n2 = 4, a = .05)

# Using the t-statistic from the model object:
    d_ind_t_t(hyp$statistic, length(indt_data$group[indt_data$group == 1]),
              length(indt_data$group[indt_data$group == 2]), .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_ind_t_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_prop")
### * d_prop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_prop
### Title: Cohen's d (SMD) for Independent Proportions (Binary Outcomes)
### Aliases: d_prop d.prop

### ** Examples

d_prop(p1 = .25, p2 = .35, n1 = 100, n2 = 100, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_prop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_single_t")
### * d_single_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_single_t
### Title: Cohen's d for One-Sample t from Summary Stats
### Aliases: d_single_t d.single.t
### Keywords: effect mean one-sample population sample single size t

### ** Examples

# Example derived from the "singt_data" dataset included in MOTE.

# A school claims their gifted/honors program outperforms the national
# average (1080). Their students' SAT scores (sample) have mean 1370 and
# SD 112.7.

    gift <- t.test(singt_data$SATscore, mu = 1080, alternative = "two.sided")

# Direct entry of summary statistics:
    d_single_t(m = 1370, u = 1080, sd = 112.7, n = 14, a = .05)

# Equivalent shorthand:
    d_single_t(1370, 1080, 112.7, 14, .05)

# Using values from the t-test object and dataset:
    d_single_t(gift$estimate, gift$null.value,
               sd(singt_data$SATscore), length(singt_data$SATscore), .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_single_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_single_t_t")
### * d_single_t_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_single_t_t
### Title: Cohen's d from t for One-Sample t-Test
### Aliases: d_single_t_t d.single.t.t
### Keywords: effect one-sample single size t

### ** Examples

# A school has a gifted/honors program that they claim is
# significantly better than others in the country. The gifted/honors
# students in this school scored an average of 1370 on the SAT,
# with a standard deviation of 112.7, while the national average
# for gifted programs is a SAT score of 1080.

    gift <- t.test(singt_data$SATscore, mu = 1080, alternative = "two.sided")

# Direct entry of t-statistic and sample size:
    d_single_t_t(9.968, 15, .05)

# Equivalent shorthand:
    d_single_t_t(9.968, 15, .05)

# Using values from a t-test object and dataset:
    d_single_t_t(gift$statistic, length(singt_data$SATscore), .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_single_t_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_to_r")
### * d_to_r

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_to_r
### Title: r and Coefficient of Determination (R2) from d
### Aliases: d_to_r d.to.r
### Keywords: correlation effect size

### ** Examples


# The following example is derived from the "indt_data"
# dataset, included in the MOTE library.

# A forensic psychologist conducted a study to examine whether
# being hypnotized during recall affects how well a witness
# can remember facts about an event. Eight participants
# watched a short film of a mock robbery, after which
# each participant was questioned about what he or she had
# seen. The four participants in the experimental group
# were questioned while they were hypnotized. The four
# participants in the control group received the same
# questioning without hypnosis.

# Contrary to the hypothesized result, the group that underwent
# hypnosis were significantly less accurate while reporting
# facts than the control group with a large effect size, t(6) = -2.66,
# p = .038, d_s = -1.88.

     d_to_r(d = -1.88, n1 = 4, n2 = 4, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_to_r", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_z_mean")
### * d_z_mean

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_z_mean
### Title: Cohen's d for Z-test from Population Mean and SD
### Aliases: d_z_mean d.z.mean
### Keywords: effect size z-test

### ** Examples


# The average quiz test taking time for a 10 item test is 22.5
# minutes, with a standard deviation of 10 minutes. My class of
# 25 students took 19 minutes on the test with a standard deviation of 5.

d_z_mean(mu = 22.5, m1 = 19, sig = 10, sd1 = 5, n = 25, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_z_mean", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("d_z_z")
### * d_z_z

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: d_z_z
### Title: Cohen's d from z-statistic for Z-test
### Aliases: d_z_z d.z.z
### Keywords: effect size z-test

### ** Examples


# A recent study suggested that students (N = 100) learning
# statistics improved their test scores with the use of
# visual aids (Z = 2.5). The population standard deviation is 4.

# You can type in the numbers directly as shown below,
# or refer to your dataset within the function.

    d_z_z(z = 2.5, sig = 4, n = 100, a = .05)

    d_z_z(z = 2.5, n = 100, a = .05)

    d.z.z(2.5, 4, 100, .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("d_z_z", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("delta_ind_t")
### * delta_ind_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: delta_ind_t
### Title: d_{delta} for Between Subjects with Control Group SD Denominator
### Aliases: delta_ind_t delta.ind.t
### Keywords: delta effect independent size t

### ** Examples


# The following example is derived from the "indt_data"
# dataset, included in the MOTE library.

# A forensic psychologist conducted a study to examine whether
# being hypnotized during recall affects how well a witness
# can remember facts about an event. Eight participants
# watched a short film of a mock robbery, after which
# each participant was questioned about what he or she had
# seen. The four participants in the experimental group
# were questioned while they were hypnotized. The four
# participants in the control group received the same
# questioning without hypnosis.

    hyp <- t.test(correctq ~ group, data = indt_data)

# You can type in the numbers directly, or refer to the dataset,
# as shown below.

    delta_ind_t(m1 = 17.75, m2 = 23,
               sd1 = 3.30, sd2 = 2.16,
                n1 = 4, n2 = 4, a = .05)

    delta_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

    delta_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
            mean(indt_data$correctq[indt_data$group == 2]),
            sd(indt_data$correctq[indt_data$group == 1]),
            sd(indt_data$correctq[indt_data$group == 2]),
            length(indt_data$correctq[indt_data$group == 1]),
            length(indt_data$correctq[indt_data$group == 2]),
            .05)

# Contrary to the hypothesized result, the group that underwent hypnosis were
# significantly less accurate while reporting facts than the control group
# with a large effect size, t(6) = -2.66, p = .038, d_delta = 1.59.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("delta_ind_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("epsilon_full_ss")
### * epsilon_full_ss

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: epsilon_full_ss
### Title: epsilon^2 for ANOVA from F and Sum of Squares
### Aliases: epsilon_full_ss epsilon.full.SS
### Keywords: ANOVA effect epsilon size

### ** Examples


# The following example is derived from the "bn1_data"
# dataset, included in the MOTE library.

# A health psychologist recorded the number of close inter-personal
# attachments of 45-year-olds who were in excellent, fair, or poor
# health. People in the Excellent Health group had 4, 3, 2, and 3
# close attachments; people in the Fair Health group had 3, 5,
# and 8 close attachments; and people in the Poor Health group
# had 3, 1, 0, and 2 close attachments.

anova_model <- lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

epsilon_full_ss(dfm = 2, dfe = 8, msm = 12.621,
                mse = 2.458, sst = (25.24 + 19.67), a = .05)

# Backwards-compatible dotted name (deprecated)
epsilon.full.SS(dfm = 2, dfe = 8, msm = 12.621,
                mse = 2.458, sst = (25.24 + 19.67), a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("epsilon_full_ss", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("eta_f")
### * eta_f

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: eta_f
### Title: eta^2 and Coefficient of Determination (R^2) for ANOVA from F
### Aliases: eta_f eta.F
### Keywords: ANOVA effect eta size

### ** Examples


# The following example is derived from the "bn1_data"
# dataset, included in the MOTE library.

# A health psychologist recorded the number of close inter-personal
# attachments of 45-year-olds who were in excellent, fair, or poor
# health. People in the Excellent Health group had 4, 3, 2, and 3
# close attachments; people in the Fair Health group had 3, 5,
# and 8 close attachments; and people in the Poor Health group
# had 3, 1, 0, and 2 close attachments.

anova_model <- lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

eta_f(dfm = 2, dfe = 8,
      Fvalue = 5.134, a = .05)

# Backwards-compatible dotted name (deprecated)
eta.F(dfm = 2, dfe = 8,
      Fvalue = 5.134, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("eta_f", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("eta_full_ss")
### * eta_full_ss

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: eta_full_ss
### Title: eta^2 for ANOVA from F and Sum of Squares
### Aliases: eta_full_ss eta.full.SS
### Keywords: ANOVA effect eta size

### ** Examples


# The following example is derived from the "bn1_data"
# dataset, included in the MOTE library.

# A health psychologist recorded the number of close inter-personal
# attachments of 45-year-olds who were in excellent, fair, or poor
# health. People in the Excellent Health group had 4, 3, 2, and 3
# close attachments; people in the Fair Health group had 3, 5,
# and 8 close attachments; and people in the Poor Health group
# had 3, 1, 0, and 2 close attachments.

anova_model <- lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

eta_full_ss(dfm = 2, dfe = 8, ssm = 25.24,
            sst = (25.24 + 19.67), f_value = 5.134, a = .05)

# Backwards-compatible dotted name (deprecated)
eta.full.SS(dfm = 2, dfe = 8, ssm = 25.24,
            sst = (25.24 + 19.67), Fvalue = 5.134, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("eta_full_ss", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("eta_partial_ss")
### * eta_partial_ss

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: eta_partial_ss
### Title: eta^2_p for ANOVA from F and Sum of Squares
### Aliases: eta_partial_ss eta.partial.SS
### Keywords: ANOVA effect eta size

### ** Examples


# The following example is derived from the "bn2_data"
# dataset, included in the MOTE library.

# Is there a difference in athletic spending budget for different sports?
# Does that spending interact with the change in coaching staff?
# This data includes (fake) athletic budgets for baseball, basketball,
# football, soccer, and volleyball teams with new and old coaches
# to determine if there are differences in
# spending across coaches and sports.

# Example using reported ANOVA table values directly
eta_partial_ss(dfm = 4, dfe = 990,
               ssm = 338057.9, sse = 32833499,
               f_value = 2.548, a = .05)

# Example computing Type III SS with code (requires the "car" package)
if (requireNamespace("car", quietly = TRUE)) {

  # Fit the model using stats::lm
  mod <- stats::lm(money ~ coach * type, data = bn2_data)

  # Type III table for the effects
  aov_type3 <- car::Anova(mod, type = 3)

  # Extract DF, SS, and F for the interaction (coach:type)
  dfm_int <- aov_type3["coach:type", "Df"]
  ssm_int <- aov_type3["coach:type", "Sum Sq"]
  F_int   <- aov_type3["coach:type", "F value"]

  # Residual DF and SS from the standard ANOVA table
  aov_type1 <- stats::anova(mod)
  dfe <- aov_type1["Residuals", "Df"]
  sse <- aov_type1["Residuals", "Sum Sq"]

  # Calculate partial eta-squared for the interaction using Type III SS
  eta_partial_ss(dfm = dfm_int, dfe = dfe,
                 ssm = ssm_int, sse = sse,
                 f_value = F_int, a = .05)
#'
# Backwards-compatible dotted name (deprecated)
eta.partial.SS(dfm = 4, dfe = 990,
               ssm = 338057.9, sse = 32833499,
               Fvalue = 2.548, a = .05)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("eta_partial_ss", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("g_ind_t")
### * g_ind_t

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: g_ind_t
### Title: d_g Corrected for Independent t
### Aliases: g_ind_t g.ind.t
### Keywords: @keywords ANOVA correction effect independent omega size
###   size#' t

### ** Examples


# The following example is derived from the "indt_data"
# dataset, included in the MOTE library.

# A forensic psychologist conducted a study to examine whether
# being hypnotized during recall affects how well a witness
# can remember facts about an event. Eight participants
# watched a short film of a mock robbery, after which
# each participant was questioned about what he or she had
# seen. The four participants in the experimental group
# were questioned while they were hypnotized. The four
# participants in the control group received the same
# questioning without hypnosis.

   t.test(correctq ~ group, data = indt_data)

# You can type in the numbers directly, or refer to the dataset,
# as shown below.

    g_ind_t(m1 = 17.75, m2 = 23, sd1 = 3.30,
           sd2 = 2.16, n1 = 4, n2 = 4, a = .05)

    g_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)

    g_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
            mean(indt_data$correctq[indt_data$group == 2]),
            sd(indt_data$correctq[indt_data$group == 1]),
            sd(indt_data$correctq[indt_data$group == 2]),
            length(indt_data$correctq[indt_data$group == 1]),
            length(indt_data$correctq[indt_data$group == 2]),
            .05)

# Contrary to the hypothesized result, the group that underwent hypnosis were
# significantly less accurate while reporting facts than the control group
# with a large effect size, t(6) = -2.66, p = .038, d_g = 1.64.




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("g_ind_t", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ges_partial_ss_mix")
### * ges_partial_ss_mix

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ges_partial_ss_mix
### Title: eta^2_{G} (Partial Generalized Eta-Squared) for Mixed Design
###   ANOVA from F
### Aliases: ges_partial_ss_mix ges.partial.SS.mix
### Keywords: ANOVA effect ges size

### ** Examples


# The following example is derived from the
# "mix2_data" dataset, included in the MOTE library.

# Given previous research, we know that backward strength in free
# association tends to increase the ratings participants give when
# you ask them how many people out of 100 would say a word in
# response to a target word (like Family Feud). This result is
# tied to people’s overestimation of how well they think they know
# something, which is bad for studying. So, we gave people instructions
# on how to ignore the BSG.  Did it help? Is there an interaction
# between BSG and instructions given?

# You would calculate one partial GES value for each F-statistic.
# Here's an example for the interaction using reported ANOVA values.
ges_partial_ss_mix(dfm = 1, dfe = 156,
                   ssm = 71.07608,
                   sss = 30936.498,
                   sse = 8657.094,
                   f_value = 1.280784, a = .05)

# Backwards-compatible dotted name (deprecated)
ges.partial.SS.mix(dfm = 1, dfe = 156,
                   ssm = 71.07608,
                   sss = 30936.498,
                   sse = 8657.094,
                   Fvalue = 1.280784, a = .05)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ges_partial_ss_mix", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ges_partial_ss_rm")
### * ges_partial_ss_rm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ges_partial_ss_rm
### Title: eta^2_{G} (Partial Generalized Eta-Squared) for
###   Repeated-Measures ANOVA from F
### Aliases: ges_partial_ss_rm ges.partial.SS.rm
### Keywords: ANOVA effect ges size

### ** Examples


# The following example is derived from the "rm2_data" dataset, included
# in the MOTE library.

# In this experiment people were given word pairs to rate based on
# their "relatedness". How many people out of a 100 would put LOST-FOUND
# together? Participants were given pairs of words and asked to rate them
# on how often they thought 100 people would give the second word if shown
# the first word.  The strength of the word pairs was manipulated through
# the actual rating (forward strength: FSG) and the strength of the reverse
# rating (backward strength: BSG). Is there an interaction between FSG and
# BSG when participants are estimating the relation between word pairs?

# You would calculate one partial GES value for each F-statistic.
# Here's an example for the interaction with typing in numbers.
ges_partial_ss_rm(dfm = 1, dfe = 157,
                  ssm = 2442.948, sss = 76988.13,
                  sse1 = 5402.567, sse2 = 8318.75, sse3 = 6074.417,
                  f_value = 70.9927, a = .05)

# Backwards-compatible dotted name (deprecated)
ges.partial.SS.rm(dfm = 1, dfe = 157,
                  ssm = 2442.948, sss = 76988.13,
                  sse1 = 5402.567, sse2 = 8318.75, sse3 = 6074.417,
                  Fvalue = 70.9927, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ges_partial_ss_rm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("h_prop")
### * h_prop

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: h_prop
### Title: Cohen's h for Independent Proportions
### Aliases: h_prop h.prop

### ** Examples

h_prop(p1 = .25, p2 = .35, n1 = 100, n2 = 100, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("h_prop", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("odds_ratio")
### * odds_ratio

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: odds_ratio
### Title: Odds Ratio from 2x2 Table
### Aliases: odds_ratio odds
### Keywords: effect odds ratios size

### ** Examples


# A health psychologist was interested in the rates of anxiety in
# first generation and regular college students. They polled campus
# and found the following data:

  # |              | First Generation | Regular |
  # |--------------|------------------|---------|
  # | Low Anxiety  | 10               | 50      |
  # | High Anxiety | 20               | 15      |

# What are the odds for the first generation students to have anxiety?

odds_ratio(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = .05)

# Backwards-compatible wrapper (deprecated name)
odds(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("odds_ratio", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("omega_f")
### * omega_f

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: omega_f
### Title: omega^2 for ANOVA from F
### Aliases: omega_f omega.F
### Keywords: ANOVA effect omega size

### ** Examples


# The following example is derived from
# the "bn1_data" dataset, included in the MOTE library.

# A health psychologist recorded the number of close inter-personal
# attachments of 45-year-olds who were in excellent, fair, or poor
# health. People in the Excellent Health group had 4, 3, 2, and 3
# close attachments; people in the Fair Health group had 3, 5,
# and 8 close attachments; and people in the Poor Health group
# had 3, 1, 0, and 2 close attachments.

anova_model <- lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

omega_f(dfm = 2, dfe = 8,
        f_value = 5.134, n = 11, a = .05)

# Backwards-compatible dotted name (deprecated)
omega.F(dfm = 2, dfe = 8,
        Fvalue = 5.134, n = 11, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("omega_f", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("omega_full_ss")
### * omega_full_ss

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: omega_full_ss
### Title: omega^2 for One-Way and Multi-Way ANOVA from F
### Aliases: omega_full_ss omega.full.SS
### Keywords: ANOVA effect omega size

### ** Examples


# The following example is derived from the "bn1_data"
# dataset, included in the MOTE library.

# A health psychologist recorded the number of close inter-personal
# attachments of 45-year-olds who were in excellent, fair, or poor
# health. People in the Excellent Health group had 4, 3, 2, and 3
# close attachments; people in the Fair Health group had 3, 5,
# and 8 close attachments; and people in the Poor Health group
# had 3, 1, 0, and 2 close attachments.

anova_model <- lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

omega_full_ss(dfm = 2, dfe = 8,
              msm = 12.621, mse = 2.548,
              sst = (25.54 + 19.67), a = .05)

# Backwards-compatible dotted name (deprecated)
omega.full.SS(dfm = 2, dfe = 8,
              msm = 12.621, mse = 2.548,
              sst = (25.54 + 19.67), a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("omega_full_ss", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("omega_g_ss_rm")
### * omega_g_ss_rm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: omega_g_ss_rm
### Title: omega^2_G (Generalized Omega Squared) for Multi-Way and Mixed
###   ANOVA from F
### Aliases: omega_g_ss_rm omega.gen.SS.rm
### Keywords: ANOVA effect omega size

### ** Examples


# The following example is derived from the "mix2_data"
# dataset, included in the MOTE library.

# Given previous research, we know that backward strength in free
# association tends to increase the ratings participants give when
# you ask them how many people out of 100 would say a word in
# response to a target word (like Family Feud). This result is
# tied to people’s overestimation of how well they think they know
# something, which is bad for studying. So, we gave people instructions
# on how to ignore the BSG.  Did it help? Is there an interaction
# between BSG and instructions given?

# You would calculate one partial GOS value for each F-statistic.
# Here's an example for the main effect 1 with typing in numbers.
omega_g_ss_rm(dfm = 1, dfe = 156,
          ssm = 6842.46829,
          ssm2 = 14336.07886,
          sst = sum(c(30936.498, 6842.46829,
                      14336.07886, 8657.094, 71.07608)),
          mss = 30936.498 / 156,
          j = 2, f_value = 34.503746, a = .05)

# Backwards-compatible dotted name (deprecated)
omega.gen.SS.rm(dfm = 1, dfe = 156,
                ssm = 6842.46829,
                ssm2 = 14336.07886,
                sst = sum(c(30936.498, 6842.46829,
                            14336.07886, 8657.094, 71.07608)),
                mss = 30936.498 / 156,
                j = 2, Fvalue = 34.503746, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("omega_g_ss_rm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("omega_partial_ss_bn")
### * omega_partial_ss_bn

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: omega_partial_ss_bn
### Title: omega^2_p (Partial Omega Squared) for Between-Subjects ANOVA
###   from F
### Aliases: omega_partial_ss_bn omega.partial.SS.bn
### Keywords: ANOVA effect omega size

### ** Examples


# The following example is derived from the "bn2_data"
# dataset, included in the MOTE library.

# Is there a difference in athletic spending budget for different sports?
# Does that spending interact with the change in coaching staff?
# This data includes (fake) athletic budgets for baseball,
# basketball, football, soccer, and volleyball teams
# with new and old coaches to determine if there are differences in
# spending across coaches and sports.

# You would calculate one omega value for each F-statistic.
# Here's an example for the interaction using reported ANOVA values.
omega_partial_ss_bn(dfm = 4, dfe = 990,
                    msm = 338057.9 / 4,
                    mse = 32833499 / 990,
                    ssm = 338057.9,
                    n = 1000, a = .05)

# Backwards-compatible dotted name (deprecated)
omega.partial.SS.bn(dfm = 4, dfe = 990,
                    msm = 338057.9 / 4,
                    mse = 32833499 / 990,
                    ssm = 338057.9,
                    n = 1000, a = .05)

# The same analysis can be fit with stats::lm and car::Anova(type = 3).
# This example shows how to obtain the ANOVA table and plug its values
# into omega.partial.SS.bn without relying on ezANOVA.
if (requireNamespace("car", quietly = TRUE)) {

  mod <- stats::lm(money ~ coach * type, data = bn2_data)

  # Type I table (for residual SS and df)
  aov_type1 <- stats::anova(mod)

  # Type III SS table for the effects
  aov_type3 <- car::Anova(mod, type = 3)

  # Extract dfs and sums of squares for the interaction coach:type
  dfm_int <- aov_type3["coach:type", "Df"]
  ssm_int <- aov_type3["coach:type", "Sum Sq"]
  msm_int <- ssm_int / dfm_int

  dfe <- aov_type1["Residuals", "Df"]
  sse <- aov_type1["Residuals", "Sum Sq"]
  mse <- sse / dfe

  omega_partial_ss_bn(dfm = dfm_int,
                      dfe = dfe,
                      msm = msm_int,
                      mse = mse,
                      ssm = ssm_int,
                      n = nrow(bn2_data),
                      a = .05)
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("omega_partial_ss_bn", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("omega_partial_ss_rm")
### * omega_partial_ss_rm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: omega_partial_ss_rm
### Title: omega^2_p (Partial Omega Squared) for Repeated Measures ANOVA
###   from F
### Aliases: omega_partial_ss_rm omega.partial.SS.rm
### Keywords: ANOVA effect omega size

### ** Examples


# The following example is derived from the "rm2_data" dataset,
# included in the MOTE library.

# In this experiment people were given word pairs to rate based on
# their "relatedness". How many people out of a 100 would put LOST-FOUND
# together? Participants were given pairs of words and asked to rate them
# on how often they thought 100 people would give the second word if shown
# the first word.  The strength of the word pairs was manipulated through
# the actual rating (forward strength: FSG) and the strength of the reverse
# rating (backward strength: BSG). Is there an interaction between FSG and
# BSG when participants are estimating the relation between word pairs?

# You would calculate one partial GOS value for each F-statistic.
# You can leave out the MS options if you include all the SS options.
# Here's an example for the interaction with typing in numbers.
omega_partial_ss_rm(dfm = 1, dfe = 157,
                    msm = 2442.948 / 1,
                    mse = 5402.567 / 157,
                    mss = 76988.130 / 157,
                    ssm = 2442.948, sss = 76988.13,
                    sse = 5402.567, a = .05)

# Backwards-compatible dotted name (deprecated)
omega.partial.SS.rm(dfm = 1, dfe = 157,
                    msm = 2442.948 / 1,
                    mse = 5402.567 / 157,
                    mss = 76988.130 / 157,
                    ssm = 2442.948, sss = 76988.13,
                    sse = 5402.567, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("omega_partial_ss_rm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("r_correl")
### * r_correl

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: r_correl
### Title: r to Coefficient of Determination (R^2) from F
### Aliases: r_correl r.correl
### Keywords: correlation effect size

### ** Examples


# This example is derived from the mtcars dataset provided in R.

# What is the correlation between miles per gallon and car weight?

cor.test(mtcars$mpg, mtcars$wt)

r_correl(r = -0.8676594, n = 32, a = .05)

# Backwards-compatible dotted name (deprecated)
r.correl(r = -0.8676594, n = 32, a = .05)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("r_correl", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("r_effect")
### * r_effect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: r_effect
### Title: r-family effect size wrapper
### Aliases: r_effect

### ** Examples

# From Cohen's d for independent groups to r and R^2
r_effect(d = -1.88, n1 = 4, n2 = 4, a = .05, design = "d_to_r")
# From a sample correlation to r and R^2
r_effect(r = -0.8676594, n = 32, a = .05, design = "r_correl")
# From a chi-square test of association to Cramer's V
r_effect(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05, design = "v_chi_sq")
# From F and degrees of freedom to eta^2
r_effect(dfm = 2, dfe = 8, f_value = 5.134, a = .05, design = "eta_f")
# From F, degrees of freedom, and N to omega^2
r_effect(dfm = 2, dfe = 8, n = 11, f_value = 5.134,
a = .05, design = "omega_f")
# From sums of squares to omega^2
r_effect(
  dfm   = 2,
  dfe   = 8,
  msm   = 12.621,
  mse   = 2.548,
  sst   = (25.54 + 19.67),
  a     = .05,
  design = "omega_full_ss"
)
# From sums of squares to partial eta^2
r_effect(
  dfm    = 4,
  dfe    = 990,
  ssm    = 338057.9,
  sse    = 32833499,
  f_value = 2.548,
  a      = .05,
  design = "eta_partial_ss"
)
# From mixed-design sums of squares to partial generalized eta^2
r_effect(
  dfm     = 1,
  dfe     = 156,
  ssm     = 71.07608,
  sss     = 30936.498,
  sse     = 8657.094,
  f_value = 1.280784,
  a       = .05,
  design  = "ges_partial_ss_mix"
)

# From repeated-measures sums of squares to partial generalized eta^2
r_effect(
  dfm     = 1,
  dfe     = 157,
  ssm     = 2442.948,
  sss     = 76988.13,
  sse1    = 5402.567,
  sse2    = 8318.75,
  sse3    = 6074.417,
  f_value = 70.9927,
  a       = .05,
  design  = "ges_partial_ss_rm"
)

# From repeated-measures sums of squares to partial omega^2_p
r_effect(
  dfm   = 1,
  dfe   = 157,
  msm   = 2442.948 / 1,
  mse   = 5402.567 / 157,
  mss   = 76988.130 / 157,
  ssm   = 2442.948,
  sss   = 76988.13,
  sse   = 5402.567,
  a     = .05,
  design = "omega_partial_ss_rm"
)

# From repeated-measures sums of squares to generalized omega^2_G
r_effect(
  dfm     = 1,
  dfe     = 156,
  ssm     = 6842.46829,
  ssm2    = 14336.07886,
  sst     = sum(c(30936.498, 6842.46829,
                  14336.07886, 8657.094, 71.07608)),
  mss     = 30936.498 / 156,
  j       = 2,
  f_value = 34.503746,
  a       = .05,
  design  = "omega_g_ss_rm"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("r_effect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("v_chi_sq")
### * v_chi_sq

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: v_chi_sq
### Title: V for Chi-Square
### Aliases: v_chi_sq v.chi.sq
### Keywords: chi-square effect size

### ** Examples


# The following example is derived from the "chisq_data"
# dataset, included in the MOTE library.

# Individuals were polled about their number of friends (low, medium, high)
# and their number of kids (1, 2, 3+) to determine if there was a
# relationship between friend groups and number of children, as we
# might expect that those with more children may have less time for
# friendship maintaining activities.

chisq.test(chisq_data$kids, chisq_data$friends)

v_chi_sq(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05)

# Backwards-compatible dotted name (deprecated)
v.chi.sq(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("v_chi_sq", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')

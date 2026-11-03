# Starting estimate (prior) for the final margin: an adjusted average of NC-1 polls. Approved by ZH on 2026-11-03.
# Source: NYT poll file https://static01.nyt.com/newsgraphics/polls/house.csv (free with credit to The Times).
# Rule: head-to-head Davis v Buckhout questions with fieldwork ending on or after 2026-08-01; polls the file marks
# as partisan are moved 4.5 points toward the other party; polls are weighted by the square root of sample size.
# Writes data/baseline/prior_polls.csv. Run by hand; config.R holds the rounded result.
source(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])), "config.R"))
root <- needle_root()
x <- read.csv("https://static01.nyt.com/newsgraphics/polls/house.csv", stringsAsFactors = FALSE)
x <- subset(x, state == "NC" & seat_number == 1 & stage == "general")
x$end <- as.Date(x$end_date, "%m/%d/%y")
x <- subset(x, end >= as.Date("2026-08-01"))
q <- split(x, x$question_id)
q <- Filter(function(d) all(c("Don Davis", "Laurie Buckhout") %in% d$candidate_name), q)
p <- do.call(rbind, lapply(q, function(d) data.frame(
  pollster = d$pollster[1], sponsors = d$sponsors[1], partisan = d$partisan[1], end_date = d$end[1],
  n = d$sample_size[1],
  margin = d$pct[d$candidate_name == "Don Davis"] - d$pct[d$candidate_name == "Laurie Buckhout"])))
adj <- 4.5
p$adjusted <- p$margin - ifelse(p$partisan == "DEM", adj, ifelse(p$partisan == "REP", -adj, 0))
p$weight <- sqrt(p$n) / sum(sqrt(p$n))
p <- p[order(p$end_date, decreasing = TRUE), ]
write.csv(p, file.path(root, "data/baseline/prior_polls.csv"), row.names = FALSE)
print(p, row.names = FALSE)
cat(sprintf("Adjusted average: D%+.2f from %d polls; unadjusted: D%+.2f\n",
            sum(p$weight * p$adjusted), nrow(p), sum(p$weight * p$margin)))

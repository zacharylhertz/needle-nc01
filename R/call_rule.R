# Calling rules as pure functions. Each condition is reported separately; thresholds come from CFG.
# A "hold" tracks whether a pass has lasted across updates for at least hold_minutes.
hold_new <- function() list(pending_since = NA_real_, leader = NA_character_, called = FALSE, called_at = NA_real_,
                            called_leader = NA_character_, withdrawn_at = NA_real_)

# now in minutes or seconds-as-numeric; hold_len in the same unit.
hold_step <- function(h, now, pass, leader, hold_len) {
  ok <- isTRUE(pass) && !is.na(leader)
  if (!ok || (!is.na(h$leader) && !identical(h$leader, leader))) {
    if (h$called) { h$called <- FALSE; h$withdrawn_at <- now; h$called_leader <- NA_character_ }
    if (ok) { h$pending_since <- now; h$leader <- leader } else { h$pending_since <- NA_real_; h$leader <- NA_character_ }
    h$held <- FALSE
    return(h)
  }
  if (is.na(h$pending_since)) { h$pending_since <- now; h$leader <- leader }
  h$held <- (now - h$pending_since) >= hold_len
  if (h$held && !h$called) { h$called <- TRUE; h$called_at <- now; h$called_leader <- leader }
  h
}

# Original briefed rule: conditions before the hold.
orig_eval <- function(est, cfg, counted, lead_votes, ed_counties, prov_out, p_min = cfg$orig$p_min) {
  o <- cfg$orig
  leader_p <- if (est$p_davis >= 0.5) "Davis" else "Buckhout"
  p_lead <- max(est$p_davis, est$p_buckhout)
  count_leader <- if (lead_votes > 0) "Davis" else if (lead_votes < 0) "Buckhout" else NA_character_
  proj <- if (identical(count_leader, "Davis")) est$margin_mid else if (identical(count_leader, "Buckhout")) -est$margin_mid else 0
  c1 <- p_lead >= p_min
  c2 <- ed_counties >= o$ed_counties_min
  c3 <- est$pct_counted >= o$pct_counted_min
  c4 <- !is.na(count_leader) && identical(count_leader, leader_p) && abs(lead_votes) > prov_out && proj > o$projected_margin_min
  list(leader = count_leader, c1 = c1, c2 = c2, c3 = c3, c4 = c4, pass = c1 && c2 && c3 && c4,
       p_lead = p_lead, ed_counties = ed_counties, pct = est$pct_counted, lead_abs = abs(lead_votes), prov_out = prov_out, proj = proj)
}

# Cannot-catch-up conditions for one buffer. r is the output of ccu_test.
ccu_eval <- function(r, b_entry) {
  list(leader = r$leader, fav = b_entry$passed, lead_ok = b_entry$lead_ok, pass = isTRUE(b_entry$passed && b_entry$lead_ok),
       fav_lead = b_entry$fav_lead, lead_pts = abs(r$lead_pts), unstressed = r$unstressed, B = b_entry$B)
}

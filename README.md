# NC-1 needle

Reads North Carolina State Board of Elections (NCSBE) files on election night, estimates the final Davis minus Buckhout margin and win probability, tests a calling rule, and writes `data/live/state.json` for a separate static page.

## Running it

Election morning, once: `Rscript R/refresh_early.R` (downloads the newest voter file if its ETag changed, updates denominators and the early electorate panel).

Election night: `./run_loop.sh` (no push) or `./run_loop.sh --push` (commits and pushes only `data/live/state.json`). The loop updates every 120 seconds, holds a lock so only one copy runs, backs off 15 minutes on HTTP 403 or 429, and never stops on its own: after 5 failed updates in a row it logs a WARNING line and shows a macOS notification, then keeps retrying (`--max-errors N` makes it stop instead). The log is `data/live/loop.log`.

A call: `Rscript R/sign_off.R status`, then `Rscript R/sign_off.R Davis` or `Buckhout`. It refuses unless the test is currently met for that candidate. `Rscript R/sign_off.R clear` removes the sign-off, and a published call then becomes withdrawn at the next update.

Checks: `Rscript tests/test_model.R`, `Rscript tests/rehearse.R`, `Rscript tests/stress.R` (feed failures, loop mechanics, a local-only push test), `Rscript tests/backtest.R 200 8` (about 7 minutes on 8 cores). Rebuild the baseline with `Rscript R/build_baseline.R`, then `Rscript R/refresh_early.R --no-download`.

All thresholds and assumptions are in `R/config.R`.

## Files

`R/config.R` settings. `R/lib_data.R` readers. `R/extract_voter_file.pl` strips the voter file to non-identifying columns, outside Dropbox. `R/build_baseline.R`, `R/refresh_early.R` build inputs. `R/model.R` estimator and favourable-case test. `R/call_rule.R` both calling rules. `R/fetch.R`, `R/publish.R`, `R/step.R`, `run_loop.sh` the loop. `R/sign_off.R` the hand sign-off.

`data/baseline/`: `recheck.txt` (recheck of the briefed figures), `blend_regression.csv`, `cells_2026.csv` (baseline by reporting cell), `early_expected.csv` (accepted ballots by cell), `site_xwalk.csv` and `precinct_xwalk.csv` (voter file names to results units), `split_factors.csv`, `county_ids.csv`, `geo_2026.csv`, `accepted_past_by_county.csv`, `early_2024_shares.csv`, `county_groups.csv` (county shares and cut points for the groups panel, from `R/build_groups.R`, rerun by `R/refresh_early.R`).

`~/needle_nc01_raw/` holds downloads and extracts. Nothing with names, addresses or voter ids enters this folder.

## The model

A reporting cell is county x reporting unit x vote method. Everything is D minus R as a share of all votes, so Bailey's votes sit in the denominator. Davis wins a draw when his margin is above zero; Bailey is not modelled as a possible winner.

**Baseline.** 0.75 Harris plus 0.25 Stein by cell, each cell shrunk toward its county x method figure with 25 votes of weight, and falling back to county x method where a cell has no 2024 match. Why 0.75: Davis's own 2024 cell margins, where old NC-1 overlaps the new lines, load 0.77 on Harris (SE 0.02, p < .001; 380 cells; within county x method 0.78, SE 0.02, p < .001). The 2022 analogue, Beasley on Biden and Cooper, gives 0.58 (SE 0.14, p < .001) overall and 0.21 (SE 0.13, p = .10) within county x method, but those are different candidates.

**Swing.** Reported margin minus baseline, modelled as district swing + method deviation + county effect + county x method deviation. Observations are precision weighted with variance `tau_e^2 + sig2 / votes`, so weight grows with votes and saturates. It is a linear Gaussian posterior in closed form.

**Prior (approved by ZH 2026-11-03).** D+4.5 on the final margin with t(5), scale 8; analytic P(Davis) = 0.701. The centre is an adjusted average of NC-1 polls, built by `R/build_prior.R` and saved in `data/baseline/prior_polls.csv`: head-to-head Davis v Buckhout questions in the NYT poll file (`static01.nyt.com/newsgraphics/polls/house.csv`) with fieldwork ending on or after 2026-08-01. Four qualify: co/efficient (marked Republican) R+1, n=623, ended Oct 8; Neighborhood Research D+10, n=303, Oct 6; GQR (marked Democratic) D+8, n=500, Oct 4; GBAO for House Majority PAC (Democratic) D+7, n=500, Sep 11. Polls the file marks as partisan move 4.5 points toward the other party, and polls are weighted by the square root of sample size: D+4.54 adjusted, D+5.56 unadjusted. The scale is 8 rather than 6 because there are only four polls and three are partisan. A RedBox (D) poll at D+1 from Oct 23 is not in the NYT file and is not included. Until 2026-11-03 the prior was D+2, scale 6, an assumption rather than a poll average.

**Outstanding votes.** Early and mail: accepted ballots x 0.99 (House votes per ballot, from 2022 and 2024 ratios) minus reported, plus an assumed 5% of late mail. Election Day: 2024 precinct votes x a turnout ratio with prior 1.15 (assumption), SD 0.25 on the log scale, updated by county from reported precincts. Precincts far below the implied turnout count as partial, stay outstanding, and stay out of the ratio. Provisional: the 2024 count, 1,595 on these lines, not expected tonight.

**Simulation.** 20,000 draws (2,000 in the backtest). A shared t(5) mixing draw fattens all errors. Method errors are correlated through the district swing and the mixing draw.

Two additions came from the backtest. `tau_out` (4 points) is a shared error on what is still out, by method, which fades only as the outstanding share falls. `tau_d` went to 10 points. The first backtest with `tau_d` = 8 and no `tau_out` covered the 90% interval only 70 to 77% of the time. Method deviations are constrained to average near zero, so they do not widen the prior on the final margin.

**Variance sizing.** On these lines, 2020 President to each later contest: county SD 3.0 to 8.2; county x method deviation 3.2 to 3.8; Election Day cell residual SD 10 to 12 at about 300 votes. The Election Day minus early gap moved 2 points from 2020 to 2022 and 23 to 26 points from 2022 to 2024.

**Status.** `pre`: no votes. `counting`: votes in. `final`: all Election Day, early and mail in. Provisionals remain. `stale`: no feed change for 30 minutes after polls close.

**Source choice.** Later Last-Modified wins, unless the other source has more votes and the gap is under 10 minutes. The choice is written to `feed.source`, and switches are logged.

## Page additions (2026-11-02)

These are reporting outputs. None of them feeds the estimate, the prior or the calling rules.

**Intervals.** `estimate` and each `history` entry carry 50% (`margin_lo50`, `margin_hi50`) and 95% (`margin_lo95`, `margin_hi95`) quantiles of the simulated final district margin, beside the 90% keys. History entries also carry `reported_margin` (margin of votes counted so far, null at zero) and `pct_counted_est`. History entries written before this change have only `t`, `p_davis`, `margin_mid`, `counted_votes`; both forms validate.

**Sources.** `sources = {prior, model, actual}` sums to 1. `actual` = votes counted / the median simulated final total. The outstanding share, 1 - actual, is split by W: for each county c and method m with votes outstanding, the projected swing is s = g + d_m + a_c (district swing, method deviation, county effect), and W_cm = Var_post(s) / Var_prior(s). In a Gaussian model this ratio is the weight the posterior mean puts on the prior mean: 1 before any votes, falling toward 0 as reported results pin s down. W is the average of W_cm weighted by expected outstanding votes (Election Day: 2024 votes still out x the county turnout-ratio median; early, mail, provisional: the outstanding count). prior = (1 - actual) W, model = (1 - actual)(1 - W). It is 1/0/0 before any votes and 0/0/1 once nothing is outstanding. With only the 1,595 provisionals out, actual is about 0.995.

**County final margins.** `by_county` adds `expected_total` (median), `pct_counted_est`, and `est_margin_mid`, `_lo50`, `_hi50`, `_lo95`, `_hi95`: each county's simulated final margin, counted plus projected outstanding, from the same draws as the district, so county draws add up to the district draw by draw.

**Benchmark.** How a county would vote in an exactly even district race: the cell baseline `b` minus `Bbar`, its expected-vote-weighted district mean, so the district benchmark is 0 when weighted by prior expected votes (E0). It is evaluated like for like on counted cells, weighted by votes reported per cell: `benchmark_margin` (null at zero votes), `vs_benchmark` = margin - benchmark_margin, `vs_benchmark_votes` = vs_benchmark / 100 x votes (net votes ahead of benchmark, positive for Davis).

**Additions (2026-11-03, `R/additions.R`).** Three optional state.json blocks, each with `available`; any failure publishes the unavailable shape and leaves the main estimate alone. `davis_v_cooper`: Davis minus Buckhout and Cooper minus Whatley (US Senate) on NC-1 cells where both contests have votes, by county and district, and their difference. `redistricting`: the Senate margin on the old (2024) NC-1 lines and the new lines, each next to Harris minus Trump on the same counted votes; old-lines units from `R/build_old_lines.R` (`data/baseline/old_lines_*.csv`, `lines_2024.csv`). Names match for 84.1% of 2024 old-NC-1 votes (renamed early-voting sites in six whole counties), so `low_match_flag` is true; unmatched units in whole counties use their county x method 2024 margin, giving 99.6% coverage. Old lines read the statewide zip, since the county JSON files cover only new-NC-1 counties. `trailing_needs`: the margin on the model's median remaining votes the trailing candidate needs to tie, and the model's expected margin on those votes.

**Groups.** Each county has `groups`: `pres_lean` (raw 2024 Harris minus Trump county margin on the new lines, from the county check in `recheck.txt`: at least +15 Strong for Harris, at most -15 Strong for Trump, otherwise More competitive; 6, 6 and 13 counties), `area` (Beaufort, Carteret, Craven, Dare, Hyde, Pamlico, Onslow are new; 7 and 18), `race` and `age` (share Black, share 65+ among 2026 accepted early and mail ballots in NC-1, split at the median county, a county at the median going to More; 13 and 12 each). Top-level `groups` pools counted votes for each dimension x group: `leader_margin`, `benchmark_margin` like for like, `vs_benchmark`, and `status`: Too early under 10% of the group's expected votes counted, Neither when |vs_benchmark| is under `bench_threshold` (3 points, an assumption ZH must approve, in `R/config.R`), otherwise the candidate ahead of benchmark.

## Calling rules

Live mode `call_mode = "signoff"`: cannot catch up, with hand sign-off. The pipeline never publishes a call by itself. `call.status` stays `none` until `data/live/call_signoff.json` names the candidate and the test holds. Any later failure gives `withdrawn` with a timestamp, and a new sign-off is then required.

**Cannot catch up.** For each outstanding cell, the trailing candidate gets baseline + the largest swing to them seen tonight in any county for that method + B points. Volumes are the high end: upper 90% turnout bound on Election Day, accepted minus reported for early and mail, 95th percentile for provisionals. It passes if the leader is still ahead, the count lead is above 0.5 points, and this has held for 10 minutes across updates.

Deviation from the written rule: "a method with no reported votes fails the test" would fail every run, because provisionals are not counted on election night. Provisionals therefore use the largest swing seen in any other method. The no-report failure still applies to Election Day, early and mail.

**Original rule** (logged to `data/live/rule_log.csv`, and shown as `orig_*` conditions; ZH rejected it). P at least 0.99, Election Day votes in at least 18 counties, 80% counted, lead above outstanding provisionals, projected margin above 1 point, held 10 minutes.

An "update" means one loop cycle, so the hold can complete with no new votes arriving. Requiring a new feed change instead would block a call after the last drop of the night.

## Facts in the brief that changed

- Granville: of the 6 units in the NC-1 contest, voter-file labels show SALEM split (64.3% in district 1) and WOEL with none of 669 accepted ballots in district 1. Baseline votes are scaled for this; the correction moves margins by under 0.1 point (President -11.64, Governor +2.62, AG -6.18).
- The split counties had no pseudo-precinct votes in 2024, so there was nothing to allocate pro rata.
- Provisional votes on these lines in 2024: 1,603 before the Granville scaling, not about 1,500.
- 2026 pseudo-precinct lists differ from 2024 in Camden, Martin, Perquimans and Nash. The 2026 file is a template dated 5 October, so new units may appear tonight; they get the county x method baseline and do not break the county denominators.
- The JSON field `evc` is Election Day, not early. `ovc` is early, `avc` is mail, `pvc` is provisional (checked against the 2024 zip for Beaufort).
- Accepted ballots: 204,929 with status exactly ACCEPTED and a district 1 label; 205,458 counting all ACCEPTED statuses; 205,479 adding 21 rows with a blank district label in whole counties, which is the denominator used.

## Limits

The reporting order is simulated, and no real election-night snapshots exist. Inside a county, past accepted ballots are split among units by final votes. Crosswalks: site to unit is by name where possible, and by size order or geography in Edgecombe, Halifax and Northampton (see `confidence` in `site_xwalk.csv`); precinct names to codes come from matching 2024 ballot and vote counts (median difference 0.6%, largest 6.2%). Camden, Martin and Perquimans precinct denominators are allocated by 2024 Election Day votes. County totals are exact in all cases. The zip has no reporting flag, so a county where every precinct is half in looks like low turnout until the rest arrive.

## Backtest

See `tests/backtest_results/` for full tables.

Replays run through the real estimator and both rules: 5 contests x 5 shifts (as run, and true margin at -3, -1, +1, +3) x 200 orderings, 5,000 replays, updates every 10 simulated minutes. Contests are Senate 2022, Governor, AG and President 2024 (baseline 2020 President only), plus Senate 2022 on the 2024 blend. That last one breaks the earlier-election rule on purpose, because it is the only presidential-to-midterm pair without COVID voting. Early and mail denominators are the real accepted-ballot counts for 2022 and 2024.

Pooled over the five contests (per-contest rows in `rule_table.csv`):

| Rule | True margin | False calls | Runs with a call | Median first call, % counted | 10th pct | Runs with a withdrawal |
|---|---|---|---|---|---|---|
| Original, P 0.99 | +/-1 | 0 of 2,000 | 57.4% | 96.7 | 92.4 | 10.6% |
| Original, P 0.99 | +/-3 | 0 of 2,000 | 100% | 89.7 | 84.9 | 0.2% |
| Cannot catch up, B 0 | +/-1 | 15 of 2,000 (0.75%) | 100% | 98.4 | 93.5 | 7.6% |
| Cannot catch up, B 0 | +/-3 | 4 of 2,000 (0.20%) | 100% | 88.0 | 63.9 | 27.7% |
| Cannot catch up, B 5 | +/-1 | 2 of 2,000 (0.10%) | 100% | 98.9 | 97.1 | 2.0% |
| Cannot catch up, B 5 | +/-3 | 1 of 2,000 (0.05%) | 100% | 92.8 | 77.9 | 12.0% |
| Cannot catch up, B 10 | +/-1 | 1 of 2,000 (0.05%) | 100% | 99.2 | 98.0 | 0.5% |
| Cannot catch up, B 10 | +/-3 | 0 of 2,000 | 100% | 94.9 | 90.2 | 3.6% |

The original rule cannot call a race whose projected margin is under 1 point, which explains the 57.4%. Most of its +/-1 withdrawals are the estimate crossing that threshold, since those runs are built to end exactly at it.

The default is B = 10. It is a proposal for ZH: in +/-1 races 99.7% of first passes come at or after 95% counted, so it almost never passes before then.

Calibration of P(Davis), all update points (mean P, then share won): .013/.010 in the lowest decile, then .15/.31, .25/.50, .35/.56, .45/.55, .55/.45, .65/.56, .75/.76, .86/.75, .98/.97. P above .99: 2 wrong of 21,991 update points. P below .01: 0 wrong of 36,092. These rest on 25 contest-and-shift outcomes, not independent replays, so middle deciles are noisy and 0.99 has not been shown wrong rather than shown right.

Mean absolute error of the median, points: 4.8 at 25% counted, 3.2 at 50%, 0.73 at 75%, 0.27 at 90%. The 90% interval covered 88%, 91%, 96%, 93%.

**First-drop risk.** In the reverse-time stress run the median overstates the Democrat by 8.9 points at 25% counted and 3.8 at 50%, and the interval covers 64% and 77%. In 2022 the early vote was far more Democratic relative to Election Day than in 2024. Setting `d_prior_mean` to ED -8, EV +4, Mail +4 cuts the 25% error to 6.4 in that run and from 3.6 to 2.1 in plain Senate 2022 (`tests/backtest_results/dgap/`). It is off by default because it is an unapproved assumption resting on one cycle.

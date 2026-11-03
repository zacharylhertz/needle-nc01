#!/bin/bash
# Election-night loop: fetch, model, publish, once every 120 seconds; one process only.
# Usage: ./run_loop.sh [--push] [--once] [--interval SECONDS] [--max-errors N]
# --push commits and pushes only data/live/state.json, and is OFF unless given.
# Never stops on errors by default: after 5 failed updates in a row it logs a warning (and a macOS notification)
# and keeps retrying. --max-errors N restores a hard stop after N in a row (used by the tests).
set -u
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PUSH=0; ONCE=0; INTERVAL=120; MAXERR=0; WARNERR=5
while [ $# -gt 0 ]; do
  case "$1" in
    --push) PUSH=1 ;;
    --once) ONCE=1 ;;
    --interval) shift; INTERVAL="$1" ;;
    --max-errors) shift; MAXERR="$1" ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
  shift
done
# Without its own .git here, git would act on the parent Dropbox/git repo.
if [ "$PUSH" = 1 ] && [ ! -e "$ROOT/.git" ]; then
  echo "--push refused: $ROOT has no .git of its own. Run git init and add the remote first." >&2
  exit 5
fi
LIVE="${NEEDLE_LIVE_DIR:-$ROOT/data/live}"
mkdir -p "$LIVE"
LOG="$LIVE/loop.log"
LOCK="$LIVE/.loop.lock"
if ! mkdir "$LOCK" 2>/dev/null; then
  echo "another loop holds $LOCK (pid $(cat "$LOCK/pid" 2>/dev/null)); not starting. Remove the folder if that process is gone." >&2
  exit 4
fi
echo $$ > "$LOCK/pid"
trap 'rm -rf "$LOCK"' EXIT

errors=0
push_state() {
  [ "$PUSH" = 1 ] || return 0
  cd "$ROOT" || return 1
  git add data/live/state.json || return 1
  if git diff --cached --quiet -- data/live/state.json; then return 0; fi
  git commit -q -m "needle update $(date -u +%H:%MZ)" -- data/live/state.json || return 1
  # Never wait on a password or passphrase prompt: fail, log, and try again next cycle.
  GIT_TERMINAL_PROMPT=0 GIT_SSH_COMMAND="ssh -o BatchMode=yes -o ConnectTimeout=20" git push -q || return 1
}

while :; do
  Rscript "$ROOT/R/step.R" >>"$LOG" 2>&1
  rc=$?
  if [ $rc -eq 0 ]; then
    errors=0
    if ! push_state >>"$LOG" 2>&1; then echo "$(date) push failed; state.json is still current locally" >>"$LOG"; fi
  elif [ $rc -eq 3 ]; then
    echo "$(date) server refused or backoff active; waiting" >>"$LOG"
  else
    errors=$((errors + 1))
    echo "$(date) update failed with exit $rc ($errors in a row)" >>"$LOG"
    if [ "$MAXERR" -gt 0 ] && [ $errors -ge $MAXERR ]; then
      echo "$(date) STOPPING after $errors consecutive errors. Last lines above give the reason." >>"$LOG"
      tail -n 20 "$LOG" >&2
      exit 1
    fi
    if [ $errors -eq $WARNERR ]; then
      echo "$(date) WARNING: $errors failed updates in a row; still retrying. Last lines above give the reason." >>"$LOG"
      tail -n 20 "$LOG" >&2
      command -v osascript >/dev/null && osascript -e "display notification \"$errors failed updates in a row; see loop.log\" with title \"NC-1 needle\"" >/dev/null 2>&1
    fi
  fi
  [ "$ONCE" = 1 ] && exit $rc
  sleep "$INTERVAL"
done

#!/bin/bash
# Commit script - always uses web-flow as author
# REMINDER: Commit messages should be neutral, terse, and professional

MESSAGE="${1:-}"

git add -A
GIT_AUTHOR_NAME="web-flow" \
GIT_AUTHOR_EMAIL="noreply@github.com" \
GIT_COMMITTER_NAME="web-flow" \
GIT_COMMITTER_EMAIL="noreply@github.com" \
git commit --allow-empty-message -m "$MESSAGE" || exit 1

git remote get-url origin >/dev/null 2>&1 || git remote add origin https://github.com/J0pari/ProcGen.git
git push --set-upstream origin master || git push || {
    git pull --rebase && git push
} || exit 1

#!/bin/bash

developers=("Philip ks")

date_run=$(date +%d.%m.%y)
total_commits=$(git rev-list --all --count)

start_file=".github/workflows/prefix.md"
template_file=".github/workflows/template.md"
output_file="docs/STATS.md"

sed -e "s/{{date_run}}/$date_run/" \
  -e "s/{{total_commits}}/$total_commits/" \
  "$start_file" > "$output_file"

for developer in "${developers[@]}"; do
  dev_cloc_add=$(git log --author="$developer" --pretty=tformat: --numstat | awk '{ add += $1 } END { print add }')
  dev_commits=$(git log --author="$developer" --pretty=oneline | wc -l)
  dev_percent=$(awk -v dev_commits="$dev_commits" -v total_commits="$total_commits" 'BEGIN { printf "%.2f", (dev_commits / total_commits) * 100 }')

  sed -e "s/{{dev_name}}/$developer/" \
    -e "s/{{dev_cloc_add}}/$dev_cloc_add/" \
    -e "s/{{dev_commits}}/$dev_commits/" \
    -e "s/{{dev_percent}}/$dev_percent/" \
    "$template_file" >> "$output_file"
  done

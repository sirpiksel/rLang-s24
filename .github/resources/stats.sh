#!/bin/sh

# Two separate lists for display names and corresponding identifiers
display_names=("Daniela Stängle" "Alexander Klaus" "Sebastian Meier" "Clemens Glomb" "Philip Kastura-Sahl")
identifiers=("daniela.staengle@hotmail.de" "alexaander01" "smeier.ngd@gmail.com" "75215973+Clemens-Uni@users.noreply.github.com" "3000piksel@gmail.com")

date_run=$(date +%d.%m.%y)
total_cloc=$(git log --all --pretty=tformat: --numstat | awk '{ add += $1 } END { print add }')
total_commits=$(git log --all --pretty=tformat:'%ae' | wc -l)

start_file=".github/resources/prefix.md"
template_file=".github/resources/template.md"
output_file="docs/STATS.md"

# Insert date and total commits into the output file
sed -e "s/{{date_run}}/$date_run/" \
    -e "s/{{total_cloc}}/$total_cloc/" \
    -e "s/{{total_commits}}/$total_commits/" \
    "$start_file" > "$output_file"

# Loop over the display names and corresponding identifiers
i=0
for display_name in "${display_names[@]}"; do
  id="${identifiers[$i]}"

  dev_cloc_add=$(git log --all --author="$id" --pretty=tformat: --numstat | awk '{ add += $1 } END { print add }')
  dev_commits=$(git log --all --author="$id" --pretty=tformat:'%ae' | wc -l)
  dev_cloc_per=$(awk -v dev_cloc_add="$dev_cloc_add" -v total_cloc="$total_cloc" 'BEGIN { printf "%.2f", (dev_cloc_add / total_cloc) * 100 }')
  dev_commit_per=$(awk -v dev_commits="$dev_commits" -v total_commits="$total_commits" 'BEGIN { printf "%.2f", (dev_commits / total_commits) * 100 }')

  # Replace the placeholders in the template with the actual values
  sed -e "s/{{dev_name}}/$display_name/" \
      -e "s/{{dev_cloc_add}}/$dev_cloc_add/" \
      -e "s/{{dev_commits}}/$dev_commits/" \
      -e "s/{{dev_cloc_per}}/$dev_cloc_per/" \
      -e "s/{{dev_commit_per}}/$dev_commit_per/" \
      "$template_file" >> "$output_file"

  i=$((i + 1))
done

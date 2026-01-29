cargo run --release ../../compiler 2>&1 \
| grep -A 1 'took' \
| grep -v '^--$' \
| awk '
  /took/ {
    label = $1
    next
  }
  /^[0-9]+$/ {
    sum[label] += $1
  }
  END {
    for (l in sum)
      printf "%s: %d ms\n", l, sum[l]
  }
'

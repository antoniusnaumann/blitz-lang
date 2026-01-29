cargo build --release

for i in 1 2 3; do
  echo -n "run $i: "
  /usr/bin/time -p cargo run --release -- ../../compiler 2>&1 | awk '/^real/ {print $2 " s"}'
done

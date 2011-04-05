for f in superdupergames-archive/*.raw; do
  superdupergames-archive/cook.pl < $f > ${f/.raw/}.cooked
  echo $f
  echo -e "quit\n" | cat ${f/.raw/}.cooked - | ./annotate -verify
done

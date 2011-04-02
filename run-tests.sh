for f in tests/p0-win*; do
  echo $f
  echo -e "ai_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  ## OUTPUT should contain exactly 'The game is already over!', no more.
  echo -e "The game is already over!" | diff --brief - OUTPUT
done
for f in tests/p0-nowin*; do
  echo $f
  echo -e "ai_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  ## OUTPUT should be empty.
  diff --brief /dev/null OUTPUT
done
rm OUTPUT

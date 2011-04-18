make annotate
for f in tests/p0-win*; do
  echo $f

  echo -e "ai_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  ## OUTPUT should contain exactly 'The game is already over!', no more.
  echo "The game is already over!" | diff --brief - OUTPUT

  echo -e "win_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  ## OUTPUT should contain exactly 'The game is already over!', no more.
  echo "The game is already over!" | diff --brief - OUTPUT
done

for f in tests/p0-nowin*; do
  echo $f

  echo -e "ai_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  ## OUTPUT should be empty.
  diff --brief /dev/null OUTPUT

  echo -e "win_move\nai_move\nquit\n" | cat $f - | ./annotate -auto >OUTPUT
  echo "AI for Player1 found no winning move." | diff --brief - OUTPUT
done

rm OUTPUT

<html>
<head>
<title>Got history</title>
</head>
<body>

<h2>Homeworlds AI</h2>

<p>
Best move: <pre>{{chosen_move}}</pre>
</p>

<form action="/submit-move/{{game_id}}" method="post">
<textarea name="chosen-move" cols="50" rows="7">{{chosen_move}}</textarea>
<input type="submit" value="OK, submit this move to SDG"><br>
</form>


<p>
Here is the current state of game {{game_id}} (with attacker {{attacker}}):
<pre>{{state}}</pre>
</p>

<p>
Retrieved raw history from SDG:
<pre>{{raw_history}}</pre>
</p>

</body>
</html>

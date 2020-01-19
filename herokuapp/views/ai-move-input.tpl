<!DOCTYPE html>
<html>
<head>
</head>
<body>

<h2>Homeworlds AI</h2>

% if pending_challenges:
<b>Pending challenges</b>
<ul>
    % for c in pending_challenges:
    <li>
        Challenged by <b>{{c['opponent']}}</b> —
        <a href="/accept-challenge/{{c['game_id']}}/{{c['join_url']}}">Accept</a> —
        <a href="/reject-challenge/{{c['game_id']}}/{{c['leave_url']}}">Reject</a>
    </li>
    % end
</ul>
% end

<form action="/ai-move" method="post">
<input type="submit" value="Get Move"><br>
Attacker: <input type="text" width="100%" name="attacker" placeholder="0"><br>
<textarea name="state" cols="40" rows="20" placeholder="Game state goes here"></textarea>
</form>

</body>
</html>

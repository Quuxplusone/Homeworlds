<!DOCTYPE html>
<html>
<head>
</head>
<body>

<h2>Homeworlds AI</h2>

% if pending_challenges or pending_moves:
<b>Pending actions</b>
<ul>
    % for c in pending_challenges:
    <li>
        Challenged by <b>{{c['opponent']}}</b> —
        <a href="/accept-challenge/{{c['game_id']}}/{{c['join_url']}}">Accept</a> —
        <a href="/reject-challenge/{{c['game_id']}}/{{c['leave_url']}}">Reject</a>
    </li>
    % end
    % for c in pending_moves:
    <li>
        You have {{c['time_left']}} left to move in game {{c['game_id']}} with <b>{{c['opponent']}}</b> —
        <a href="/ai-make-move/{{c['game_id']}}">Make your move</a>
    </li>
    % end
</ul>
% else:
<b>Nothing currently requires your attention.</b>
% end

<form action="/ai-move" method="post">
<input type="submit" value="Get Move"><br>
Attacker: <input type="text" width="100%" name="attacker" placeholder="0"><br>
<textarea name="state" cols="40" rows="20" placeholder="Game state goes here"></textarea>
</form>

</body>
</html>

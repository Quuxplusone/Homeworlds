<html>
<head>
<title>Display stored mail</title>
</head>
<body>

<h2>Display stored mail</h2>

<p>
    This bot handles mail addressed to <code>{{inbound_address}}</code>.
    Here are the messages received since the last server reset:
</p>

%for post in posts_received:
<p><pre>
    {{post}}
</pre></p>
%end

</body>
</html>

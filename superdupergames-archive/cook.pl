#!/usr/bin/perl -w

use strict;
use warnings;

my $color = "[[:alpha:]]+";
my $piece = "[RrYyGgBb][123]";
my $where = "[A-Za-z_][A-Za-z0-9_]*";

sub tc { return ucfirst(lc($_[0])); }

sub parse_single_action {
    my $line = shift @_;
    if ($line =~ /^[Bb]uild ($piece) ($where)$/) {
        my $what = lc($1);
        my $at = tc($2);
        return "build $what at $at";
    } elsif ($line =~ /^[Cc]onstruct ($piece) ($where)$/) {
        my $what = lc($1);
        my $at = tc($2);
        return "build $what at $at";
    } elsif ($line =~ /^[Aa]ttack ($piece)[NS]? ($where)$/) {
        my $what = lc($1);
        my $at = tc($2);
        return "capture $what at $at";
    } elsif ($line =~ /^[Tt]rade ($piece) ($piece) ($where)$/) {
        my $from = lc($1);
        my $to = lc($2);
        my $at = tc($3);
        return "convert $from to $to at $at";
    } elsif ($line =~ /^[Dd]iscover ($piece) ($where) ($piece) ($where)$/) {
        my $movewhat = lc($1);
        my $whence = tc($2);
        my $starpiece = lc($3);
        my $whither = tc($4);
        return "move $movewhat from $whence to $whither ($starpiece)";
    } elsif ($line =~ /^[Mm]ove ($piece) ($where) ($where)$/) {
        my $movewhat = lc($1);
        my $whence = tc($2);
        my $whither = tc($3);
        return "move $movewhat from $whence to $whither";
    } elsif ($line =~ /^[Ss]acrifice ($piece) ($where)$/) {
        my $what = lc($1);
        my $at = tc($2);
        return "sacrifice $what at $at";
    } elsif ($line =~ /^[Cc]atastrophe ($where) ($color)$/) {
        my $at = tc($1);
        my $how = $2;
        my $cookedhow = lc($how);
        if ($cookedhow eq "g") { $cookedhow = "green"; }
        elsif ($cookedhow eq "r") { $cookedhow = "red"; }
        elsif ($cookedhow eq "y") { $cookedhow = "yellow"; }
        elsif ($cookedhow eq "b") { $cookedhow = "blue"; }
        unless ($cookedhow eq "red" or $cookedhow eq "yellow" or
                $cookedhow eq "green" or $cookedhow eq "blue") {
            print "Unparsed color: $cookedhow\n";
            die;
        }
        return "catastrophe $cookedhow at $at";
    } elsif ($line =~ /^[Pp]ass$/) {
        return "pass";
    }
    return "";
}

my $currentphase = 0;
sub phase {
    my $num = shift @_;
    if ($currentphase == $num) { return; }
    if ($num == $currentphase+1) {
        $currentphase = $num;
        return;
    }
    print "Phase error: line is expected in phase $num, we're in phase $currentphase\n";
    die;
}


my $firstplayer = "";
my $secondplayer = "";
my $currentmove = "";

while (<STDIN>) {
    my $line = $_;
    chomp $line;
    if ($line =~ /^#/) {
        # silently chomp comments
    } elsif ($line eq "") {
        # silently chomp blank lines
    } elsif ($line =~ /^Homeworlds Online \(SDG# ([0-9]+)\)/) {
        my $num = $1;
        phase(1);
        print "# Parsing SDG game number $num...\n";
    } elsif ($line =~ /^Started: (.+), Ended: (.+)/) {
        phase(1);
        print "# Started/ended\n";
    } elsif ($line =~ /^Variants: "(.+)/) {
        phase(1);
        print "# Variants\n";
    } elsif ($line =~ /^Participants: (.+) \(S\), (.+) \(N\)/) {
        $firstplayer = tc($2);
        $secondplayer = tc($1);
        phase(1);
        print "# First player is $firstplayer, second is $secondplayer.\n";
    } elsif ($line =~ /^Winner: (.*)/) {
        my $winner = tc($1);
        phase(1);
        print "# Winner is $winner.\n";
        die unless (($winner eq $firstplayer) || ($winner eq $secondplayer));
    } elsif ($line =~ /^1\) (.+): Homeworld ($piece) ($piece) ($piece)/) {
        my $name = tc($1);
        my $hw1 = lc($2);
        my $hw2 = lc($3);
        my $hwship = lc($4);
        phase(2);
        die unless ($name eq $firstplayer);
        print "$name (0, $hw1$hw2) $hwship-\n";
    } elsif ($line =~ /^2\) (.+): Homeworld ($piece) ($piece) ($piece)/) {
        my $name = tc($1);
        my $hw1 = lc($2);
        my $hw2 = lc($3);
        my $hwship = lc($4);
        phase(3);
        die unless ($name eq $secondplayer);
        print "$name (1, $hw1$hw2) -$hwship\n";
    } elsif ($line =~ /^([0-9]+)\) (.*): (.*)$/) {
        my $movenum = $1;
        my $attacker = lc $2;
        my $action = $3;
        
        if ($currentmove ne "") {
            print "$currentmove\n";
            $currentmove = "";
        }
        
        phase(4);
        die unless ($attacker eq lc (($movenum % 2) ? $firstplayer : $secondplayer));
        my $cookedaction = parse_single_action($action);
        if ($cookedaction eq "") {
            print "Unparsed action: $line\n";
            die;
        }
        $currentmove = "$cookedaction";
    } elsif (($line =~ /^(.+?):/) && ((lc $1 eq lc $firstplayer) || (lc $1 eq lc $secondplayer))) {
        print "# skipped a comment by $1\n";
    } elsif (($line =~ /^SYSTEM: (.*) resigns./) && ((lc $1 eq lc $firstplayer) || (lc $1 eq lc $secondplayer))) {
        if ($currentmove ne "") {
            print "$currentmove\n";
            $currentmove = "";
        }
        print "# At this point, $1 resigned.\n";
    } else {
        my $action = parse_single_action($line);
        if ($action eq "") {
            print "Unparsed line: $line\n";
            die;
        } else {
            die unless ($currentmove ne "");
            # Don't append redundant "pass" actions.
            if ($action ne "pass") {
                $currentmove .= "; $action";
            }
        }
    }
}

# Print the last move of the game, if it wasn't already printed.
if ($currentmove ne "") {
    print "$currentmove\n";
}



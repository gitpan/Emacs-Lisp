# -*-perl-*-
# Tests for behavior after nonlocal jumps.

BEGIN { $| = 1; $^W = 1 }
use Emacs::Lisp;

# Avoid warning "used only once":
*arith_error = *arith_error;

@tests =
    (
     sub {
	 eval { &error ("314159") };
	 ($@ =~ /^314159/, $@);
     },
     sub {
	 eval q(&error ("26535"));
	 ($@ =~ /^26535/, $@);
     },
     sub {
	 eval { &perl_eval (q(die 89793)) };
	 ($@ =~ /^89793 at /, $@);
     },
     sub {
	 eval { &perl_eval (q(&perl_eval (q(die 83279)))) };
	 ($@ =~ /^83279 at /, $@);
     },
     sub {
	 eval { &perl_eval (q(&perl_eval (q(&error ("50288"))))) };
	 ($@ =~ /^50288/, $@);
     },
     sub {
	 use Emacs::Lisp qw($err $x);
	 sub signalbla { &signal (\*arith_error, [26, 4, 33]) }
	 &eval (&read (q((condition-case var
			  (progn (setq x 8)
			   (perl-call "::signalbla")
			   (setq x 3))
			  (arith-error (setq err (cdr var)))))));
	 $x == 8 && "@$err" eq "26 4 33";
     },
     sub {
	 eval { &perl_call (sub { die 23846 }) };
	 ($@ =~ /^23846 at /, $@);
     },
     sub {
	 eval { &perl_call (sub { &error ("41971") }) };
	 ($@ =~ /^41971/, $@);
     },
     sub {
	 69399 == catch \*arith_error, sub {
	     &throw (\*arith_error, 69399);
	 };
     },
     sub {
	 37510 == catch \*arith_error, sub {
	     &perl_eval (q(&throw (\*arith_error, 37510)));
	 };
     },

    # More tests needed:
    # - goto
    # - die from within condition-case handler
    # - throw/signal from within $SIG{__DIE__}
    # - throw/signal from within FETCH, DESTROY, sort, etc.
    # - uncaught errors (in a subprocess)
    # - during global destruction (*shudder*)
    );

print "1..".@tests."\n";
$test_number = 1;
for my $test (@tests) {
    my ($ok, $comment) = &$test();
    print (($ok ? "" : "not "), "ok $test_number");
    if ($comment) {
	$comment =~ s/\s+/ /g;
	print " # $comment\n";
    } else {
	print "\n";
    }
    $test_number ++;
}

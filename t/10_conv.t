# -*-perl-*-

BEGIN { $| = 1; $^W = 1 }
use Emacs::Lisp;

# Avoid warning "used only once":
*string = *string;
*float = *float;
*perl_scalar = *perl_scalar;
*cons = *cons;
*perl_array = *perl_array;

@tests =
    (
     sub { &eq (1, 1) },
    sub { ! &eq (1, "1") },
    sub { &eq (&intern("integer"), \*::integer) },
    sub { &eq (&type_of (1), \*::integer) },
    sub { &eq (&type_of ("1"), \*::string) },
    sub { &eq (&type_of (1.0), \*::float) },
    sub { &type_of (\1) eq \*::perl_scalar },
    sub { &type_of ([1]) eq \*::cons },
    sub { &type_of (lisp [1]) == \*::perl_array },

    sub {
	my $x = &cdr ([1, 2, 3]);
	$#$x == 1 && $x->[0] == 2 && $x->[1] == 3;
    },
    );

print "1..".@tests."\n";
$test_number = 1;
for my $test (@tests) {
  print (&$test() ? "ok $test_number\n" : "not ok $test_number\n");
  $test_number ++;
}

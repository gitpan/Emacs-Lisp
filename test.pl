# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

sub setq (&);

BEGIN {
  @tests =
    (
     sub { &eq (1, 1) },
     sub { ! &eq (1, "1") },
     sub { &eq (&intern("foo"), \*::foo) },
     
     sub {
       import Emacs::Lisp '$foo';
       &set (\*foo, 3);
       $foo == 3;
     },
     
     sub {
       import Emacs::Lisp '$foo';
       $foo = 5;
       &eq (&symbol_value(\*foo), 5);
     },

     sub {
       import Emacs::Lisp '%bar';
       &put (\*bar, \*baz, 78);
       $bar{\*baz} == 78;
     },

     sub {
       import Emacs::Lisp '%bar';
       $bar{\*baz} = 156;
       &get (\*bar, \*baz) == 156;
     },

     sub {
       setq { $narf = "oblert" };
       &equal (&symbol_value(\*narf), "oblert");
     },

     sub {
       setq { $zab{\*rab} = 'oof' };
       &get (\*zab, \*rab) eq 'oof';
     },
    );
}

####################### We [finish] with some black magic to print on failure.

BEGIN { $^W = $| = 1; print "1..", 1 + @tests, "\n"; }
END {print "not ok 1\n" unless $loaded;}
use Emacs::Lisp;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

$test_number = 2;
for my $test (@tests) {
  print (&$test() ? "ok $test_number\n" : "not ok $test_number\n");
  $test_number ++;
}

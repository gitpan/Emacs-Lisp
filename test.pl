# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#
# Prototypes like this are normally not necessary, but we
# use these subs in a BEGIN block before "use Emacs::Lisp"!
sub setq (&);
sub save_excursion (&);
sub t ();	# prevents a core dump, at least w/ 5.004_03 + MULTIPLICITY

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

### XXX commented out because they don't work any more!
     sub {
       defun (\*funnie, 'doc', &interactive(), sub { $_[0] + $_[1] });
       &funnie(45,60) == 105;
     },

     sub {
       $x = 1;
       save_excursion {
	 &set_buffer (&get_buffer_create ("b1"));
	 save_excursion {
	   &set_buffer (&get_buffer_create ("b2"));
	   $x++ unless &eq (&current_buffer(), &get_buffer("b1"));
	 };
	 $x++ if &eq (&current_buffer(), &get_buffer("b1"));
       } == 2 && $x == 3;
     },

     sub {
       $ENV{var} = "";
       &setenv("var", 'yoohoo!');
       $ENV{var} eq 'yoohoo!';
     },

     sub {
       save_excursion {
	 &set_buffer (&get_buffer_create ("test"));
	 $ENV{TEST} = 'yowsa';
	 &call_process($^X, undef, t, undef,
		       '--perl', '-e', 'print $ENV{TEST}');
#	 print STDERR "buffer_string: >>".&buffer_string()."<<\n";
	 &buffer_string() eq 'yowsa';
       };
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

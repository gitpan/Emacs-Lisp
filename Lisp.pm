#
#  "Not all of them use vi."
#

package Emacs::Lisp;

use Carp;
unless (defined (&Emacs::Lisp::Object::DESTROY)) {
  croak ("Emacs::Lisp can only be used by a perl embedded in GNU Emacs");
}

use strict;
no strict 'refs';
use vars qw ($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $AUTOLOAD
	     %EXPORT);
my %special;

require Exporter;
require DynaLoader;

@ISA = qw (Exporter DynaLoader);

$VERSION = '0.61';
bootstrap Emacs::Lisp $VERSION;


package Emacs::Lisp::Variable;

sub TIESCALAR	{ bless ($_[1], $_[0]) }
sub FETCH	{ &Emacs::Lisp::symbol_value }
sub STORE	{ &Emacs::Lisp::set }


package Emacs::Lisp::Vector;

sub TIEARRAY	{ bless (\$_[1], $_[0]) }
sub FETCH	{ &Emacs::Lisp::aref(${$_[0]}, $_[1]) }
sub STORE	{ &Emacs::Lisp::aset(${$_[0]}, $_[1], $_[2]) }


package Emacs::Lisp::Plist;

sub TIEHASH	{ bless (\$_[1], $_[0]) }
sub FETCH	{ &Emacs::Lisp::get(${$_[0]}, $_[1]) }
sub STORE	{ &Emacs::Lisp::put(${$_[0]}, $_[1], $_[2]) }
# XXX missing methods


package Emacs::Lisp;

sub import {
  my @newlist = ();
  for my $i (1..$#_) {
    next if $_[$i] !~ /([\$\@\%])(.*)/;
    my ($type, $name) = ($1, $2);
    if ($type eq '@') {
      next if tied @$name;
      if ($^W && !(&boundp(\*{"::$name"})
		   && &arrayp(&symbol_value(\*{"::$name"}))))
	{
	  my $lispname = $name;
	  $lispname =~ tr/-_/_-/;
	  carp "Warning: `$lispname' is not a Lisp array";
	}
      unless (tied $$name) {
	tie $$name, 'Emacs::Lisp::Variable', \*{"::$name"};
	push @newlist, "\$$name";
      }
      tie @$name, 'Emacs::Lisp::Vector', $$name;
    } elsif ($type eq '%') {
      next if tied %$name;
      tie %$name, 'Emacs::Lisp::Plist', \*{"::$name"};
    } else {
      next if tied $$name;
      tie $$name, 'Emacs::Lisp::Variable', \*{"::$name"};
    }
    push @newlist, $_[$i];
  }
  # XXX  Making use of some undocumented Exporter.pm behavior here...
  if (%EXPORT) {
    @EXPORT{@newlist} = (1) x @newlist;
  } else {
    push @EXPORT_OK, @newlist;
  }
  goto &Exporter::import;
}

# Normally, we would not export an AUTOLOAD sub by default.
# But presumably, if you're using this module, you are running Emacs
# and want easy access to Lisp functions.
# Say `use Emacs::Lisp ();' to avoid importing AUTOLOAD.
#
# XXX Regardless, this should probably be fixed to insert AUTOLOAD
# as a closure/hook if there already is one.  Maybe the place to fix
# it is in Exporter.pm. (?)

@EXPORT = qw(
	     t
	     nil
	     funcall
	     AUTOLOAD
	     save_excursion
	     setq
	     );
%EXPORT_TAGS = (
		'funcs' => [qw(funcall AUTOLOAD)],
		'special' => [qw(
				 save_excursion
				 setq
				 )],
		);

sub t () { \*::t }
sub nil () { undef }

sub AUTOLOAD {
  my $function = $AUTOLOAD;
  $function =~ s/.*:://;
  if (exists $special{$function}) {
    my $msg = $special{$function};
    $msg = $special{$1}
      while $msg =~ /^\*(.*)/;
    $msg = (defined ($msg) ? "; $msg" : "");
    croak "`$function' not implemented in Emacs::Lisp$msg";
  }
  $function = \*{"::$function"};
  *$AUTOLOAD = sub { &funcall($function, @_) };
  goto &$AUTOLOAD;
}

%special =
  (
  'interactive'		=> "functionality to be added",
  'setq-default'	=> "use `&set_default(\\*::symbol, \$value)' instead",
  'save-current-buffer'	=> "functionality to be added",
  'save-restriction'	=> "functionality to be added",
  'or'			=> "use Perl's `or' or `||' operator instead",
  'and'			=> "use Perl's `and' or `&&' operator instead",
  'if'			=> "use Perl's `if' or `?...:' operator instead",
  'cond'		=> "use Perl's `if', `elsif' and `else' instead",
  'progn'		=> "use a code block instead",
  'prog1'		=> "use a temporary variable instead",
  'prog2'		=> '*prog1',
  'quote'		=> "use `\\*::symbol' to quote a symbol, and "
			     . "`&list(\@items)' to make a list",
  'function'		=> '*quote',
  'defun'		=> "functionality to be added",
  'defmacro'		=> "functionality to be added",
  'defvar'		=> "functionality to be added",
  'defconst'		=> '*defvar',
  'let*'		=> '*let',
  'let'			=> "functionality to be added",
  'while'		=> "use Perl's `for', `while' or `until' instead",
  'catch'		=> "functionality to be added",
  'unwind-protect'	=> "functionality to be added",
  'condition-case'	=> "functionality to be added",
  'track-mouse'		=> "functionality to be added",
  'ml-if'		=> undef,
  );

sub setq (&) {
  my $coderef = shift;
  my $callpkg = caller;
  my @vars = _assignees($coderef);
  local $Exporter::ExportLevel = 1;
  import Emacs::Lisp grep { s/\Q$callpkg\E\:\:// && !/\:\:/ } @vars;
  &$coderef;
}

sub save_excursion (&) {
  &eval(&list(\*::save_excursion, &list(\*::funcall, $_[0])));
}

package Emacs::Lisp;
1;
__END__


=head1 NAME

Emacs::Lisp - Support for Perl embedded in GNU Emacs

=head1 SYNOPSIS

=head2 In Emacs

  M-x load-library RET perl RET
  M-x perl-eval-expression RET 2+2 RET

=head2 In Perl

  use Emacs::Lisp;

  &switch_to_buffer('*scratch*');
  &insert("Hello, world!\n");

  setq { $cperl_font_lock = t };

  &add_hook(\*find_file_hooks,
	    sub { &message("found a file!") });

  use Emacs::Lisp qw($emacs_version);
  save_excursion {
    &set_buffer(&get_buffer_create("whatever"));
    &insert("This is Emacs version $emacs_version, sort of.\n");
    &insert("Emacs::Lisp version is $Emacs::Lisp::VERSION.\n");
  };


=head1 DESCRIPTION


Until now, you could customize your Emacs environment using Lisp.  Now
you can use Perl, too.  This module allows Perl code to call functions
and access variables of Lisp.  It also maps some Perl syntax into Lisp
semantics.

You still need to learn some Lisp in order to understand the elisp
manual, which you will need if you wish to learn about the details of
Emacs programming.  Hopefully, this situation will be cured by the
appearance of well-documented Perl modules that give everything in
Emacs a nice, object-oriented wrapping.


=head1 EMACS SUPPORT FOR PERL


A patched B<emacs> program with embedded Perl can compile and evaluate
Perl code via Emacs Lisp or the command line, independently of the
Emacs::Lisp module.

=head2 Functions

The Perlmacs patch builds some Perl-related functions into Lisp.  Use
C<C-h f function-name RET> within Emacs for documentation on these.

  perl-eval		EXPRESSION &optional CONTEXT
  perl-call		CODEREF &optional CONTEXT &rest ARGS
  make-perl-interpreter	&rest ARGV
  perl-run		&optional INTERPRETER
  perl-destruct		&optional INTERPRETER

You need to do C<M-x load-library RET perl RET> (or hack your
F<~/.emacs>) to get the following functions.

  perl-eval-expression	EXPRESSION
  perl-eval-region	START END
  perl-eval-buffer
  perl-load-file	NAME

Note that the exact meaning of I<perl-load-file> is a topic of
research and will likely change.


=head2 Data Conversions

When data is passed between Lisp and Perl within Emacs, some basic
principles apply.  These are goals that may never be entirely met, due
to the differences between Perl and Lisp.

=over 4

=item * Whatever Perl gives to Lisp should get dereferenced once.

A Perl reference gets converted into a Lisp reference to the thing
referenced, not a Lisp reference to the Perl reference.  (I find this
to be intuitively correct behavior.)

=item * Converting a non-dereferenceable value requires some kind of
primitive conversion.

For example, Lisp integers, floats, and strings all become Perl
scalars.  A scalar (other than a reference) converted to Lisp will
become either an integer, a float, or a string.  Glob references in
Perl become symbols in Lisp (subject to the usual underscore-to-hyphen
translation).

=item * Lisp's `nil' is equivalent to Perl's `undef' or `()'.

In Lisp, C<nil> is really a symbol.  However, it is typically used as
the boolean value I<false>.  Perl's equivalent of symbols (glob
references) evaluate to I<true> in boolean contexts.  Converting
C<nil> to anything other than C<undef> (C<()> in list context) would
be disastrous for programmers' mental health!

=item * Generally, converted values can be converted back to Perl more
or less unchanged.

But not always.  For example, C<\*nil> would become C<undef>.

=item * Lisp objects which are not converted Perl stuff and have no
natural counterpart become `Emacs::Lisp::Object' blessed references.

These hold the actual Lisp data and a means of protecting it from
garbage collection.  An I<Emacs::Lisp::Object> reference converted
back to Lisp is the Lisp object.

=back


=head2 Scripts

Emacs with embedded Perl can run Perl programs.  If the first
command-line argument is B<--perl>, the rest will be parsed as if by
the perl program.

When you C<use Emacs::Lisp> in a script started this way, a Perl sub
named I<Emacs::main> may be used to invoke the Emacs editor.  This
makes it possible to put customization code, which would normally
appear in F<~/.emacs>, into a "Perlmacs script".  For example, this
startup code

  (setq
   user-mail-address "gnaeus@perl.moc"
   mail-self-blind t
   mail-yank-prefix "> "
   )

  (put 'eval-expression 'disabled nil)

  (global-font-lock-mode 1 t)
  (set-face-background 'highlight "maroon")
  (set-face-background 'region "Sienna")

could be placed in a file F<myemacs.pl> with the following contents:

  #! /usr/local/bin/emacs --perl

  use Emacs::Lisp;

  setq {
    $user_mail_address = 'gnaeus@perl.moc';
    $mail_self_blind = t;
    $mail_yank_prefix = '> ';
    $eval_expression{\*disabled} = undef;
  };

  &global_font_lock_mode(1, t);
  &set_face_background(\*highlight, "maroon");
  &set_face_background(\*region, "Sienna");

  exit Emacs::main($0, "-q", @ARGV);

When you wanted to run Emacs, you would invoke this program.

The arguments to C<Emacs::main> correspond to the I<argv> of the
I<main> function in a C program.  The first argument should be the
program's invocation name, as in this example.  B<-q> inhibits
running F<~/.emacs> (which is what we're trying to get away from,
after all).

See also L</BUGS>.


=head1 PERL SUPPORT FOR LISP


The Emacs::Lisp module allows Perl programs to invoke Lisp functions
and handle Lisp data using Perl's syntax.

You don't have to do any work, other than C<use Emacs::Lisp>, to make
subs call their Lisp counterpart.  However, tying Lisp variables to
Perl variables is not quite so automatic.  In all cases, hyphens
appearing in Lisp names are translated to underscores in Perl, and
vice versa.


=head2 Functions

This code calls the hypothetical Lisp function C<foo-bar> with
arguments C<4> and C<t>.

  &foo_bar(4, t);

The Lisp syntax for the same call would be

  (foo-bar 4 t)

The ampersand (&) is only really needed for calling Lisp functions
such as C<read>, C<eval>, and C<print>, which are Perl keywords. But
using it is a good habit.


=head2 Symbols

Many Lisp functions take arguments that may be, or are required to be,
symbols.  Lisp programs typically use the C<quote> operator to specify
a symbol.  For example, this Lisp code refers to the C<beep> symbol:

  (run-at-time nil 1 'beep)

Perl uses glob references of package C<main> to specify symbols.  A
literal glob reference begins with a backslash followed by an
asterisk, so the last example would be written as

  &run_at_time(undef, 1, \*beep);

in Perl.  (You may want to do C<&cancel_function_timers(\*beep)> soon
after trying this.)

Note that only globs from package C<main> get converted to symbols, so
code that is compiled in another package must use C<\*::sym> rather
than C<\*sym> to refer to the Lisp symbol C<sym>.


=head2 Variables

In Lisp, variables play a role akin to that of Perl scalars.  A
variable may hold a number, a string, or a reference to any type of
complex Lisp data structure.  (They are not called references in Lisp,
but rather "objects".)

You can create a Perl alias for any reasonably named Lisp variable by
saying C<use Emacs::Lisp qw($varname)>.  Thereafter, assignment to
C<$varname> will update the Lisp value.  Changes made to the variable
in Lisp will be reflected in C<$varname> when used in Perl
expressions.

This example saves and replaces the value of the Lisp variable
C<window-system>:

  use Emacs::Lisp qw($window_system);
  $old_ws = $window_system;
  $window_system = \*y;

This sort of thing could be accomplished in Lisp as follows:

  (setq old-ws window-system)
  (setq window-system 'y)

See also the C<setq> function below.


=head2 Property Lists

Lisp symbols all have an associated object called a plist (for
"property list").  Although it is actually an object just like any
other, the plist is typically used in a way vaguely resembling Perl
hashes.

[INCOMPLETE]


=head2 Special Forms

So-called "special forms" in Lisp, such as C<setq> and C<defun>, do
not work the same way functions do.  Some of them are equivalent to
Perl operators, such as C<if> and C<and>.  Others have meanings
peculiar to Lisp.

A few special forms are implemented in Emacs::Lisp.  They are listed
below.  If you try to call one that's not been implemented, you will
get an error message, which may propose an alternative.

=over 8

=item save_excursion BLOCK

Execute BLOCK within a Lisp C<save-excursion> construct.  This
restores the current buffer and other settings to their original
values after the code has completed.

Please read the elisp manual for details.

=item setq BLOCK

BLOCK is searched for assignments (at top level) of the form

    $var = EXPR;

where I<$var> is a scalar variable or hash element.  Every such
variable is imported from the Emacs::Lisp module.  (That is, it is
tied to the similarly-named Lisp variable or plist.)

Afterwards, BLOCK is executed.  Thus, this code

    use Emacs::Lisp qw(setq);
    setq {
      $A = 2*$foo[5];
      $B{\*foo} = "more than $A";
    };

would have exactly the same effect as

    use Emacs::Lisp qw(setq $A %B);
    $A = 2*$foo[5];
    $B{\*foo} = "more than $A";

The following, which does not tie or import any symbols, has the same
effect on Lisp as the above:

    use Emacs::Lisp ();
    &Emacs::Lisp::set( \*A, 2*$foo[5] );
    &Emacs::Lisp::put( \*B, \*foo, "more than "
      . &Emacs::Lisp::symbol_value( \*A ));

=back


=head1 SUGGESTIONS/HELP REQUESTED


The guiding principle for Perlmacs is:

  "Every interface has an implementation."

to which one could add (citing the Least Upper Bound principle):

  "Every interface has one or more /best/ implementations."

The I<best> implementations are not necessarily I<good>, but one may
hope that they are at least fair-to-middling.  This is especially
relevant when gluing together such incompatibles as Perl and Lisp.

Don't be too concerned about the quality of the implementation yet.
What matters most is to come up with a good interface between Emacs
and Perl (as we know them).

I will happily accept additions and bugfixes to the Perl module.
Trivial fixes in the Emacs source will be accepted, but anything meaty
may have to undergo the Full FSF Rigmarole.  For a description of the
rigmarole, please consult
L<http://www.cygnus.com/egcs/contribute.html>.


=head1 BUGS


=head2 To be addressed probably in Emacs C source

=over 4

=item * Crashes during complex throw/die and similar jump operations.

I think this is fixable, it just hasn't been fixed yet.  (Remember,
this is an ALPHA version.)

=item * Memory leaks.

=item * %ENV, %SIG, `setenv'.

Perl and Emacs use environment variables and signal handlers in
different, incompatible ways.  This needs to be coordinated.  This
issue is not unique to Emacs but affects every nontrivial Perl
embedding.  I don't know whether it's been solved in a general way.

=item * Input/Output.

If Perl code tries to read from I<STDIN>, Emacs becomes unresponsive.
This is very distressing.  Also, Perl loves to print error and warning
messages to I<STDERR>, which can be disconcerting.  Perl's use of
Standard I/O must be controlled.  (I shudder to think what would
happen if you used Perl compiled with B<sfio>.)

=item * `Emacs::main' does not clean up.

The I<Emacs::main> sub may open an X display and not close it.  That
is the most obvious of many problems with I<Emacs::main>.

For best results, the value returned by I<Emacs::main> should be
passed to Perl's I<exit> soon, as in this code:

  exit (Emacs::main($0, @args));

=item * Probably can't use a coderef as an error handler or protected
form.

I think this will be easy to fix.

=item * A crash is likely if Perl code modifies
`&Emacs::Lisp::Object::DESTROY'.

So don't.

=back

See also the (many) FIXMEs in F<src/perlmacs.c> and elsewhere.


=head2 Other places

=over 4

=item * Perl's `local()' doesn't have the effect of Lisp's `let'.

It should.  At least, there should be an easy way to make a local
binding of a Lisp variable in Perl.

=item * Equality operators don't do Lisp `eq' or `equal'.

This is at best counterintuitive for Perl programmers.

=item * Lacks texinfo documentation

=item * Need to update .gdbinit, make-dist, and other places

=back


=head1 CAVEATS


=over 4

=item * Circular data structures are bad.

See L<perlobj/"Two-Phased Garbage Collection">.  Lisp data structures
may be recursive (contain references to themselves) without the danger
of a memory leak, because Lisp uses a periodic-mark-and-sweep garbage
collection model.  However, if a recursive structure involves I<any>
Perl references, it may I<never> be destroyable.

=item * Cross-language references incur overhead.

For the benefit of Lisp's garbage collection, all Perl data that is
referenced by Lisp participates in the mark phase.  For the benefit of
Perl's garbage collection, all Lisp objects that are referenced by
Perl maintain a reference count.

A chain of Perl -> Lisp -> ... -> Perl references may take several
garbage collection cycles to be freed.  It is therefore probably best
to keep the number and complexity of such references to a minimum.

To the extent that the Perl-to-Emacs interface is independent of the
Lispish implementation of Emacs, these performance issues may be
fixable by reimplementing Emacs' internals.  Or by rewriting Perl in
Lisp.... but don't hold your breath.  ;-)

=item * Perl and Lisp don't trap each other's exceptions.

Perl's I<eval> won't trap Lisp errors.  Lisp's I<condition-case> won't
trap Perl errors, although I<perl-eval> will convert a Perl error into
a Lisp error.

=back


=head1 TODO


=head2 Special Forms

There needs to be better support for certain special forms (see
I<%special> in F<Lisp.pm>).

The basic problem is that Perl evaluates every sub's arguments before
running the sub.  This translates fine for ordinary Lisp functions.
However, certain things which appear to be functions in Lisp are
really either I<special forms> or I<macros>, meaning that they receive
their arguments from Lisp in an unevaluated list.

Many of the (fortunately finite) special forms play a role which in
Perl would be filled by a builtin operator or syntactic construct.
For example, the I<setq> special form is analogous to the assignment
operator C<=>.  The I<while> special form roughly corresponds to
Perl's I<while> construct.

It would be nice just to be able to say, "Special forms don't work in
Perl, use the corresponding Perl syntax instead."  However, in some
cases, this leads to excessive code, while in others, there is really
no Perl equivalent (cf. I<track-mouse>).

Often, the problematic forms take an argument that is to be evaluated
as code.  In such cases, I think the syntax offered by Perl subs with
a prototype containing C<&> may be best suited.  (See
L<perlsub/"Prototypes">) This allows a Perl sub to recieve a coderef
as an argument whose syntax is a I<BLOCK> (C<{...}>), just like that
of several Perl builtin operators.

That should work fine for forms like I<track-mouse> which do nothing
more to their unevalled arguments than eval them.  However, the most
interesting and useful special forms, such as I<setq> and I<defun>, do
more than eval certain of their arguments.  They analyze the unevalled
list before deciding what to do.  This is unfortunate for Perl, which
can perform (essentially) only one operation on a coderef: namely, run
it.

Luckily (I guess), XS can do more.  Perl's version of I<setq> is
implemented with the help of XS code.  (See L<perlxs>, F<Lisp.pm>, and
F<Lisp.xs> if you're curious.)


=head2 New Lisp functions

=over 4

=item * current-perl-interpreter

=item * set-perl-interpreter INTERP

=item * (?) perl-error-text &optional INTERP

=item * (?) perl-phase &optional INTERP

=item * (?) perl-status &optional INTERP

=item * perl-replace-regexp, and such.

=back


=head2 Miscellany

=over 4

=item * Better support for Lisp macros.

=item * Parse the cmd line as Perl unless prog name contains "emacs".

=item * Find a way to convert filehandles to the Emacs equivalent.

=item * Complete the plist TIEHASH interface.

=item * Document or scrap the vector TIEARRAY interface.

=item * Improve perl-eval-buffer, perl-load-file, et al. (in C?)

=item * Design a scheme for autoloading Perl files as is done with
Lisp.

=item * Arrange for load-library and friends to recognize `.p[ml]'
files.

=item * Allow *pre-dump* Perl initialization.

=item * Allow multiple Perl interpreters (incomplete).

=back


=head1 ACKNOWLEDGMENTS


These are among the giants on whose shoulders we stand:

=over 4

=item Larry Wall.

'Nuff said.

=item The developers of GNU, and Richard Stallman in particular.

Many thanks for the most beautiful code base that it has ever been, or
will ever likely be, my pleasure to hack.

=item The inventor of Lisp, whose name presently escapes me.

=item The inventors of Perl's truest mother language, C.

=item Å…variste Galois (1811-1832) and Wolfgang Amadeus Mozart
(1756-1791).

Each of them demonstrated just how much a young man can accomplish
with a good mind.

=item Ludwig van Beethoven (1770-1827).

Showed that even neurotic, deaf, aging crabapples can, from time to
time, engender lasting beauty.

=item Tim Bunce, whom I associate with the `DynaLoader' module and the
Perl5 Module List.

I was lucky enough to come onto the Perl scene soon after dynamic
loading had reached its present form (well, but before B<swig>
appeared).  How you all got by before then completely baffles me.

=item Doug MacEachern, author of the `ExtUtils::Embed' module.

ExtUtils::Embed is a cornerstone of Perlmacs.

=back

This list is incomplete.

Personal thanks to Nate Patwardhan, who sparked my early interest in
Perl--and shared his F<.emacs> with me--during our NFIC days.  Nate
also introduced me to gdb under gud-mode, O well was I served!  :)

Thanks also to Ilya Zakharevich for (1) encouraging me in my first
contribution to the Perl development effort (an B<xsubpp> patch), and
(2) a comment in F<cperl-mode.el> about changing Emacs C source.  If
not for that comment, I may never have realized that it is even
I<possible> for mortals to change Emacs C source. ;-)  Although I
didn't implement the change he requested, I hope Ilya approves.


=head1 COPYRIGHT

Copyright (C) 1998 by John Tobey, jtobey@channel1.com.  All rights
reserved.  This module may be distributed and modified under the same
terms as GNU Emacs.  You should have received a copy of the GNU
General Public License along with Emacs::Lisp; see the file COPYING.
If not, retrieve it from ftp://prep.ai.mit.edu/pub/gnu/COPYING .


=head1 SEE ALSO

L<perl>, B<emacs>, and the I<elisp manual> (available where you got
the Emacs source ...one would hope).

=cut

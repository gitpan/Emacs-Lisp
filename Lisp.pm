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

$VERSION = '0.70';
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

%EXPORT_TAGS = (
		'funcs' => [qw(funcall AUTOLOAD)],
		'special' => [qw(
				 defun
				 interactive
				 save_current_buffer
				 save_excursion
				 save_restriction
				 setq
				 track_mouse
				 )],
		);
@EXPORT = (
	   qw(t nil),
	   @{$EXPORT_TAGS{'funcs'}},
	   @{$EXPORT_TAGS{'special'}},
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
  'setq-default'	=> "use `&set_default(\\*::symbol, \$value)' instead",
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
  'defmacro'		=> "functionality to be added",
  'defvar'		=> "functionality to be added",
  'defconst'		=> '*defvar',
  'let*'		=> '*let',
  'let'			=> "functionality to be added",
  'while'		=> "use Perl's `for', `while' or `until' instead",
  'catch'		=> "functionality to be added",
  'unwind-protect'	=> "functionality to be added",
  'condition-case'	=> "functionality to be added",
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

sub interactive ($) {
  my $what = shift;
  bless \$what, 'Emacs::InteractiveSpec';
}

sub defun ($$;$$) {
  my $sym = shift;
  $sym = &intern("$sym")
    unless ref ($sym) eq 'GLOB';
  my ($next, $docstring, $interactive, $body);
  $next = shift;
  if (! ref ($next)) {
    $docstring = $next;
    $next = shift;
  }
  if (ref ($next) eq 'Emacs::InteractiveSpec') {
    $interactive = $next;
    $next = shift;
  }
  ref ($body = $next) && $#_ == -1
    or croak 'Usage: defun ($sym, [$docstring], [&interactive($spec)], $code)';
    
  my $form = &list(&list(\*::apply, $body, \*::_Emacs__Lisp_args));
  if (defined ($interactive)) {
    $interactive = $$interactive;
    if (ref ($interactive) eq 'CODE') {
      $interactive = &list (\*::perl_call, $interactive, \*::list_context);
    }
    if (defined ($interactive)) {
      $form = &cons (&list (\*::interactive, $interactive), $form);
    } else {
      $form = &cons (&list (\*::interactive), $form);
    }
  }
  if (defined ($docstring)) {
    $form = &cons ($docstring, $form);
  }
  $form = &append (&list (\*::lambda,
			  &list (\*{"::&rest"}, \*::_Emacs__Lisp_args)),
		   $form);
  &fset($sym, $form);
  return $sym;
}

sub save_excursion (&) {
  &eval (&list (\*::save_excursion, &list (\*::funcall, $_[0])));
}

sub save_current_buffer (&) {
  &eval (&list (\*::save_current_buffer, &list (\*::funcall, $_[0])));
}

sub save_restriction (&) {
  &eval (&list (\*::save_restriction, &list (\*::funcall, $_[0])));
}

sub track_mouse (&) {
  &eval (&list (\*::track_mouse, &list (\*::funcall, $_[0])));
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

  use Emacs::Lisp qw($emacs_version $perlmacs_version);
  save_excursion {
    &set_buffer(&get_buffer_create("whatever"));
    &insert("This is Emacs version $emacs_version,\n");
    &insert("Perlmacs version $perlmacs_version.\n");
    &insert("Emacs::Lisp version is $Emacs::Lisp::VERSION.\n");
  };


=head1 DESCRIPTION


Until now, you could customize your Emacs environment using Lisp.  Now
you can use Perl, too.  This module allows Perl code to call functions
and access variables of Lisp.  It also maps some Perl syntax into Lisp
semantics.

You still need to learn some Lisp in order to understand the Elisp
Manual, which you will need if you wish to learn about the details of
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

Note that the exact meaning of C<perl-load-file> is a topic of
research and may change.


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
C<nil> to anything other than C<undef> would be disastrous for
programmers' mental health!

=item * Generally, converted values can be converted back to Perl more
or less unchanged.

But not always.  For example, C<\*nil> would become C<undef>.

=item * Lisp objects which are not converted Perl stuff and have no
natural counterpart become `Emacs::Lisp::Object' blessed references.

These hold the actual Lisp data and a means of protecting it from
garbage collection.  An C<Emacs::Lisp::Object> reference converted
back to Lisp is the Lisp object.

=back


=head2 Scripts

Emacs with embedded Perl can run Perl programs.  If the first
command-line argument is B<--perl>, the rest will be parsed as if by
the B<perl> program.

When you C<use Emacs::Lisp> in a script started this way, a Perl sub
named C<Emacs::main> may be used to invoke the Emacs editor.  This
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

could be placed in a file with the following contents:

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

The arguments to C<Emacs::main> correspond to the C<argv> of the
C<main> function in a C program.  The first argument should be the
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

The ampersand (C<&>) is only really needed for calling Lisp functions
such as C<read>, C<eval>, and C<print>, which are Perl keywords.  But
using it is a good habit.


=head2 Symbols

Many Lisp functions take arguments that may be, or are required to be,
symbols.  Lisp programs typically use the C<quote> operator to specify
a symbol.  For example, this Lisp code refers to the C<beep> symbol:

  (run-at-time nil 1 'beep)

Perlmacs uses glob references of package C<main> to specify symbols.
A literal globref begins with a backslash followed by an asterisk, so
the last example would be written as

  &run_at_time(undef, 1, \*beep);

in Perl.  (You may want to do C<&cancel_function_timers(\*beep)> soon
after trying this.)

Note that only globs from package C<main> get converted to symbols, so
code that is compiled in another package must use the form C<\*::sym>
rather than C<\*sym>.


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
C<inhibit-eol-conversion>:

  use Emacs::Lisp qw($inhibit_eol_conversion);
  $old_val = $inhibit_eol_conversion;
  $inhibit_eol_conversion = 1;

This sort of thing could be accomplished in Lisp as follows:

  (setq old-val inhibit-eol-conversion)
  (setq inhibit-eol-conversion 1)

See also the C<setq> function below.


=head2 Property Lists

Lisp symbols all have an associated object called a plist (for
"property list").  Although it is actually an object just like any
other, the plist is typically used in a way vaguely resembling Perl
hashes.

Note that a plist is different from a Perl hash.  Lookups are not
based on string equality as with Perl, but rather on Lisp object
equality (of the C<eq> variety, I believe).  For this reason, it is
best to stick to the Lisp convention of using only symbols as "keys".

Emacs::Lisp provides a shorthand notation for getting and setting
plist elements.  If you say C<use Emacs::Lisp qw(%any_name)>, then
subsequent access to the hash elements of C<%any_name> will really get
or set the corresponding plist entries (i.e., properties of the Lisp
symbol C<any-name>).

For example, the following Perl and Lisp fragments are more or less
equivalent:

  # Perl fragment
  use Emacs::Lisp qw(%booboo %upcase_region);
  $booboo{\*error_conditions} = &list(\*booboo, \*error);
  $can_upcase = ! $upcase_region{\*disabled};

  ; Lisp fragment
  (put 'booboo 'error-conditions '(booboo error))
  (setq can-upcase (not (get 'upcase-region 'disabled)))

See also the C<setq> function below.


=head2 Special Forms

So-called "special forms" in Lisp, such as C<setq> and C<defun>, do
not work the same way functions do.  Some of them are equivalent to
Perl operators, such as C<if> and C<and>.  Others have meanings
peculiar to Lisp.

A few special forms are implemented in Emacs::Lisp.  They are listed
below.  If you try to call one that's not been implemented, you will
get an error message, which may propose an alternative.

=over 8

=item defun SYMBOL,DOCSTRING,SPEC,CODE

=item defun SYMBOL,DOCSTRING,CODE

=item defun SYMBOL,SPEC,CODE

=item defun SYMBOL,CODE

Make CODE callable as the Lisp function SYMBOL.  This is Lisp's
version of Perl's C<sub> keyword.  A function defined in this way
becomes visible to Lisp code.

This is primarily useful for defining Emacs I<commands>.  Commands are
functions that the user can invoke by typing
C<M-x E<lt>function-nameE<gt>>.  A command may be bound to a key or
sequence of keystrokes.  See the Emacs documentation for specifics.

As in Emacs Lisp, when defining a command, you must specify the
interactive nature of the command.  There are various codes to
indicate that the command acts on the current region, a file name to
be read from the minibuffer, etc.  Please see the Elisp Manual for
details.  Emacs::Lisp' C<defun> uses a value returned by
C<interactive> as the SPEC for this purpose.  See L</interactive>.

This example creates a command, C<reverse-region-words>, that replaces
a region of text with the same text after reversing the order of
words.  To be user-friendly, we'll provide a documentation string,
which will be accessible through the Emacs help system (C<C-h f>).

  use Emacs::Lisp;
  defun (\*reverse_region_words,
	 "Reverse the order of the words in the region.",
	 interactive("r"),
	 sub {
	     my ($start, $end) = @_;
	     my $text = &buffer_substring($start, $end);
	     $text = join(' ', reverse split (/\s+/, $text));
	     &delete_region($start, $end);
	     &insert($text);
	 });

=item interactive SPEC

=item interactive

Used to generate the third (or, in the absence of a doc string,
second) argument to C<defun>, which see.  This determines how a
command's arguments are obtained.

SPEC may be a string as described in the Elisp Manual or a reference
to code which returns the argument list.

=item save_excursion BLOCK

Execute BLOCK within a Lisp C<save-excursion> construct.  This
restores the current buffer and other settings to their original
values after the code has completed.

Please read the elisp manual for details.

=item setq BLOCK

BLOCK is searched for assignments (at top level) of the form

    $var = EXPR;

where C<$var> is a scalar variable or hash element.  Every such
variable is imported from the Emacs::Lisp module, as if you had said
C<use Emacs::Lisp qw($var)>.

Afterwards, BLOCK is executed.  Thus, this code

    use Emacs::Lisp;
    setq {
      $A = 2*$foo[5];
      $B{\*foo} = "more than $A";
    };

would have exactly the same effect as

    use Emacs::Lisp qw(:DEFAULT $A %B);
    $A = 2*$foo[5];
    $B{\*foo} = "more than $A";

The following, which does not tie or import any symbols, has the same
effect on Lisp as the above:

    use Emacs::Lisp ();
    &Emacs::Lisp::set( \*A, 2*$foo[5] );
    &Emacs::Lisp::put( \*B, \*foo, "more than "
      . &Emacs::Lisp::symbol_value( \*A ));

=back


=head1 BUGS


=over 4

=item * Crashes during complex jump operations.

For example, both of these generate a core dump on my system:

  emacs --perl -MEmacs::Lisp -e 'perl_eval("goto foo");foo:'

  (defun throwfoo () (throw 'foo t))
  (defun catchfoo () (catch 'foo (perl-eval "&throwfoo()")))
  (perl-eval "&catchfoo()")

I think this is fixable, it just hasn't been fixed yet.  (As of this
writing, Perlmacs is an ALPHA version.  This is the major issue
preventing me from releasing either Perlmacs or Emacs::Lisp as BETA.)

=item * Memory leaks and other problems with `Emacs::main'.

The C<Emacs::main> sub may open an X display and not close it.  That
is the most obvious of many problems with C<Emacs::main>.

For best results, the value returned by C<Emacs::main> should be
passed to Perl's C<exit> soon, as in this code:

  exit (Emacs::main($0, @args));

=item * %ENV, %SIG, `setenv'.

Perl and Emacs use environment variables and signal handlers in
different, incompatible ways.  This needs to be coordinated.  This
issue is not unique to Emacs but affects every nontrivial Perl
embedding.  I don't know whether it's been solved in a general way.

=item * Input/Output.

If Perl code tries to read from C<STDIN>, Emacs becomes unresponsive.
This is very distressing.  Also, Perl loves to print error and warning
messages to C<STDERR>, which can be disconcerting.  Perl's use of
Standard I/O must be controlled.  (I shudder to think what would
happen if you used Perl compiled with B<sfio>.)

=item * Perl's `local()' doesn't have the effect of Lisp's `let'.

It should.  At least, there should be an easy way to make a local
binding of a Lisp variable in Perl.

=item * Probably can't use a coderef as an error handler or protected
form.

I think this will be easy to fix, I just haven't had the need.

=item * A crash is likely if Perl code modifies
`&Emacs::Lisp::Object::DESTROY' or the scalar value in an
`Emacs::Lisp::Object' blessed reference.

So don't.

=back

See also the (many) FIXMEs in F<src/perlmacs.c> and elsewhere in the
patched Emacs source.


=head1 CAVEATS


=over 4

=item * Circular data structures are bad.

See L<perlobj/"Two-Phased Garbage Collection">.  Lisp data structures
may be recursive (contain references to themselves) without the danger
of a memory leak, because Lisp uses a periodic-mark-and-sweep garbage
collection model.  However, if a recursive structure involves I<any>
Perl references, it may I<never> be destroyable.

For best results, Perl code should deal mainly with Perl data, and
Lisp code should deal mainly with Lisp data.

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
fixable by reimplementing Emacs' internals, or by rewriting Perl in
Lisp.... but don't hold your breath.  ;-)

=item * Perl and Lisp don't trap each other's exceptions.

Perl's C<eval> won't trap Lisp errors.  Lisp's C<condition-case> won't
trap Perl errors, although C<perl-eval> will convert a Perl error into
a Lisp error.

=back


=head1 TODO


=over 4

=item * Special forms: let, catch, unwind-protect, condition-case,
etc.

=item * Better support for Lisp macros.

=item * Parse the cmd line as Perl unless prog name contains "emacs".

=item * Find a way to convert filehandles to the Emacs equivalent.

=item * Complete the plist TIEHASH interface.

=item * Document or scrap the vector TIEARRAY interface.

=item * Improve perl-eval-buffer, perl-load-file, et al. (in C?)

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
reserved.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; see the file COPYING.  If not, write to the
  Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
  MA 02111-1307  USA

B<Please note:> The GNU Emacs license (which is the GNU General Public
License or "GPL") requires that all extensions and code designed
specifically for use with Emacs be distributable under the same
license.  At least, that is the intention of its author.  This
includes dynamically linked code such as the Emacs::Lisp module and
(probably) any other Perl modules that use Emacs::Lisp or the
GPL-covered functions of Emacs.  Refer to the file F<COPYING> and the
Emacs documentaion for full details.


=head1 SEE ALSO

L<perl>, B<emacs>, and the I<elisp manual> (available where you got
the Emacs source ...one would hope).

=cut

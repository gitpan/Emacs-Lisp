#
#  "Not all of them use vi."
#

package Emacs::Lisp;

use 5.005;  # This version requires Perlmacs 0.9, which requires 5.005.
use Carp;

# This is, at least for now, the canonical way to test for Perlmacs.
unless (defined (&Emacs::Lisp::Object::DESTROY)) {
  croak ("Emacs::Lisp can only be used by a perl embedded in GNU Emacs");
}

use strict;
no strict 'refs';
use vars qw ($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $AUTOLOAD
	     %EXPORT);
my (%special);

require Exporter;
require DynaLoader;

@ISA = qw (Exporter DynaLoader);
$VERSION = '0.87';
bootstrap Emacs::Lisp $VERSION;

# Closure generator shared by Emacs::Lisp::AUTOLOAD and
# Emacs::Lisp::Object::AUTOLOAD.
# FIXME: This code tries to be very clever for best performance,
# but it should be thoroughly tested.
my $get_funcall_closure = sub {
    my ($whose_funcall, $function) = @_;
    my ($fullname);

    $function =~ s/.*:://;
    $fullname = "${whose_funcall}::$function";
    if (defined &$fullname) {
	return \&$fullname;
    }
    if (exists $special{$function}) {
	my $msg = $special{$function};
	$msg = $special{$1}
	while $msg =~ /^\*(.*)/;
	$msg = (defined ($msg) ? "; $msg" : "");
	$function =~ tr/_/-/;
	local $Carp::CarpLevel = $Carp::CarpLevel + 1;
	croak "`$function' not implemented$msg";
    }
    $function = \*{"::$function"};
    $whose_funcall .= "::funcall";
    return *$fullname = sub { &$whose_funcall ($function, @_) };
};

# FIXME: need test for this.
my $can = sub {
    my ($symbol);
    $symbol = \*{"::$_[1]"};
    if (&fboundp ($symbol)
	and do {
	    my ($f);
	    $f = Emacs::Lisp::Object::symbol_function ($symbol);
	    (not $f->consp->is_nil) or $f->car->eq (\*::macro)->is_nil;
	}) {
	return eval { &$get_funcall_closure };
    }
    return undef;
};


package Emacs::Lisp::Object;

use vars qw ($AUTOLOAD);

sub AUTOLOAD {
    *$AUTOLOAD = $get_funcall_closure->(__PACKAGE__, $AUTOLOAD);
    goto &$AUTOLOAD;
}
sub can { &UNIVERSAL::can || $can->(__PACKAGE__, $_[1]) }

sub is_nil	($) { defined $_[0]->null->to_perl }
sub new		($$) { &to_lisp ($_[1]) }


package Emacs::Lisp::Variable;

sub TIESCALAR	{ bless ($_[1], $_[0]) }
sub FETCH	{ &Emacs::Lisp::symbol_value }
sub STORE	{ &Emacs::Lisp::set }


package Emacs::Lisp::Plist;

# tied hash interface to Lisp property lists
# tie (%sym, 'Emacs::Lisp::Plist', \*::sym)

sub TIEHASH	{ bless (\$_[1], $_[0]) }
sub FETCH	{ &Emacs::Lisp::get (${$_[0]}, $_[1]) }
sub STORE	{ &Emacs::Lisp::put (${$_[0]}, $_[1], $_[2]) }
sub CLEAR	{ &Emacs::Lisp::setplist (${$_[0]}, undef) }

# Look for $key (a Perl thing, typically a \*::globref) in all the
# even-numbered positions (zero-based) in $list (a Lisp list).
# If found, return the list's tail and the tail from one position
# up (needed for DELETE).  If not found, return an empty list.
#
# Assumes $list has an even number of elts.
# Should be called in list context.
sub memq_even ($$) {
    my ($list, $key) = @_;
    my ($prev);
    for (; not $list->is_nil; $list = ($prev = $list->cdr)->cdr)
    {
	if (&Emacs::Lisp::eq ($list->car, $key)) {
	    return ($list, $prev);
	}
    }
    return ();
}

sub EXISTS {
    #     "[T]here is no distinction between a value of `nil'
    #     and the absence of the property."
    #
    #     - *Note (elisp)Symbol Plists::. (Elisp Manual)
    #
    # Well now there is.  :-)
    #
    my ($symbol, $key) = @_;
    return (memq_even
	    (&Emacs::Lisp::Object::symbol_plist ($$symbol),
	     $key))[0] ? 1 : 0;
}

sub DELETE {
    my ($symbol, $key) = @_;
    my ($plist, $list, $prev);

    $plist = &Emacs::Lisp::Object::symbol_plist ($$symbol);
    if ($plist->is_nil) {
	return undef;
    }
    ($list, $prev) = memq_even ($plist, $key);
    if ($list) {
	if ($prev) {
	    $prev->setcdr ($list->cdr->cdr);
	} else {
	    &Emacs::Lisp::setplist ($$symbol, $list->cdr->cdr);
	}
    }
    return undef;
}

sub FIRSTKEY {
    my ($symbol) = @_;
    my ($plist);
    $plist = &Emacs::Lisp::Object::symbol_plist ($$symbol);
    return $plist->is_nil ? undef : $plist->car->to_perl;
}

sub NEXTKEY {
    my ($symbol, $lastkey) = @_;
    my ($plist, $list);
    $plist = &Emacs::Lisp::Object::symbol_plist ($$symbol);
    ($list) = memq_even ($plist, $lastkey);
    if ($list) {
	$list = $list->cdr->cdr;
	if (not $list->is_nil) {
	    # bad if nil is a key, I guess
	    return $list->car->to_perl;
	}
    }
    return undef;
}


package Emacs::Lisp;

sub import {
  my @newlist = ();
  for my $i (1..$#_) {
    next if $_[$i] !~ /([\$\@\%])(.*)/;
    my ($type, $name) = ($1, $2);
    if ($type eq '%') {
      next if tied %$name;
      tie %$name, 'Emacs::Lisp::Plist', \*{"::$name"};
    } else {
      next if tied $$name;
      tie $$name, 'Emacs::Lisp::Variable', \*{"::$name"};
    }
    push @newlist, $_[$i];
  }
  # XXX  Accommodating some undocumented Exporter.pm behavior here...
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

%EXPORT_TAGS =
    (
     funcs	=> [qw(
		       funcall
		       AUTOLOAD
		       )],
     special	=> [qw(
		       catch
		       defun
		       interactive
		       save_current_buffer
		       save_excursion
		       save_restriction
		       setq
		       track_mouse
		       )],
     extra	=> [qw(
		       lisp
		       t
		       nil
		       )],
     );

# Sorry guys, but you know Emacs is a hog.
@EXPORT = map { @$_ } values %EXPORT_TAGS;
# Is it too hard to add () after "use Emacs::Lisp"?

sub t () { \*::t }
sub nil () { undef }

sub AUTOLOAD {
    *$AUTOLOAD = $get_funcall_closure->(__PACKAGE__, $AUTOLOAD);
    goto &$AUTOLOAD;
}
sub can { &UNIVERSAL::can || $can->(__PACKAGE__, $_[1]) }

%special =
  (
  'setq_default'	=> "use `&set_default(\\*::symbol, \$value)' instead",
  'or'			=> "use Perl's `or' or `||' operator instead",
  'and'			=> "use Perl's `and' or `&&' operator instead",
  'if'			=> "use Perl's `if' or `?...:' operator instead",
  'cond'		=> "use Perl's `if', `elsif' and `else' instead",
  'progn'		=> "use a code block instead",
  'prog1'		=> "use a temporary variable instead",
  'prog2'		=> '*prog1',
  'quote'		=> "use `\\*::symbol' to quote a symbol, and "
			     . "`[\@items]' to make a list",
  'function'		=> '*quote',
  'defmacro'		=> "functionality to be added",
  'defvar'		=> "functionality to be added",
  'defconst'		=> '*defvar',
  'let*'		=> '*let',
  'let'			=> "functionality to be added",
  'while'		=> "use Perl's `for', `while' or `until' instead",
  'unwind_protect'	=> "use Perl's `local' or object destructors instead",
  'condition_case'	=> "use Perl's `eval' instead",
  'ml_if'		=> undef,
  'eval_when_compile'	=> "use a `BEGIN' block or Perl's `use' instead",
  );

sub setq (&) {
  my $coderef = shift;
  my $callpkg = caller;
  my @vars = _assignees($coderef);
  local $Exporter::ExportLevel = 1;
  import Emacs::Lisp grep { s/\Q$callpkg\E\:\:// && !/\:\:/ } @vars;
  &$coderef;
}

sub interactive (;$) {
  my $what = shift;
  bless \$what, 'Emacs::InteractiveSpec';
}

sub defun ($$;$$) {
  my $sym = shift;
  $sym = \*$sym
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
    
  my @form = (\*::lambda, [\*{"::&rest"}, \*::_Emacs__Lisp_args]);
  if (defined ($docstring)) {
    push @form, $docstring;
  }
  if (defined ($interactive)) {
    $interactive = $$interactive;
    if (ref ($interactive) eq 'CODE') {
      $interactive = [\*::perl_call, $interactive, \*::list_context];
    }
    if (defined ($interactive)) {
      push @form, [\*::interactive, $interactive];
    } else {
      push @form, [\*::interactive];
    }
  }
  push @form, [\*::apply, $body, \*::_Emacs__Lisp_args];
  &fset ($sym, [@form]);
  return $sym;
}

sub catch ($&) {
    &eval ([\*::catch, [\*::quote, $_[0]], [\*::funcall, $_[1]]]);
}

sub save_excursion (&) {
  &eval ([\*::save_excursion, [\*::funcall, $_[0]]]);
}

sub save_current_buffer (&) {
  &eval ([\*::save_current_buffer, [\*::funcall, $_[0]]]);
}

sub save_restriction (&) {
  &eval ([\*::save_restriction, [\*::funcall, $_[0]]]);
}

sub track_mouse (&) {
  &eval ([\*::track_mouse, [\*::funcall, $_[0]]]);
}

package Emacs::Lisp;
1;
__END__


=head1 NAME

Emacs::Lisp - Low-level support for Perl embedded in GNU Emacs

=head1 SYNOPSIS

=head2 In Emacs

  M-x perl-eval-expression RET 2+2 RET
  M-x perl-eval-region RET
  M-x perl-eval-buffer RET
  ... and more ...

=head2 In Perl

  use Emacs::Lisp;

  &switch_to_buffer('*scratch*');
  &insert("Hello, world!\n");

  setq { $cperl_font_lock = t };

  &add_hook(\*find_file_hooks,
	    sub { &message("found a file!") });

  use Emacs::Lisp qw($emacs_version $perlmacs_version);
  save_excursion {
    &set_buffer(&get_buffer_create("*test*"));
    &insert("This is Emacs version $emacs_version,\n");
    &insert("Perlmacs version $perlmacs_version.\n");
    &insert("Emacs::Lisp version is $Emacs::Lisp::VERSION.\n");
  };


=head1 DESCRIPTION

Until now, you could customize your Emacs environment using Lisp.  Now
you can use Perl, too.  This module allows Perl code to call functions
and access variables of Lisp.  It also maps some Perl syntax into Lisp
semantics.

You still need to learn some Lisp in order to understand I<The Elisp
Manual>, which you will need if you wish to learn about the details of
Emacs programming.  Hopefully, this situation will be cured by the
appearance of well-documented modules that give everything in Emacs a
nice, object-oriented Perl wrapping.


=head1 EMACS SUPPORT FOR PERL

The B<pmacs> program can compile and evaluate Perl code via Emacs Lisp
or the command line, independently of the Emacs::Lisp module.

=head2 Functions

Some Perl-related functions are built into Lisp.  Use `C<C-h f
E<lt>function-nameE<gt> RET>' within Emacs for documentation on these.

  perl-eval-expression	EXPRESSION
  perl-eval-region	START END
  perl-eval-buffer
  perl-load-file	NAME
  perl-eval		STRING &optional CONTEXT
  perl-call		SUB &optional CONTEXT &rest ARGS
  make-perl-interpreter	&rest ARGV
  get-perl-interpreter
  set-perl-interpreter	INTERPRETER
  perl-run		&optional INTERPRETER
  perl-destruct		&optional INTERPRETER

=head2 Data Conversions

When data is passed between Lisp and Perl, some basic principles
apply.  These are goals that will never be entirely met, due to the
differences between Perl and Lisp.

=over 4

=item * Whatever Perl gives to Lisp should get dereferenced once.

A Perl reference gets converted into a Lisp reference to the thing
referenced, not a Lisp reference to the Perl reference.  For example,
this code

  setq { $x = 16 };

sets the Lisp variable C<x> to the I<Lisp> integer 16, whereas

  setq { $x = \16 };

sets it to an object of type C<perl-scalar> which holds a I<Perl>
number 16.  I find this to be intuitively correct behavior.

=item * As an exception to the previous rule, arrayrefs become lists.

Lists are a central data structure in Lisp.  To make it as easy as
possible to pass lists to Lisp functions that require them, I think it
is necessary to perform a double dereference when converting Perl
array references.  (By "double dereference" I mean that C<\@a> is
dereferenced once to produce C<@a>, and again to get C<$a[0]>,
C<$a[1]>, and so on.)

Therefore, a Perl expression such as

  ["x", ["y", 1]]

is converted to

  '("x" ("y" 1))

in Lisp.

This kind of conversion entails quite a bit of overhead and precludes
"passing by reference" between the two languages, since it is a "deep"
copying operation.  Changes made by Lisp to the list will not affect
the Perl array of which it is a copy.

All of the above comments apply in reverse when one converts Lisp
lists to Perl.

=item * Converting a non-dereferenceable value requires some kind of
primitive conversion.

Lisp integers, floats, and strings all become Perl scalars.  A scalar
(other than a reference) converted to Lisp will become either an
integer, a float, or a string.  Glob references in package C<main>
become symbols in Lisp (subject to the usual underscore-to-hyphen
translation).

=item * Lisp's `nil' is equivalent to Perl's `undef' or `()'.

In Lisp, C<nil> is really a symbol.  However, it is typically used as
the boolean value I<false>.  Perl's equivalent of symbols (glob
references) evaluate to I<true> in boolean contexts.  Converting
C<nil> to anything other than C<undef> would be disastrous for
programmers' mental health!

=item * Generally, converted values can be converted back to Perl
unchanged (but often copied).

There are, however, exceptions.  For example, C<\*::nil> would become
C<undef>.

=item * Lisp objects which are not Perl stuff and have no natural
counterpart become `Emacs::Lisp::Object' blessed references.

These hold the actual Lisp data and a means of protecting them from
garbage collection.  An C<Emacs::Lisp::Object> reference converted
back to Lisp is the Lisp object, naturally.

=back

=head2 Scripts

Perlmacs can run Perl programs.  By default, Perlmacs is installed
under two names, B<pmacs> and B<perlmacs>.  Which name is used to
invoke the program can determine how it parses its command line.

If B<perlmacs> is used (or, to be precise, any name containing
"B<perl>"), it behaves like Perl (expecting a script, etc.).
Otherwise, it behaves like Emacs (opening a window, creating a buffer,
etc.).  In either case, the first command line argument can take
precedence.  If it is B<--emacs>, Emacs takes control.  If it is
B<--perl>, we play by Perl's rules.

The I<Emacs> module (that is, the Perl module named `Emacs') includes
support for starting an Emacs editing session from within a Perlmacs
script.  See L<Emacs>.


=head1 PERL SUPPORT FOR LISP

The Emacs::Lisp module allows Perl programs to invoke Lisp functions
and handle Lisp variables using Perl's syntax.

You don't have to do any work, other than C<use Emacs::Lisp>, to make
subs call their Lisp counterpart.  However, tying Lisp variables to
Perl variables is not quite so automatic.  In all cases, hyphens
(C<->) appearing in Lisp names are translated to underscores (C<_>) in
Perl, and vice versa.

=head2 Functions

This code calls the hypothetical Lisp function C<foo-bar> with
arguments C<4> and C<t>.

  &foo_bar(4, t);

The Lisp syntax for the same call would be

  (foo-bar 4 t)

The ampersand (C<&>) is really only needed for calling Lisp functions,
such as C<read>, C<eval>, and C<print>, which are Perl keywords.  But
using it is a good habit.

=head2 Symbols

Many Lisp functions take arguments that may be, or are required to be,
I<symbols>.  In Lisp, a symbol is a kind of name, but it not the same
type as a string.

Lisp programs typically use the C<quote> operator to specify a symbol.
For example, this Lisp code refers to the C<beep> symbol:

  (run-at-time nil 1 'beep)

The above is actually an abbreviated syntax for this:

  (run-at-time nil 1 (quote beep))

Perlmacs uses glob references of package C<main> to specify symbols.
A literal globref begins with a backslash followed by an asterisk, so
the last example would be written as

  &run_at_time(undef, 1, \*beep);

in Perl.  (You may want to do C<&cancel_function_timers(\*beep)> soon
after trying this example.)

Note that only globs from package C<main> may be used as Lisp symbols,
so code that is compiled in another package must use the form
C<\*::sym> rather than C<\*sym>.

When comparing the returned values of Lisp functions to each other and
to symbols, it is best to use the Lisp C<eq> function instead of
Perl's equality operators.

  ### PREFERRED
  if (&eq(&type_of($x), \*::cons)) { ... }

  ### PROBABLY OK
  if (&type_of($x) eq \*::cons) { ... }
  if (&type_of($x) == \*cons) { ... }

=head2 Variables

In Lisp, variables play a role akin to that of Perl scalars.  A
variable may hold a number, a string, or a reference to any type of
complex Lisp data structure.  (They are not called references in Lisp,
but rather "objects".)

You can create a Perl alias for any reasonably named Lisp variable by
saying C<use Emacs::Lisp qw($varname)>.  Thereafter, assignment to
C<$varname> will update the Lisp value.  Changes made to the variable
in Lisp will be reflected in Perl when C<$varname> is used in
expressions.

This example saves and replaces the value of the Lisp variable
C<inhibit-eol-conversion>:

  use Emacs::Lisp qw($inhibit_eol_conversion);
  $old_val = $inhibit_eol_conversion;
  $inhibit_eol_conversion = 1;

This sort of thing could be accomplished in Lisp as follows:

  (setq old-val inhibit-eol-conversion)
  (setq inhibit-eol-conversion 1)

(but you would probably rather use C<let> instead, for which there is
still no convenient Emacs::Lisp equivalent).  See also the C<setq>
function below.

=head2 Property Lists

Lisp symbols all have an associated object called a plist (for
"property list").  Although it is actually an object just like any
other, the plist is typically used in a way vaguely resembling Perl's
hashes.

Plists are not used nearly as often as functions and variables in
Lisp.  If you are new to Lisp, you can probably skip this section.

Note that a plist is different from a Perl hash.  Lookups are not
based on string equality as with Perl, but rather on Lisp object
equality (of the C<eq> variety).  For this reason, it is best to stick
to the Lisp convention of using only symbols as keys.  (See
L</Symbols>.)

Emacs::Lisp provides a shorthand notation for getting and setting
plist elements.  If you say C<use Emacs::Lisp qw(%any_name)>, then
subsequent access to the hash elements of C<%any_name> will really get
or set the corresponding plist entries (i.e., properties of the Lisp
symbol C<any-name>).

For example, the following Perl and Lisp fragments are more or less
equivalent:

  # Perl fragment
  use Emacs::Lisp qw(%booboo %upcase_region);
  $booboo{\*error_conditions} = [\*booboo, \*error];
  $can_upcase = ! $upcase_region{\*disabled};

  ; Lisp fragment
  (put 'booboo 'error-conditions '(booboo error))
  (setq can-upcase (not (get 'upcase-region 'disabled)))

See also the C<setq> function below.

=head2 Macros

So-called "macros" in Lisp, such as C<setq> and C<defun>, do not work
the same way functions do, although they are invoked using the
function syntax.  (Here you see the vast philosophical chasm
separating Perl from Lisp.  While Perl might have five syntaxes for
doing the same thing, Lisp uses one syntax for two different
purposes!)

Some macros are equivalent to Perl operators, such as C<if> and
C<while>.  Others have meanings peculiar to Lisp.  A few macros are
implemented in Emacs::Lisp.  They are listed below.  If you try to
call a macros that has not been implemented, you will get an error
message which may propose an alternative.

=over 8

=item catch SYMBOL,CODE

Evaluate CODE in a Lisp `catch' construct.  At any point during CODE's
execution, the `throw' function may be used to return control to the
end of the `catch' block.  For example:

  $x = catch \*::out, sub {
      $y = 1;
      &throw(\*::out, 16);
      $y = 2;
  };
  print $x;  # prints 16
  print $y;  # prints 1

Some Perl constructs have similar functionality to `throw', for
example, `return', `last LABEL'.  However, they do not work with
catches in Lisp code.

=item defun SYMBOL,DOCSTRING,SPEC,CODE

=item defun SYMBOL,DOCSTRING,CODE

=item defun SYMBOL,SPEC,CODE

=item defun SYMBOL,CODE

Make CODE callable as the Lisp function SYMBOL.  This is Lisp's
version of Perl's C<sub> keyword.  A function defined in this way
becomes visible to Lisp code.

This is useful for defining Emacs I<commands>.  Commands are functions
that the user can invoke by typing C<M-x E<lt>function-nameE<gt>>.  A
command may be bound to a key or sequence of keystrokes.  See the
Emacs documentation for specifics.

As explained in I<The Elisp Manual>, when defining a command, you must
specify the interactive nature of the command.  There are various
codes to indicate that the command acts on the current region, a file
name to be read from the minibuffer, etc.  Please see I<The Elisp
Manual> for details.

Emacs::Lisp's C<defun> uses a SPEC returned by C<interactive> for
specifying a command's interactivity.  If no SPEC is given, the
function will still be callable by Lisp, but will not be available to
the user via `C<M-x E<lt>function-nameE<gt> RET>' and cannot be bound
to a sequence of keystrokes.  See L</interactive>.

This example creates a command, C<reverse-region-words>, that replaces
a region of text with the same text after reversing the order of
words.  To be user-friendly, we'll provide a documentation string,
which will be accessible through the Emacs help system (`C<C-h f
reverse-region-words RET>').

  use Emacs::Lisp;
  defun (\*reverse_region_words,
	 "Reverse the order of the words in the region.",
	 interactive("r"),
	 sub {
	     my ($start, $end) = @_;
	     my $text = &buffer_substring($start, $end);
	     $text = join('', reverse split (/(\s+)/, $text));
	     &delete_region($start, $end);
	     &insert($text);
	 });

=item interactive SPEC

=item interactive

Used to generate the third (or, in the absence of a doc string, the
second) argument to C<defun>, which see.  This determines how a
command's arguments are obtained.

What distinguishes a "command" from an ordinary function, in the Emacs
parlance, is the presence of an C<interactive> specifier in the
C<defun> expression.

SPEC may be a string, as described in I<The Elisp Manual>, or a
reference to code which returns the argument list.

=item save_excursion BLOCK

Execute BLOCK within a Lisp C<save-excursion> construct.  This
restores the current buffer and other settings to their original
values after the code has completed.

Please read I<The Elisp Manual> for details.

=item setq BLOCK

BLOCK is searched for assignments (currently only at top level, but
this may change) of the form

    $var = EXPR;

where C<$var> is a scalar variable or hash element.  Every such
variable is imported from the Emacs::Lisp module as if you had said,
`C<use Emacs::Lisp qw($var)>'.

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

The following, which does not tie or import any variables, has the
same effect on Lisp as the above:

    use Emacs::Lisp ();
    &Emacs::Lisp::set( \*A, 2*$foo[5] );
    &Emacs::Lisp::put( \*B, \*foo, "more than "
      . &Emacs::Lisp::symbol_value( \*A ));

=back


=head1 BUGS

These are some of the known bugs in Perlmacs and Emacs::Lisp.  See
also the file F<BUGS> in the Perlmacs distribution.  If you find other
bugs, please check that you have the latest version, and email me.

=over 4

=item * Perl's `local()' doesn't have the effect of Lisp's `let'.

It should.  At least, there should be an easy way to make a local
binding of a Lisp variable in Perl.

=item * Function arg names in online documentation.

If you `defun' a function in Perl and later access its documentation
using Emacs' builtin mechanism, it always says the argument list is
`&rest -Emacs--Lisp-args'.

=item * Probably can't use a coderef as an error handler or protected
form.

I think this will be easy to fix, I just haven't had the need yet.

=item * A crash is likely if Perl code modifies the scalar value in an
`Emacs::Lisp::Object' blessed reference or explicity calls DESTROY on
it.

Don't do that.

=item * Possible memory leaks.

I have not tested for memory leaks.

=back

See also the (many) FIXMEs in F<src/perlmacs.c> and elsewhere in the
Perlmacs source.


=head1 CAVEATS

=over 4

=item * Circular data structures are bad.

See L<perlobj/"Two-Phased Garbage Collection">.  Lisp data structures
may be recursive (contain references to themselves) without the danger
of a memory leak, because Lisp uses a periodic-mark-and-sweep garbage
collector.

However, if a recursive structure involves I<any> Perl references, it
may I<never> be destroyable.

For best results, Perl code should deal mainly with Perl data, and
Lisp code should deal mainly with Lisp data.

=item * Cross-language references incur a slight overhead.

For the benefit of Lisp's garbage collection, all Perl data that is
referenced by Lisp participates in the mark phase.  For the benefit of
Perl's garbage collection, all Lisp objects that are referenced by
Perl maintain a (kind of) reference count.

A chain of Perl -> Lisp -> ... -> Perl references may take several
garbage collection cycles to be freed.  (At least, that is my theory.
I haven't verified it experimentally.)  It is therefore probably best
to keep the number and complexity of such references to a minimum.

(To the extent that the Perl-to-Emacs interface is independent of the
Lispish implementation of Emacs, these performance issues are fixable
in principle by reimplementing Emacs' internals.)

=back


=head1 TO DO

=over 4

=item * Bundle Perlmacs and its related modules.

=item * Provide XSubs for common, non-evalling functions.

There is substantial overhead in calling an arbitrary Lisp function,
because care must be taken to restore the Perl interpreter's state
when Lisp performs a non-local jump out of the function call.  This
can be avoided in the case of functions like cons, null bufferp, car,
eq, and symbol-value, for which a simple check can determine whether a
jump will occur.

=item * Special forms: unwind-protect, let, defmacro, defvar.

=item * Find a way to convert filehandles to the Emacs equivalent.

=item * Make a way to get a tied filehandle that reads a buffer.

=item * Improve perl-eval-buffer, perl-load-file, et al. (in C?)

=back


=head1 ACKNOWLEDGMENTS

These are among the giants on whose shoulders we stand:

=over 4

=item Larry Wall, inventor of Perl.

'Nuff said.

=item The developers of GNU, and Richard Stallman in particular.

Many thanks for the most beautiful code base that it has ever been, or
will ever likely be, my pleasure to hack.

=item John McCarthy, inventor of Lisp.

=item Dennis Ritchie, inventor of C.

Both Perl and Emacs are written in C.  The existence of Perlmacs kind
of rests on that fact.

=item Å…variste Galois (1811-1832) and Wolfgang Amadeus Mozart
(1756-1791).

Each of them demonstrated just how much a young man can accomplish
with a good mind.

=item Ludwig van Beethoven (1770-1827).

Showed that even neurotic, deaf, aging crabapples can, from time to
time, engender lasting beauty.

=item Tim Bunce, author of the `DynaLoader' module and much else.

I was lucky enough to come onto the Perl scene soon after dynamic
loading had reached its present form.  How you all got by before then
completely baffles me.

=item Doug MacEachern, author of the `ExtUtils::Embed' module and
mod_perl.

ExtUtils::Embed is a cornerstone of Perlmacs.

=back

This list is incomplete.

Thank you, Di Zhao <dzhao@primeon.com>, for braving the alphas and
showing me what can be done with Perlmacs.  If not for you, I would
still be wondering whether it could possibly have any use.

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

Copyright (C) 1998,1999 by John Tobey, jtobey@channel1.com.  All
rights reserved.

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

L<perl>, L<Emacs>, B<emacs>, and I<The Elisp Manual> (available where
you got the Emacs source, or from ftp://ftp.gnu.org/pub/gnu/emacs/).

=cut

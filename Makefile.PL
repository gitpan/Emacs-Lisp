use ExtUtils::MakeMaker;

use strict;
my ($emacs_info, $perlmacs_ver, $header_dir, $config_h_dir, $required_ver);
my ($cmd);

$required_ver = 0.8;

if (not defined &Emacs::Lisp::Object::DESTROY) {
    die <<DEAD;
You are using an ordinary Perl.  $0 must be run using Perlmacs.
For example:

    perlmacs $0

If you do not have Perlmacs, see the file README.

### $0 aborted.
DEAD
}

print "Checking Perlmacs version ($required_ver required)...\n";

### XXX Unixish
$cmd = ($^X.($^X =~ m:perl[^/]*$: ? " --emacs" : "")." -batch -eval"
	.q( '(progn (princ (concat perlmacs-version " ") (quote external-debugging-output)) (princ (concat include-directory " " exec-directory "\n")))')
	.' 2>&1');
$emacs_info = `$cmd`;
$emacs_info =~ s/\s*$//;
($perlmacs_ver, $header_dir, $config_h_dir) = split (/ /, $emacs_info);

$perlmacs_ver >= $required_ver
    or die <<DEAD;

Your Perlmacs (version $perlmacs_ver) is too old for this Emacs::Lisp.
Please upgrade to version $required_ver or higher before proceeding.

### $0 aborted.
DEAD

$perlmacs_ver >= 0.8
    && defined $header_dir
    && defined $config_h_dir
    or die <<DEAD;

Running this command:

    $cmd

gave the following unexpected output:

"$emacs_info"

This probably means your Perlmacs has not been properly installed, or
it is an old version. (version $required_ver is required)

### $0 aborted.
DEAD

$config_h_dir .= "include";
-f "$header_dir/emacs/lisp.h"
    && -f "$config_h_dir/emacs/config.h"
    or die <<DEAD;

Required header files are missing from $header_dir
and/or $config_h_dir.

### $0 aborted.
DEAD

WriteMakefile(
    'NAME'	=> 'Emacs::Lisp',
    'VERSION_FROM' => 'Lisp.pm', # finds $VERSION
    'INC'	=> "-I$config_h_dir -I$header_dir",
);

use ExtUtils::MakeMaker;

use strict;
my ($perlmacs_ver, @include_dirs, $required_ver);

$required_ver = '0.9';

$| = 1;

check_perlmacs ();
get_perlmacs_info ();
check_perlmacs_version ();
check_headers (qw(emacs/config.h emacs/lisp.h emacs/perlmacs.h));

WriteMakefile(
    'NAME'	=> 'Emacs::Lisp',
    'VERSION_FROM' => 'Lisp.pm', # finds $VERSION
    'INC'	=> join (" ", map { "-I$_" } @include_dirs),
    'dist'	=> { COMPRESS => 'gzip', SUFFIX => 'gz' },
);
exit;

sub check_perlmacs {
    return if (defined &Emacs::Lisp::Object::DESTROY
	       or defined &Emacs::constant);

    die <<DEAD;

You are using an ordinary Perl.  $0 must be run using Perlmacs.
For example:

    perlmacs $0

If you do not have Perlmacs, see the file README.

### $0 aborted.
DEAD
}

sub check_perlmacs_version {
    my (@wanted, @got, $sign);

    @wanted = split /\./, $required_ver;
    @got = split /\./, $perlmacs_ver;
    while (1) {
	if (!@wanted && !@got) {
	    $sign = 0; last;
	} elsif ($wanted[0] ne $got[0]) {
	    $sign = $got[0] <=> $wanted[0]; last;
	} else {
	    shift @wanted; shift @got;
	}
    }
    if ($sign == 0 && $perlmacs_ver =~ /pre/) {
	print <<END;

I see you are using a pre-release.  If you have any problems with this
module, please upgrade to at least Perlmacs $required_ver.  End of sermon.

END
    }
    return if ($sign >= 0);

    die <<DEAD;

Your Perlmacs (version $perlmacs_ver) is too old for this Emacs::Lisp.
Please upgrade to version $required_ver or higher before proceeding.

### $0 aborted.
DEAD
}

sub get_perlmacs_info {
    my ($cmd, $emacs_info, $header_dir, $exec_dir, $include_path);

    print "Checking Perlmacs version ($required_ver required)...";
    if (defined &Emacs::constant) {
	($perlmacs_ver, $include_path)
	    = map { Emacs::constant($_) }
		qw(PERLMACS_VERSION INCLUDE_PATH);
	print "$perlmacs_ver\n";
	@include_dirs = split /:/, $include_path;
	return;
    }

    $cmd = ($^X.($^X =~ m:perl[^/]*$: ? " --emacs" : "")." -batch -eval"
	    .q( '(progn (princ (concat perlmacs-version " ") (quote external-debugging-output)) (princ (concat include-directory " " exec-directory "\n")))')
	    .' 2>&1');
    $emacs_info = `$cmd`;
    $emacs_info =~ s/\s*$//;
    ($perlmacs_ver, $header_dir, $exec_dir) = split (/ /, $emacs_info);
    @include_dirs = ("${exec_dir}include", $header_dir);

$perlmacs_ver >= 0.8
    && defined $header_dir
    && defined $exec_dir
    or die <<DEAD;

Running this command:

    $cmd

gave the following unexpected output:

"$emacs_info"

This probably means your Perlmacs has not been properly installed, or
it is an old version. (version $required_ver is required)

### $0 aborted.
DEAD
   
    print "$perlmacs_ver\n";
}

sub check_headers {
    my (@missing, $pretty_dirs);
    while (my $header = shift) {
	grep { -f "$_/$header" } @include_dirs
	    or push @missing, $header;
    }
    return unless @missing;

    $pretty_dirs = join ("\n    ", @include_dirs);
    die <<DEAD;

These required header files:

    @missing

are missing from these directories:

    $pretty_dirs

You probably need to reinstall Perlmacs.

### $0 aborted.
DEAD
}

use ExtUtils::MakeMaker;

WriteMakefile(
    'NAME'	=> 'Emacs::Lisp',
    'VERSION_FROM' => 'Lisp.pm', # finds $VERSION
    'INC'	=> '-I$(INST_ARCHAUTODIR) -I$(INSTALLSITEARCH)/auto',
);

/* Copyright (C) 1998,1999 by John Tobey.  All rights reserved.

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
*/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

#include <emacs/config.h>
#include <emacs/lisp.h>
#include <emacs/perlmacs.h>


MODULE = Emacs::Lisp		PACKAGE = Emacs::Lisp

BOOT:
	perlmacs_init (ARGS);

SV *
funcall(...)
	PROTOTYPE: $@
	CODE:
	{
	  Lisp_Object *args, args_as_string, ret, tail;
	  int i;

	  args = (Lisp_Object *) alloca (items * sizeof (Lisp_Object));
	  for (i = 0; i < items; i++)
	    args [i] = perlmacs_sv_to_lisp (ST (i));

	  ret = perlmacs_funcall (items, args);
	  if (GIMME_V == G_VOID)
	    RETVAL = &PL_sv_undef;
	  else
	    RETVAL = perlmacs_lisp_to_sv (ret);
	}
	OUTPUT:
	RETVAL

SV *
lisp(sv)
	SV *sv;
	PROTOTYPE: $
	CODE:
	if (SvROK (sv))
	  RETVAL = perlmacs_sv_wrap_lisp (perlmacs_lisp_wrap_sv (SvRV (sv)));
	else
	  RETVAL = perlmacs_sv_wrap_lisp (perlmacs_sv_to_lisp (sv));
	OUTPUT:
	RETVAL


MODULE = Emacs::Lisp		PACKAGE = Emacs::Lisp::Object

# This funcall is identical to Emacs::Lisp::funcall except in how the
# returned value is converted to Perl.  Emacs::Lisp::funcall performs
# meaningful conversions of similar types, including deep copying of
# nested lists as arrayrefs.  Here we do not convert, we merely wrap.

# FIXME: Common parts of the two funcalls should be factored out.

SV *
funcall(...)
	PROTOTYPE: $@
	CODE:
	{
	  Lisp_Object *args, args_as_string, ret, tail;
	  int i;

	  args = (Lisp_Object *) alloca (items * sizeof (Lisp_Object));
	  for (i = 0; i < items; i++)
	    args [i] = perlmacs_sv_to_lisp (ST (i));

	  ret = perlmacs_funcall (items, args);
	  if (GIMME_V == G_VOID)
	    RETVAL = &PL_sv_undef;
	  else
	    RETVAL = perlmacs_sv_wrap_lisp (ret);
	}
	OUTPUT:
	RETVAL

SV *
to_perl(sv)
	SV *sv;
	PROTOTYPE: $
	CODE:
	if (! SV_LISPP (sv))
	  Perl_croak ("Not a Lisp object");
	RETVAL = perlmacs_lisp_to_sv (XSV_LISP (sv));
	OUTPUT:
	RETVAL

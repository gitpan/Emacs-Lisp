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


/* Try to ferret out the variables that get assigned to in a coderef.
   We only bother with simple top-level assignments and chains such as

      $foo = $bar = 0;

   not things like $baz in  $foo[$baz=4] = 7 .  Currently, we're only
   looking for scalars and hash elements.  */
/* XXX Replace with Perl code that uses B.  */
static void
push_op_assignees (startop)
     OP *startop;
{
  dSP;
  OP *op0, *op1, *op2, *op3;
  GV *gv;
  SV *sv;
  char *prefix;

  if (! (startop && (startop->op_flags & OPf_KIDS)))
    return;

  for (op0 = ((UNOP *) startop)->op_first; op0; op0 = op0->op_sibling) {
    if (op0->op_type == OP_SASSIGN) {
      PUTBACK;
      push_op_assignees(op0);
      SPAGAIN;
      if ((op0->op_flags & OPf_KIDS) && ((UNOP *) op0)->op_first) {

	op1 = ((UNOP *) op0)->op_first->op_sibling;
	if (op1 && op1->op_type == OP_NULL && (op1->op_flags & OPf_KIDS))
	  op1 = ((UNOP *) op1)->op_first;
	if (! op1)
	  continue;

	if (op1->op_type == OP_GVSV && (gv = ((GVOP *)op1)->op_gv))
	  prefix = "$";
	else if (op1->op_type == OP_HELEM
		 && (op1->op_flags & OPf_KIDS)
		 && (op2 = ((UNOP *) op1)->op_first)
		 && op2->op_type == OP_RV2HV
		 && (op2->op_flags & OPf_KIDS)
		 && (op3 = ((UNOP *) op2)->op_first)
		 && op3->op_type == OP_GV
		 && (gv = ((GVOP *)op3)->op_gv))
	  {
	    prefix = "%";
	  }
	else
	  continue;

	sv = newSV (0);
	gv_fullname3 (sv, gv, prefix);
	XPUSHs (sv_2mortal (sv));
      }
    }
  }
  PUTBACK;
}


MODULE = Emacs::Lisp		PACKAGE = Emacs

BOOT:
	if (top_level_perl)
	  {
	    char *dummy_argv[] = { "perl", 0 };
	    noninteractive = 1;
	    init_lisp (1, dummy_argv, 0);
	  }

int
main(...)
	PROTOTYPE: @
	CODE:
	{
	  char **argv;
	  int i;
	  int len;
	  SV *in_main;

	  if (! top_level_perl)
	    croak ("Can't call Emacs::main from within Emacs Lisp");

	  in_main = perl_get_sv ("Emacs::in_main", TRUE);
	  if (SvTRUE (in_main))
	    croak ("Sub Emacs::main can't be called recursively");

	  if (items == 0)
	    {
	      items = 1;
	      EXTEND (sp, 1);
	      /* Use "$0" as the arg if none given.  */
	      ST (0) = GvSV (gv_fetchpv("0", 1, SVt_PV));
	    }

	  argv = (char **) alloca ((items + 1) * sizeof (char *));
	  for (i = 0; i < items; i ++)
	    /* FIXME: Would it be too paranoid if we strdup() the args?  */
	    argv [i] = SvPV (ST (i), na);
	  argv [items] = 0;

	  sv_setsv (in_main, &sv_yes);
	  RETVAL = perl_call_emacs_main (items, argv, 0);
	  sv_setsv (in_main, &sv_no);
	}
	OUTPUT:
	RETVAL


MODULE = Emacs::Lisp		PACKAGE = Emacs::Lisp

SV *
funcall(...)
	PROTOTYPE: $@
	CODE:
	{
	  Lisp_Object *args, args_as_string, ret, tail;
	  int i;

	  args = (Lisp_Object *) alloca (items * sizeof (Lisp_Object));
	  for (i = 0; i < items; i++)
	    args [i] = sv_to_lisp (ST (i));

	  ret = perlmacs_funcall (items, args);
	  if (GIMME_V == G_VOID)
	    RETVAL = &PL_sv_undef;
	  else
	    RETVAL = lisp_to_sv (ret);
	}
	OUTPUT:
	RETVAL

void
_assignees(coderef)
	SV *	coderef;
	PROTOTYPE: &
	PPCODE:
	{
	  SV *rv;
	  OP *root;

	  /* Paranoid typechecking here.  */
	  if (! (SvROK(coderef) && (rv = SvRV(coderef))
		 && SvTYPE(rv) == SVt_PVCV && (root = CvROOT(rv))
		 && ((UNOP *) root)->op_type == OP_LEAVESUB
		 && (root->op_flags & OPf_KIDS)))
	    {
	      XSRETURN_EMPTY;
	    }

	  PUTBACK;
	  push_op_assignees(((UNOP *) root)->op_first);
	  SPAGAIN;
	}


MODULE = Emacs::Lisp		PACKAGE = Emacs::Lisp::Object

# This funcall is identical to Emacs::Lisp::funcall except in how the
# returned value is converted to Perl.  Emacs::Lisp::funcall performs
# meaningful conversions of similar types, including deep copying of
# nested lists as arrayrefs.  Here we do not convert, we merely wrap.

SV *
funcall(...)
	PROTOTYPE: $@
	CODE:
	{
	  Lisp_Object *args, args_as_string, ret, tail;
	  int i;

	  args = (Lisp_Object *) alloca (items * sizeof (Lisp_Object));
	  for (i = 0; i < items; i++)
	    args [i] = sv_to_lisp (ST (i));

	  ret = perlmacs_funcall (items, args);
	  if (GIMME_V == G_VOID)
	    RETVAL = &PL_sv_undef;
	  else
	    RETVAL = sv_wrap_lisp (ret);
	}
	OUTPUT:
	RETVAL

SV *
to_perl(sv)
	SV *sv;
	PROTOTYPE: $
	CODE:
	if (SV_LISPP (sv))
	  RETVAL = lisp_to_sv (XSV_LISP (sv));
	else
	  Perl_croak ("Not a Lisp object");
	OUTPUT:
	RETVAL

#if 0
SV *
to_lisp(sv)
	SV *sv;
	PROTOTYPE: $
	CODE:
	/* XXX is this useful?  will it ever be? */
	if (SV_LISPP (sv))  /* XXX overhead. needed? */
	  RETVAL = sv;
	else
	  RETVAL = sv_wrap_lisp (lisp_wrap_sv (sv));
	OUTPUT:
	RETVAL

#endif

/*
 * Copyright (C) 1998 by John Tobey.  All rights reserved.
 *
 * This file is *NOT* part of GNU Emacs.
 * However, this file may be distributed and modified under the same
 * terms as GNU Emacs.
 */

#include <Emacs/config.h>
#include <Emacs/lisp.h>

#include <setjmp.h>
#include <Emacs/eval.h>

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include <Emacs/perlmacs.h>


extern int perl_call_emacs_main _((int, char **, char **));


/* Try to ferret out the variables that get assigned to in a coderef.
   We only bother with simple top-level assignments and chains such as

      $foo = $bar = 0;

   not things like $baz in  $foo[$baz=4] = 7 .  Currently, we're only
   looking for scalars and hash elements.  */
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

	sv = newSV(0);
	gv_fullname3(sv, gv, prefix);
	XPUSHs(sv_2mortal(sv));
      }
    }
  }
  PUTBACK;
}

/* Recover Emacs state during a Perl nonlocal jump operation.
   cleanup_lisp() is a callback registered by SAVEDESTRUCTOR(), to be
   called by Perl when leaving the scope of a Perl-to-Lisp call.

   `catch->val' will be non-nil if the call exited normally (so there is
   no need for cleanup).  */
static void
cleanup_lisp (catch)
     struct catchtag *catch;
{
  struct catchtag *probe, *old = catch->next;

  if (NILP (catch->val))
    {
      /* This is a modified version of eval.c:unwind_to_catch().
	 It does not, however, call longjmp().  */

      /* Make sure we still have a place to unwind to.  */
      /* FIXME: A simple < or > comparison would be more efficient,
	 if we know the stack direction.  */
      for (probe = catchlist; probe != old; probe = probe->next)
	if (! probe)
	  return;

      /* See comment in perl_call_lisp() below.
      if (catchlist != old)
	set_poll_suppress_count (old->poll_suppress_count); */

      while (catchlist != old)
	{
	  /* Call any unwind-protect handlers in the right context.  */
	  unbind_to (catchlist->pdlcount, Qnil);
	  handlerlist = catchlist->handlerlist;
	  catchlist = catchlist->next;
	}
      unbind_to (catch->pdlcount, Qnil);
      handlerlist = catch->handlerlist;
      catchlist = catch->next;
      gcprolist = catch->gcpro;
      backtrace_list = catch->backlist;
      lisp_eval_depth = catch->lisp_eval_depth;
    }
}

static Lisp_Object
perl_call_lisp (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  struct catchtag c;
  Lisp_Object retval;

  /* Similar to eval.c:internal_catch().  */
  c.next = catchlist;
  /* No need to specify c.tag, since this struct will never be on the
     catchlist.  */
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.handlerlist = handlerlist;
  c.lisp_eval_depth = lisp_eval_depth;
  c.pdlcount = specpdl_ptr - specpdl;
  c.gcpro = gcprolist;
  /* I don't think this does anything useful, and it requires us
     to include keyboard.h.
  c.poll_suppress_count = poll_suppress_count;  */

  /* If Perl jumps out of this call, have it fix Lisp's state.  */
  ENTER;
  SAVEDESTRUCTOR (cleanup_lisp, &c);

  retval = Ffuncall (nargs, args);

  c.val = Qt;  /* Tell cleanup_lisp() to do nothing.  */
  LEAVE;

  return retval;
}

static Lisp_Object
perlmacs_error_handler (data)
     Lisp_Object data;
{
  croak("Lisp error: %s",
	SvPV(lisp_to_sv(Fprin1_to_string(data, Qt)), na));
}

static Lisp_Object
perl_enter_lisp (arr)
     Lisp_Object arr;
{
  int len = XSTRING(arr)->size;
  Lisp_Object *args = (Lisp_Object *) XSTRING(arr)->data;

  return perl_call_lisp(len / sizeof (Lisp_Object), args);
}


MODULE = Emacs::Lisp		PACKAGE = Emacs

BOOT:
	if (top_level_perl)
	  {
	    char *dummy_argv[] = { "perl", 0 };
	    noninteractive = 1;
	    init_lisp(1, dummy_argv, 0);
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

	  in_main = perl_get_sv("Emacs::in_main", TRUE);
	  if (SvTRUE(in_main))
	    croak ("Sub Emacs::main can't be called recursively");

	  if (items == 0)
	    {
	      items = 1;
	      EXTEND(sp, 1);
	      /* Use "$0" as the arg if none given.  */
	      ST (0) = GvSV(gv_fetchpv("0", 1, SVt_PV));
	    }

	  argv = (char **) alloca (items * sizeof (char *));
	  for (i = 0; i < items; i ++)
	    /* FIXME: Would it be too paranoid if we strdup() the args?  */
	    argv[i] = SvPV(ST(i), na);

	  sv_setsv(in_main, &sv_yes);
	  RETVAL = perl_call_emacs_main (items, argv, 0);
	  sv_setsv(in_main, &sv_no);
	}
	OUTPUT:
	RETVAL


MODULE = Emacs::Lisp		PACKAGE = Emacs::Lisp

Lisp_Object
funcall(...)
	PROTOTYPE: $@
	CODE:
	{
	  Lisp_Object *args;
	  int i;
	  int len = items * sizeof (Lisp_Object);

	  args = (Lisp_Object *) alloca(len);
	  for (i = 0; i < items; i++) {
	    args[i] = sv_to_lisp(ST(i));
	  }
	  RETVAL = catchlist ? perl_call_lisp(items, args)
	    : internal_condition_case_1(perl_enter_lisp,
					make_string ((char *) args, len),
					Qt, perlmacs_error_handler);
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

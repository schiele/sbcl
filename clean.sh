#!/bin/sh
set -e

# Remove everything in directories which are only used for output.
# In most cases, we can remove the directories, too.
#
# Preserving the skeletons of some directories might or might not not be relevant.
rm -rf obj/* src/runtime/genesis/ src/runtime/sbcl.mk src/runtime/*.dSYM

if [ -z "$SBCL_LEAVE_OUTPUT" ]
then
    rm -fr output/*
else
    for f in output/*; do
        
        if [ $f != output/ucd ]; then
            rm -r $f
        fi
    done
fi

# Ensure that we know GNUMAKE.
. ./find-gnumake.sh
find_gnumake

# Ask some other directories to clean themselves up.
original_pwd=`pwd`
for d in tools-for-build; do
    cd ./$d > /dev/null
    # I hope the -s option is standard. At least GNU make and BSD make
    # support it. It silences make, since otherwise the output from
    # this script is just the operations done by these make's, which
    # is misleading when this script does lots of other operations too.
    # -- WHN
    $GNUMAKE -I ../src/runtime -s clean
    cd "$original_pwd" > /dev/null
done
( cd ./doc && sh ./clean.sh )

# Within all directories, remove things which don't look like source
# files. Some explanations:
#   sbcl
#     the runtime environment, created by compiling C code
#   Config, target
#     architecture-dependent or OS-dependent symlinks
#   core
#     probably a Unix core dump -- not part of the sources anyway
#   *.o, *.so, *.lib, *.nm, a.out
#     results of C-style linking, assembling, etc.
#   *.core, *.map
#     looks like SBCL SAVE-LISP-AND-DIE or GENESIS output, and
#     certainly not source
#   *~, #*#
#     common names for editor temporary files
#   TAGS, tags
#     files created by GNU etags and ctags
#   .#*, *.orig, .*.orig, *.rej
#     rubbish left behind by CVS updates
#   *.htm, *.html
#     The system doc sources are mostly texinfo, plus various odds
#     and ends like docstrings embedded in .lisp sources; any HTML is
#     automatically-generated output.
#   depend, *.d
#     made by "make depend" (or "gmake depend" or some such thing)
#   *.lisp-obj, *.fasl, *.x86f, *.axpf, *.lbytef, *.lib
#     typical extensions for fasl files (not just from SBCL, but
#     from other Lisp systems which might be used as xc hosts)
#   *.tmp, *.lisp-temp
#     conventional names for temporary files autogenerated in
#     building or testing
find . \( \
        -name _darcs -o \
        -name '{arch}' -o \
        -name CVS -o \
        -name .hg -o \
        -name .git -o \
        -name .svn -o \
        -name android-libs \) -type d -prune -o \
       \( \
        \( -type l -path ./src/runtime/\* \) -o \
        -name '*~' -o \
        -name '#*#' -o \
        -name '.#*' -o \
        -name '*.orig' -o \
        -name '.*.orig' -o \
        -name '*.rej' -o \
        -name '?*.x86f' -o \
        -name '?*.axpf' -o \
        -name '?*.lbytef' -o \
        -name '?*.fasl' -o \
        -name '?*.FASL' -o \
        -name 'core' -o \
        -name '?*.core' -o \
        -name '*.map' -o \
        -name '*.host-obj' -o \
        -name '*.lisp-obj' -o \
        -name '*.target-obj' -o \
        -name '*.lib' -o \
        -name '*.tmp' -o \
        -name '*.lisp-temp' -o \
        -name '*.o' -o \
        -name '*.so' -o \
        -name '*.d' -o \
        -name 'foo.c' -o \
        -name 'test-lab' -o \
        -name 'encodings.texi-temp' -o \
        -name 'stack-alignment-offset' -o \
        -name 'test-status.lisp-expr' -o \
        -name 'last-random-state.lisp-expr' -o \
        -name 'test.log' -o \
        -name 'a.out' -o \
        -name 'sbcl' -o \
        -name 'ppc-linux-mcontext.h' -o \
        -name 'depend' -o \
        -name 'TAGS' -o \
        -name 'tags' -o \
        -name 'local-target-features.lisp-expr' \) -print | xargs rm -fr
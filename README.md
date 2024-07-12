# mon-systems Description

MON utils agglomerated.

Known to compile on SBCL 2.4.6

# mon-cl-systems dependencies:

SPLIT-SEQUENCE
STRING-CASE
ALEXANDRIA
CL-PPCRE
FLEXI-STREAMS
IRONCLAD
SALZA2
CHIPZ
CLOSER-MOP
CFFI
UIOP
OSICAT
:cl-fad  ;; :NOTE This is likely to be deprecated in favor of osicat/uiop which provides most of same.
:local-time

# Installation

```
shell> git clone https://github.com/monkpearman/mon-systems-cl.git
````

start sbcl from emacs or shell

```Common Lisp
CL-USER> (push asdf:*central-registry* (push #P"/parent/dir/where/you/put/cl-repos/mon-cl-systems/"            asdf:*central-registry*)

CL-USER> (quicklisp:quickload :mon :verbose t :explain t)

CL-USER> (in-package :mon)

CL-USER> ... Hacks and Glory Await ... 
```

# System Breakdown

## File specials.lisp

Special Variables, Constants, and convenience macros for binding them.
Aslo, includes functions for adding docstrings.  

## File types.lisp

Common type definitions; simple and compound.

## File macros.lisp

Common macro idioms, culled from variouis sources.

## File File-io.lisp

Functions and with sytle macros for frobbing files and file streams.

## File environ.lisp

Functions for inspecting and frobbing the system and lisp envirionment.

## File char-numeric.lisp

This file exists for the sole purpose of segregating `char-numeric='
from the rest of the system. We do this b/c on SBCL `%char-numeric=' is
defined with significant optimizations and if any portion of this file is
changed we will get a restart at compile-time. IOW unless specifically
editing `%char-numeric=' do your edits elswhere!

## File chars.lisp

Character frobbing, inspection, and introspection utilities.

## File seqs.lisp

Sequence related utilities.

## File class-utils.lisp

Class frobbing, inspection, and introspection utilities.

## File numbers.lisp

"Number" frobbing utilities.

## File plist.lisp

Property list frobbing utilities.

## File alist.lisp

Association list frobbing utilities.

## File hash.lisp

Hash-table frobbing, inspection, and introspection utilities.

## File strings.lisp

String frobbing, inspection, and introspection utilities.

## File introspect.lisp

Utilities for frobbing, inspecting, and introspecting symbols, function
definitions, keywords, packages.

## File bit-twiddle.lisp

Utilities for bit twiddling.

## File arrays.lisp

Utilities for working with arrays.

## File file-dir.lisp

Utilities for frobbing, inspecting, and introspecting on files, directories,
namestrings, and pathnames.

## File io.lisp

Utilities for reading/writing wth streams and cl:*print-<FOO>* variables.

## File chronos.lisp

Utilities for time related operations.

## File regexp.lisp

Utilities for working with regular expressions and string frobbing.

## File format.lisp

Utilities for working with CL:FOMRAT.

## File compose.lisp

Utilities for functional composition.

## File conditions.lisp

Some condition definitions for better error handling. 

## File class-doc.lisp

Utilities for class documention.

## File docs.lisp

A revamp of sb-texinfo for documentation generation. 

## File loadtime-bind and loadtime-bind.lisp

File laodtime-bind adds a reader conditional #+/-:IS-MON-P 
it is read at system loadtime and allows conditionalizing code according to the environment.

File laodtime-bind.lisp actualizes the conditionalizing per above.





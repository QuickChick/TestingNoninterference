#!/usr/bin/perl -w
use strict;

# This script takes our prettified output, translates it back into a
# Haskell-readable, and places each value in the output onto a separate line.

# Slurp in all of the input.
$/ = undef;
$_ = <>;

s/^[^\S\n]*[\w']+[^\S\n]+(?=[AC]S)//gm;           # Remove wrappers (e.g., Just and Shrink2)
s/([^ ]+)@([LH])/(Labeled $2 ($1))/gm;            # Make labeled values readable
s/^[^\S\n]+(?=[AC]S)/[ /gm;                       # Try to detect if we're starting a trace
s/^-->[^\S\n]*/, /gm;                             # Separate trace entries with commas
s/^[^\S\n]+//gm;                                  # Remove leading spaces
tr/\n//d;                                         # Remove newlines
s/STOP/ ]\n/gm;                                   # End traces (with newlines, too)
s/(?<!, )([AC]S\s*\{.+?\})(?!,|\s*\])/$1\n/gm;    # Append a newline to values not in traces
s/^(?=.*<([\w']+)>)/{which_tmm_routine = $1}/gm;  # Figure out which TMM routine is being used and produce the record update
s/\[ <([\w']+)>,? /tmmRoutine ++ [ /gm;           # Reify the TMM routine
s/^((\{.+?\})?)/let ?dfs = dynFlagsDflt$1 in /gm; # Bind the dynamic flags (leaving the record update in place)

print

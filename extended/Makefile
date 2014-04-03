execs=Driver

ghc_compilation_opts = -O2 -Werror
ghc_warning_opts     = -fwarn-missing-signatures  \
                       -fwarn-unused-do-bind -fwarn-incomplete-patterns \
                       -fwarn-tabs
ghc_include_opts     = -i$(top_level_dir)/common
ifdef DEBUG
ghc_debug_opts       = -rtsopts -prof -auto-all
else
ghc_debug_opts       =
endif

ghc_common_opts = $(ghc_warning_opts) $(ghc_include_opts)
all_ghc_opts    = $(ghc_compilation_opts) $(ghc_common_opts) $(ghc_debug_opts) $(ghc_opts)

all: $(execs)

% : %.hs *.hs Makefile
	ghc --make -main-is $@ $(all_ghc_opts) $<

%.hi %.o : %.hs
	ghc --make -c $(all_ghc_opts) $<

clean:
	rm -f *.hi *.o $(execs)

exec_subdirs = basic extended
util_subdirs = common
.PHONY : $(exec_subdirs) $(util_subdirs)

all   : $(exec_subdirs)
tests : all $(exec_subdirs:=-tests)
clean : $(exec_subdirs:=-clean) $(util_subdirs:=-clean)

rebuild:
	svn up
	$(MAKE) -C tmu picotables-parallel
	svn up
	svn commit -m "Picotables rebuilt"

rebuild-slow:
	svn up
	$(MAKE) -C tmu picotables-slow
	svn up
	svn commit -m "Picotables rebuilt (on just one core, 1000s)"

$(exec_subdirs) :
	$(MAKE) -C $@ all

%-all :
	$(MAKE) -C $* all

%-tests :
	$(MAKE) -C $* tests

%-clean :
	$(MAKE) -C $* clean

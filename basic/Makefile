execs = Driver

## NB: A string containing a Haskell list
# SIMPLEBUGS = "[ IfcBugArithNoTaint \
#               , IfcBugPushNoTaint  \
#               , IfcBugLoadNoTaint  \
#               , IfcBugStoreNoPointerTaint \
#               , IfcBugAllowWriteDownThroughHighPtr ]"
# 
# INSTRS = Basic
# SMART_INTS = True
# EQUIV = Full
GENERIC_TMU_OPTIONS = --which-tmm-routine=NoTMMRoutine

include ../Makefile.common

tests : Driver
	-./Driver

temp:
	make -f Makefile.picotables picotable-Cally-EENI-Low-Initial-ByExec2-True EXTRATMUARGS="--tmu-timeout=300" SANITY_CHECK=
	make -f Makefile.picotables picotable-Cally-EENI-Low-QuasiInitial-ByExec2-True EXTRATMUARGS="--tmu-timeout=300" SANITY_CHECK=

temp1:
	make -f Makefile.picotables picotable-Cally-EENI-Low-Initial-ByExec2-True EXTRATMUARGS="--tmu-timeout=3" SANITY_CHECK=
	make -f Makefile.picotables picotable-Cally-EENI-Low-QuasiInitial-ByExec2-True EXTRATMUARGS="--tmu-timeout=3" SANITY_CHECK=


#########################################################

latex-counterexample-old: Driver
	export FILENAME="../picopaper/counter-$(INSTRS)-$(PROP)-$(EQUIV)-$(BUG).tex" ; \
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=Instrs$(INSTRS) \
	  --ifc-semantics="[IfcBug$(BUG)]" \
	  --tmu-prop-test=Prop$(PROP) \
	  --equiv=Equiv$(EQUIV) \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --shrink-nothing=False \
	  --show-counterexamples=True \
	  --run-timeout-tests=False \
	  --tmu-max-tests=100000 \
	  --latex-output \
	  $(EXTRATMUARGS) \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME

counterexamples: ce1 ce10 ce11

ce1:
	$(MAKE) latex-counterexample-old INSTRS=Basic BUG=PushNoTaint PROP=EENI EQUIV=Mem STRATEGY=Naive START=Initial SMART_INTS=False

ce10:
	$(MAKE) latex-counterexample-old INSTRS=Jumpy BUG=JumpNoRaisePc PROP=EENI EQUIV=Mem STRATEGY=ByExec START=Initial SMART_INTS=True

ce11:
	$(MAKE) latex-counterexample-old INSTRS=Jumpy BUG=JumpLowerPc PROP=EENI EQUIV=Mem STRATEGY=ByExec START=Initial SMART_INTS=True

#########################################################

latex-counterexample-aux: Driver
	-export FILENAME="../picopaper/counter-$(NAME).tex" ; \
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=Instrs$(INSTRS) \
	  --ifc-semantics="[$(BUGS)]" \
	  --tmu-prop-test=Prop$(PROP) \
	  --equiv=Equiv$(EQUIV) \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --shrink-nothing=False \
	  --show-counterexamples=True \
	  --run-timeout-tests=False \
	  --tmu-max-tests=100000 \
	  --latex-output \
	  $(EXTRATMUARGS) \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME

latex-counterexample:
	$(MAKE) latex-counterexample-aux \
            PROP=EENI EQUIV=Mem STRATEGY=ByExec2 START=Initial \
	    SMART_INTS=True 

basic1:
	$(MAKE) latex-counterexample \
	    NAME=basic1 \
            INSTRS=Basic \
	    BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugStoreNoValueTaint,IfcBugAllowWriteDownThroughHighPtr 

basic2:
	$(MAKE) latex-counterexample \
	    NAME=basic2 \
            INSTRS=Basic \
	    BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr 

basic3:
	$(MAKE) latex-counterexample \
	    NAME=basic3 \
            INSTRS=Basic \
	    BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugAllowWriteDownThroughHighPtr 

basic4:
	$(MAKE) latex-counterexample \
	    NAME=basic4 \
            INSTRS=Basic \
	    BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint 

basic5:
	$(MAKE) latex-counterexample \
	    NAME=basic5 \
            INSTRS=Basic \
	    BUGS=IfcBugLoadNoTaint 

jumpy1:
	$(MAKE) latex-counterexample \
	    NAME=jumpy1 \
            INSTRS=Jumpy \
	    BUGS=IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc

jumpy2:
	$(MAKE) latex-counterexample \
	    NAME=jumpy2 \
            INSTRS=Jumpy \
	    BUGS=IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc

# These two counterexamples are not findable for the Jumpy machine
# we need calls and returns to find these
# jumpy-no-bug:
# 	$(MAKE) latex-counterexample \
# 	    NAME=jumpy-no-bug \
#             INSTRS=Jumpy \
# 	    BUGS=IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc

cally1:
	$(MAKE) latex-counterexample \
	    NAME=cally1 \
            INSTRS=Cally \
	    BUGS=IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns

cally2:
	$(MAKE) latex-counterexample \
	    NAME=cally2 \
            INSTRS=Cally \
	    BUGS=IfcBugAllowWriteDownWithHighPc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns

cally3:
	$(MAKE) latex-counterexample \
	    NAME=cally3 \
            INSTRS=Cally \
	    BUGS=IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns

cally4:
	$(MAKE) latex-counterexample \
	    NAME=cally4 \
            INSTRS=Cally \
	    BUGS=IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns

cally5:
	$(MAKE) latex-counterexample \
	    NAME=cally5 \
            INSTRS=Cally \
	    BUGS=IfcBugPopPopsReturns

bug1:
	$(MAKE) latex-counterexample \
	    NAME=bug1 \
            INSTRS=Cally \
	    BUGS=IfcBugArithNoTaint

bug2:
	$(MAKE) latex-counterexample \
	    NAME=bug2 \
            INSTRS=Cally \
	    BUGS=IfcBugPushNoTaint

bug3:
	$(MAKE) latex-counterexample \
	    NAME=bug3 \
            INSTRS=Cally \
	    BUGS=IfcBugLoadNoTaint

bug4:
	$(MAKE) latex-counterexample \
	    NAME=bug4 \
            INSTRS=Cally \
	    BUGS=IfcBugStoreNoPointerTaint

bug5:
	$(MAKE) latex-counterexample \
	    NAME=bug5 \
            INSTRS=Cally \
	    BUGS=IfcBugAllowWriteDownThroughHighPtr

bug6:
	$(MAKE) latex-counterexample \
	    NAME=bug6 \
            INSTRS=Cally \
	    BUGS=IfcBugStoreNoValueTaint

bug7:
	$(MAKE) latex-counterexample \
	    NAME=bug7 \
            INSTRS=Cally \
	    BUGS=IfcBugJumpNoRaisePc

bug8:
	$(MAKE) latex-counterexample \
	    NAME=bug8 \
            INSTRS=Cally \
	    BUGS=IfcBugJumpLowerPc

bug9:
	$(MAKE) latex-counterexample \
	    NAME=bug9 \
            INSTRS=Cally \
	    BUGS=IfcBugStoreNoPcTaint

bug10:
	$(MAKE) latex-counterexample \
	    NAME=bug10 \
            INSTRS=Cally \
	    BUGS=IfcBugAllowWriteDownWithHighPc

bug11:
	$(MAKE) latex-counterexample \
	    NAME=bug11 \
            INSTRS=Cally \
	    BUGS=IfcBugCallNoRaisePc

bug12:
	$(MAKE) latex-counterexample \
	    NAME=bug12 \
            INSTRS=Cally \
	    BUGS=IfcBugReturnNoTaint

bug13:
	$(MAKE) latex-counterexample \
	    NAME=bug13 \
            INSTRS=Cally \
	    BUGS=IfcBugValueOrVoidOnReturn

bug14:
	$(MAKE) latex-counterexample \
	    NAME=bug14 \
            INSTRS=Cally \
	    BUGS=IfcBugPopPopsReturns

high-equiv-everything:
	$(MAKE) latex-counterexample-aux \
            PROP=EENI EQUIV=Mem STRATEGY=ByExec2 START=Initial \
	    SMART_INTS=True \
	    NAME=high-equiv-everything \
            INSTRS=Cally \
	    BUGS=\
            EXTRATMUARGS="--atom-equiv=HighEquivEverything"

not-observable:
	$(MAKE) latex-counterexample-aux \
            PROP=EENI EQUIV=Mem STRATEGY=ByExec2 START=Initial \
	    SMART_INTS=True \
	    NAME=not-observable \
            INSTRS=Cally \
	    BUGS=\
            EXTRATMUARGS="--atom-equiv=LabelsNotObservable"

eeni-no-low:
	$(MAKE) latex-counterexample-aux \
            PROP=EENInoLow EQUIV=Mem STRATEGY=ByExec2 START=Initial \
	    SMART_INTS=True \
	    NAME=eeni-no-low \
            INSTRS=Cally \
	    BUGS=\

stk-elt-equiv-label-on-top:
	$(MAKE) latex-counterexample-aux \
            PROP=EENI EQUIV=Low STRATEGY=ByExec2 START=QuasiInitial \
	    SMART_INTS=True \
	    NAME=stk-elt-equiv-label-on-top \
            INSTRS=Cally \
	    BUGS=\
            EXTRATMUARGS="--stk-elt-equiv=LabelOnTop" \

ssni-equivlow-too-weak:
	$(MAKE) latex-counterexample-aux \
            PROP=SSNI EQUIV=Low STRATEGY=TinySSNI START=Arbitrary \
	    SMART_INTS=True \
	    NAME=ssni-equivlow-too-weak \
            INSTRS=Cally \
	    BUGS=\

ssni-equivwrongfull:
	$(MAKE) latex-counterexample-aux \
            PROP=SSNI EQUIV=WrongFull STRATEGY=TinySSNI START=Arbitrary \
	    SMART_INTS=True \
	    NAME=ssni-equivwrongfull \
            INSTRS=Basic \
	    BUGS=\

all-bugs: bug1 bug2 bug3 bug4 bug5 bug6 bug7 bug8 bug9 bug10 bug11 bug12 bug13 bug14

latex-counterexamples: basic1 basic2 basic3 basic4 basic5 basic6 jumpy1 jumpy2 cally1 cally2 cally3 cally 4 cally5

#########################################################

Makefile.picoprofiles: Driver GenerateTablesMakefile.hs
	runhaskell GenerateTablesMakefile.hs --config=GenMakefileProf > Makefile.picoprofiles
	runhaskell GenerateTablesMakefile.hs --config=GenTablesProf   > ../picopaper/picoprofile-names.tex

picoprofiles: Driver Makefile.picoprofiles
	$(MAKE) -f Makefile.picoprofiles

picoprofile:
	export FILENAME="../picopaper/prof-$(START)-$(STRATEGY)-$(SMART_INTS).tex" ; \
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=InstrsBasic \
	  --ifc-semantics="[IfcDefault]" \
	  --tmu-prop-test=PropJustProfile \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --latex-output \
	  --shrink-nothing=True \
	  $(EXTRATMUARGS) \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME

picoprofilevar:
	export FILENAME="../picopaper/profvar-$(START)-$(STRATEGY)-$(SMART_INTS).tex" ; \
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=InstrsBasic \
	  --ifc-semantics="[IfcDefault]" \
	  --tmu-prop-test=PropJustProfileVariation \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --latex-output \
	  --shrink-nothing=True \
	  $(EXTRATMUARGS) \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME


#########################################################

Makefile.picotables: Driver GenerateTablesMakefile.hs
	runhaskell GenerateTablesMakefile.hs --config=GenMakefileRel > Makefile.picotables
	runhaskell GenerateTablesMakefile.hs --config=GenTablesRel > ../picopaper/picotable-names.tex

picotables: Driver picosettings Makefile.picotables
	time $(MAKE) -f Makefile.picotables

sanity-checks:
	@$(MAKE) picotables MAKE_PICOTABLE=

picotables-fast : 
	@$(MAKE) picotables SANITY_CHECK=

picotables-fast-sanity-checks : 
	@$(MAKE) picotables

picotables-parallel : 
	@$(MAKE) -j4 picotables EXTRATMUARGS="--tmu-timeout=300" SANITY_CHECK=

picotables-slow : 
	@$(MAKE) picotables EXTRATMUARGS="--tmu-timeout=1000" SANITY_CHECK=

SANITY_CHECK   = sanity-check
MAKE_PICOTABLE = make-picotable

picotables-sequence-eeni: Driver picosettings Makefile.picotables
	time $(MAKE) picotable-Basic-EENI-Mem-Initial-Sequence-False -f Makefile.picotables
	time $(MAKE) picotable-Basic-EENI-Mem-Initial-Sequence-True -f Makefile.picotables

sanity-check:
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=Instrs$(INSTRS) \
	  --ifc-semantics="[IfcDefault]" \
	  --tmu-prop-test=Prop$(PROP) \
	  --equiv=Equiv$(EQUIV) \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --shrink-nothing=False \
	  --show-counterexamples=True \
	  --run-timeout-tests=False \
	  --tmu-max-tests=10000 \
	  $(EXTRATMUARGS)

make-picotable: $(SANITY_CHECK)
	export FILENAME="../picopaper/exp-$(INSTRS)-$(PROP)-$(EQUIV)-$(START)-$(STRATEGY)-$(SMART_INTS).tex" ; \
	echo '\\gdef\\bugtableinfo{'$$FILENAME'}%' > $$FILENAME.tmp ; \
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	  --gen-instrs=Instrs$(INSTRS) \
	  --ifc-semantics="$(BUGS)" \
	  --tmu-prop-test=Prop$(PROP) \
	  --equiv=Equiv$(EQUIV) \
	  --starting-as=Start$(START) \
	  --gen-strategy=Gen$(STRATEGY) \
	  --smart-ints=$(SMART_INTS) \
	  --shrink-nothing=True \
	  --show-counterexamples=False \
	  --latex-output \
	  $(EXTRATMUARGS) \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME

picotable: $(SANITY_CHECK) $(MAKE_PICOTABLE)

picosettings :
	echo '% Automatically generated -- do not modify by hand' > ../picopaper/expsettings.tex
#	echo '\\newcommand{\\TMUATTEMPTS}{$(TMUATTEMPTS)}' >> ../picopaper/expsettings.tex
#	echo '\\newcommand{\\TMUTESTS}{$(TMUTESTS)}' >> ../picopaper/expsettings.tex

%.picotables : Driver %.picotableheader \
	$*.picobugs

%.picotable : Driver 
	echo % Stats for $* > ../picopaper/exp-$*.tex
	$(MAKE) $*.picobugs \
	    >> ../picopaper/exp-$*.tex

%.picobugs :
	@./Driver $(GENERIC_TMU_OPTIONS) \
	             --show-counterexamples=False --gen-strategy=$* $(EXTRATMUARGS) \
	             --ifc-semantics=$(SIMPLEBUGS) --tmu-prop-test=PropEENI --starting-as=StartQuasiInitial --gen-instrs=Instrs$(INSTRS) \
	             --latex-output 

test-sequence:
	time $(MAKE) -j4 -f Makefile.picotables \
		picotable-Basic-EENI-Mem-Initial-Sequence-False \
		EXTRATMUARGS="--tmu-timeout=300" \
		SANITY_CHECK=

BUGS="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoValueTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugPopPopsReturns,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn]"

test-sequence2:
	./Driver \
	   --which-tmm-routine=NoTMMRoutine \
	   --gen-instrs=InstrsCally \
	   --ifc-semantics=$(BUGS) \
	   --tmu-prop-test=PropLLNI \
	   --equiv=EquivLow \
	   --starting-as=StartQuasiInitial \
	   --gen-strategy=GenSequence \
	   --smart-ints=True \
	   --shrink-nothing=True \
	   --show-counterexamples=False \
	   --latex-output \

test-genbyexec2:
	./Driver   --which-tmm-routine=NoTMMRoutine   --gen-instrs=InstrsCally   --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoValueTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugPopPopsReturns,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn]"   --tmu-prop-test=PropEENI   --equiv=EquivMem   --starting-as=StartInitial   --gen-strategy=GenByExec2   --smart-ints=True   --shrink-nothing=True   --show-counterexamples=False   --latex-output   --tmu-timeout=300

test-genbyexec4:
	./Driver   --which-tmm-routine=NoTMMRoutine   --gen-instrs=InstrsCally   --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoValueTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugPopPopsReturns,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn]"   --tmu-prop-test=PropEENI   --equiv=EquivMem   --starting-as=StartInitial   --gen-strategy=GenByExec4   --smart-ints=True   --shrink-nothing=True   --show-counterexamples=False   --latex-output   --tmu-timeout=300

test-genbyfwdexec:
	./Driver   --which-tmm-routine=NoTMMRoutine   --gen-instrs=InstrsCally   --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoValueTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugPopPopsReturns,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn]"   --tmu-prop-test=PropEENI   --equiv=EquivMem   --starting-as=StartInitial   --gen-strategy=GenByFwdExec   --smart-ints=True   --shrink-nothing=True   --show-counterexamples=False   --latex-output   --tmu-timeout=300

test-genbyexec:
	./Driver   --which-tmm-routine=NoTMMRoutine   --gen-instrs=InstrsCally   --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoValueTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugPopPopsReturns,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn]"   --tmu-prop-test=PropEENI   --equiv=EquivMem   --starting-as=StartInitial   --gen-strategy=GenByExec   --smart-ints=True   --shrink-nothing=True   --show-counterexamples=False   --latex-output   --tmu-timeout=300

labelObsBug:
	./Driver --ifc-semantics="[]" --if-labels-observable=False --gen-strategy=genbyexec2 --show-counterexamples --run-timeout-test=False --tmu-prop-test=propeeni --equiv=equivfull --starting-as=startinitial --which-tmm-routine=notmmroutine --gen-instrs=instrscally --shrink-nothing=False --shrink-noops=True --shrink-to-noop=True --latex-output

prof-sequence3: Driver
	export FILENAME="../picopaper/prof-Cally-Initial-Sequence-False.tex" ; \
	./Driver \
	  --which-tmm-routine=NoTMMRoutine \
	  --gen-instrs=InstrsCally \
	  --ifc-semantics="[IfcDefault]" \
	  --tmu-prop-test=PropJustProfile \
	  --starting-as=StartQuasiInitial \
	  --gen-strategy=GenSequence \
	  --smart-ints=True \
	  --latex-output \
	  --shrink-nothing=True \
	   \
	| tee -a $$FILENAME.tmp; \
	mv $$FILENAME.tmp $$FILENAME

talk: Driver
	./Driver \
	  $(GENERIC_TMU_OPTIONS) \
	   --gen-instrs=InstrsBasic \
	   --ifc-semantics="[$(BUGS)]" \
	   --tmu-prop-test=PropEENI \
	   --equiv=EquivMem \
	   --starting-as=StartInitial \
	   --gen-strategy=GenByExec2 \
	   --smart-ints=True \
	   --shrink-nothing=False \
	   --show-counterexamples=True \
	   --run-timeout-tests=False \
	   --tmu-max-tests=100000 \
	   --latex-output=False \
	   $(EXTRATMUARGS) \
	  | sed "s/^\([0-9]*\)@L/\1/" \
          | sed s/M=// | sed s/S=// | sed s/next=//

talk1:
	$(MAKE) talk \
	BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugStoreNoValueTaint,IfcBugAllowWriteDownThroughHighPtr

talk2:
	$(MAKE) talk \
	BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr

talk3:
	$(MAKE) talk \
	BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugAllowWriteDownThroughHighPtr

talk4:
	$(MAKE) talk \
	BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint

talk5:
	$(MAKE) talk \
	BUGS=IfcBugLoadNoTaint

talkstore2:
	$(MAKE) talk \
	BUGS=IfcBugStoreNoPointerTaint

talknoshrinking:
	$(MAKE) talk \
	EXTRATMUARGS=--shrink-nothing=True \
	BUGS=IfcBugArithNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugStoreNoValueTaint,IfcBugAllowWriteDownThroughHighPtr

talkloadsmall:
	$(MAKE) talk \
	BUGS=IfcBugLoadNoTaint \
	EXTRATMUARGS="--equiv=EquivLow --starting-as=StartQuasiInitial"

talkpop:
	$(MAKE) talk \
	BUGS=IfcBugPopPopsReturns \
	EXTRATMUARGS="--gen-instrs=InstrsCally --equiv=EquivLow --starting-as=StartQuasiInitial"

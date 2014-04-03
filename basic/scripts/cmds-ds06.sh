#!/bin/bash
source ~/.bash_profile
export LD_LIBRARY_PATH=$HOME/lib
export FILENAME="../picopaper/exp-Basic-EENI-Mem-Initial-ByExec-True.tex" ; \
	echo '\\gdef\\bugtableinfo{'$FILENAME'}%' > $FILENAME.tmp ; \
	./TMUDriver \
	  --which-tmm-routine=NoTMMRoutine \
	  --gen-instrs=InstrsBasic \
	  --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugStoreNoValueTaint]" \
	  --tmu-prop-test=PropEENI \
	  --equiv=EquivMem \
	  --starting-as=StartInitial \
	  --gen-strategy=GenByExec \
	  --smart-ints=True \
	  --shrink-nothing=True \
	  --show-counterexamples=False \
	  --latex-output \
	  --tmu-timeout=1000 \
	| tee -a $FILENAME.tmp; \
	mv $FILENAME.tmp $FILENAME; \
	scp $FILENAME antals@eniac.seas.upenn.edu:~/html/picotables/
mail -s 'Finished running commands on ds06 <EOM>' antals@seas.upenn.edu < /dev/null

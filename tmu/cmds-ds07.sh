#!/bin/bash
source ~/.bash_profile
export LD_LIBRARY_PATH=$HOME/lib
export FILENAME="../picopaper/exp-Cally-EENI-Mem-Initial-ByExec2-True.tex" ; \
	echo '\\gdef\\bugtableinfo{'$FILENAME'}%' > $FILENAME.tmp ; \
	./TMUDriver \
	  --which-tmm-routine=NoTMMRoutine \
	  --gen-instrs=InstrsCally \
	  --ifc-semantics="[IfcBugArithNoTaint,IfcBugPushNoTaint,IfcBugLoadNoTaint,IfcBugStoreNoPointerTaint,IfcBugAllowWriteDownThroughHighPtr,IfcBugStoreNoValueTaint,IfcBugJumpNoRaisePc,IfcBugJumpLowerPc,IfcBugStoreNoPcTaint,IfcBugAllowWriteDownWithHighPc,IfcBugCallNoRaisePc,IfcBugReturnNoTaint,IfcBugValueOrVoidOnReturn,IfcBugPopPopsReturns]" \
	  --tmu-prop-test=PropEENI \
	  --equiv=EquivMem \
	  --starting-as=StartInitial \
	  --gen-strategy=GenByExec2 \
	  --smart-ints=True \
	  --shrink-nothing=True \
	  --show-counterexamples=False \
	  --latex-output \
	  --tmu-timeout=1000 \
	| tee -a $FILENAME.tmp; \
	mv $FILENAME.tmp $FILENAME; \
	scp $FILENAME antals@eniac.seas.upenn.edu:~/html/picotables/
mail -s 'Finished running commands on ds07 <EOM>' antals@seas.upenn.edu < /dev/null

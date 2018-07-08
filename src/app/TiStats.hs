module TiStats where

type TiStats = Int

tiStateInitial = 0
tiStatIncSteps s = s+1
tiStatGetSteps s = s

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (stack, dump, heap, globals, stats)
	= (stack, dump, heap, globals, f stats)
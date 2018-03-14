module CorePrelude where
import Language

-- TODO: Define lexical structure for the Prelude Applications functions

preludeDefns :: CoreProgram
preludeDefns = [
    ("I", ["x"], Evar "x"),
    ("K", ["x", "y"], Evar "x"), 
    ("K1", ["x", "y"], Evar "y"),
    ("S", ["f", "g", "x"], (EAp (EAp (Evar "f") (Evar "x")) (EAp (Evar "g") (Evar "x")))),
    ("Compose", ["f", "g", "x"], (EAp (Evar "f") (EAp (Evar "g") (Evar "x")))),
    ("twice", ["f"], (EAp (EAp (Evar "Compose") (Evar "f")) (Evar "f"))) ]




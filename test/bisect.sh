#!/bin/bash

BI=$(ocamlfind query bisect)
set -x

rm bisect*.out

ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotTypes.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotTypes.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotExtBatSet.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotExtBatSet.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotPath.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotPath.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotTerm.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotTerm.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotRankedAlphabet.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotRankedAlphabet.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotHORS.mli
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ ../src/hotHORS.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ test.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotTypes.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotExtBatSet.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotPath.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotTerm.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotRankedAlphabet.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testHotHORS.ml
ocamlfind ocamlc -package batteries,oUnit,bisect -c -dtypes -g -pp "camlp4o str.cma $BI/bisect_pp.cmo" -I ../src/ testMain.ml
ocamlfind ocamlc \
				-package batteries,oUnit,bisect -linkpkg \
				 -I ../src/   -g    -ccopt -L../src/ bisect.cma -o higherOrderToolsTest \
				../src/hotTypes.cmo ../src/hotExtBatSet.cmo ../src/hotPath.cmo ../src/hotTerm.cmo ../src/hotRankedAlphabet.cmo ../src/hotHORS.cmo test.cmo testHotTypes.cmo testHotExtBatSet.cmo testHotPath.cmo testHotTerm.cmo testHotRankedAlphabet.cmo testHotHORS.cmo testMain.cmo

./higherOrderToolsTest

bisect-report -html report *.out

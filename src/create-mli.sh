#!/bin/bash

FILES=""

set -x

for file in $FILES; do
	ocamlfind ocamlc -package batteries -dtypes -i $file > ${file}i
done

boxoffice:  query.ml io.ml main.ml 
	ocamlc -o boxoffice str.cma query.ml io.ml main.ml 

check: boxoffice
	./boxoffice -echo < data/trial1.txt

topG:  boxoffice
	./boxoffice -sort-gross < data/G.txt | ./boxoffice -take 1

part1:  part1.ml
	ocamlc -o part1 part1.ml
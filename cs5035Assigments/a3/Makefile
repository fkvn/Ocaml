all: expression mapreduce

mapreduce: mapreduce.ml
	ocamlc -g -o mapreduce mapreduce.ml

LIB_FILES=ast.ml expressionLibrary.ml 
FILES=$(LIB_FILES) expression.ml

expression: $(FILES)
	ocamlc -g -o expression $(FILES)

clean: 
	rm -f expression mapreduce *.cmi *.cmo

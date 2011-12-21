OBJS = ast.cmo parser.cmo scanner.cmo lsystemstd.cmo semantic.cmo compile.cmo lsystem.cmo 

TESTS = \
print

TARFILES = Makefile testall.sh scanner.mll parser.mly \
	ast.ml compile.ml lsystem.ml lsystemstd.ml \
	$(TESTS:%=tests/test-%.mc) \
	$(TESTS:%=tests/test-%.out)

lsystem : $(OBJS)
	ocamlc str.cma unix.cma -o lsystem $(OBJS) 

.PHONY : test
test : lsystem testall.sh
	./testall.sh

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

lsystem.tar.gz : $(TARFILES)
	cd .. && tar czf lsystem/lsystem.tar.gz $(TARFILES:%=lsystem/%)

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml testall.log \
	*.cmo *.cmi *.out *.diff *.java *.class lsystem

# Generated by ocamldep *.ml *.mli
ast.cmo: 
ast.cmx: 
compile.cmo: lsystemstd.cmo ast.cmo 
compile.cmx: lsystemstd.cmx ast.cmx 
lsystem.cmo: semantic.cmo scanner.cmo parser.cmi compile.cmo ast.cmo 
lsystem.cmx: semantic.cmx scanner.cmx parser.cmx compile.cmx ast.cmx 
lsystemstd.cmo: 
lsystemstd.cmx: 
parser.cmo: ast.cmo parser.cmi 
parser.cmx: ast.cmx parser.cmi 
scanner.cmo: parser.cmi 
scanner.cmx: parser.cmx 
semantic.cmo: lsystemstd.cmo ast.cmo 
semantic.cmx: lsystemstd.cmx ast.cmx 
parser.cmi: ast.cmo

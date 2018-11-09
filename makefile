

OCB_FLAGS = -I src -use-ocamlfind -pkg graphics -use-ocamlfind -pkg yojson 
OCB = ocamlbuild $(OCB_FLAGS)

all: defualt	


defualt: 
	$(OCB) main.native 
	mv main.native raytracer

clean:
	$(OCB) main.native -clean
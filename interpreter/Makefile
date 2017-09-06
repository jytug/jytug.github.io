all:
	happy -gca ParTiny.y
	alex -g LexTiny.x
	ghc --make Main.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocTiny.* LexTiny.* ParTiny.* LayoutTiny.* SkelTiny.* PrintTiny.* TestTiny.* AbsTiny.* TestTiny ErrM.* SharedString.* ComposOp.* tiny.dtd XMLTiny.* Makefile*
	


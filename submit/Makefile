# ---------------------------------------------------------------------
# -- COMP90045 Programming Language Implementation                   --
# -- Programming Project: Goat                                       --
# --                                                                 --
# -- Team: GOAT SIMULATOR                                            --
# -- Members:                                                        --
# --          Chenqin Zhang, Geoffrey Ka-Hoi Law, Yun Chen           --
# --          733301, 759218, 760419                                 --
# --          {chenqinz, glaw, yunc4}@student.unimelb.edu.au         --
# ---------------------------------------------------------------------



Goat: *.hs
	(ghc Main.hs -o Goat) || (stack ghc -- Main.hs -o Goat) 

clean:
	rm -f *.o *.hi
	rm -f Goat


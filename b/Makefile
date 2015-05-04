.PHONY: Stock

Stock: Stock.hs
	ghc Stock.hs -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3

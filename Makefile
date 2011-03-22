build:
	ghc -o cluster -O Cluster.hs StrictRead.hs Main.hs --make
clean:
	rm *.hi *.o cluster

CC = ghc

play: play.hs Chess.hs
	$(CC) --make play.hs -o play

.Phony: clean
clean:
	rm -f play *.hi *.o Chess/*.o Chess/*.hi

.Phony: all
all: clean play

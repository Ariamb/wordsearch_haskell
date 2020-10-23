# wordsearch_haskell
Small university homework. A wordsearch game implemented on haskell, generating a random table with hidden words.

You need system.random and system.console.ANSI.
This small program creates an basic table with hidden words and random letters. The player ojective is to find the words.
He can do that by typing the start and end of the word for the monad IO. 

Following the functional programming idea, the code has is divided in all pure functions and IO functions (monads). A lot of functions are screwed all around because i got a little carried away, and i was a bit short on time.

Upon entering, type an integer and press enter to set the size.
Then, type [column] [space] [line]. first number, type a space, and then the second number. This is tell the "i,j" position of the start of the word.
Do the exact same thing again, but this time with the coordinate of the ending of the word.

Once all the words are found, you win the game.

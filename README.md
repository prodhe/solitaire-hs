# The Solitaire Cipher

An implementation in Haskell of Bruce Schneier's deck-of-cards cipher.

Read [more about it](https://www.schneier.com/solitaire.html) on his website!

## Explanation

### Encrypt
    
1. Discard any non A-Z chars and UPPERCASE
2. Group by 5 and pad with X's at the end if needed
3. Generate keystream for each letter
4. Convert PLAIN into corresponding numbers (1-26)
5. Convert KEYSTREAM into corresponding numbers (1-26)
6. Add PLAIN numbers and KEYSTREAM numbers and subtract 26 if >26
7. Convert the resulting numbers back to letters

### Decrypt
    
1. Use the same Solitaire to generate keystream for each letter
2. Convert CIPHER message to numbers
3. Convert KEYSTREAM letters to numbers
4. Subtract keystream numbers from cipher numbers and add 26 if C <= K
5. Convert numbers back into letters and you have PLAIN

### Solitaire keystream generation

- Full 52 deck of cards + 2 visually distinct jokers (A and B)
- Face value of cards: Ace=1, 2..10, 11, 12, 13
    - Clubs is base value
    - Diamonds is +13
    - Hearts is +26
    - Spades is +39
- Joker values 53
- When cards represents a letter, Clubs and Diamonds are taken to be the
    number of the letter (1-26), as are Hearts and Spades after subtracting
    26 (27-52 drops to 1-26)
- Key the deck (arrange the cards in a "secret" order)
- Consider the deck to be circular

- Generate output letter
    - Move the A joker down one card
    - Move the B joker down two cards
    - Perform a triple cut
        - All the cards above the top joker moves below the bottom and vice
        versa. The jokers and the cards between them do not move.
    - Perform a count cut
        - Cut bottom card's value from the top and move it to just above
        bottom card
    - Convert top card to it's value and count down from the top, including
        the card itself. The card immediately after the count is the output
        letter (for the keystream). If it's a joker, no letter is generated.
        This step does not alter the deck.
    - Loop this process if more letters are needed

First ten letters of an unkeyed deck:

    D (4)  W (49)  J (10)  Skip Joker (53)  X (24)  H (8)
    Y (51)  R (44)  F (6)  D (4)  G (33)

## How to use

Compile the source code and run as follows:

    $ ghc solitaire.hs
    $ ./solitaire <encrypt/decrypt> <message_file> <key_file>

### Secret key

The key file is obviously your secret key and it is represented by one card per line.

For example:

    C8  is 8 of Clubs
    D13 is King of Diamonds
    H1  is Ace of Hearts
    S12 is Queen of Spades
    J1  is Joker A
    J2  is Joker B

## Bugs

I made this to get to know the Haskell language a little better. The code is messy
and does not error check your input for example. But the algorithm works. :-)

      ******************************************************************
      * Author: Will Verplaetse
      * Date: 5/1/2024
      * Purpose: This program will implement simple 4 player blackjack using a
      *        queue made from COBOL's tables. This is simple blackjack
      *        There is no doubling down, splitting or insurance. There
      *        is only a 1.5x bonus for naturals (blackjack on the first
      *        draw).
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdvancedProgram.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT randomDeck ASSIGN TO deckName LINE SEQUENTIAL.



       DATA DIVISION.
       FILE SECTION.

       FD randomDeck.
       01 card.
           88 endOfFile    VALUE HIGH-VALUES.
           02 fileSuite    PIC X.
           02 fileCardNumber   PIC 99.





       WORKING-STORAGE SECTION.

      *The deck of cards
       01 deckOfCards.
           02 deckSize         PIC 99.
           88 emptyDeck        VALUE 0.

           02 cards OCCURS 52 TIMES.
               03 suite        PIC X.
               03 cardNumber   PIC 99.

           02 topOfDeck        PIC 99 VALUE 1.
           02 bottomOfDeck     PIC 99 VALUE 1.

       01 dealer.
           02 dealerCards OCCURS 12 TIMES.
               03 dealerSuite      PIC X.
               03 dealerNumber     PIC 99.
           02 topOfDealerDeck      PIC 99 VALUE 0.
           02 dealerAces           PIC 9 VALUE ZERO.

           02 dealerSum            PIC 99 VALUE ZERO.
           88 dealerBlackjack      VALUE 21.
           88 dealerBust           VALUE 22 THRU 99.
           02 altDealerSum         PIC 99 VALUE ZERO.
           88 altCutoffRange       VALUE 17 THRU 20.
           88 altDealerBlackjack   VALUE 21.
           88 altDealerBust        VALUE 22 THRU 99.

           02 dealerMoney          PIC 9(10) VALUE 1000000.


       01 players OCCURS 4 TIMES.
           02 playerName       PIC X(15).
           02 playerCards OCCURS 12 TIMES.
               03 playerSuite      PIC x.
               03 playerNumber     PIC 99.
           02 topOfPlayerDeck      PIC 99 VALUE 0.
           02 numOfAces            PIC 9 VALUE ZERO.

           02 playerSum            PIC 99 VALUE ZERO.
           88 over21               VALUE 22 THRU 99.
           88 blackjack            VALUE 21.
           02 altPlayerSum         PIC 99 VALUE ZERO.
           88 altover21            VALUE 22 THRU 99.
           88 altBlackjack         VALUE 21.

           02 betAmount            PIC 9(5).
           02 playerMoney          PIC 9(5) VALUE 5000.





       01 curPlayer        PIC 9 VALUE 1.
       01 curPlaceholder   PIC 9.
       01 handCounter      PIC 99.

       01 playerChoice     PIC X.
        88 hit             VALUE "y" "Y".


       01 playAgain        PIC X.
       88 yes              VALUE "y".


       01 cardHolder       PIC 99.

       01 betReward        PIC 9(5).

       01 deckName         PIC X(10).


       01 counter          PIC 99 VALUE 0.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "Enter the deck you would like to use: "
           ACCEPT deckName

           OPEN INPUT randomDeck

      *    Creating the deck
            READ randomDeck
               AT END SET endOfFile TO TRUE.

            PERFORM fillDeck UNTIL endOfFile

            MOVE 52 TO topOfDeck


           CLOSE randomDeck

           PERFORM playGame.



           STOP RUN.




      *Plays the game
       playGame.

      *Prompt for bets
           PERFORM 4 TIMES

           PERFORM promptBets

           ADD 1 TO curPlayer


           IF curPlayer IS EQUAL TO 5 THEN
               MOVE 1 TO curPlayer
           END-IF


           END-PERFORM
      *End bet prompting

           PERFORM 51 TIMES

           DISPLAY cards(topOfDeck - counter)
           DISPLAY "Top - count = " topOfDeck counter
           ADD 1 TO counter
           END-PERFORM


      *    Dealing the starting hands
           PERFORM 2 TIMES

      *    Dealing cards to players
           PERFORM 4 TIMES
               PERFORM dealCardToPlayer

      *    There is no mod operator and 0 based indexing so more work
      *    is needed to make circular tables work
               ADD 1 TO curPlayer


               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF

           END-PERFORM

           PERFORM dealCardToDealer

           END-PERFORM

      *    End dealing the starting hands




      *    Showing the players' starting hand
           PERFORM 4 TIMES

               PERFORM showHand
               ADD 1 TO curPlayer


               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF

           END-PERFORM

           DISPLAY "Dealer has " dealerCards(topOfDealerDeck)







      *    Checking for winners and potential payouts



           IF NOT altDealerBlackJack THEN

           PERFORM 4 TIMES

            IF blackjack(curPlayer) OR altBlackjack(curPlayer) THEN
               PERFORM payoutNaturalBet
            END-IF

               ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF

           END-PERFORM

           ELSE

             IF NOT blackjack(curPlayer) AND NOT altBlackjack(curPlayer)
             PERFORM  payupBet
             END-IF

             ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF


           END-IF

      *    End dealing with naturals











      *    Dealing Cards until players bust or stand
           PERFORM 4 TIMES

           DISPLAY "Would Player " curPlayer " like to hit (y) or stand "
      -    "(n)"

           ACCEPT playerChoice

           PERFORM UNTIL (NOT hit) OR (over21(curPlayer)
           AND altover21(curPlayer)) OR (blackjack(curPlayer)
           OR altblackjack(curPlayer))


              PERFORM dealCardToPlayer
              DISPLAY "You drew a " WITH NO ADVANCING
              DISPLAY playerCards(curPlayer, topOfPlayerDeck(curPlayer))



      *    If the player has an ace and the alt sum is not over 21 it
      *    displays that sum otherwise it will display the sum where
      *    all aces are treated as zeroes
             IF altPlayerSum(curPlayer) NOT = 0
             AND NOT altover21(curPlayer)

             DISPLAY "Your total is " altPlayerSum(curPlayer)

             ELSE

             DISPLAY "Your total is " playerSum(curPlayer)


             END-IF






           IF altover21(curPlayer) AND over21(curPlayer) THEN
               DISPLAY "You busted"

           END-IF

           IF altBlackJack(curPlayer) OR blackjack(curPlayer) THEN
               DISPLAY "Player " curPlayer " has a blackjack"
           END-IF

           IF NOT (altover21(curPlayer) AND over21(curPlayer))
           AND NOT( blackjack(curPlayer) OR altBlackjack(curPlayer))
           DISPLAY "Would Player " curPlayer " like to hit (y) or stand "
      -    "(n)"

           ACCEPT playerChoice

           END-IF


           END-PERFORM


           ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF

           END-PERFORM

      *    End dealing cards to players





      *    Start dealing cards to dealer
           PERFORM UNTIL altDealerBlackJack OR altCutoffrange
           OR dealerBlackJack OR altDealerBust OR dealerBust

            PERFORM dealCardToDealer

            DISPLAY "Dealer drew a " dealerCards(topOfDealerDeck)
            IF altDealerSum > 16 THEN
                DISPLAY "Dealer total is " dealerSum

           ELSE
               DISPLAY "Dealer total is " altDealerSum
           END-IF

           END-PERFORM

      *    End dealing cards to dealer





      *    Checking winners and paying bets out
           PERFORM 4 TIMES

      *All player who do not have a blackjack when the dealer has a
      *blackjack payup
      *IF STATEMENT A
           IF dealerBlackjack OR altDealerBlackjack THEN
               IF NOT blackjack(curPlayer) THEN
                   PERFORM payupBet
               END-IF

      *Next condition: dealer does not have blackjack and did not bust
      *ELSE STATEMENT A
           ELSE

      *IF STATEMENT B
               IF altDealerSum < 21 THEN

               IF (playerSum(curPlayer) > altdealerSum AND < 22)
               OR (altplayerSum(curPlayer) > altdealerSum AND < 22) THEN
                   PERFORM payoutBet

               ELSE
                   PERFORM payupBet
               END-IF

      *ELSE STATEMENT B
               ELSE

      *IF STATEMENT C
               IF altDealerSum > 21 AND dealerSum < 21 THEN


      *Next condition: counting the 11 caused the dealer to bust
      *so their deck that counted aces as 1's must be tested
               IF (playerSum(curPlayer) > dealerSum AND < 22)
               OR (altPlayerSum(curPlayer) > dealerSum AND < 22) THEN
                      PERFORM payoutBet
                  ELSE
                      PERFORM payupBet
                  END-IF


      *ELSE STATEMENT C
                ELSE

               IF dealerBust AND altDealerBust THEN
              IF NOT over21(curPlayer) OR NOT altover21(curPLayer) THEN
                   PERFORM payoutBet
                   END-IF
               END-IF


      *Closes IF ELSE C
               END-IF


      *Closes IF ELSE B
               END-IF


      *Closes IF ELSE A
           END-IF




              ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF




           END-PERFORM




      *Showing results
           PERFORM 4 TIMES

           DISPLAY "Player " curPlayer " has $" playerMoney(curPlayer)

           ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF


           END-PERFORM




      *Prompting to play again

           DISPLAY "Would you like to play again (y/n) "

           ACCEPT playAgain


      *Using the GO TO statement we can jump back to the beginning
           IF yes THEN
               PERFORM gameCleanup
               PERFORM playGame
           END-IF


           .

      *End of main program








      **********************************************************
      **********************************************************
      **********************************************************
      **********************************************************
      **********************************************************


      *Start helper subroutines





      *Fills the deck from a file
       fillDeck.

           MOVE card TO cards(topOfDeck)


           ADD 1 TO topOfDeck
           READ randomDeck
               AT END SET endOfFile TO TRUE.



       promptBets.
           DISPLAY "What would player " curPlayer " like to bet: "
           ACCEPT betAmount(curPlayer)
           DISPLAY "Player " curPlayer " has bet $" betAmount(curPlayer)
           .

      *Deals a card to the dealer
       dealCardToDealer.

           ADD 1 TO topOfDealerDeck
           MOVE cards(topOfDeck) TO dealerCards(topOfDealerDeck)


      *    Checking if an ace was drawn, if drawn and it is the first ace
      *    then it is kept in an alterante sum which counts it as an eleven
      *    all other aces will be counted as ones since two 11s would betAmount
      *    over 21

           IF dealerNumber(topOfDealerDeck) = 1 THEN

           IF dealerAces > 0 THEN
               ADD dealerNumber(topOfDealerDeck) TO dealerSum
               ADD dealerNumber(topOfDealerDeck) TO altDealerSum
               ADD 1 TO dealerAces

           ELSE

               ADD 1 TO dealerSum
               ADD 11 TO altDealerSum
               ADD 1 TO dealerAces

           END-IF

           ELSE


               ADD dealerNumber(topOfDealerDeck) TO dealerSum
               ADD dealerNumber(topOfDealerDeck) TO altDealerSum

           END-IF

           IF topOfDeck > 1 THEN
           SUBTRACT 1 FROM topOfDeck
           ELSE
               MOVE 52 TO topOfDeck
           END-IF

           .

       dealCardToPlayer.

           ADD 1 TO topOfPlayerDeck(curPlayer)


           MOVE cards(topOfDeck)
      -    TO playerCards(curPlayer, topOfPlayerDeck(curPlayer))



      *    Dealing With Aces same logic as the dealer
           IF playerNumber(curPlayer, topOfPlayerDeck(curPlayer)) = 1
           THEN

      *    Adding a one when they have more than one ace
           IF numOfAces(curPlayer) > 0 THEN

           ADD playerNumber(curPlayer, topOfPlayerDeck(curPlayer))
      -    TO playerSum(curPlayer)

           ADD playerNumber(curPlayer, topOfPlayerDeck(curPlayer))
      -    TO altplayerSum(curPlayer)

           ADD 1 TO numOfAces(curPlayer)

      *    Adding 11 as an alt sum, will only happen once
           ELSE
               ADD 11 TO altPlayerSum(curPlayer)
               ADD 1 TO playerSum(curPlayer)
               ADD 1 to numOfAces(curPlayer)

           END-IF


           ELSE

      *    What happens when a non ace card is drawn
           ADD playerNumber(curPlayer, topOfPlayerDeck(curPlayer))
      -    TO playerSum(curPlayer)

           ADD playerNumber(curPlayer, topOfPlayerDeck(curPlayer))
      -    TO altplayerSum(curPlayer)



           END-IF


           IF topOfDeck > 1 THEN
           SUBTRACT 1 FROM topOfDeck
           ELSE
               MOVE 52 TO topOfDeck
           END-IF

           .





      *Displays the players hand
       showHand.
           DISPLAY "Player " curPlayer "'s cards are: "
           MOVE topOfPlayerDeck(curPlayer) TO handCounter

           PERFORM UNTIL handCounter = 0
               DISPLAY playerCards(curPlayer, handCounter)
               SUBTRACT 1 FROM handCounter
           END-PERFORM.





      *Rewards the player with 1.5x their bet
       payoutNaturalBet.


           MULTIPLY betAmount(curPLayer) BY 1.5 GIVING betReward

           DISPLAY "Player " curPlayer " won $" betReward
           ADD betReward TO playerMoney(curPlayer)
           SUBTRACT betReward FROM dealerMoney
           DISPLAY "Player " curPlayer " won $" betReward.


      *Player pays the dealer their bet amount
       payupBet.

           SUBTRACT betAmount(curPlayer) FROM playerMoney(curPlayer)
           ADD betAmount(curPlayer) TO dealerMoney
           DISPLAY "Player " curPlayer " lost $" betAmount(curPlayer).



       payoutBet.
           SUBTRACT betAmount(curPlayer) FROM dealerMoney
           ADD betAmount(curPlayer) TO playerMoney(curPlayer)
           DISPLAY "Player " curPlayer " won $" betAmount(curPlayer).



      *Returning cards to the deck and resetting player sums
       gameCleanup.

           PERFORM 4 TIMES

           PERFORM UNTIL topOfPlayerDeck(curPlayer) = 0

            IF bottomOfDeck = 1 THEN
                MOVE 52 TO bottomOfDeck

            ELSE
                SUBTRACT 1 FROM bottomOfDeck
            END-IF

            MOVE playerCards(curPlayer, topOfPlayerDeck(curPlayer))
            TO cards(bottomOfDeck)



            MOVE ZEROES TO playerSum(curPLayer)
            MOVE ZEROES TO altPlayerSum(curPlayer)

            SUBTRACT 1 FROM topOfPlayerDeck(curPlayer)

           END-PERFORM

           ADD 1 TO curPlayer

               IF curPlayer IS EQUAL TO 5 THEN
                   MOVE 1 TO curPlayer
               END-IF



           END-PERFORM


      *Dealer cleanup

           PERFORM UNTIL topOfDealerDeck = 0

           IF bottomOfDeck = 1 THEN
                MOVE 52 TO bottomOfDeck

            ELSE
                SUBTRACT 1 FROM bottomOfDeck
            END-IF

           MOVE dealerCards(topOfDealerDeck) TO cards(bottomOfDeck)

           SUBTRACT 1 FROM topOfDealerDeck

           END-PERFORM

           MOVE ZEROES TO dealerSum
           MOVE ZEROES TO altDealerSum.


       END PROGRAM AdvancedProgram.

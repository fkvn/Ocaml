CS5035 Assignment 2 Part 3: Answering the Box Office Trivia Questions [25 Points]




For each of the following questions, 

(A) give the answer 

(B) give the Unix shell command (or commands) you used to answer the
question and in a sentence, explain why your shell command gives you
helpful data.  

Use the data given in the files G.txt, PG.txt, PG-13.txt, R.txt for
the set of top-grossing films in each rating category (G, PG, PG-13
and R).

1.[5 POINTS] What is the top-grossing film of all time (use the alltime.txt data)?

(A) Gone with the Wind|MGM|1582.0094|1939

(B) Unix shell command:  ./boxoffice -sort-gross < data/alltime.txt | ./boxoffice -take 1  
    Explantion: by -sort-gross function, we sort the list of movies in alltime.txt. 
                As the result, the top-grossing film would be the first one of the list.
                Then, use -take function to pick the first one of the return list.

2.[5 POINTS] What is the 50th ranked R film by gross on the list?

(A) The Silence of the Lambs|Orion|130.742922|1991

(B) Unix shell command: ./boxoffice -sort-gross < data/R.txt | ./boxoffice -drop 49 | ./boxoffice -take 1
    Explantion: First, sort the movies in R.txt by grossing. Then drop the first 49 of the list.
                As a result, the first one after dropping 49 movies is the 50th.
                Then use -take function to take the first one of the renaming list (after dropping)

3.[5 POINTS] Suppose you had a chance to make 1 film with a top director and the
director was so good you were guaranteed that whatever film you made
would be in the top 5 grossing films in its ratings category (G, PG,
PG-13, R) --- and equally likely to be ranked 1, 2, 3, 4, or 5.  What
rating (G, PG, PG-13, R) would you choose to give your film if you
wanted to make the most money?

(A)  PG-13

(B) Unix shell command:
    1. ./boxoffice -sort-gross < data/G.txt | ./boxoffice -take 5 | ./boxoffice -average 
    2. ./boxoffice -sort-gross < data/PG.txt | ./boxoffice -take 5 | ./boxoffice -average 
    3. ./boxoffice -sort-gross < data/PG-13.txt | ./boxoffice -take 5 | ./boxoffice -average 
    4. ./boxoffice -sort-gross < data/R.txt | ./boxoffice -take 5 | ./boxoffice -average 

   Explantion:  We have the results of 4 commands are 335.8458128, 429.210264, 555.686177, 283.7813354, respectively.
                Therefore, the average of top 5 grossing films of PG-13 is the highest one (555.686177). In other words,
                choosing PG-13 would make the most money.

4.[5 POINTS] Using the data in alltime.txt, would you have preferred to make
money off of movies in the 70s or in the 80s?

(A) the movies in the 70s

(B) Unix shell command:
    1. ./boxoffice -decade 70 < data/alltime.txt | ./boxoffice -average
    2. ./boxoffice -decade 80 < data/alltime.txt | ./boxoffice -average
    
   Explantion:  The resuls of those 2 commands are 499.294617241, 421.016036667, respectively. 
                As a result, we can see that the movies in the 70s has the higher average money
                off than the movies in the 80s. Therefore, making money off of movies in the 70s (499.294617241)
                is better than the movies in the 80s (421.016036667).

5.[5 POINTS] Using the data in alltime.txt, which studio made the most
money off of movies in the 60s?

(A) Dis

(B) Unix shell command: 
    1. ./boxoffice -decade 60 < data/alltime.txt | ./boxoffice -by-studio | ./boxoffice -sort-studio | ./boxoffice -take 1

   Explantion: the result of the command is Dis.|2675.42. Therefore, Dis made the most money off of movies in the 60s.


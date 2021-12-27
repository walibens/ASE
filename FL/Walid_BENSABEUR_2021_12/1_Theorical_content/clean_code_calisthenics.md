# Clean object calisthenics 

## Introduction
**Clean Object Calisthenics** is a set of clean code rules listed by Damir Majer that can be considered as a compass gathering principles that are intended to improve the overall code quality.
Hereinafter the Cmap of these principles that will be presented in the following chapters.
![Clean Object Calisthenics](/image/cmap_coc.png)

## 4 rules of simple design
Presented by Kent Beck while he was developing XP in the 90s, the rules are the following one, ordered by priority and complementing each other:

#### 1.	Pass the tests (It works)
Writing tests (comprehensive tests) actually could lead us to better designs. It allows to create a safetynet (to eliminate the fear of regression) and refactor the code to keep the code clean.

#### 2.	Reveal intention (Easy to read and understand)
A developer will spend more time to read the code than write it. The naming must be meaningful and should immediately tells what the code is doing.


When you look at a piece of code it should immediately tell you what it does and it shouldn’t surprise you. Variable, method, and class names should describe what they do. This is also refered to as the principle of least astonishment or the element of least surprise.


Naming correctly the variables, the methods and classes will help the understanding, and the more the names are well choosen
we need to be expressive
Choose a good name that represents the things
Keep your functions and classes small. It’s easier to name, write, and understand it.
Challenge and commit yourself to write code so that it reads as documentation

gradually improving names.
If the method name is too long, this could mean it hase multiple responsibilities which is a violation to SRP

#### 3.	No duplication (DRY)
DRY everything should be said once and only once
single responsibility
Kent expressed it as saying everything should be said "Once and only Once."

Every piece of knowledge should have one and only one representation 
Once and Only Once

Bad example : 
``` abap
    DATA iv_i TYPE i VALUE 1.

    DO 10 TIMES.
      IF is_strike( iv_i ).
        rv_score += 10 + strike_bonus( iv_i  ).
        iv_i += 1.
      ELSEIF is_spare( iv_i ).
        rv_score += 10 + spare_bonus( iv_i ).
        iv_i += 2.
      ELSE.
        rv_score += sum_of_balls_in_frame( iv_i ).
        iv_i += 2.
      ENDIF.
```
Good example : 
DATA iv_frame_index TYPE i VALUE 1.

    DO 10 TIMES.
      IF is_strike( iv_frame_index ).
        rv_score += 10 + strike_bonus( iv_frame_index  ).
        iv_frame_index += 1.
      ELSEIF is_spare( iv_frame_index ).
        rv_score += 10 + spare_bonus( iv_frame_index ).
        iv_frame_index += 2.
      ELSE.
        rv_score += sum_of_balls_in_frame( iv_frame_index ).
        iv_frame_index += 2.
      ENDIF.


  METHOD credit.
    DATA(lv_balance) = go_wallet->get_balance( ).
    ADD iv_credit TO lv_balance.
    go_wallet->set_balance( lv_balance ).
  ENDMETHOD.


CLASS lcl_transaction  IMPLEMENTATION.

  METHOD constructor.
    go_wallet = io_wallet.
  ENDMETHOD.

  METHOD credit.
    go_wallet->credit( iv_credit ).
  ENDMETHOD.

  METHOD debit.
    go_wallet->debit(
      EXPORTING
        iv_debit         = iv_debit
      EXCEPTIONS
        not_enough_funds = 4
    ).
    IF sy-subrc <> 0.
      WRITE 'Transaction cancelled because not enough funds'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


The order of 2nd and 3rd rule might be slightly different in some places, in Robert C. Martin’s book: Clean Code book, it’s No duplication first and Reveals Intention the later, while in Martin Fowler article, it’s Reveals Intention first and No duplication the later.
Minimizes duplication seems to trump Maximizes clarity, but when reading code, the reverse seems to be true.





#### 4.	Fewest Elements
These questions can be asked to yourself when you take some time to have a step back after writing some code:
•	Do I have any dead code? Sometimes as we working through our system, we build things that aren’t final, and there are times that in the end, we don’t need it at all in the final product. If it’s happened, no question asked, just delete it.
•	Have I extracted too far? Sometimes we can do over-extract while trying to better express our intent, for example when extracting methods for readability but we do it too far to the extent that every method has its own class.

JB Rainsberger simplified the 4 rules and kept only 2 of them:


He consider that testing is evident and must not be considered as 

## Messaging

## Living Objects
Alan Kay said in his Object Oriented programming definition, "Everything is an Object".
It means only object must be manipulated in order to enable the messaging.
Below some details on Static method and primitive objects : 

#### Static method 
It should be manipulated only in specific context like Factory method. Below an example of a static method for(), used to instanciate the right object depending on the verse number of a song:
``` abap
CLASS lcl_bottle_number DEFINITION.

  PUBLIC SECTION.
  CLASS-METHODS:
      for         IMPORTING iv_number               TYPE i
                  RETURNING VALUE(ro_bottle_number) TYPE REF TO lcl_bottle_number.
ENDCLASS.

CLASS lcl_bottle_number IMPLEMENTATION.
  METHOD for.
    DATA lv_bottle_number_class TYPE string.

    CASE iv_number.
      WHEN 0.
        lv_bottle_number_class = '\PROGRAM=YR_99_ACHIEVING_OPENNESS\CLASS=LCL_BOTTLE_NUMBER_0'.
      WHEN 1.
        lv_bottle_number_class = '\PROGRAM=YR_99_ACHIEVING_OPENNESS\CLASS=LCL_BOTTLE_NUMBER_1'.
      WHEN OTHERS.
        lv_bottle_number_class = '\PROGRAM=YR_99_ACHIEVING_OPENNESS\CLASS=LCL_BOTTLE_NUMBER'.
    ENDCASE.
    CREATE OBJECT ro_bottle_number TYPE (lv_bottle_number_class) 
		EXPORTING iv_number = iv_number.
  ENDMETHOD.

ENDCLASS.
```

#### Primitive type 
It's a basic type that a programming language provides (String, Integer, Date). As in an OO context we manipulate entities (Bottle, Player, Car, Invoice, Roman number, etc.), it makes more sense to encapsulate them within objects as well and not keeping them as primitive type.
Here an example on the Connect4 game. We started initially by 2 string attributes player1 and player2 in the main class, but instead of keeping them as primitive attributes, the class YCL_PLAYER has been created :
``` abap
CLASS ycl_player DEFINITION PUBLIC FINAL CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_nickname TYPE string
          iv_disc_value TYPE yif_c4_cell=>gc_disc_value,
       nickname RETURNING VALUE(rv_nickname) TYPE string,
      disc_value
            RETURNING
              value(rv_result) TYPE yif_c4_cell=>gc_disc_value.
  PRIVATE SECTION.
  DATA : mv_nickname TYPE string,
         mv_disc_value TYPE yif_c4_cell=>gc_disc_value.
ENDCLASS.

CLASS ycl_player IMPLEMENTATION.
  METHOD constructor.
    mv_nickname = iv_nickname.
    mv_disc_value = iv_disc_value.
  ENDMETHOD.

  METHOD nickname.
    rv_nickname = mv_nickname.
  ENDMETHOD.

  METHOD disc_value.
    rv_result = mv_disc_value.
  ENDMETHOD.

ENDCLASS.
```

## IOSP
Invented by Ralf Westphal, IOSP means "Integration Operation Segregration Principle". 
It consists on separating the calls of the methods (Integration method) from the logic (Operation method).

It has several advantages : 
- Comprehensive. Like in a book, there are the main chapters (Integration) with their respective sub chapters (Operation)
- Avoid functional dependencies
- Easier to test the operations as they contains only logic
- Decreases function size and tends to follow SRP with pure functions

Here an example of a code where the method next_round() contains logic and method call.
``` abap
  METHOD next_round.
    mo_current_player = COND #(
       WHEN mo_current_player = mt_players[ 1 ] THEN mt_players[ 2 ]
       ELSE mt_players[ 1 ]
    ).
	 
    rv_message = |{ mo_current_player->nickname(  ) }'s Turn|.
  ENDMETHOD.
```

Following the IOSP, the next_round() method is now an integration class, calling 2 operation methods set_current_player() and next_player().
As each method has one responsibility and the logic is reduced, it's easier to test
``` abap
  METHOD next_round.
    set_current_player( ).
    rv_message = next_player(  ).
  ENDMETHOD.

  METHOD set_current_player.
    mo_current_player = COND #(
       WHEN mo_current_player = mt_players[ 1 ] THEN mt_players[ 2 ]
       ELSE mt_players[ 1 ]
    ).
  ENDMETHOD.

  METHOD next_player.
    rv_message = |{ mo_current_player->nickname(  ) }'s Turn|.
  ENDMETHOD.
```


## Software dimension
The shape of the code, by considering the indentation and the length, is important to get an idea on its complexity. It's what is behind software dimension.

Below an code snippet (from a 3K LoC subroutine) with nested ifs, increasing drastically the code comprehension:
``` abap
IF w_molga = '06' .  "FRANCE
  SELECT COUNT(*) FROM yhr_mig_cdxhra
	WHERE pernr = enrich_table-pernr AND
		  bukrs = z_bukrs.  "rajout de cette ligne avec le ticket I1097695
  IF sy-subrc = 0.
	IF  enrich_table-pernr(1) <> '9' AND enrich_table-pernr(2) <> '78' AND enrich_table-pernr(1) <> '5' .
	  CLEAR : w_agr_name.
	  SELECT SINGLE agr_name FROM agr_users INTO w_agr_name
							 WHERE uname    = sy-uname
							 AND   from_dat LE sy-datum
							 AND   to_dat   GE sy-datum
							 AND   agr_name = 'SA.AM.N1_2'.  " VESI FUNC HR
	  IF sy-subrc <> 0.
		MESSAGE i044(zbc_codex) WITH enrich_table-pernr.
	  ENDIF.
	ENDIF.
  ENDIF.
ENDIF.
```

It could be splitted in Operation methods, smaller in size, with a single responsibility for each method. Or as we can see in the 1st line a condition on the Molga field (country field), we could use here a factory method to instanciate the object  related to the country, and redefine the methods if required. 
These actions will help to reduce the code complexity and thus have a better software dimension.

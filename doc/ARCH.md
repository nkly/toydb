# Architecture notes

This documents describes what features will be implemented and how it's going
to be done.

## Database features

**Datatypes:**

* Integer, `int` (represented as signed 32-bit integer)
* Double, `double` (represented as 64-bit floating-point number)
* Fixed-size string, `varchar(n)` (represented as an array of 8-bit characters
  padded with zeroes up to length of `n` chars)
  
**Commands:**

TBD language spec

* Create and drop table
* Select from table (both simple and joined)
* Insert, update and delete
* Create and drop index
* Vacuum

## API features

TBD. 
DBMS will be implemented as Haskell library. An example console REPL will use this library.
Basically support for callbacks for various commands. Maybe cursors. 

## Internal design

* Query compiler (Parser + Optimizer)
* Executor (VM Interpreter)
* Page Manager

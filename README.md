# CircTex

* Intractive Haskell based command line application.
* CircTex takes a boolean expression with variable inputs. ( user named ) 
* Due to Haskell's versatility the code can be easily modified for different purposes.
* Multiple useful functions are implemented, all being marked in the modules.

Symbols and precedence
======================

Symbol precedence is from top to bottom. Parentheses are supported.

* !  - logic not
* *  - logic and
* +  - logic or
* *! - logic nand
* +! - logic nor
* ^. - logic xor
* ^! - logic xnor

Modules and extensions
======================

Note you can find internal useful functions. Internal libraries are also marked and separated inside the files.

* Logic   - In this we define the basic expression classes. An evaluator is also provided and can be used via GHCi running the Main.hs file. 
* DNF     - Module that supports minterm calculation and expression grouping functions.
* Circuit - This module implements circuit generation out of expressions.
* Draw    - Converts the circuit to a single string containing TeX code. 
* Format  - Formats the circuit by given parameters found inside the last section of the file.
* Parser  - Expression parser. Symbols and precedence can be modifed by preference inside the file.
* Main    - used for running 

Install
=======

Download and unzip the files. 

Type following terminal commands:

> chmod +x ./Installer.sh

> chmod +x ./Main.sh

> sudo ./Installer.sh

Running
=======

Run from terminal:

> chmod +x ./Main.sh

> ./Main.sh

For quitting use Ctrl+C.

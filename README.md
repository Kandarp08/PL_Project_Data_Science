# Introduction
In this project, we explore a purely functional approach to data engineering. We have designed a mini functional data processing library that emphasizes the concepts of immutability and lazy evaluation.

# Setup Instructions
1. Clone the repository.  
2. Run the `make` command in the root directory of the project to compile the project and all dependencies.  
3. The **main.ml** file contains the source code of your program. Execute your program using `./main.exe`

# Project Structure
## Data_Loading
This folder contains files required to lazily load datasets. The supported file formats are **.csv** and **.json**.

Constructs like data_object and dataframe are defined here which are used by the library functions as well as the user program.

## Data_Operations
* **operations** – Consists of various operations that can be applied on sequences.  
* **transformations** – Consists of different transformations that can be applied on integer and float sequences.  
* **utils** – Consists of several utility functions for integer and float sequences.  
* **lib.ml** – The library functions consisting of all data operations are defined here.

## main.ml
The user program needs to be written in this file. On running `make`, the corresponding executable **main.exe** is generated.

# Contributors
1. Siddharth Reddy Maramreddy  
2. Shashank Devarmani  
3. Kandarp Dave

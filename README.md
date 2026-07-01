#  Tiling Language Interpreter: From Mondrian to Escher

This repository contains my implementation of a tiling language interpreter developed in **OCaml** for the Languages and Programming Environments course. 

The project's goal is to interpret simple programs that generate abstract geometric compositions inspired by **Piet Mondrian** and complex tessellations inspired by **M.C.

## Language Features

The interpreter processes `.pict` text files composed of labels and instructions.Execution always starts at the `main` label

### Drawing Primitives
* `square color x y w h`: Draws a filled rectangle/square
* `line color lw x1 y1 x2 y2`: Draws a line
* `text color x y size words`: Draws text on the screen
* `image file x y scale rotation`: Loads and draws an image

### Control & Geometric Transformations
The interpreter's engine is based on an immutable referential system (state `t`) to isolate local transformations.
* `call label`: Executes a block of instructions defined by another label
* `translate dx dy`: Shifts the origin (0,0) of the coordinate system for subsequent instructions within the label
* `rotate deg`: Rotates the drawing frame by the specified angle for subsequent instructions
* `repeatrotate label n deg`: Draws a label *n* times, applying a cumulative rotation at each iteration
* `repeatplanar label nx ny dx dy sy`: Draws a 2D grid of a label, applying both horizontal and vertical accumulated offsets

## Technologies & Architecture

* **Language:** OCaml
* **Build System:** Dune 
* **Graphics:** OCaml `Graphics` module


# FLP - Decision Tree Classifier (Haskell)

## About the Project

This project is an implementation of a **decision tree** in **Haskell**, developed for the course **Functional and Logical Programming (FLP) 2024/25** at FIT VUT.  

The project consists of two main tasks:

1. **Reading a decision tree from a text file and classifying new data**.
2. **Training a decision tree based on input training data** (a simplified CART algorithm).

## Table of Contents

- [Features](#features)
- [Requirements](#requirements)
- [Build & Run](#build--run)
- [Usage](#usage)
- [Examples](#examples)
- [Project Structure](#project-structure)
- [Contact](#contact)

## Features

### Task 1: Decision Tree Classification
- **Load a decision tree** from a specified text format.
- **Load new data** for classification.
- **Classify new data** using the loaded tree.

### Task 2: Decision Tree Training
- **Train a decision tree** from labeled training data.
- **Save the generated tree** in the same text format as used in Task 1.

## Requirements

- **Haskell (GHC) version 8.8.4** or later.
- Only standard libraries are used: `base`, `containers`, `parsec`, `vector`, `split`, `directory`, `random`.

## Build & Run

### Build

The project is built using `Makefile`:

```bash
make
```
#### This will produce the executable:
```bash
flp-fun
```
### Run
#### Task 1: Classification
```bash
./flp-fun -1 <tree_file> <data_file>
```
#### Task 2: Training
```bash
./flp-fun -2 <training_data_file>

```
## Usage
### Decision Tree Format:
```bash
Node: feature_index, threshold
  Node: feature_index, threshold
    Leaf: Class1
    Leaf: Class2
  Leaf: Class3
  ```

### Input Data Format for Classification:
```bash
1.2,3.4
5.6,7.8
...
```

### Training Data Format:
```bash
1.2,3.4,Class1
5.6,7.8,Class2
...
```

## Examples
### Example Decision Tree:
```bash
Node: 0, 5.5
  Leaf: TridaA
  Node: 1, 3.0
    Leaf: TridaB
    Leaf: TridaC
```
### Example Input Data for Classification:
```bash
2.4,1.3
6.1,0.3
6.3,4.4
```
#### Expected Output:
```bash
TridaA
TridaB
TridaC
```
## Project Structure
```bash

├── src/                # Source code in Haskell
├── test/               # Test files
├── Makefile            # Build script
├── flp-fun             # Compiled executable (after running make)
└── README.md           # This documentation file
```

## Contact
- Author: Dinara Garipova
- VUT Login: xgarip00
- Year: 2024/25
- Course Supervisor: Ing. Daniel Poliakov
- Supervisor Email: poliakov@fit.vut.cz


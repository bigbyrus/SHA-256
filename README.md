# SHA-256

This project implements a simplified SHA-256 hashing algorithm using a finite state machine programmed in SystemVerilog. It is designed to process 512-bit message blocks to compute a 256-bit hash.

## Features

- Separates input message into N 512-bit blocks 
- Handles multiple 512-bit message blocks (16 x 32-bit words each)

## FSM States Overview

- `IDLE`: Waits for a `start` signal to begin hashing
- `WAIT`: Sets up memory reads
- `BLOCK`: Loads next message block from memory
- `COMPUTE`: Performs 64 rounds of SHA-256 compression
- `WRITE`: Writes the final digest to memory
- `DONE`: Hashing complete

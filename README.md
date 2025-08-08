# SHA-256 FSM (SystemVerilog)

This project implements a simplified SHA-256 hashing algorithm using a finite state machine in SystemVerilog. It is designed to process 512-bit message blocks and compute a 256-bit hash, following the core structure of the SHA-256.

## Features

- **FSM-based implementation** of the SHA-256 compression function
- Handles multiple 512-bit message blocks (16 x 32-bit words each)

## FSM States Overview

- `IDLE`: Waits for a `start` signal to begin hashing
- `WAIT`: Sets up memory reads
- `BLOCK`: Loads the next message block from memory
- `COMPUTE`: Performs 64 rounds of SHA-256 compression
- `WRITE`: Writes the final digest to memory
- `DONE`: Hashing is complete

## Key Concepts

- **Message Schedule Buffer Shifting**:
  ```systemverilog
  for (int n = 0; n < 15; n++) 
      w[n] <= w[n+1];
  w[15] <= expansion;
--> Implements rolling message schedule for word expansion, which conserves FPGA resources

#!/bin/bash

# Set the binary name and target location
BINARY_NAME="compiler_c"
TARGET_PATH="/usr/local/bin/$BINARY_NAME"

# Step 1: Build the project in release mode
echo "Building the project in release mode..."
cargo build --release

# Step 2: Copy the binary to the target location
echo "Copying the binary to $TARGET_PATH..."
cp target/release/$BINARY_NAME $TARGET_PATH

# Step 3: Make the binary executable
echo "Making the binary executable..."
chmod +x $TARGET_PATH

# Done
echo "Deployment successful! You can now run the compiler using '$BINARY_NAME /path/to/program.c'."
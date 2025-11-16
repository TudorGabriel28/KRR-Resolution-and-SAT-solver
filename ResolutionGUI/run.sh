#!/bin/bash
# Run the Resolution Test Cases GUI

echo "Starting Resolution Test Cases GUI..."
cd "$(dirname "$0")"
java -cp build/classes resolutiongui.ResolutionGUI

#!/bin/bash

echo "Procedural Environment Generator - Quick Start"
echo "=============================================="
echo ""

# Check for .NET
if ! command -v dotnet &> /dev/null; then
    echo "Error: .NET SDK not found. Install from https://dot.net"
    exit 1
fi

echo "Starting HTTP API server..."
echo ""

dotnet run --project Server.fsproj

# When server exits, show next steps
echo ""
echo "To use CLI:"
echo "  dotnet run --project CLI.fsproj -- generate --help"
echo ""
echo "To view web client:"
echo "  python -m http.server 8000"
echo "  Then visit http://localhost:8000/client.html"

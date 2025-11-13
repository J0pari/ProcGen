#!/usr/bin/env python3
"""
Fix all Vector3 references in F# test files to use System.Numerics.Vector3
instead of the old TestInfrastructure.Core.Vector3 record type.
"""

import re
import os
from pathlib import Path

def fix_file(filepath):
    """Fix Vector3 references in a single file."""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    original = content

    # Fix: Vector3.zero -> Vector3.Zero
    content = re.sub(r'\bVector3\.zero\b', 'Vector3.Zero', content)

    # Fix: Vector3.unitX -> Vector3.UnitX (and Y, Z)
    content = re.sub(r'\bVector3\.unitX\b', 'Vector3.UnitX', content)
    content = re.sub(r'\bVector3\.unitY\b', 'Vector3.UnitY', content)
    content = re.sub(r'\bVector3\.unitZ\b', 'Vector3.UnitZ', content)

    # Fix: Vector3.distanceTo v1 v2 -> Vector3.Distance(v1, v2)
    content = re.sub(r'\bVector3\.distanceTo\s+(\w+)\s+(\w+)', r'Vector3.Distance(\1, \2)', content)

    # Fix: Vector3.magnitude v -> v.Length()
    content = re.sub(r'\bVector3\.magnitude\s+(\w+)', r'\1.Length()', content)

    # Fix: Vector3.normalize v -> Vector3.Normalize(v)
    content = re.sub(r'\bVector3\.normalize\s+(\w+)', r'Vector3.Normalize(\1)', content)

    # Fix: Vector3.dot v1 v2 -> Vector3.Dot(v1, v2)
    content = re.sub(r'\bVector3\.dot\s+(\w+)\s+(\w+)', r'Vector3.Dot(\1, \2)', content)

    # Fix: Vector3.cross v1 v2 -> Vector3.Cross(v1, v2)
    content = re.sub(r'\bVector3\.cross\s+(\w+)\s+(\w+)', r'Vector3.Cross(\1, \2)', content)

    # Fix: { X = x; Y = y; Z = z } -> Vector3(x, y, z)
    # This regex handles the record construction syntax
    content = re.sub(
        r'\{\s*X\s*=\s*([^;]+)\s*;\s*Y\s*=\s*([^;]+)\s*;\s*Z\s*=\s*([^}]+)\s*\}',
        r'Vector3(\1, \2, \3)',
        content
    )

    # Fix: { v with X = value } -> Vector3(value, v.Y, v.Z)
    # This is trickier - need to handle record update syntax
    def replace_record_update(match):
        base_var = match.group(1)
        field = match.group(2)
        value = match.group(3)

        if field == 'X':
            return f'Vector3({value}, {base_var}.Y, {base_var}.Z)'
        elif field == 'Y':
            return f'Vector3({base_var}.X, {value}, {base_var}.Z)'
        elif field == 'Z':
            return f'Vector3({base_var}.X, {base_var}.Y, {value})'
        return match.group(0)

    content = re.sub(
        r'\{\s*(\w+)\s+with\s+([XYZ])\s*=\s*([^}]+)\s*\}',
        replace_record_update,
        content
    )

    # Fix type annotations: (v: Vector3) when TestInfrastructure.Core is in scope
    # This requires adding System.Numerics prefix in some contexts
    # For now, ensure Vector3 refers to System.Numerics.Vector3

    if content != original:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(content)
        return True
    return False

def main():
    """Fix all test files."""
    tests_dir = Path(__file__).parent.parent / 'tests'

    if not tests_dir.exists():
        print(f"Error: tests directory not found at {tests_dir}")
        return 1

    fixed_count = 0
    for fs_file in tests_dir.glob('*.fs'):
        if fix_file(fs_file):
            print(f"Fixed: {fs_file.name}")
            fixed_count += 1
        else:
            print(f"No changes: {fs_file.name}")

    print(f"\nTotal files fixed: {fixed_count}")
    return 0

if __name__ == '__main__':
    exit(main())

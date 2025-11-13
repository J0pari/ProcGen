#!/usr/bin/env python3

import re
from pathlib import Path
from functools import reduce
from typing import Callable, Tuple

# Type alias for transformation functions
Transform = Callable[[str], str]

# Kleisli composition: (a -> m b) -> (b -> m c) -> (a -> m c)
def kleisli_compose(*transforms: Transform) -> Transform:
    """Compose transformations via function composition (Kleisli arrow in Identity monad)."""
    return lambda content: reduce(lambda acc, f: f(acc), transforms, content)

# Transformation combinators
def mk_simple_subst(pattern: str, replacement: str) -> Transform:
    """Create a simple substitution transformation."""
    return lambda s: re.sub(pattern, replacement, s)

def mk_complex_subst(pattern: str, replacement: str | Callable, flags=0) -> Transform:
    """Create a complex substitution with flags."""
    return lambda s: re.sub(pattern, replacement, s, flags=flags)

def mk_record_update_transform() -> Transform:
    """Transform record update syntax { v with Field = val } to Vector3 constructor."""
    def replace(match):
        var, field, val = match.group(1), match.group(2), match.group(3)
        fields = {'X': f'Vector3({val}, {var}.Y, {var}.Z)',
                  'Y': f'Vector3({var}.X, {val}, {var}.Z)',
                  'Z': f'Vector3({var}.X, {var}.Y, {val})'}
        return fields.get(field, match.group(0))

    return mk_complex_subst(
        r'\{\s*(\w+)\s+with\s+([XYZ])\s*=\s*([^}]+?)\s*\}',
        replace,
        flags=re.DOTALL
    )

# Comonadic extraction: derive transformations from the environment
transformations = [
    # Module member capitalization (endofunctor over string substitution)
    mk_simple_subst(r'\bVector3\.zero\b', 'Vector3.Zero'),
    mk_simple_subst(r'\bVector3\.unitX\b', 'Vector3.UnitX'),
    mk_simple_subst(r'\bVector3\.unitY\b', 'Vector3.UnitY'),
    mk_simple_subst(r'\bVector3\.unitZ\b', 'Vector3.UnitZ'),

    # Method call transformations (applicative functor application)
    mk_simple_subst(r'\bVector3\.distanceTo\s+(\w+)\s+(\w+)', r'Vector3.Distance(\1, \2)'),
    mk_simple_subst(r'\bVector3\.magnitude\s+(\w+)', r'\1.Length()'),
    mk_simple_subst(r'\bVector3\.normalize\s+(\w+)', r'Vector3.Normalize(\1)'),
    mk_simple_subst(r'\bVector3\.dot\s+(\w+)\s+(\w+)', r'Vector3.Dot(\1, \2)'),
    mk_simple_subst(r'\bVector3\.cross\s+(\w+)\s+(\w+)', r'Vector3.Cross(\1, \2)'),

    # Record construction -> constructor (multiline-aware via DOTALL)
    mk_complex_subst(
        r'\{\s*X\s*=\s*([^;]+?)\s*;\s*Y\s*=\s*([^;]+?)\s*;\s*Z\s*=\s*([^}]+?)\s*\}',
        r'Vector3(\1, \2, \3)',
        flags=re.DOTALL
    ),

    # Record update syntax transformation
    mk_record_update_transform(),

    # Qualified name normalization (natural transformation)
    mk_simple_subst(r'\bCore\.Vector3\b', 'Vector3'),
    mk_simple_subst(r'\bTestInfrastructure\.Core\.Vector3\b', 'Vector3'),
]

# Compose all transformations into a single pipeline
transform_pipeline: Transform = kleisli_compose(*transformations)

def fix_file(filepath: Path) -> bool:
    """Apply the transformation pipeline to a file. Returns True if modified."""
    content = filepath.read_text(encoding='utf-8')
    transformed = transform_pipeline(content)

    if transformed != content:
        filepath.write_text(transformed, encoding='utf-8')
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

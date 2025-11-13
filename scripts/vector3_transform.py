#!/usr/bin/env python3

import re
from pathlib import Path
from functools import reduce
from typing import Callable

Transform = Callable[[str], str]

def kleisli(*fs: Transform) -> Transform:
    return lambda s: reduce(lambda acc, f: f(acc), fs, s)

def subst(pat: str, repl: str | Callable, flags=0) -> Transform:
    return lambda s: re.sub(pat, repl, s, flags=flags)

def record_update_comonad():
    def extract(m):
        v, f, x = m.groups()
        return {'X': f'Vector3({x}, {v}.Y, {v}.Z)',
                'Y': f'Vector3({v}.X, {x}, {v}.Z)',
                'Z': f'Vector3({v}.X, {v}.Y, {x})'}.get(f, m.group(0))
    return subst(r'\{\s*(\w+)\s+with\s+([XYZ])\s*=\s*([^}]+?)\s*\}', extract, re.DOTALL)

pipeline = kleisli(
    subst(r'\bVector3\.zero\b', 'Vector3.Zero'),
    subst(r'\bVector3\.unitX\b', 'Vector3.UnitX'),
    subst(r'\bVector3\.unitY\b', 'Vector3.UnitY'),
    subst(r'\bVector3\.unitZ\b', 'Vector3.UnitZ'),
    subst(r'\bVector3\.distanceTo\s+(\w+)\s+(\w+)', r'Vector3.Distance(\1, \2)'),
    subst(r'\bVector3\.magnitude\s+(\w+)', r'\1.Length()'),
    subst(r'\bVector3\.normalize\s+(\w+)', r'Vector3.Normalize(\1)'),
    subst(r'\bVector3\.dot\s+(\w+)\s+(\w+)', r'Vector3.Dot(\1, \2)'),
    subst(r'\bVector3\.cross\s+(\w+)\s+(\w+)', r'Vector3.Cross(\1, \2)'),
    subst(r'\{\s*X\s*=\s*([^;]+?)\s*;\s*Y\s*=\s*([^;]+?)\s*;\s*Z\s*=\s*([^}]+?)\s*\}',
          r'Vector3(\1, \2, \3)', re.DOTALL),
    record_update_comonad(),
    subst(r'\bCore\.Vector3\b', 'Vector3'),
    subst(r'\bTestInfrastructure\.Core\.Vector3\b', 'Vector3'),
)

def transform_file(p: Path) -> bool:
    orig = p.read_text(encoding='utf-8')
    new = pipeline(orig)
    if new != orig:
        p.write_text(new, encoding='utf-8')
        return True
    return False

if __name__ == '__main__':
    tests = Path(__file__).parent.parent / 'tests'
    fixed = sum(1 for f in tests.glob('*.fs') if transform_file(f))
    print(f"Fixed {fixed} files")

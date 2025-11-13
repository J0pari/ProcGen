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

def add_system_numerics_import():
    def add_import(content: str) -> str:
        if 'open System.Numerics' in content:
            return content

        module_pattern = r'(module\s+\w+\s*=\s*\n(?:\s*\[\<[^\]]+\>\]\s*\n)*)((?:\s*open\s+[^\n]+\n)*)'

        def insert_import(m):
            module_decl = m.group(1)
            existing_opens = m.group(2)
            if 'open System.Numerics' not in existing_opens:
                if existing_opens.strip():
                    return module_decl + existing_opens + '    open System.Numerics\n'
                else:
                    return module_decl + '    open System.Numerics\n'
            return m.group(0)

        return re.sub(module_pattern, insert_import, content, count=1)

    return add_import

def record_construct_transform():
    def replace(m):
        x, y, z = m.group(1).strip(), m.group(2).strip(), m.group(3).strip()
        return f'Vector3({x}, {y}, {z})'

    return subst(
        r'\{\s*(?:Vector3\.)?X\s*=\s*([^;{]+?)\s*;\s*Y\s*=\s*([^;{]+?)\s*;\s*Z\s*=\s*([^}{]+?)\s*\}',
        replace,
        re.DOTALL
    )

def record_update_comonad():
    def extract(m):
        v, f, x = m.groups()
        return {'X': f'Vector3({x}, {v}.Y, {v}.Z)',
                'Y': f'Vector3({v}.X, {x}, {v}.Z)',
                'Z': f'Vector3({v}.X, {v}.Y, {x})'}.get(f, m.group(0))
    return subst(r'\{\s*(\w+)\s+with\s+([XYZ])\s*=\s*([^}]+?)\s*\}', extract, re.DOTALL)

pipeline = kleisli(
    add_system_numerics_import(),
    subst(r'\bVector3\.zero\b', 'Vector3.Zero'),
    subst(r'\bVector3\.unitX\b', 'Vector3.UnitX'),
    subst(r'\bVector3\.unitY\b', 'Vector3.UnitY'),
    subst(r'\bVector3\.unitZ\b', 'Vector3.UnitZ'),
    subst(r'\bVector3\.distanceTo\s+(\w+)\s+(\w+)', r'Vector3.Distance(\1, \2)'),
    subst(r'\bVector3\.magnitude\s+(\w+)', r'\1.Length()'),
    subst(r'\bVector3\.normalize\s+(\w+)', r'Vector3.Normalize(\1)'),
    subst(r'\bVector3\.dot\s+(\w+)\s+(\w+)', r'Vector3.Dot(\1, \2)'),
    subst(r'\bVector3\.cross\s+(\w+)\s+(\w+)', r'Vector3.Cross(\1, \2)'),
    record_construct_transform(),
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

    for f in tests.glob('*.fs'):
        if transform_file(f):
            print(f"Fixed: {f.name}")
        else:
            print(f"No changes: {f.name}")

    fixed = sum(1 for f in tests.glob('*.fs') if f.stat().st_mtime > tests.stat().st_mtime)
    print(f"\nTotal files fixed: {fixed}")

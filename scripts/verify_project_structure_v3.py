#!/usr/bin/env python3

import sys
import re
from pathlib import Path
from collections import defaultdict
import xml.etree.ElementTree as ET

ROOT = Path(__file__).parent.parent

class ProjectVerifier:
    def __init__(self):
        self.errors = []
        self.projects = {}
        self.project_files = defaultdict(set)
        self.all_files = set()
        self.source_files = set()
        self.metadata_files = set()
        self.build_files = set()
        self.binary_files = set()
        self.alternative_builds = defaultdict(set)
        self.transitive_deps = defaultdict(set)
        self.project_extensions = set()
        self.include_patterns = []

    def error(self, msg):
        self.errors.append(msg)
        print(f"ERROR: {msg}")

    def is_build_artifact_dir(self, path):
        return any(part in {'obj', 'bin', 'node_modules', '.git', '__pycache__', 'target'} for part in path.parts)

    def classify_file(self, path):
        try:
            with open(path, 'rb') as f:
                header = f.read(8192)

            if b'\x00' in header[:512]:
                return 'binary'

            try:
                text = header.decode('utf-8', errors='strict')
            except UnicodeDecodeError:
                return 'binary'

            lines = text.split('\n')
            non_empty_lines = [l.strip() for l in lines if l.strip()]

            if len(non_empty_lines) == 0:
                return 'metadata'

            if any(kw in text for kw in ['<?xml', '<project', '<Project', 'cmake_minimum_required', 'package.json']):
                return 'build'

            code_indicators = sum(1 for line in lines if any(kw in line for kw in
                ['import ', '#include', 'def ', 'fn ', 'func ', 'class ', 'struct ', 'public ', 'private ',
                 'namespace ', 'module ', 'let ', 'const ', 'var ', 'type ', '::']))

            function_definitions = sum(1 for line in lines if re.search(r'\b(def|fn|func|function|sub|let\s+\w+\s*=.*=>)\s+\w+\s*\(', line))

            avg_line_length = sum(len(l) for l in non_empty_lines) / len(non_empty_lines)
            variance = sum((len(l) - avg_line_length) ** 2 for l in non_empty_lines) / len(non_empty_lines)

            has_structure = function_definitions > 0 or code_indicators > len(lines) * 0.1
            has_low_variance = variance < 500

            if has_structure and has_low_variance:
                return 'source'

            return 'metadata'

        except Exception:
            return 'unknown'

    def discover_file_classifications(self):
        print("=" * 80)
        print("DISCOVERING AND CLASSIFYING ALL FILES")
        print("=" * 80)

        for item in ROOT.rglob("*"):
            if not item.is_file():
                continue
            if self.is_build_artifact_dir(item):
                continue

            self.all_files.add(item)
            classification = self.classify_file(item)

            if classification == 'binary':
                self.binary_files.add(item)
            elif classification == 'build':
                self.build_files.add(item)
            elif classification == 'source':
                self.source_files.add(item)
            elif classification == 'metadata':
                self.metadata_files.add(item)

        print(f"  Total files: {len(self.all_files)}")
        print(f"  Source files: {len(self.source_files)}")
        print(f"  Build files: {len(self.build_files)}")
        print(f"  Metadata files: {len(self.metadata_files)}")
        print(f"  Binary files: {len(self.binary_files)}")

    def discover_include_patterns(self):
        print("\n" + "=" * 80)
        print("DISCOVERING INCLUDE PATTERNS")
        print("=" * 80)

        pattern_candidates = [
            r'#include\s+[<"]([^>"]+)[>"]',
            r'import\s+(\S+)',
            r'open\s+(\w+(?:\.\w+)*)',
            r'require\s+["\']([^"\']+)["\']',
            r'use\s+(\w+(?:::\w+)*)',
            r'from\s+(\S+)\s+import',
        ]

        pattern_samples = defaultdict(set)

        for source_file in self.source_files:
            try:
                content = source_file.read_text(errors='ignore')
                for line in content.split('\n')[:100]:
                    for pattern in pattern_candidates:
                        if re.search(pattern, line):
                            pattern_samples[pattern].add(source_file.suffix)
            except Exception:
                pass

        for pattern, exts in pattern_samples.items():
            self.include_patterns.append(pattern)
            print(f"  Pattern: {pattern}")
            print(f"    Used by: {', '.join(sorted(exts))}")

        if not self.include_patterns:
            print("  No include patterns discovered")

    def discover_transitive_dependencies(self):
        print("\n" + "=" * 80)
        print("DISCOVERING TRANSITIVE DEPENDENCIES")
        print("=" * 80)

        for source_file in self.source_files:
            try:
                content = source_file.read_text(errors='ignore')
                src_dir = source_file.parent

                for line in content.split('\n'):
                    for pattern in self.include_patterns:
                        match = re.search(pattern, line)
                        if match:
                            ref = match.group(1)

                            candidates = [
                                src_dir / ref,
                                src_dir / f"{ref}.h",
                                src_dir / f"{ref}.hpp",
                                src_dir / f"{ref}.cuh",
                                src_dir / f"{ref}.hh",
                            ]

                            for cand in candidates:
                                resolved = cand.resolve()
                                if resolved.exists() and resolved in self.source_files:
                                    self.transitive_deps[resolved].add(source_file)
                                    break

            except Exception:
                pass

        if self.transitive_deps:
            print(f"\nFound {len(self.transitive_deps)} transitive dependencies:")
            for header, sources in sorted(self.transitive_deps.items()):
                print(f"  {header.relative_to(ROOT)}")
                print(f"    Included by {len(sources)} source files")
        else:
            print("  No transitive dependencies discovered")

    def discover_alternative_builds(self):
        print("\n" + "=" * 80)
        print("DISCOVERING ALTERNATIVE BUILD SYSTEMS")
        print("=" * 80)

        for build_file in self.build_files:
            print(f"\nBuild file: {build_file.relative_to(ROOT)}")

            try:
                content = build_file.read_text(errors='ignore')
                build_dir = build_file.parent

                referenced_files = set()
                for line in content.split('\n'):
                    for match in re.finditer(r'[\w/\-]+\.\w+', line):
                        filename = match.group(0)
                        candidate = (build_dir / filename).resolve()
                        if candidate.exists() and candidate in self.source_files:
                            referenced_files.add(candidate)

                if referenced_files:
                    print(f"  References {len(referenced_files)} source files:")
                    for f in sorted(referenced_files):
                        rel = f.relative_to(ROOT)
                        print(f"    - {rel}")
                        self.alternative_builds[f].add(build_file.relative_to(ROOT))

            except Exception as e:
                print(f"  Could not parse: {e}")

    def discover_project_types(self):
        print("\n" + "=" * 80)
        print("DISCOVERING PROJECT TYPES")
        print("=" * 80)

        for item in self.build_files:
            if '*proj' in item.name or item.suffix.endswith('proj'):
                ext = item.suffix
                if ext not in self.project_extensions:
                    self.project_extensions.add(ext)
                    print(f"  Found project type: {ext}")

        if not self.project_extensions:
            print("  No project files found")

    def discover_project_sources(self):
        print("\n" + "=" * 80)
        print("DISCOVERING PROJECT SOURCE FILES")
        print("=" * 80)

        for ext in self.project_extensions:
            for proj_file in [f for f in self.build_files if f.suffix == ext]:
                proj_dir = proj_file.parent
                proj_name = proj_file.stem
                self.projects[proj_name] = proj_file

                print(f"\nProject: {proj_name}")
                print(f"Location: {proj_file.relative_to(ROOT)}")

                try:
                    tree = ET.parse(proj_file)
                    root = tree.getroot()

                    compile_items = []
                    for item_group in root.findall(".//ItemGroup"):
                        for elem in item_group:
                            if elem.tag in {"Compile", "Content", "EmbeddedResource", "None"}:
                                include = elem.get("Include")
                                if include:
                                    compile_items.append(include)

                    print(f"Files declared in project ({len(compile_items)}):")
                    for item in sorted(compile_items):
                        file_path = (proj_dir / item).resolve()
                        print(f"  - {item}")
                        if file_path.exists():
                            self.project_files[file_path].add(proj_name)
                        else:
                            self.error(f"{proj_name} references non-existent: {item}")
                except Exception as e:
                    print(f"  Could not parse (skipping): {e}")

    def verify(self):
        print("\n" + "=" * 80)
        print("VERIFICATION RESULTS")
        print("=" * 80)

        all_managed = set(self.project_files.keys()) | set(self.alternative_builds.keys()) | set(self.transitive_deps.keys())

        source_exts = defaultdict(int)
        for f in self.source_files:
            source_exts[f.suffix] += 1

        print(f"\nTotal source files: {len(self.source_files)}")
        print(f"Source file types: {', '.join(f'{ext}({count})' for ext, count in sorted(source_exts.items()))}")
        print(f"Files assigned to projects: {len(self.project_files)}")
        print(f"Files in alternative builds: {len(self.alternative_builds)}")
        print(f"Transitive dependencies: {len(self.transitive_deps)}")
        print(f"Total managed: {len(all_managed)}")

        orphans = self.source_files - all_managed

        if orphans:
            print(f"\nTRUE ORPHANS ({len(orphans)}):")
            print("These source files are not referenced by any build system:")
            for orphan in sorted(orphans):
                rel = orphan.relative_to(ROOT)
                print(f"  - {rel}")
                self.error(f"ORPHANED: {rel}")

        alt_only = set(self.alternative_builds.keys()) - set(self.project_files.keys())
        if alt_only:
            print(f"\nALTERNATIVE BUILD FILES ({len(alt_only)}):")
            print("These files are built by alternative systems (not project files):")
            for f in sorted(alt_only):
                rel = f.relative_to(ROOT)
                build_sys = ', '.join(str(b) for b in sorted(self.alternative_builds[f]))
                print(f"  - {rel}")
                print(f"    Built by: {build_sys}")

        if self.transitive_deps:
            print(f"\nTRANSITIVE DEPENDENCIES ({len(self.transitive_deps)}):")
            print("These files are included/imported by other source files:")
            for header in sorted(self.transitive_deps.keys()):
                rel = header.relative_to(ROOT)
                sources = self.transitive_deps[header]
                print(f"  - {rel}")
                print(f"    Included by: {len(sources)} files")

        duplicates = {f: projs for f, projs in self.project_files.items() if len(projs) > 1}
        if duplicates:
            print(f"\nDUPLICATE ASSIGNMENTS ({len(duplicates)}):")
            for file_path, projs in sorted(duplicates.items()):
                print(f"  - {file_path.name} in: {', '.join(sorted(projs))}")
                self.error(f"DUPLICATE: {file_path.name} in {', '.join(sorted(projs))}")

        print(f"\n{'FAILED' if self.errors else 'PASSED'}")
        print(f"Errors: {len(self.errors)}")
        return len(self.errors) == 0

    def run(self):
        self.discover_file_classifications()
        self.discover_project_types()
        self.discover_project_sources()
        self.discover_include_patterns()
        self.discover_alternative_builds()
        self.discover_transitive_dependencies()
        return self.verify()

if __name__ == "__main__":
    verifier = ProjectVerifier()
    success = verifier.run()
    sys.exit(0 if success else 1)

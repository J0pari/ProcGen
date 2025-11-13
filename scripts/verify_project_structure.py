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
        self.all_source_files = set()
        self.alternative_builds = defaultdict(set)
        self.transitive_deps = defaultdict(set)
        self.project_extensions = set()

    def error(self, msg):
        self.errors.append(msg)
        print(f"ERROR: {msg}")

    def should_exclude(self, path):
        parts = set(path.parts)
        if parts & {'obj', 'bin', 'node_modules', '.git', '__pycache__', 'target'}:
            return True
        ext = path.suffix.lower()
        if ext in {'.exe', '.dll', '.so', '.lib', '.exp', '.pdb', '.o', '.a',
                   '.txt', '.md', '.json', '.xml', '.yml', '.yaml', '.log',
                   '.sln', '.sh', '.bat', '.ps1', '.props', '.config'}:
            return True
        return False

    def discover_transitive_dependencies(self):
        print("\n" + "=" * 80)
        print("DISCOVERING TRANSITIVE DEPENDENCIES")
        print("=" * 80)

        for source_file in self.all_source_files:
            if source_file.suffix.lower() not in {'.c', '.cpp', '.cu', '.h', '.hpp', '.cuh', '.fs'}:
                continue

            try:
                content = source_file.read_text(errors='ignore')
                src_dir = source_file.parent

                for line in content.split('\n'):
                    include_match = re.search(r'#include\s+[<"]([^>"]+)[>"]', line)
                    if include_match:
                        header_name = include_match.group(1)
                        header_path = (src_dir / header_name).resolve()
                        if header_path.exists() and header_path in self.all_source_files:
                            self.transitive_deps[header_path].add(source_file)

                    open_match = re.search(r'open\s+(\w+(?:\.\w+)*)', line)
                    if open_match:
                        pass

            except Exception:
                pass

        if self.transitive_deps:
            print("\nFound transitive dependencies:")
            for header, sources in sorted(self.transitive_deps.items()):
                print(f"  {header.relative_to(ROOT)}")
                print(f"    Included by {len(sources)} source files")

    def discover_alternative_builds(self):
        print("\n" + "=" * 80)
        print("DISCOVERING ALTERNATIVE BUILD SYSTEMS")
        print("=" * 80)

        build_scripts = []
        for pattern in ['Makefile', '*.mk', 'CMakeLists.txt', '*.cmake', 'build.sh', 'build.bat']:
            build_scripts.extend(ROOT.rglob(pattern))

        for script in build_scripts:
            print(f"\nBuild script: {script.relative_to(ROOT)}")

            try:
                content = script.read_text(errors='ignore')
                script_dir = script.parent

                referenced_files = set()
                for line in content.split('\n'):
                    for match in re.finditer(r'[\w/\-]+\.\w+', line):
                        filename = match.group(0)
                        candidate = (script_dir / filename).resolve()
                        if candidate.exists() and candidate.is_file():
                            if not self.should_exclude(candidate):
                                referenced_files.add(candidate)

                if referenced_files:
                    print(f"  References {len(referenced_files)} source files:")
                    for f in sorted(referenced_files):
                        rel = f.relative_to(ROOT)
                        print(f"    - {rel}")
                        self.alternative_builds[f].add(script.relative_to(ROOT))

            except Exception as e:
                print(f"  Could not parse: {e}")

    def discover_project_types(self):
        print("=" * 80)
        print("DISCOVERING PROJECT TYPES")
        print("=" * 80)

        for item in ROOT.rglob("*proj"):
            if item.is_file():
                ext = item.suffix
                if ext not in self.project_extensions:
                    self.project_extensions.add(ext)
                    print(f"  Found project type: {ext}")

        if not self.project_extensions:
            print("  No project files found")

    def discover_sources(self):
        print("\n" + "=" * 80)
        print("DISCOVERING SOURCE FILES")
        print("=" * 80)

        for ext in self.project_extensions:
            for proj_file in ROOT.rglob(f"*{ext}"):
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

        print("\n" + "=" * 80)
        print("SCANNING FILESYSTEM FOR ALL SOURCE FILES")
        print("=" * 80)

        source_dirs = set(p.parent for p in self.projects.values())

        for src_dir in source_dirs:
            print(f"\nScanning: {src_dir.relative_to(ROOT)}")
            files_found = []
            for item in src_dir.rglob("*"):
                if item.is_file() and not self.should_exclude(item) and item.suffix not in self.project_extensions:
                    self.all_source_files.add(item)
                    files_found.append(item)

            print(f"Found {len(files_found)} source files:")
            by_ext = defaultdict(list)
            for f in sorted(files_found):
                by_ext[f.suffix].append(f.name)

            for ext in sorted(by_ext.keys()):
                print(f"\n  {ext} files ({len(by_ext[ext])}):")
                for name in sorted(by_ext[ext]):
                    print(f"    - {name}")

    def verify(self):
        print("\n" + "=" * 80)
        print("VERIFICATION RESULTS")
        print("=" * 80)

        all_managed = set(self.project_files.keys()) | set(self.alternative_builds.keys()) | set(self.transitive_deps.keys())

        print(f"\nTotal source files found: {len(self.all_source_files)}")
        print(f"Files assigned to projects: {len(self.project_files)}")
        print(f"Files in alternative builds: {len(self.alternative_builds)}")
        print(f"Transitive dependencies: {len(self.transitive_deps)}")
        print(f"Total managed: {len(all_managed)}")

        orphans = self.all_source_files - all_managed

        if orphans:
            print(f"\nTRUE ORPHANS ({len(orphans)}):")
            print("These files are not referenced by any build system:")
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
        self.discover_project_types()
        self.discover_sources()
        self.discover_alternative_builds()
        self.discover_transitive_dependencies()
        return self.verify()

if __name__ == "__main__":
    verifier = ProjectVerifier()
    success = verifier.run()
    sys.exit(0 if success else 1)

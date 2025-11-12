#!/bin/bash
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly ROOT_DIR="$(dirname "$SCRIPT_DIR")"
readonly TESTS_DIR="$ROOT_DIR/tests"

run_all_tests() {
    cd "$TESTS_DIR"
    dotnet test Tests.fsproj --no-build --verbosity normal --logger "console;verbosity=detailed"
}

run_filtered_tests() {
    local filter=$1
    cd "$TESTS_DIR"
    dotnet test Tests.fsproj --no-build --filter "$filter" --verbosity normal --logger "console;verbosity=detailed"
}

list_tests() {
    cd "$TESTS_DIR"
    dotnet test Tests.fsproj --list-tests
}

watch_tests() {
    cd "$TESTS_DIR"
    dotnet watch test Tests.fsproj --verbosity normal
}

show_help() {
    cat << 'EOF'
test_runner.sh - Interactive test execution

Usage:
  ./test_runner.sh [command] [args]

Commands:
  all                Run all tests
  filter <pattern>   Run tests matching pattern
  list               List all available tests
  watch              Run tests in watch mode (rerun on file change)

Examples:
  ./test_runner.sh all
  ./test_runner.sh filter "GPUIntegrationTests"
  ./test_runner.sh filter "FullyQualifiedName~collision"
  ./test_runner.sh list
  ./test_runner.sh watch

Filter patterns:
  FullyQualifiedName~<substring>     Match test name
  Category=<category>                Match test category
  Priority=<number>                  Match priority
EOF
}

main() {
    case "${1:-all}" in
        all)
            run_all_tests
            ;;
        filter)
            if [[ -z "${2:-}" ]]; then
                echo "Error: filter requires a pattern argument" >&2
                exit 1
            fi
            run_filtered_tests "$2"
            ;;
        list)
            list_tests
            ;;
        watch)
            watch_tests
            ;;
        help|--help|-h)
            show_help
            ;;
        *)
            echo "Error: unknown command '$1'" >&2
            show_help
            exit 1
            ;;
    esac
}

main "$@"

# JVM Performance Optimization Rules

When writing or reviewing JVM code, follow these principles:

## 1. Hot Path Awareness
- Identify code that runs millions of times per second
- Optimize only hot paths (use profiling)
- Keep business logic outside hot paths

## 2. Avoid Allocations
- Do not allocate objects inside loops
- Prefer primitives (int, long, double)
- Reuse objects where possible

## 3. Avoid Boxing
- Do not use generic types in hot paths
- Prefer primitive-specific implementations

## 4. Control Polymorphism
- Keep call-sites monomorphic or bimorphic
- Avoid interfaces/traits in tight loops
- Avoid dynamic dispatch in hot paths

## 5. Structure Code Properly
- Above the line: clean, abstract, maintainable
- Below the line: fast, minimal, imperative

## 6. JIT Optimization Awareness
- Stable types enable inlining and escape analysis
- Avoid unpredictable control flow

## 7. Always Measure
- Use JMH or profilers before optimizing
- Validate performance changes

When in doubt:
- Optimize only after profiling
- Prefer clarity outside hot paths
- Prefer performance inside hot paths
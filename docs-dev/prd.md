# Product Requirements Document: GF-Core Library Issue Resolution

## Executive Summary

The gf-core Rust library has critical parsing functionality issues that prevent it from fulfilling its core purpose as a Grammatical Framework runtime. While the foundational components (PGF loading, JSON conversion, data structures) work correctly, the parsing components that convert strings to Abstract Syntax Trees (ASTs) are broken. This PRD outlines the specific issues and requirements to make gf-core a fully functional GF runtime library.

## Problem Statement

### Current State Analysis

**Working Components:**
- ✅ PGF binary file loading and parsing
- ✅ PGF to JSON conversion
- ✅ Runtime grammar structure creation
- ✅ Linearization framework architecture
- ✅ Debug output and logging

**Critical Issues:**
- ❌ Natural language parsing via `ConcreteGrammar::parse_string()` fails completely
- ❌ Abstract syntax parsing via `AbstractGrammar::parse_tree()` creates malformed ASTs
- ❌ End-to-end parsing → linearization pipeline non-functional
- ❌ No successful round-trip: text → AST → text

### Impact

These issues render gf-core unusable for its primary intended purposes:
- Natural language processing with GF grammars
- Multi-language translation through abstract syntax
- Grammar-based text generation and validation
- Integration with existing GF ecosystem

## Goals and Objectives

### Primary Goals
1. **Fix Natural Language Parsing**: Implement working `parse_string()` functionality
2. **Fix Abstract Syntax Parsing**: Implement working `parse_tree()` functionality  
3. **Ensure End-to-End Functionality**: Complete parsing → linearization pipeline
4. **Maintain Compatibility**: Preserve existing working components

### Success Criteria
- Natural language sentences parse correctly according to grammar rules
- Abstract syntax strings parse into proper nested AST structures
- Linearization produces correct output from parsed ASTs
- All examples in `/examples/` directory work without errors
- Round-trip parsing maintains semantic equivalence

## Functional Requirements

### 1. Natural Language Parsing (`ConcreteGrammar::parse_string`)

**Current Issue:** Parsing fails immediately at first token
```
[DEBUG] Parsing failed at token 'this'
```

**Requirements:**
- **FR-1.1**: Parse tokenized natural language input according to concrete grammar production rules
- **FR-1.2**: Handle multiple parse trees when input is ambiguous
- **FR-1.3**: Return empty vector when no valid parse exists (don't crash)
- **FR-1.4**: Support all symbol types: SymKS (keywords), SymCat (categories), SymLit (literals)
- **FR-1.5**: Implement proper chart parsing or similar algorithm for context-free grammar parsing
- **FR-1.6**: Debug output should show parsing steps, not just failure points

**Test Cases:**
```rust
// These should succeed with Food grammar
eng_concrete.parse_string("this fish is delicious", "Phrase") → [AST]
eng_concrete.parse_string("that wine is expensive", "Phrase") → [AST] 
eng_concrete.parse_string("cheese is boring", "Phrase") → [AST]

// These should return empty (but not crash)
eng_concrete.parse_string("hello world", "Phrase") → []
eng_concrete.parse_string("", "Phrase") → []
```

### 2. Abstract Syntax Parsing (`AbstractGrammar::parse_tree`)

**Current Issue:** Flat parsing instead of nested structure
```
// Input: "Is(This(Fish), Delicious)"
// Current (Wrong): Fun { name: "Is", args: [Fun{name:"This"}, Fun{name:"Fish"}, Fun{name:","}...] }
// Required (Right): Fun { name: "Is", args: [Fun{name:"This", args:[Fun{name:"Fish"}]}, Fun{name:"Delicious"}] }
```

**Requirements:**
- **FR-2.1**: Parse nested function call syntax `Function(Arg1, Arg2, ...)`
- **FR-2.2**: Handle zero-argument functions `Function()`
- **FR-2.3**: Support deeply nested structures `Is(This(Very(Fresh(Fish))), Delicious)`
- **FR-2.4**: Validate function arities against abstract grammar definitions
- **FR-2.5**: Return proper error messages for malformed syntax
- **FR-2.6**: Support whitespace tolerance

**Test Cases:**
```rust
// Simple functions
parse_tree("Fish", None) → Fun { name: "Fish", args: [] }
parse_tree("This(Fish)", None) → Fun { name: "This", args: [Fun { name: "Fish", args: [] }] }

// Complex nested
parse_tree("Is(This(Fish), Delicious)", None) → properly nested AST

// Error cases
parse_tree("Is(Fish)", None) → Error (wrong arity)
parse_tree("Is(This(Fish), Delicious, Extra)", None) → Error (wrong arity)
```

### 3. Linearization Integration

**Requirements:**
- **FR-3.1**: Linearization must work with ASTs from both parsing methods
- **FR-3.2**: Support all sequence types: SymKS, SymCat, SymLit
- **FR-3.3**: Generate proper debug output during linearization process
- **FR-3.4**: Handle linearization failures gracefully (return empty string with debug info)

### 4. Round-Trip Validation

**Requirements:**
- **FR-4.1**: Natural language → AST → Natural language should preserve meaning
- **FR-4.2**: Abstract syntax → AST → Abstract syntax should be identical
- **FR-4.3**: Cross-language linearization should work (AST from English → French output)

## Technical Requirements

### 1. Parser Implementation

**TR-1.1**: Implement proper recursive descent or chart parsing algorithm
**TR-1.2**: Use the PGF production rules correctly to guide parsing
**TR-1.3**: Handle category-to-function-ID mapping correctly (fid references)
**TR-1.4**: Implement proper token matching for SymKS elements

### 2. AST Construction  

**TR-2.1**: Build proper nested Fun structures with correct argument lists
**TR-2.2**: Validate function names against abstract grammar function definitions
**TR-2.3**: Type checking: ensure function categories match expected types
**TR-2.4**: Memory-efficient AST representation

### 3. Error Handling

**TR-3.1**: Comprehensive error types for different parsing failures
**TR-3.2**: Useful error messages with position information
**TR-3.3**: No panics or crashes on invalid input
**TR-3.4**: Debug output configurable via `set_debug()` function

### 4. Performance

**TR-4.1**: Parsing should scale reasonably with input length
**TR-4.2**: Memory usage should be reasonable for typical grammar sizes
**TR-4.3**: No memory leaks in parsing operations

## Implementation Plan

### Phase 1: Fix Abstract Syntax Parsing (2-3 weeks)
1. Implement proper recursive descent parser for function call syntax
2. Add parentheses, comma, and argument parsing
3. Validate against abstract grammar function definitions
4. Add comprehensive test suite

### Phase 2: Fix Natural Language Parsing (3-4 weeks)  
1. Research and implement chart parsing or similar CFG parsing algorithm
2. Correctly interpret PGF production rules and sequences
3. Handle symbol types (SymKS, SymCat, SymLit) properly
4. Add tokenization and token matching logic

### Phase 3: Integration and Testing (1-2 weeks)
1. Ensure linearization works with fixed parsing
2. Add round-trip tests
3. Update all examples to demonstrate working functionality
4. Performance testing and optimization

### Phase 4: Documentation and Polish (1 week)
1. Update API documentation
2. Add usage examples and tutorials
3. Error message improvements
4. Final testing across different grammars

## Success Metrics

### Functional Metrics
- [ ] All examples in `/examples/` run without errors
- [ ] At least 10 different natural language sentences parse correctly for Food grammar
- [ ] At least 10 different abstract syntax expressions parse correctly
- [ ] Round-trip parsing maintains semantic equivalence in 95%+ of test cases

### Quality Metrics
- [ ] No memory leaks in parsing operations
- [ ] Parsing operations complete in reasonable time (<100ms for typical sentences)
- [ ] Comprehensive error messages for all failure modes
- [ ] 90%+ test coverage for parsing components

### Integration Metrics
- [ ] Works with existing PGF files from GF ecosystem
- [ ] Compatible with JSON export/import workflow
- [ ] Debug output provides useful information for troubleshooting

## Dependencies and Constraints

### Dependencies
- Existing PGF loading and JSON conversion functionality must remain intact
- Current grammar data structures should not require major changes
- Backward compatibility with existing API surface

### Constraints
- Must work with existing GF-generated PGF files
- Should not break existing integrations
- Memory usage should remain reasonable
- Performance should not regress significantly

## Risk Assessment

### High Risk
- **Complex parsing algorithms**: Chart parsing can be complex to implement correctly
- **PGF format interpretation**: Incorrect understanding of PGF structure could cause issues

### Medium Risk  
- **Performance regression**: New parsing algorithms might be slower
- **Memory usage**: Parser state might require significant memory

### Mitigation Strategies
- Start with simpler recursive descent parsing before attempting chart parsing
- Extensive testing with various PGF grammars
- Performance benchmarking throughout development
- Incremental development with frequent testing

## Conclusion

Fixing the parsing components in gf-core is essential to make it a functional GF runtime library. The issues are well-understood and the solutions are achievable with proper parser implementation. Success in this effort will make gf-core a valuable tool for the GF ecosystem and Rust-based natural language processing applications.
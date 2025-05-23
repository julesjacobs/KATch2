# KATch2 Exercise System Implementation Plan

## Overview
Implement an interactive exercise system where users write NetKAT programs to match target expressions, with live equivalence checking and counterexample display.

## Backend/Rust Changes

### 1. Unified Difference Analysis (`src/`)
- [ ] **Single difference analysis function**
  - Implement `analyze_difference(expr1: &str, expr2: &str, max_traces: usize) -> DifferenceResult`
  - Computes `expr1 - expr2` and returns example traces or syntax errors
  - Returns `DifferenceResult { expr1_errors, expr2_errors, example_traces: Vec<Trace> }`
  - When no syntax errors and `example_traces` is empty → expressions are equivalent (for this direction), i.e., difference expr1 - expr2 is empty.

- [ ] **Simple structures**
  ```rust
  struct Trace {
      packets: Vec<Packet>, // Sequence of packets in the trace
  }
  ```

- [ ] **WASM Export**
  - Export `analyze_difference(expr1: &str, expr2: &str, max_traces: usize)` to JavaScript
  - Keep existing `analyze_expression(expr: &str)` for backward compatibility
  - Handle all error cases and serialization properly

## Frontend/JavaScript Changes

### 3. Exercise Enhancement (`ui/katch2ui/katch2-editor.js`)

- [ ] **Enhanced `<netkat>` Element with Exercise Mode**
  ```html
  <!-- Basic exercise -->
  <netkat exercise="Write a program that sets x0 to 1">
    x0 := 1
  </netkat>
  
  <!-- Exercise with target editor -->
  <netkat exercise="Basic assignment exercise" target="main-editor">
    x0 := 1
  </netkat>
  ```

- [ ] **New Exercise Attributes**
  - `exercise`: Exercise description/instruction (enables exercise mode)
  - Target expression comes from element content (like regular `<netkat>` elements)
  - `target`: Optional target editor ID for loading exercises (reused from existing functionality)
  - `lines`: Height control (existing)
  - `line-numbers`: Show line numbers option (existing)

### 4. Exercise Editor Interface

- [ ] **Exercise Editor Layout**
  ```
  ┌─────────────────────────────────┐
  │ Exercise Description            │
  ├─────────────────────────────────┤
  │ [Monaco Editor for user code]   │
  ├─────────────────────────────────┤
  │ ✓ Equivalent / ✗ Not Equivalent │
  │ ❌ Missing: [x0=0] → [x0=1]     │
  │ ➕ Extra: [x0=1] → (dropped)    │
  └─────────────────────────────────┤
  ```

- [ ] **Live Equivalence Checking (JavaScript Logic)**
  - Call `analyze_difference(target, user, 3)` to get missing traces
  - Call `analyze_difference(user, target, 3)` to get extra traces  
  - Equivalent when both calls return empty `example_traces` and no `syntax_errors`
  - Display visual indicators and counterexamples based on results

- [ ] **Success/Failure States**
  - Green border + checkmark when equivalent
  - Red border + X when not equivalent  
  - Neutral border when empty/analyzing
  - Success animation when user solves exercise

### 5. Exercise Loading System

- [ ] **Exercise Loader Button**
  - Similar to "Analyze →" button but labeled "Try Exercise →"
  - Loads exercise description and target into target editor
  - Target editor becomes exercise mode automatically

- [ ] **Exercise Mode for Target Editors**
  - When loaded with exercise, target editor shows:
    - Exercise description at top
    - User input area
    - Live equivalence feedback
    - Target expression hidden (stored internally)

### 6. Counterexample Display

- [ ] **Trace Visualization**
  ```
  Your solution differs from the target:
  
  ❌ Missing traces (target accepts, but you don't):
     📝 [x0=0,x1=0] → [x0=1,x1=0] → [x0=1,x1=1]
     📝 [x0=1,x1=1] → [x0=0,x1=1]
  
  ➕ Extra traces (you accept, but target doesn't):
     📝 [x0=0,x1=0] → [x0=0,x1=1] → [x0=1,x1=1]
     📝 [x0=1,x1=0] → (dropped)
  ```

## UI/UX Enhancements

### 7. Exercise Styling
- [ ] **Exercise Theme**
  - Distinct styling from regular editors
  - Exercise description area with light blue background
  - Status indicators with colors (green/red/neutral)

- [ ] **Interactive Elements**
  - "Show Solution" button (optional, for learning)

### 8. Example Documentation

- [ ] **Update README with Exercise Examples**
  ```html
  <h2>Exercise: Basic Assignment</h2>
  <p>Write a program that assigns 1 to field x0:</p>
  <netkat exercise="Your goal is to create a program that sets x0 to 1">
    x0 := 1
  </netkat>
  
  <h2>Interactive Exercise Loading</h2>
  <div style="display: grid; grid-template-columns: 1fr 1fr; gap: 20px;">
    <div>
      <h3>Exercises:</h3>
      <netkat exercise="Basic assignment exercise" target="exercise-editor">
        x0 := 1
      </netkat>
      
      <netkat exercise="Sequential composition exercise" target="exercise-editor">
        x0 := 0; x1 := 1
      </netkat>
    </div>
    
    <div>
      <h3>Solve Here:</h3>
      <netkat id="exercise-editor" lines="6">
        // Exercises will load here
      </netkat>
    </div>
  </div>
  ```
---
name: implement
description: Executes tasks sequentially from `Tasks.md`, validates edits using local tools, spins up an isolated sub-agent reviewer, and compresses context upon task completion.
---

## 1. Orchestrator / Implementer Protocol

### Step 1: Read Task
1. Read `TASKS.md`. Locate the first pending task marked `[ ]`.
2. Extract the exact requirement and acceptance criteria.
3. Set your internal target state to this specific task.

### Step 2: Implement & Self-Validate
1. Implement the required changes in code.
2. **Deterministic Check Gate:** Run local test suites, compilers, or linters via terminal tools.
   - If tests fail, fix the code immediately. Do NOT spawn a reviewer until local tests/builds pass cleanly.

### Step 3: Invoke Reviewer Sub-Agent
Once tests pass, invoke an isolated **Reviewer Sub-Agent** in a fresh context window.

Pass **ONLY** the following context payload to the reviewer:
- **Task Requirement:** The exact text/spec from `Tasks.md`.
- **Git Diff:** The output of `git diff` for the changes made.

---

## 2. Reviewer Sub-Agent Prompt & Protocol

> **System Prompt for Reviewer Sub-Agent:**
>
> You are a hyper-critical, read-only code reviewer. Your job is to verify that the proposed `git diff` satisfies the given task requirements without introducing regressions, syntax errors, or bad practices.
>
> **Constraints:**
> - You have NO file-write or execution tools.
> - Focus strictly on requirement completeness, logical edge cases, and safety.
> - Do not praise good work. Be direct and precise.
>
> **Output Format Requirement:**
> You MUST begin your response with exactly one status header on line 1, followed by a brief bulleted breakdown.
>
> Format on PASS:
> ```
> STATUS: PASSED
> - Reason/Verification summary
> ```
>
> Format on REJECT:
> ```
> STATUS: REJECTED
> - [Issue 1]: Specific actionable fix required
> - [Issue 2]: Specific actionable fix required
> ```

---

## 3. Loop Handling & State Transitions

Upon receiving the Reviewer Sub-Agent's response:

### Case A: Reviewer Returns `STATUS: PASSED`
1. Mark the current task as completed (`[x]`) in `Tasks.md`.
2. Commit changes if required by workspace rules (`git commit`).
3. **Compress Context:** Execute `pi-blackhole` (or run your context compression routine) to purge execution noise.
4. Proceed to **Step 1** for the next pending task.

### Case B: Reviewer Returns `STATUS: REJECTED`
1. Read the bulleted issue list.
2. Apply the necessary code fixes.
3. Rerun deterministic test/linter checks.
4. Increment your review attempt counter for this task.
5. Invoke the Reviewer Sub-Agent again (max **2 review cycles** per task).

### Case C: Circuit Breaker (2 Rejections)
If the reviewer rejects the implementation a **second time**:
1. Do NOT attempt another edit.
2. Update `Tasks.md` to flag the task as blocked: `[ ] Task name <!-- [NEEDS_HUMAN_REVIEW]: Reason -->`.
3. Compress context via `pi-blackhole`.
4. Skip to the next independent task in `Tasks.md` or inform the user.

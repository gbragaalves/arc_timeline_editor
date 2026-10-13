## Test budget
- Default for any change: parse-check, the translations check, the unit tests, and ONE browser run (PT, 1366×768) of only the feature that changed.
- Full browser matrix (768 and 650 px × PT and EN) and the regression suites for other features (lasso, pins/splices, period, stations): only when I ask for it, or when I say "pre-merge".
- Inventories (September or 2026): only when the routing profiles, the matching, or the review metrics change. Never for UI-only changes.
- If a test fails, fix it and re-run only that test.
- Aim for under 30 minutes per task. If a task will clearly take longer, say so at the start, with a rough estimate, and ask whether to cut scope.
- End every report with the time spent per phase (code / tests / debugging).

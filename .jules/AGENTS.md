# Jules / Agent Knowledge Base

## File Hygiene

*   **Do not commit temporary files.** Avoid adding files like `build_output.txt`, `run.txt`, `report.txt`, or any other logs to the repository.
*   **Do not commit build artifacts.** Executables (e.g., `chess-new`, `bench-search`) and object files should be ignored.
*   **Update .gitignore.** If you generate a new type of artifact that is not yet ignored, update `.gitignore` immediately.
*   **Clean up.** Remove any temporary scripts (like `install_ghcup.sh`) or proposal documents (`*_PROPOSAL.md`) once they are no longer needed or have been incorporated into the main documentation.
*   **Documentation.** Keep `ARCHITECTURE.md` and `README.md` as the source of truth. Avoid proliferating `PROPOSAL.md` files unless they are active RFCs.
